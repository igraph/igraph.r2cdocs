#' @importFrom roxygen2 roclet
#' @export

docs_rd <- function() {
  roclet(c("docs_rd", "rd"))
}

#' @importFrom roxygen2 roclet_process
#' @export

roclet_process.roclet_docs_rd <- function(x, blocks, env, base_path) {
  NextMethod()
}

#' @importFrom roxygen2 roclet_output
#' @export

roclet_output.roclet_docs_rd <- function(x, results, base_path, ...) {
  results <- lapply_with_names(results, add_cdocs_section, base_path)
  update_pkgdown(results, base_path)
  NextMethod()
}

# https://github.com/gaborcsardi/roxygenlabs/blob/5a7e4449c28f9e423de3f4e8e1be9bc9080f4e52/R/themed-rd.R#L26
lapply_with_names <- function(X, FUN, ...) {
  structure(
    lapply(X, FUN, ...),
    names = names(X) %||% (if (is.character(X)) X)
  )
}

add_cdocs_section <- function(topic, base_path) {
  alias <- topic$get_value("alias")
  impl <- purrr::map(alias, find_impl, base_path = base_path) |> unlist()

  if (length(impl) == 0) {
    return(topic)
  }

  impl <- sub("_impl", "", impl)
  impl <- sprintf("igraph_%s", impl)

  content <- present_cdocs_link(impl, base_path)

  # Only add section if there are actual links
  if (content == "") {
    return(topic)
  }

  topic$add_section(roxygen2::rd_section(
    "section",
    list(
      title = "Related documentation in the C library",
      content = content
    )
  ))

  topic
}

present_cdocs_link <- function(value, base_path) {
  strings <- map_chr(
    unique(value),
    format_cdocs_single_link,
    base_path
  )

  # Filter out empty strings
  strings <- strings[strings != ""]

  paste(strings, collapse = ", ")
}


format_cdocs_single_link <- function(x, base_path) {
  # special case
  if (endsWith(x, "motifs_randesu_callback_closure")) {
    url <- sprintf(
      "https://igraph.org/c/html/%s/igraph-Motifs.html#igraph_motifs_randesu_callback",
      igraph_c_version(base_path)
    )

    return(
      sprintf("\\href{%s}{\\code{motifs_randesu_callback_closure()}}", url)
    )
  }

  clinks <- c_links(base_path)

  # we can use igraph_ or not
  df <- clinks[clinks$method %in% c(x, sprintf("igraph_%s", x)), ]
  if (nrow(df) == 0) {
    cli::cli_warn("Can't find C entry for {x}!")
    return("")
  }
  sprintf("\\href{%s}{\\code{%s()}}", df$url, sub("igraph_", "", df$method))
}

find_impl <- function(topic, base_path) {
  treesitter_graph <- treesitter_graph(base_path)

  # Get vertex names properly
  vertex_names <- igraph::V(treesitter_graph)$name
  vertex <- which(vertex_names == topic)

  # If topic not found, return empty character vector
  if (length(vertex) == 0) {
    return(character(0))
  }

  subcomponent <- names(igraph::subcomponent(
    treesitter_graph,
    vertex,
    mode = "out"
  ))
  subcomponent[endsWith(subcomponent, "_impl")]
}


.treesitter_graph <- function(base_path) {
  all_r_scripts <- fs::dir_ls(fs::path(base_path, "R"), glob = "*R")
  all_r_code <- purrr::map_chr(
    all_r_scripts,
    \(x) readLines(x) |> paste(collapse = "\n")
  ) |>
    paste(collapse = "\n")

  r_language <- treesitter.r::language()
  r_parser <- treesitter::parser(language = r_language)
  tree <- treesitter::parser_parse(r_parser, all_r_code)

  # Query to find all function definitions with their names
  func_def_query <- treesitter::query(
    r_language,
    '(binary_operator
      lhs: (identifier) @func_name
      rhs: (function_definition) @func_def
    )'
  )

  # Extract all function definitions
  root_node <- treesitter::tree_root_node(tree)
  all_matches <- treesitter::query_matches(func_def_query, root_node)[[1]]

  # Extract function names and definition nodes from matches
  # Each match contains paired func_name and func_def captures
  func_names <- purrr::map_chr(all_matches, \(m) {
    treesitter::node_text(m$node[[which(m$name == "func_name")]])
  })
  func_def_nodes <- purrr::map(all_matches, \(m) {
    m$node[[which(m$name == "func_def")]]
  })

  # Query to find all function calls
  func_call_query <- treesitter::query(
    r_language,
    '(call
      function: (identifier) @call_name
    )'
  )

  # Build the call graph table
  call_graph_df <- purrr::map2_dfr(
    func_def_nodes,
    func_names,
    \(func_def_node, func_name) {
      # Query for function calls within this function definition
      calls <- treesitter::query_captures(func_call_query, func_def_node)

      if (length(calls$node) > 0) {
        call_names <- purrr::map_chr(calls$node, treesitter::node_text)
        data.frame(
          from = func_name,
          to = call_names,
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(from = character(), to = character())
      }
    }
  )

  igraph::graph_from_data_frame(call_graph_df)
}

.igraph_c_version <- function(base_path) {
  c_lines <- readLines(file.path(
    base_path,
    "src",
    "vendor",
    "igraph_version.h"
  ))
  version_line <- c_lines[startsWith(c_lines, "#define IGRAPH_VERSION ")]

  regmatches(version_line, regexec('"([^"]+)"', version_line))[[1]][2]
}

.c_links <- function(base_path) {
  igraph_version <- igraph_c_version(base_path)

  local_cache_dir <- file.path(
    tools::R_user_dir("igraph", "data"),
    "links",
    igraph_version
  )
  local_cache <- file.path(local_cache_dir, "clinks.csv")
  if (file.exists(local_cache)) {
    return(read.csv(local_cache))
  }

  dir.create(local_cache_dir, recursive = TRUE)

  index_url <- sprintf("https://igraph.org/c/html/%s/ix01.html", igraph_version)
  index <- xml2::read_html(index_url)

  entries <- xml2::xml_find_all(index, ".//dt")

  clinks <- purrr::map_df(entries, handle_dt)
  write.csv(clinks, local_cache, row.names = FALSE)
  clinks
}

handle_dt <- function(dt) {
  href <- xml2::xml_attr(xml2::xml_child(dt), "href")
  url <- sprintf("https://igraph.org/c/html/%s/%s", igraph_version, href)
  method <- sub(".*#", "", href)
  tibble::tibble(
    method = method,
    url = url
  )
}

update_pkgdown <- function(results, base_path) {
  categories_path <- file.path(
    base_path,
    "src/vendor/cigraph/interfaces/functions-categories.yaml"
  )

  categories <- yaml::read_yaml(categories_path)

  # Build call graph once (memoized)
  ts_graph <- treesitter_graph(base_path)
  vertex_names <- igraph::V(ts_graph)$name

  # Map each Rd topic to its C category (or "other" if not mappable)
  topic_categories <- list()

  for (rd_name in names(results)) {
    topic <- results[[rd_name]]

    # Skip internal topics and package/data doc pages
    keywords <- topic$get_value("keyword")
    if ("internal" %in% keywords) next
    doc_type <- topic$get_value("docType")
    if (length(doc_type) > 0 && doc_type %in% c("package", "data")) next

    # Use the primary alias (\name{} in Rd) not the file name, since @rdname
    # can cause the file to be named differently from the actual function.
    r_name <- topic$get_value("alias")[[1]]
    aliases <- topic$get_value("alias")

    # Use only direct neighbors (distance 1) to identify the primary C function,
    # rather than the full transitive closure used for Rd link sections.
    direct_impls <- purrr::map(aliases, function(alias) {
      v <- which(vertex_names == alias)
      if (length(v) == 0) return(character(0))
      nb <- names(igraph::neighbors(ts_graph, v, mode = "out"))
      nb[endsWith(nb, "_impl")]
    }) |> unlist()

    if (length(direct_impls) == 0) {
      topic_categories[[r_name]] <- list(category = "other", subcategory = NA_character_)
      next
    }

    c_funcs <- sprintf("igraph_%s", sub("_impl$", "", direct_impls))

    cats <- purrr::map_chr(c_funcs, function(f) {
      entry <- categories[[f]]
      if (!is.null(entry)) entry[["category"]] else NA_character_
    })
    cats <- unique(cats[!is.na(cats)])

    if (length(cats) == 0) {
      topic_categories[[r_name]] <- list(category = "other", subcategory = NA_character_)
      next
    }

    subcats <- purrr::map_chr(c_funcs, function(f) {
      entry <- categories[[f]]
      if (!is.null(entry)) entry[["subcategory"]] %||% NA_character_ else NA_character_
    })
    subcats <- unique(subcats[!is.na(subcats)])

    topic_categories[[r_name]] <- list(
      category = cats[[1]],
      subcategory = if (length(subcats) > 0) subcats[[1]] else NA_character_
    )
  }

  if (length(topic_categories) == 0) {
    cli::cli_warn(
      "No R topics could be mapped to C categories; skipping {.path _pkgdown.yml} update."
    )
    return(invisible(NULL))
  }

  # Build category → subcategory → [r_names] structure
  by_cat <- list()
  for (r_name in names(topic_categories)) {
    info <- topic_categories[[r_name]]
    cat <- info$category
    sub <- info$subcategory %||% NA_character_
    if (is.null(by_cat[[cat]])) {
      by_cat[[cat]] <- list()
    }
    sub_key <- if (is.na(sub)) ".nosub" else sub
    by_cat[[cat]][[sub_key]] <- c(by_cat[[cat]][[sub_key]], r_name)
  }

  # Generate YAML lines
  start_marker <- "# BEGIN GENERATED BY igraph.r2cdocs - DO NOT EDIT"
  end_marker <- "# END GENERATED BY igraph.r2cdocs"

  gen_lines <- c(start_marker, "reference:")
  cat_names <- c(sort(setdiff(names(by_cat), "other")), intersect("other", names(by_cat)))
  for (cat_name in cat_names) {
    gen_lines <- c(gen_lines, sprintf("- title: \"%s\"", cat_name))
    subs <- by_cat[[cat_name]]
    for (sub_name in sort(names(subs))) {
      funcs <- sort(subs[[sub_name]])
      if (sub_name != ".nosub") {
        gen_lines <- c(gen_lines, sprintf("- subtitle: \"%s\"", sub_name))
      }
      gen_lines <- c(gen_lines, "- contents:", sprintf("  - %s", funcs))
    }
  }
  gen_lines <- c(gen_lines, end_marker)

  # Read and update _pkgdown.yml
  pkgdown_path <- file.path(base_path, "_pkgdown.yml")
  if (!file.exists(pkgdown_path)) {
    cli::cli_warn(
      "{.path _pkgdown.yml} not found at {.path {pkgdown_path}}, skipping."
    )
    return(invisible(NULL))
  }

  lines <- readLines(pkgdown_path)
  start_idx <- which(lines == start_marker)
  end_idx <- which(lines == end_marker)

  if (length(start_idx) == 1 && length(end_idx) == 1) {
    # Replace existing generated block
    new_lines <- c(
      lines[seq_len(start_idx - 1)],
      gen_lines,
      lines[seq(end_idx + 1, length(lines))]
    )
  } else {
    # First run: replace the existing `reference:` section.
    # Find the `reference:` line and the next top-level key after it.
    ref_idx <- which(lines == "reference:")
    if (length(ref_idx) == 0) {
      cli::cli_warn("No {.code reference:} key found in {.path _pkgdown.yml}, skipping.")
      return(invisible(NULL))
    }
    # The next top-level key is the first line after reference: that starts
    # with a non-space character and is not a YAML list item.
    after_ref <- seq(ref_idx[[1]] + 1L, length(lines))
    next_key_idx <- after_ref[grepl("^[a-zA-Z]", lines[after_ref])][[1]]
    new_lines <- c(
      lines[seq_len(ref_idx[[1]] - 1L)],
      gen_lines,
      "",
      lines[seq(next_key_idx, length(lines))]
    )
  }

  writeLines(new_lines, pkgdown_path)
  cli::cli_inform(
    "Updated {.path _pkgdown.yml}: {length(topic_categories)} R function{?s} across {length(by_cat)} categor{?y/ies}."
  )
}
