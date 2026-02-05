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
