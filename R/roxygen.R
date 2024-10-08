#' @importFrom roxygen2 roxy_tag_parse
#' @importFrom roxygen2 roxy_tag_rd
NULL

#' @export
roxy_tag_parse.roxy_tag_cdocs <- function(x) {
  roxygen2::tag_words(x)
}

#' @export
roxy_tag_rd.roxy_tag_cdocs <- function(x, base_path, env) {
  roxygen2::rd_section("cdocs", x$val)
}

#' @export
format.rd_section_cdocs <- function(x, ...) {
  paste0(
    "\\section{Related documentation in the C library}{",
  present_cdocs_link(x[["value"]]),
    ".}\n"
  )
}

present_cdocs_link <- function(value) {

  format_cdocs_single_link <- function(x, clinks) {
  # we can use igraph_ or not
    df <- clinks[clinks$method %in% c(x, sprintf("igraph_%s", x)),]
    if (nrow(df) == 0) {
      cli::cli_warn("Can't find C entry for {x}!")
    }
    sprintf("\\href{%s}{\\code{%s()}}", df$url, df$method)
  }

  strings <- map_chr(unique(value), format_cdocs_single_link, clinks = clinks())

  paste(strings, collapse = ", ")
}


#' @export
cdocs_roclet <- function() {
  roxygen2::roclet("cdocs")
}

#' @importFrom roxygen2 block_get_tags roclet_process
#' @method roclet_process roclet_cdocs
#' @export
roclet_process.roclet_cdocs <- function(x, blocks, env, base_path) {
  x
}


#' @export
#' @importFrom roxygen2 block_get_tags roclet_output
roclet_output.roclet_cdocs <- function(x, results, base_path, ...) {
  x
}

clinks <- function() {
  read.csv(system.file("clinks.csv", package = "igraph.r2cdocs"))
}
