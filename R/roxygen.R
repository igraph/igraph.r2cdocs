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
    df <- clinks[clinks$method %in% c(x, sprintf("igraph_%s", x)), ]
    if (nrow(df) == 0) {
      cli::cli_warn("Can't find C entry for {x}!")
    }
    sprintf("\\href{%s}{\\code{%s()}}", df$url, sub("igraph_", "", df$method))
  }

  strings <- map_chr(unique(value), format_cdocs_single_link, clinks = clinks())

  paste(strings, collapse = ", ")
}

clinks <- function() {
  read.csv(system.file("clinks.csv", package = "igraph.r2cdocs"))
}
