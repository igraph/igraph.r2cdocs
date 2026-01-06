#' @importFrom roxygen2 roxy_tag

add_cdocs <- function(block, base_path) {
  alias <- as.character(block$sections$alias)[2]
  impl <- find_impl(alias, base_path = base_path)
  impl <- sub("_impl", "", impl)
  impl <- sprintf("igraph_%s", impl)
  browser()
  block$add_section(roxygen2::rd_section(
    "section",
    list(
      title = "Related documentation in the C library",
      content = present_cdocs_link(impl)
    )
  ))
  block
}

find_impl <- function(topic, base_path) {
  ctags_data <- ctags_data(base_path)
  ctags_network <- ctags_data[["network"]]

  ctags_graph <- igraph::graph_from_data_frame(ctags_network[, c("from", "to")])
  vertex <- which(
    purrr::map_chr(igraph::V(ctags_graph), names) == topic
  )
  subcomponent <- names(igraph::subcomponent(ctags_graph, vertex, mode = "out"))
  subcomponent[endsWith(subcomponent, "_impl")]
}

.ctags_data <- function(base_path) {
  pkgstats::tags_data(base_path)
}

ctags_data <- memoise::memoise(.ctags_data)
