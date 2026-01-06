#' @importFrom roxygen2 roclet
#' @export

igraph.r2cdocs_rd <- function() {
  roclet(c("igraph.r2cdocs_rd", "rd"))
}

#' @importFrom roxygen2 roclet_process
#' @export

roclet_process.roclet_igraph.r2cdocs_rd <- function(x, blocks, env, base_path) {
  NextMethod()
}

#' @importFrom roxygen2 roclet_output
#' @export

roclet_output.roclet_igraph.r2cdocs_rd <- function(x, results, base_path, ...) {
  results <- lapply_with_names(results, add_cdocs, base_path)
  NextMethod()
}

lapply_with_names <- function(X, FUN, ...) {
  structure(
    lapply(X, FUN, ...),
    names = names(X) %||% (if (is.character(X)) X)
  )
}
