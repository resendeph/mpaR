#' @keywords internal
#' Convert input to an igraph DAG, attaching a standardised vertex name attribute.
#'
#' Accepts either an \pkg{igraph} graph object or a data frame with at least two
#' columns (first two columns = \code{from}, \code{to}).  Additional columns are
#' treated as edge attributes.
#'
#' @param x An \code{igraph} object or a data frame / matrix.
#' @param directed Logical; force directed graph when coercing from data frame.
#' @return A directed \code{igraph} object whose vertices carry a
#'   \code{"name"} attribute.
#' @noRd
.to_dag <- function(x, directed = TRUE) {
  if (inherits(x, "igraph")) {
    g <- x
  } else if (is.data.frame(x) || is.matrix(x)) {
    df <- as.data.frame(x)
    if (ncol(df) < 2L) {
      rlang::abort("`x` must have at least two columns (from, to).")
    }
    g <- igraph::graph_from_data_frame(df, directed = directed)
  } else {
    rlang::abort(
      paste0(
        "`x` must be an igraph object or a data frame / matrix, ",
        "not <", class(x)[1L], ">."
      )
    )
  }

  if (!igraph::is_directed(g)) {
    rlang::abort("The graph must be directed.")
  }
  if (!igraph::is_dag(g)) {
    rlang::abort(
      "The graph must be a directed acyclic graph (DAG). ",
      "Check for cycles with `igraph::is_dag()`."
    )
  }

  # Ensure vertex names exist
  if (is.null(igraph::V(g)$name)) {
    igraph::V(g)$name <- as.character(seq_len(igraph::vcount(g)))
  }

  g
}


#' @keywords internal
#' Return a topological ordering of vertex indices (1-based).
#' @noRd
.topo_order <- function(g) {
  igraph::topo_sort(g, mode = "out")
}


#' @keywords internal
#' Identify source vertices (in-degree 0) and sink vertices (out-degree 0).
#' Returns a list with integer vectors \code{sources} and \code{sinks}.
#' @noRd
.sources_sinks <- function(g) {
  indeg  <- igraph::degree(g, mode = "in")
  outdeg <- igraph::degree(g, mode = "out")
  list(
    sources = which(indeg  == 0L),
    sinks   = which(outdeg == 0L)
  )
}
