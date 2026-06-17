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
#' @keywords internal
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


#' Return a topological ordering of vertex indices (1-based).
#' @keywords internal
#' @noRd
.topo_order <- function(g) {
  igraph::topo_sort(g, mode = "out")
}


#' Identify source vertices (in-degree 0) and sink vertices (out-degree 0).
#' Returns a list with integer vectors \code{sources} and \code{sinks}.
#' @keywords internal
#' @noRd
.sources_sinks <- function(g) {
  indeg  <- igraph::degree(g, mode = "in")
  outdeg <- igraph::degree(g, mode = "out")
  list(
    sources = which(indeg  == 0L),
    sinks   = which(outdeg == 0L)
  )
}


#' @keywords internal
#' Compute the forward/backward path-count vectors shared by
#' [traversal_weights()], [edge_weights()], and [node_weights()].
#'
#' \code{f[v]}  = # paths from any global source to \code{v}.
#' \code{fa[v]} = 1 + sum(fa[u] for u -> v); paths from any ancestor of
#'   \code{v} (including \code{v} itself).
#' \code{b[v]}  = # paths from \code{v} to any global sink.
#' \code{ba[v]} = 1 + sum(ba[w] for v -> w); paths from \code{v} to any
#'   descendant (including \code{v} itself).
#'
#' @param g An \code{igraph} DAG (already validated by \code{.to_dag()}).
#' @return A list with numeric vectors \code{f}, \code{fa}, \code{b}, \code{ba},
#'   each indexed by vertex id.
#' @noRd
.path_counts <- function(g) {
  n   <- igraph::vcount(g)
  ord <- as.integer(.topo_order(g))
  ss  <- .sources_sinks(g)

  f  <- numeric(n)
  fa <- numeric(n)
  f[ss$sources] <- 1.0

  for (v in ord) {
    preds <- as.integer(igraph::neighbors(g, v, mode = "in"))
    fa[v] <- 1.0 + sum(fa[preds])
    if (length(preds) == 0L) next
    f[v]  <- sum(f[preds])
  }

  b  <- numeric(n)
  ba <- numeric(n)
  b[ss$sinks] <- 1.0

  for (v in rev(ord)) {
    succs <- as.integer(igraph::neighbors(g, v, mode = "out"))
    ba[v] <- 1.0 + sum(ba[succs])
    if (length(succs) == 0L) next
    b[v]  <- sum(b[succs])
  }

  list(f = f, fa = fa, b = b, ba = ba)
}
