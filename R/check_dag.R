#' Check DAG structure
#'
#' @description
#' Validates that a directed graph is a directed acyclic graph (DAG). If cycles
#' are present, the function identifies the back-edges responsible and reports
#' them so the user can correct the underlying network data. 
#'
#' Cycles in citation or patent networks typically indicate data-entry errors,
#' self-citations, or mutual-citation pairs. Because silently dropping edges
#' would distort the network topology, it is necessary to review and correct the source
#' data before proceeding.
#'
#' @param x An \code{igraph} object or a data frame / matrix with edge-list
#'   columns (\code{from}, \code{to}).
#' @param verbose Logical. If \code{TRUE} (default), print a summary of the
#'   check result to the console.
#'
#' @return A list with elements:
#' \describe{
#'   \item{\code{is_dag}}{Logical; \code{TRUE} if the graph is a valid DAG.}
#'   \item{\code{cycle_edges}}{A data frame with columns \code{from} and
#'     \code{to} listing the back-edges that create cycles, or \code{NULL} if
#'     the graph is a DAG.}
#'   \item{\code{n_cycle_edges}}{Integer; number of cycle-creating edges.}
#' }
#'
#' @examples
#' library(igraph)
#'
#' # Clean DAG — should pass
#' el_clean <- data.frame(from = c(1, 2, 3), to = c(2, 3, 4))
#' check_dag(el_clean)
#'
#' # Graph with a cycle: 1->2->3->1
#' el_cycle <- data.frame(
#'   from = c(1, 2, 3, 3),
#'   to   = c(2, 3, 1, 4)
#' )
#' result <- check_dag(el_cycle)
#' result$is_dag       # FALSE
#' result$cycle_edges  # the offending edge(s) — fix these in your source data
#'
#' @export
check_dag <- function(x, verbose = TRUE) {

  # Coerce to igraph
  if (inherits(x, "igraph")) {
    g <- x
  } else if (is.data.frame(x) || is.matrix(x)) {
    g <- igraph::graph_from_data_frame(as.data.frame(x), directed = TRUE)
  } else {
    rlang::abort(paste0(
      "`x` must be an igraph object or a data frame / matrix, ",
      "not <", class(x)[1L], ">."
    ))
  }

  if (!igraph::is_directed(g)) {
    rlang::abort("The graph must be directed.")
  }

  # ── Fast path: already a DAG ───────────────────────────────────────────────
  if (igraph::is_dag(g)) {
    result <- list(is_dag = TRUE, cycle_edges = NULL, n_cycle_edges = 0L)
    if (verbose) message("The graph is a valid DAG (no cycles detected).")
    return(result)
  }

  # ── Detect back-edges via DFS ──────────────────────────────────────────────
  back_edges <- .find_back_edges(g)

  el      <- igraph::as_edgelist(g, names = TRUE)
  back_df <- data.frame(
    from = el[back_edges, 1L],
    to   = el[back_edges, 2L],
    stringsAsFactors = FALSE
  )

  result <- list(
    is_dag        = FALSE,
    cycle_edges   = back_df,
    n_cycle_edges = nrow(back_df)
  )

  if (verbose) {
    message(sprintf(
      "The graph contains %d cycle-creating edge(s):\n%s\n  Please correct these in your source data before proceeding.",
      nrow(back_df),
      paste0("  ", back_df$from, " -> ", back_df$to, collapse = "\n")
    ))
  }

  result
}


# ── Internal: DFS-based back-edge detection ──────────────────────────────────

#' Identify back-edges (edges that point to an ancestor in the DFS tree).
#' Returns integer vector of edge IDs.
#' @noRd
.find_back_edges <- function(g) {
  n        <- igraph::vcount(g)
  el       <- igraph::as_edgelist(g, names = FALSE)
  from_v   <- el[, 1L]
  to_v     <- el[, 2L]

  # Build adjacency list: successors per vertex
  adj <- vector("list", n)
  for (v in seq_len(n)) adj[[v]] <- integer(0)
  # Also store which edge id corresponds to each (u,v)
  edge_of  <- vector("list", n)
  for (v in seq_len(n)) edge_of[[v]] <- list()

  for (eid in seq_len(nrow(el))) {
    u <- from_v[eid]
    v <- to_v[eid]
    adj[[u]]    <- c(adj[[u]], v)
    edge_of[[u]] <- c(edge_of[[u]], list(c(v, eid)))
  }

  # DFS: colour = 0 (white/unvisited), 1 (grey/in stack), 2 (black/done)
  colour     <- integer(n)
  back_eids  <- integer(0)

  dfs_stack <- vector("list", 0)  # stack of (vertex, iterator_index)

  for (start in seq_len(n)) {
    if (colour[start] != 0L) next
    # Push start
    colour[start] <- 1L
    dfs_stack <- c(dfs_stack, list(list(v = start, i = 1L)))

    while (length(dfs_stack) > 0L) {
      top   <- dfs_stack[[length(dfs_stack)]]
      v     <- top$v
      nbrs  <- edge_of[[v]]

      if (top$i > length(nbrs)) {
        # All neighbours processed
        colour[v] <- 2L
        dfs_stack <- dfs_stack[-length(dfs_stack)]
      } else {
        # Update iterator
        dfs_stack[[length(dfs_stack)]]$i <- top$i + 1L
        pair <- nbrs[[top$i]]
        w    <- pair[1L]
        eid  <- pair[2L]

        if (colour[w] == 1L) {
          # Back edge found
          back_eids <- c(back_eids, eid)
        } else if (colour[w] == 0L) {
          colour[w] <- 1L
          dfs_stack <- c(dfs_stack, list(list(v = w, i = 1L)))
        }
        # colour[w] == 2 → cross/forward edge, ignore
      }
    }
  }

  unique(back_eids)
}
