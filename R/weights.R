#' Compute traversal weights for edges in a citation DAG
#'
#' @description
#' Calculates one or more of the three traversal-weight measures introduced by
#' Hummon & Doreian (1989) for every edge in a directed acyclic graph (DAG):
#'
#' * **SPC** – *Search Path Count*: the number of source-to-sink paths that
#'   traverse each edge. Proposed by Batagelj (2003).
#' * **SPLC** – *Search Path Link Count*: for edge \eqn{(i \to j)}, the number
#'   of paths from any ancestor of \eqn{i} (including \eqn{i} itself) to any
#'   sink that traverse the edge. Proposed by Hummon & Doreian (1989).
#' * **SPNP** – *Search Path Node Pair*: for edge \eqn{(i \to j)}, the number
#'   of paths from any ancestor of \eqn{i} (including \eqn{i}) to any
#'   descendant of \eqn{j} (including \eqn{j}) that traverse the edge.
#'   Proposed by Hummon & Doreian (1989).
#'
#' The formulas reduce to:
#' \deqn{SPC(i \to j)  = f(i) \cdot b(j)}
#' \deqn{SPLC(i \to j) = f_a(i) \cdot b(j)}
#' \deqn{SPNP(i \to j) = f_a(i) \cdot b_a(j)}
#' where \eqn{f(i)} = paths from global sources to \eqn{i};
#' \eqn{f_a(i) = 1 + \sum_{u \to i} f_a(u)} (paths from any ancestor,
#' including \eqn{i} itself); \eqn{b(j)} = paths from \eqn{j} to global
#' sinks; \eqn{b_a(j) = 1 + \sum_{j \to w} b_a(w)}.
#'
#' All three measures are computed in a single forward–backward pass over the
#' topological ordering, so the function is \eqn{O(V + E)}.
#'
#' @param x An \code{igraph} directed acyclic graph **or** a data frame / matrix
#'   whose first two columns are the edge endpoints (\code{from}, \code{to}).
#'   Extra columns become edge attributes.
#' @param method Character vector; one or more of \code{"SPC"}, \code{"SPLC"},
#'   \code{"SPNP"}, or \code{"all"} (default).  Case-insensitive.
#'
#' @return The input graph (as an \code{igraph} object) with the requested
#'   weight(s) attached as edge attributes (\code{SPC}, \code{SPLC}, \code{SPNP}).
#'
#' @references
#' Hummon, N. P., & Doreian, P. (1989). Connectivity in a citation network:
#' The development of DNA theory. *Social Networks*, **11**(1), 39–63.
#' \doi{10.1016/0378-8733(89)90017-3}
#'
#' Batagelj, V. (2003). Efficient algorithms for citation network analysis.
#' *arXiv preprint* cs/0309023.
#'
#' @examples
#' library(igraph)
#'
#' # Small citation chain: 1 -> 2 -> 3 -> 5
#' #                              \-> 4 -> 5
#' el <- data.frame(
#'   from = c(1, 2, 2, 3, 4),
#'   to   = c(2, 3, 4, 5, 5)
#' )
#' g_w <- traversal_weights(el)
#' igraph::E(g_w)$SPC
#'
#' @export
traversal_weights <- function(x, method = "all") {
  g <- .to_dag(x)

  method <- toupper(method)
  if ("ALL" %in% method) method <- c("SPC", "SPLC", "SPNP")
  method <- match.arg(method, c("SPC", "SPLC", "SPNP"), several.ok = TRUE)

  n   <- igraph::vcount(g)
  ord <- as.integer(.topo_order(g))   # topological order (vertex indices)
  ss  <- .sources_sinks(g)

  # ------------------------------------------------------------------
  # Forward pass
  #   f[v]  = # paths from any global source to v          (used by SPC)
  #   fa[v] = 1 + sum(fa[u] for u -> v)                   (used by SPLC, SPNP)
  #           counts paths from any ancestor to v, including the trivial
  #           zero-length path from v to itself
  # ------------------------------------------------------------------
  f  <- numeric(n)
  fa <- numeric(n)

  f[ss$sources] <- 1.0

  for (v in ord) {
    preds <- as.integer(igraph::neighbors(g, v, mode = "in"))
    fa[v] <- 1.0 + sum(fa[preds])          # always: trivial path + ancestor paths
    if (length(preds) == 0L) next          # source: f already initialised
    f[v]  <- sum(f[preds])
  }

  # ------------------------------------------------------------------
  # Backward pass
  #   b[v]  = # paths from v to any global sink            (used by SPC, SPLC)
  #   ba[v] = 1 + sum(ba[w] for v -> w)                   (used by SPNP)
  #           counts paths from v to any descendant, including v itself
  # ------------------------------------------------------------------
  b  <- numeric(n)
  ba <- numeric(n)

  b[ss$sinks] <- 1.0

  for (v in rev(ord)) {
    succs <- as.integer(igraph::neighbors(g, v, mode = "out"))
    ba[v] <- 1.0 + sum(ba[succs])          # always: trivial path + descendant paths
    if (length(succs) == 0L) next          # sink: b already initialised
    b[v]  <- sum(b[succs])
  }

  # ------------------------------------------------------------------
  # Edge scores
  #   SPC (i->j)  = f[i]  * b[j]
  #   SPLC(i->j)  = fa[i] * b[j]
  #   SPNP(i->j)  = fa[i] * ba[j]
  # ------------------------------------------------------------------
  el   <- igraph::as_edgelist(g, names = FALSE)
  from <- el[, 1L]
  to   <- el[, 2L]

  if ("SPC"  %in% method) igraph::E(g)$SPC  <- f[from]  * b[to]
  if ("SPLC" %in% method) igraph::E(g)$SPLC <- fa[from] * b[to]
  if ("SPNP" %in% method) igraph::E(g)$SPNP <- fa[from] * ba[to]

  g
}
