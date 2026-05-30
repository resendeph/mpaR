#' Compute traversal weights for edges in a citation DAG
#'
#' @description
#' Calculates one or more of the three traversal-weight measures introduced by
#' Hummon & Doreian (1989) for every edge in a directed acyclic graph (DAG):
#'
#' * **SPC** – *Search Path Count*: the number of source-to-sink paths that
#'   traverse each edge.
#' * **SPLC** – *Search Path Link Count*: the sum of path lengths (in edges)
#'   over all source-to-sink paths that traverse each edge.
#' * **SPNP** – *Search Path Node Pair*: for each source-to-sink path through
#'   an edge \eqn{(i \to j)}, counts the product of nodes-before-\eqn{j} and
#'   nodes-after-\eqn{i} (including endpoints); sums this over all such paths.
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
  # Forward pass: f[v]  = # paths from any source to v
  #               fl[v] = sum of path lengths (edges) over those paths
  # ------------------------------------------------------------------
  f  <- numeric(n)   # path count
  fl <- numeric(n)   # path-length sum

  f[ss$sources] <- 1.0

  for (v in ord) {
    preds <- as.integer(igraph::neighbors(g, v, mode = "in"))
    if (length(preds) == 0L) next           # source: already initialised
    f[v]  <- sum(f[preds])
    fl[v] <- sum(fl[preds] + f[preds])      # each path to pred extended by 1
  }

  # ------------------------------------------------------------------
  # Backward pass: b[v]  = # paths from v to any sink
  #                br[v] = sum of path lengths over those paths
  # ------------------------------------------------------------------
  b  <- numeric(n)
  br <- numeric(n)

  b[ss$sinks] <- 1.0

  for (v in rev(ord)) {
    succs <- as.integer(igraph::neighbors(g, v, mode = "out"))
    if (length(succs) == 0L) next           # sink: already initialised
    b[v]  <- sum(b[succs])
    br[v] <- sum(br[succs] + b[succs])
  }

  # ------------------------------------------------------------------
  # Edge scores
  # ------------------------------------------------------------------
  el   <- igraph::as_edgelist(g, names = FALSE)   # [E x 2] integer matrix
  from <- el[, 1L]
  to   <- el[, 2L]

  if ("SPC" %in% method) {
    igraph::E(g)$SPC  <- f[from] * b[to]
  }
  if ("SPLC" %in% method) {
    # SPLC(i->j) = fl[i]*b[j]  +  f[i]*b[j]  +  f[i]*br[j]
    #            = (fl[i] + f[i]) * b[j]  +  f[i] * br[j]
    igraph::E(g)$SPLC <- (fl[from] + f[from]) * b[to] + f[from] * br[to]
  }
  if ("SPNP" %in% method) {
    # SPNP(i->j) = fn[i] * bn[j]  where fn = fl+f, bn = br+b
    igraph::E(g)$SPNP <- (fl[from] + f[from]) * (br[to] + b[to])
  }

  g
}
