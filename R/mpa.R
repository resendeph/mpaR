#' Main Path Analysis
#'
#' @description
#' A one-call convenience wrapper that coerces the input to an \code{igraph}
#' DAG, computes the requested traversal weight, and extracts the main path(s).
#' For finer control — e.g. pre-computing weights once and extracting several
#' paths — use [traversal_weights()] and [main_path()] separately.
#'
#' @param x An \code{igraph} directed acyclic graph, or a data frame / matrix
#'   whose first two columns are the edge list (\code{from}, \code{to}).
#' @param type Main-path extraction strategy: \code{"global"} (default),
#'   \code{"local"}, or \code{"key_route"}.  See [main_path()] for details.
#' @param weight Traversal weight to compute and use: \code{"SPC"} (default),
#'   \code{"SPLC"}, or \code{"SPNP"}.
#' @param k Integer; number of key-route seed edges.
#'   Ignored unless \code{type = "key_route"}.  Defaults to \code{1L}.
#' @param threshold Numeric in \code{(0, 1]}.  \code{1.0} (default) gives the
#'   classic single optimal path; lower values broaden the path to include
#'   near-optimal edges.  See [main_path()] for a full description.
#'
#' @return An \code{igraph} subgraph of the main path(s), carrying the chosen
#'   traversal weight as an edge attribute.
#'
#' @seealso [main_path()] for the lower-level function and full parameter
#'   documentation, [traversal_weights()] to compute edge weights,
#'   [plot_mpa()] to visualise results.
#'
#' @references
#' Hummon, N. P., & Doreian, P. (1989). Connectivity in a citation network:
#' The development of DNA theory. *Social Networks*, **11**(1), 39–63.
#' \doi{10.1016/0378-8733(89)90017-8}
#'
#' @examples
#' library(igraph)
#'
#' # Hummon & Doreian (1989) toy network
#' el <- data.frame(
#'   from = c(1, 1, 2, 3, 3, 4, 5, 6),
#'   to   = c(2, 3, 4, 4, 5, 6, 6, 7)
#' )
#'
#' # --- Extraction types (classic, threshold = 1.0) -------------------------
#'
#' # Global: single best source-to-sink path by SPC weight
#' mp_global <- mpa(el, type = "global", weight = "SPC")
#' igraph::as_edgelist(mp_global)
#'
#' # Local: greedy from every source, returns the union of all greedy paths
#' mp_local <- mpa(el, type = "local", weight = "SPC")
#' igraph::vcount(mp_local)
#'
#' # Key-route: top-2 seed edges extended to sources/sinks, using SPLC weights
#' mp_kr <- mpa(el, type = "key_route", weight = "SPLC", k = 2L)
#'
#' # --- Broadened paths (threshold < 1.0) -----------------------------------
#'
#' # Include all global edges on paths within 80% of the optimal SPC weight
#' mp_broad <- mpa(el, type = "global", weight = "SPC", threshold = 0.8)
#' igraph::vcount(mp_broad)   # wider than mp_global
#'
#' # Local broadened: at each node follow all edges within 90% of local max
#' mp_local_broad <- mpa(el, type = "local", weight = "SPC", threshold = 0.9)
#'
#' # Inspect how path size grows as threshold decreases
#' thresholds <- c(1.0, 0.9, 0.8, 0.7, 0.5)
#' sizes <- sapply(thresholds, function(t)
#'   igraph::vcount(mpa(el, type = "global", weight = "SPC", threshold = t))
#' )
#' data.frame(threshold = thresholds, nodes = sizes)
#'
#' @export
mpa <- function(x,
                type      = c("global", "local", "key_route"),
                weight    = c("SPC", "SPLC", "SPNP"),
                k         = 1L,
                threshold = 1.0) {
  type   <- match.arg(type)
  weight <- match.arg(weight)

  g  <- traversal_weights(x, method = weight)
  main_path(g, type = type, weight = weight, k = k, threshold = threshold)
}


#' Package-level documentation
#' @keywords internal
"_PACKAGE"
