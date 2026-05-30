#' Main Path Analysis
#'
#' @description
#' A convenience wrapper that (1) coerces the input to an \code{igraph} DAG,
#' (2) computes the requested traversal weight(s), and (3) extracts the
#' main path(s) — all in one call.
#'
#' @param x An \code{igraph} directed acyclic graph or a data frame / matrix
#'   with edge-list columns (\code{from}, \code{to}).
#' @param type Main-path extraction strategy: \code{"global"} (default),
#'   \code{"local"}, or \code{"key_route"}.
#' @param weight Traversal weight to use: \code{"SPC"} (default),
#'   \code{"SPLC"}, or \code{"SPNP"}.
#' @param k Integer; number of seed edges for key-route extraction.
#'   Ignored unless \code{type = "key_route"}.
#'
#' @return An \code{igraph} subgraph of the main path(s), with the chosen
#'   traversal weight as an edge attribute.
#'
#' @seealso [traversal_weights()], [main_path()]
#'
#' @references
#' Hummon, N. P., & Doreian, P. (1989). Connectivity in a citation network:
#' The development of DNA theory. *Social Networks*, **11**(1), 39–63.
#' \doi{10.1016/0378-8733(89)90017-3}
#'
#' @examples
#' library(igraph)
#'
#' # Reproduce the small example from Hummon & Doreian (1989)
#' el <- data.frame(
#'   from = c(1, 1, 2, 3, 3, 4, 5, 6),
#'   to   = c(2, 3, 4, 4, 5, 6, 6, 7)
#' )
#'
#' # Global main path using SPC weights
#' mp_global <- mpa(el, type = "global", weight = "SPC")
#' igraph::as_edgelist(mp_global)
#'
#' # Local main path using SPNP weights
#' mp_local <- mpa(el, type = "local", weight = "SPNP")
#'
#' # Key-route main path (top 2 edges) using SPLC weights
#' mp_kr <- mpa(el, type = "key_route", weight = "SPLC", k = 2)
#'
#' @export
mpa <- function(x,
                type   = c("global", "local", "key_route"),
                weight = c("SPC", "SPLC", "SPNP"),
                k      = 1L) {
  type   <- match.arg(type)
  weight <- match.arg(weight)

  g  <- traversal_weights(x, method = weight)
  main_path(g, type = type, weight = weight, k = k)
}


#' @keywords internal
#' Package-level documentation
"_PACKAGE"
