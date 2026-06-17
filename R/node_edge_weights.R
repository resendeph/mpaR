#' Tabulate edge-level traversal weights
#'
#' @description
#' Computes the SPC, SPLC, and/or SPNP traversal weights for every edge in a
#' directed acyclic graph (via [traversal_weights()]) and returns them as a
#' tidy data frame, one row per edge — handy for inspecting or exporting the
#' raw weights without working with `igraph` edge attributes directly.
#'
#' @param x An \code{igraph} directed acyclic graph **or** a data frame /
#'   matrix whose first two columns are the edge endpoints (\code{from},
#'   \code{to}).
#' @param method Character vector; one or more of \code{"SPC"}, \code{"SPLC"},
#'   \code{"SPNP"}, or \code{"all"} (default). Case-insensitive.
#'
#' @return A data frame with columns \code{from}, \code{to}, and one column
#'   per requested weight (\code{SPC}, \code{SPLC}, \code{SPNP}).
#'
#' @seealso [traversal_weights()] to attach weights as edge attributes on the
#'   graph itself, [node_weights()] for the node-level analogue.
#'
#' @references
#' Hummon, N. P., & Doreian, P. (1989). Connectivity in a citation network:
#' The development of DNA theory. *Social Networks*, **11**(1), 39–63.
#' \doi{10.1016/0378-8733(89)90017-8}
#'
#' @examples
#' library(igraph)
#'
#' el <- data.frame(
#'   from = c(1, 2, 2, 3, 4),
#'   to   = c(2, 3, 4, 5, 5)
#' )
#' edge_weights(el)
#' edge_weights(el, method = "SPC")
#'
#' @export
edge_weights <- function(x, method = "all") {
  g <- traversal_weights(x, method = method)

  method <- toupper(method)
  if ("ALL" %in% method) method <- c("SPC", "SPLC", "SPNP")
  method <- match.arg(method, c("SPC", "SPLC", "SPNP"), several.ok = TRUE)

  el <- igraph::as_edgelist(g, names = TRUE)

  out <- data.frame(
    from = el[, 1L],
    to   = el[, 2L],
    stringsAsFactors = FALSE
  )

  for (m in method) {
    out[[m]] <- igraph::edge_attr(g, m)
  }

  out
}


#' Tabulate node-level traversal weights
#'
#' @description
#' Computes the node-level analogue of the SPC, SPLC, and SPNP traversal
#' weights: the number of source-to-sink paths passing **through** each
#' vertex, rather than across a single edge. Using the same forward/backward
#' path counts described in [traversal_weights()] — \eqn{f(v)}, \eqn{f_a(v)},
#' \eqn{b(v)}, \eqn{b_a(v)} — the node-level weights are:
#' \deqn{SPC(v)  = f(v)   \cdot b(v)}
#' \deqn{SPLC(v) = f_a(v) \cdot b(v)}
#' \deqn{SPNP(v) = f_a(v) \cdot b_a(v)}
#'
#' This is the direct generalization of the edge formula
#' \eqn{SPC(i \to j) = f(i) \cdot b(j)} to a single vertex (setting
#' \eqn{i = j = v}), and counts every source-to-sink path that visits
#' \eqn{v}, regardless of which edge it uses to arrive or leave.
#'
#' @param x An \code{igraph} directed acyclic graph **or** a data frame /
#'   matrix whose first two columns are the edge endpoints (\code{from},
#'   \code{to}).
#' @param method Character vector; one or more of \code{"SPC"}, \code{"SPLC"},
#'   \code{"SPNP"}, or \code{"all"} (default). Case-insensitive.
#'
#' @return A data frame with columns \code{name} and one column per
#'   requested weight (\code{SPC}, \code{SPLC}, \code{SPNP}).
#'
#' @seealso [edge_weights()] for the edge-level table,
#'   [traversal_weights()] for the underlying edge-level computation.
#'
#' @references
#' Hummon, N. P., & Doreian, P. (1989). Connectivity in a citation network:
#' The development of DNA theory. *Social Networks*, **11**(1), 39–63.
#' \doi{10.1016/0378-8733(89)90017-8}
#'
#' @examples
#' library(igraph)
#'
#' el <- data.frame(
#'   from = c(1, 2, 2, 3, 4),
#'   to   = c(2, 3, 4, 5, 5)
#' )
#' node_weights(el)
#' node_weights(el, method = "SPNP")
#'
#' @export
node_weights <- function(x, method = "all") {
  g <- .to_dag(x)

  method <- toupper(method)
  if ("ALL" %in% method) method <- c("SPC", "SPLC", "SPNP")
  method <- match.arg(method, c("SPC", "SPLC", "SPNP"), several.ok = TRUE)

  pc <- .path_counts(g)

  out <- data.frame(
    name = igraph::V(g)$name,
    stringsAsFactors = FALSE
  )

  if ("SPC"  %in% method) out$SPC  <- pc$f  * pc$b
  if ("SPLC" %in% method) out$SPLC <- pc$fa * pc$b
  if ("SPNP" %in% method) out$SPNP <- pc$fa * pc$ba

  out
}
