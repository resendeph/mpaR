#' Classify vertices by their role in the citation network
#'
#' @description
#' Labels every vertex in a directed graph as one of four roles based on its
#' in-degree and out-degree:
#'
#' \strong{Citation-network convention} (default, \code{convention = "citation"}):
#' \describe{
#'   \item{\code{"source"}}{Pioneer / precursor patents: receive citations but
#'     cite no one (\eqn{k^{in} > 0,\; k^{out} = 0}).}
#'   \item{\code{"terminal"}}{Cutting-edge / most-recent patents: cite others
#'     but have not yet been cited (\eqn{k^{in} = 0,\; k^{out} > 0}).}
#'   \item{\code{"user"}}{Intermediate patents: both cite and are cited
#'     (\eqn{k^{in} > 0,\; k^{out} > 0}).}
#'   \item{\code{"isolated"}}{No citations in either direction
#'     (\eqn{k^{in} = 0,\; k^{out} = 0}).}
#' }
#'
#' \strong{Graph-theory convention} (\code{convention = "graph"}):
#' \describe{
#'   \item{\code{"source"}}{In-degree zero (\eqn{k^{in} = 0}).}
#'   \item{\code{"sink"}}{Out-degree zero (\eqn{k^{out} = 0}).}
#'   \item{\code{"user"}}{Both degrees positive.}
#'   \item{\code{"isolated"}}{Both degrees zero.}
#' }
#'
#' @note
#' The two conventions assign \emph{opposite} meanings to "source":
#' \itemize{
#'   \item In a patent citation network, edge direction is
#'     \eqn{A \to B} meaning "A cites B". The oldest, foundational patent B
#'     accumulates in-citations but cites nothing — it is the \emph{source} of
#'     the knowledge flow, hence called \code{"source"} in the citation
#'     convention even though it is a \emph{sink} in graph-theory terms.
#'   \item Use \code{convention = "graph"} if you are working with a DAG where
#'     edge direction represents precedence or causality rather than citation.
#' }
#'
#' @param x An \code{igraph} directed graph or a data frame / matrix edge list.
#' @param convention Character; \code{"citation"} (default) or \code{"graph"}.
#'   See Description for the difference.
#' @param as_data_frame Logical. If \code{TRUE} (default), return a data frame
#'   with one row per vertex. If \code{FALSE}, return a named character vector
#'   of labels indexed by vertex name.
#'
#' @return
#' A data frame with columns \code{name} (vertex name), \code{in_degree},
#' \code{out_degree}, and \code{role}; or a named character vector if
#' \code{as_data_frame = FALSE}.
#'
#' @references
#' Hummon, N. P., & Doreian, P. (1989). Connectivity in a citation network:
#' The development of DNA theory. *Social Networks*, **11**(1), 39–63.
#' \doi{10.1016/0378-8733(89)90017-8}
#'
#' Liu, J. S., Lu, L. Y. Y., Lu, W. M., & Lin, B. J. Y. (2012). A survey of
#' DEA applications. *Omega*, **41**(5), 893–902.
#'
#' @examples
#' library(igraph)
#'
#' el <- data.frame(
#'   from = c(1, 2, 2, 3, 4),
#'   to   = c(2, 3, 4, 5, 5)
#' )
#'
#' # Citation-network convention (default)
#' classify_nodes(el)
#'
#' # Graph-theory convention
#' classify_nodes(el, convention = "graph")
#'
#' # As a named vector
#' classify_nodes(el, as_data_frame = FALSE)
#'
#' @export
classify_nodes <- function(x,
                           convention    = c("citation", "graph"),
                           as_data_frame = TRUE) {

  convention <- match.arg(convention)

  # Coerce — allow non-DAG graphs (ownership networks, etc.)
  if (inherits(x, "igraph")) {
    g <- x
    if (!igraph::is_directed(g)) {
      rlang::abort("The graph must be directed.")
    }
  } else if (is.data.frame(x) || is.matrix(x)) {
    g <- igraph::graph_from_data_frame(as.data.frame(x), directed = TRUE)
  } else {
    rlang::abort(paste0(
      "`x` must be an igraph object or a data frame / matrix, ",
      "not <", class(x)[1L], ">."
    ))
  }

  if (is.null(igraph::V(g)$name)) {
    igraph::V(g)$name <- as.character(seq_len(igraph::vcount(g)))
  }

  indeg  <- igraph::degree(g, mode = "in")
  outdeg <- igraph::degree(g, mode = "out")
  vnames <- igraph::V(g)$name

  role <- character(length(vnames))

  if (convention == "citation") {
    # Citation-network: source = pioneer (receives cites, cites nothing)
    #                   terminal = cutting-edge (cites others, not cited yet)
    role[indeg >  0L & outdeg == 0L] <- "source"
    role[indeg == 0L & outdeg >  0L] <- "terminal"
    role[indeg >  0L & outdeg >  0L] <- "user"
    role[indeg == 0L & outdeg == 0L] <- "isolated"
  } else {
    # Graph-theory: source = in-degree 0, sink = out-degree 0
    role[indeg == 0L & outdeg >  0L] <- "source"
    role[indeg >  0L & outdeg == 0L] <- "sink"
    role[indeg >  0L & outdeg >  0L] <- "user"
    role[indeg == 0L & outdeg == 0L] <- "isolated"
  }

  if (!as_data_frame) {
    return(setNames(role, vnames))
  }

  data.frame(
    name       = vnames,
    in_degree  = as.integer(indeg),
    out_degree = as.integer(outdeg),
    role       = role,
    stringsAsFactors = FALSE
  )
}
