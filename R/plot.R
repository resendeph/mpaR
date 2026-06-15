#' Plot a network with the main path highlighted
#'
#' @description
#' Plots the full DAG and overlays the main path in a contrasting colour.
#' Nodes and edges that belong to the main path are drawn prominently;
#' the rest of the network is shown in a muted style for context.
#'
#' The layout defaults to [igraph::layout_with_sugiyama()], which respects
#' the topological order of the DAG and is therefore well suited to citation
#' and precedence networks.
#'
#' @param graph An \code{igraph} DAG — typically the output of
#'   [traversal_weights()].  Must contain at least one traversal-weight edge
#'   attribute (\code{SPC}, \code{SPLC}, or \code{SPNP}).
#' @param path An \code{igraph} subgraph representing the main path — the
#'   output of [main_path()] or [mpa()].
#' @param path_col Colour used for main-path nodes and edges.
#'   Defaults to \code{"#E63946"} (red).
#' @param bg_col Colour used for background (non-path) nodes and edges.
#'   Defaults to \code{"#AAAAAA"} (grey).
#' @param layout A numeric matrix of vertex coordinates (\eqn{V \times 2}).
#'   If \code{NULL} (default), [igraph::layout_with_sugiyama()] is used.
#' @param vertex_size Size of all vertices.  Defaults to \code{20}.
#' @param vertex_label_cex Font size multiplier for vertex labels.
#'   Defaults to \code{0.7}.
#' @param edge_width_path Line width for main-path edges.  Defaults to \code{3}.
#' @param edge_width_bg Line width for background edges.  Defaults to \code{1}.
#' @param arrow_size Arrow size passed to [igraph::plot.igraph()].
#'   Defaults to \code{0.4}.
#' @param ... Additional arguments forwarded to [igraph::plot.igraph()].
#'
#' @return Invisibly returns a list with two elements:
#'   \describe{
#'     \item{\code{layout}}{The coordinate matrix used for the plot.}
#'     \item{\code{path_vertices}}{Integer indices of main-path vertices.}
#'   }
#'   The function is called primarily for its side effect (the plot).
#'
#' @examples
#' library(igraph)
#'
#' el <- data.frame(
#'   from = c(1, 1, 2, 3, 3, 4, 5, 6),
#'   to   = c(2, 3, 4, 4, 5, 6, 6, 7)
#' )
#' g  <- traversal_weights(el)
#' mp <- main_path(g, type = "global", weight = "SPC")
#' plot_mpa(g, mp)
#'
#' # Key-route with custom colours
#' mp_kr <- main_path(g, type = "key_route", weight = "SPC", k = 2)
#' plot_mpa(g, mp_kr, path_col = "steelblue", bg_col = "#CCCCCC")
#'
#' @export
plot_mpa <- function(graph,
                     path,
                     path_col          = "#E63946",
                     bg_col            = "#AAAAAA",
                     layout            = NULL,
                     vertex_size       = 20,
                     vertex_label_cex  = 0.7,
                     edge_width_path   = 3,
                     edge_width_bg     = 1,
                     arrow_size        = 0.4,
                     ...) {

  if (!inherits(graph, "igraph")) {
    rlang::abort("`graph` must be an igraph object.")
  }
  if (!inherits(path, "igraph")) {
    rlang::abort("`path` must be an igraph object (output of main_path() or mpa()).")
  }

  # --- Identify main-path vertices and edges in the full graph ---------------
  path_vnames <- igraph::V(path)$name
  path_enames <- paste0(
    igraph::as_edgelist(path, names = TRUE)[, 1], "|",
    igraph::as_edgelist(path, names = TRUE)[, 2]
  )

  full_vnames <- igraph::V(graph)$name
  full_el     <- igraph::as_edgelist(graph, names = TRUE)
  full_enames <- paste0(full_el[, 1], "|", full_el[, 2])

  on_path_v <- full_vnames %in% path_vnames
  on_path_e <- full_enames %in% path_enames

  # --- Visual attributes -----------------------------------------------------
  v_color  <- ifelse(on_path_v, path_col, bg_col)
  v_frame  <- ifelse(on_path_v, path_col, bg_col)
  v_label_color <- ifelse(on_path_v, "white", "#555555")

  e_color  <- ifelse(on_path_e, path_col, bg_col)
  e_width  <- ifelse(on_path_e, edge_width_path, edge_width_bg)

  # --- Layout ----------------------------------------------------------------
  if (is.null(layout)) {
    layout <- igraph::layout_with_sugiyama(graph)$layout
  }

  # --- Plot ------------------------------------------------------------------
  igraph::plot.igraph(
    graph,
    layout            = layout,
    vertex.color      = v_color,
    vertex.frame.color = v_frame,
    vertex.label.color = v_label_color,
    vertex.label.cex  = vertex_label_cex,
    vertex.size       = vertex_size,
    edge.color        = e_color,
    edge.width        = e_width,
    edge.arrow.size   = arrow_size,
    ...
  )

  invisible(list(
    layout        = layout,
    path_vertices = which(on_path_v)
  ))
}
