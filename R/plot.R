#' Plot a network with the main path highlighted
#'
#' @description
#' Plots a DAG and overlays the main path in a contrasting colour.
#' The \code{scope} argument controls which nodes are drawn:
#'
#' * \code{"full"} (default) — the entire graph, with main-path nodes/edges
#'   highlighted and the rest shown in a muted style.
#' * \code{"component"} — only the weakly connected component(s) that contain
#'   at least one main-path node.  Useful for large, multi-component networks
#'   where most components are irrelevant.
#' * \code{"path"} — only the main-path subgraph itself (no background nodes).
#'
#' The layout defaults to [igraph::layout_with_sugiyama()], which respects
#' the topological order of the DAG and is therefore well suited to citation
#' and precedence networks.
#'
#' @param graph An \code{igraph} DAG — typically the output of
#'   [traversal_weights()].
#' @param path An \code{igraph} subgraph representing the main path — the
#'   output of [main_path()] or [mpa()].
#' @param scope Character; one of \code{"full"} (default), \code{"component"},
#'   or \code{"path"}.  Controls which portion of the graph is drawn.
#' @param path_col Colour for main-path nodes and edges.
#'   Defaults to \code{"#E63946"} (red).
#' @param bg_col Colour for background (non-path) nodes and edges.
#'   Defaults to \code{"#AAAAAA"} (grey).
#' @param path_label_col Colour for vertex labels on main-path nodes.
#'   Defaults to \code{"white"}.  Pass \code{NA} to suppress labels on path
#'   nodes.
#' @param bg_label_col Colour for vertex labels on background nodes.
#'   Defaults to \code{"#555555"}.  Only visible when
#'   \code{scope = "full"} or \code{"component"}.
#' @param layout A numeric matrix of vertex coordinates (\eqn{V \times 2}).
#'   If \code{NULL} (default), [igraph::layout_with_sugiyama()] is used.
#'   When \code{scope = "component"} or \code{"path"}, the layout is
#'   automatically subsetted to the plotted vertices.
#' @param vertex_size Size of all vertices.  Defaults to \code{20}.
#' @param vertex_label_cex Font size multiplier for vertex labels.
#'   Defaults to \code{0.7}.  Set to \code{0} to hide all labels.
#' @param edge_width_path Line width for main-path edges.  Defaults to \code{3}.
#' @param edge_width_bg Line width for background edges.  Defaults to \code{1}.
#' @param arrow_size Arrow size passed to [igraph::plot.igraph()].
#'   Defaults to \code{0.4}.
#' @param vertex_label_dist Distance between vertex centre and label, passed to
#'   [igraph::plot.igraph()] as \code{vertex.label.dist}.  Defaults to
#'   \code{0} (label centred inside the node).  Set to a positive value (e.g.
#'   \code{1.5}) to push labels outside the node circles, which is often more
#'   readable in dense graphs.
#' @param ... Additional arguments forwarded to [igraph::plot.igraph()].
#'   Common uses: \code{main} for a plot title, \code{vertex.label} to supply
#'   custom label strings (e.g. short names instead of node IDs).
#'
#' @return Invisibly returns a list with:
#'   \describe{
#'     \item{\code{layout}}{The full-graph coordinate matrix used.}
#'     \item{\code{path_vertices}}{Integer indices (in \code{graph}) of
#'       main-path vertices.}
#'     \item{\code{plotted_graph}}{The \code{igraph} object that was
#'       actually rendered (may be a subgraph when
#'       \code{scope != "full"}).}
#'   }
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
#'
#' # Full graph: background nodes grey, path highlighted in red
#' plot_mpa(g, mp)
#'
#' # Only the weakly connected component(s) containing the path
#' plot_mpa(g, mp, scope = "component")
#'
#' # Path nodes only — clearest view for large networks
#' plot_mpa(g, mp, scope = "path")
#'
#' # Custom colours with visible labels
#' plot_mpa(g, mp, scope = "path",
#'          path_col       = "#1D3557",
#'          path_label_col = "white",
#'          vertex_size    = 18,
#'          vertex_label_cex = 0.6)
#'
#' # Supply a title and custom label strings via ...
#' plot_mpa(g, mp, scope = "path",
#'          main         = "Global MPA — SPC",
#'          vertex.label = paste0("N", igraph::V(mp)$name))
#'
#' # Dense or large networks: shrink nodes and push labels outside the circles
#' # so they remain readable. Pass the layout explicitly to avoid recomputing it.
#' lay <- igraph::layout_with_sugiyama(g)$layout
#' plot_mpa(g, mp,
#'          scope             = "full",
#'          layout            = lay,
#'          vertex_size       = 5,
#'          vertex_label_cex  = 0.8,
#'          vertex_label_dist = 1.5,
#'          path_label_col    = "red",
#'          arrow_size        = 0.5)
#'
#' # Broadened path: threshold = 0.8 pulls in near-optimal routes
#' mp_broad <- main_path(g, type = "global", weight = "SPC", threshold = 0.8)
#' plot_mpa(g, mp_broad, scope = "path",
#'          path_col       = "#457B9D",
#'          path_label_col = "white",
#'          main           = "Broadened MPA (threshold = 0.8)")
#'
#' @export
plot_mpa <- function(graph,
                     path,
                     scope             = c("full", "component", "path"),
                     path_col          = "#E63946",
                     bg_col            = "#AAAAAA",
                     path_label_col    = "white",
                     bg_label_col      = "#555555",
                     layout            = NULL,
                     vertex_size       = 20,
                     vertex_label_cex  = 0.7,
                     edge_width_path   = 3,
                     edge_width_bg     = 1,
                     arrow_size        = 0.4,
                     vertex_label_dist = 0,
                     ...) {

  scope <- match.arg(scope)

  if (!inherits(graph, "igraph")) {
    rlang::abort("`graph` must be an igraph object.")
  }
  if (!inherits(path, "igraph")) {
    rlang::abort("`path` must be an igraph object (output of main_path() or mpa()).")
  }

  # Auto-set vertex names if missing so labels always render correctly
  if (is.null(igraph::V(graph)$name)) {
    igraph::V(graph)$name <- as.character(seq_len(igraph::vcount(graph)))
  }
  if (is.null(igraph::V(path)$name)) {
    igraph::V(path)$name <- as.character(seq_len(igraph::vcount(path)))
  }

  # ── Identify main-path vertices and edges in the full graph ────────────────
  path_vnames <- igraph::V(path)$name
  path_el     <- igraph::as_edgelist(path, names = TRUE)
  path_enames <- if (nrow(path_el) > 0L)
                   paste0(path_el[, 1], "|", path_el[, 2])
                 else
                   character(0)

  full_vnames <- igraph::V(graph)$name
  full_el     <- igraph::as_edgelist(graph, names = TRUE)
  full_enames <- paste0(full_el[, 1], "|", full_el[, 2])

  on_path_v <- full_vnames %in% path_vnames
  on_path_e <- full_enames %in% path_enames

  # ── Compute layout on the full graph first (used for all scopes) ───────────
  if (is.null(layout)) {
    layout <- igraph::layout_with_sugiyama(graph)$layout
  }

  # ── Determine the graph to actually plot ──────────────────────────────────
  if (scope == "full") {

    plot_g         <- graph
    plot_lay       <- layout
    plot_on_path_v <- on_path_v
    plot_on_path_e <- on_path_e

  } else if (scope == "component") {

    memb       <- igraph::components(graph, mode = "weak")$membership
    path_v_ids <- which(on_path_v)
    keep_comps <- unique(memb[path_v_ids])
    keep_v     <- which(memb %in% keep_comps)

    plot_g   <- igraph::induced_subgraph(graph, keep_v)
    plot_lay <- layout[keep_v, , drop = FALSE]

    sub_vnames     <- igraph::V(plot_g)$name
    sub_el         <- igraph::as_edgelist(plot_g, names = TRUE)
    sub_enames     <- paste0(sub_el[, 1], "|", sub_el[, 2])
    plot_on_path_v <- sub_vnames %in% path_vnames
    plot_on_path_e <- sub_enames %in% path_enames

  } else {   # scope == "path"

    plot_g   <- path
    path_idx <- match(igraph::V(path)$name, full_vnames)
    path_idx <- path_idx[!is.na(path_idx)]
    plot_lay <- if (length(path_idx) > 0L)
                  layout[path_idx, , drop = FALSE]
                else
                  igraph::layout_with_sugiyama(path)$layout
    plot_on_path_v <- rep(TRUE, igraph::vcount(path))
    plot_on_path_e <- rep(TRUE, igraph::ecount(path))

  }

  # ── Visual attributes ──────────────────────────────────────────────────────
  v_color       <- ifelse(plot_on_path_v, path_col, bg_col)
  v_frame       <- ifelse(plot_on_path_v, path_col, bg_col)
  v_label_color <- ifelse(plot_on_path_v, path_label_col, bg_label_col)
  e_color       <- ifelse(plot_on_path_e, path_col, bg_col)
  e_width       <- ifelse(plot_on_path_e, edge_width_path, edge_width_bg)

  # ── Plot ───────────────────────────────────────────────────────────────────
  igraph::plot.igraph(
    plot_g,
    layout             = plot_lay,
    vertex.color       = v_color,
    vertex.frame.color = v_frame,
    vertex.label.color = v_label_color,
    vertex.label.cex   = vertex_label_cex,
    vertex.size        = vertex_size,
    edge.color         = e_color,
    edge.width         = e_width,
    edge.arrow.size    = arrow_size,
    vertex.label.dist  = vertex_label_dist,
    ...
  )

  invisible(list(
    layout        = layout,
    path_vertices = which(on_path_v),
    plotted_graph = plot_g
  ))
}