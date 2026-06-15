#' Read a Gephi export file (GEXF or GraphML)
#'
#' @description
#' Reads a graph exported from Gephi in GEXF (`.gexf`) or GraphML
#' (`.graphml` / `.xml`) format and returns an \code{igraph} object.
#'
#' GEXF files are parsed directly via their XML structure using the
#' \pkg{xml2} package.  GraphML files are read with
#' \code{igraph::read_graph()}.
#'
#' Gephi's native project format (`.gephi`) uses a proprietary binary
#' serialisation that cannot be parsed reliably outside of Gephi.  Use
#' **File → Export → Graph File** in Gephi and choose `.gexf` or `.graphml`
#' before calling this function.
#'
#' @param file Path to a `.gexf` or `.graphml` file exported from Gephi.
#' @param directed Logical or \code{NULL}.  If \code{NULL} (default), the
#'   directionality declared in the file is respected.  Set to \code{TRUE} or
#'   \code{FALSE} to override.
#'
#' @return An \code{igraph} object.  For GEXF files, vertex attributes include
#'   \code{name} (node id), \code{label} (if present), \code{x}, \code{y},
#'   \code{size}, and any \code{<attvalue>} columns defined in the file.
#'   Edge attributes include \code{weight} and any additional attributes.
#'
#' @examples
#' \dontrun{
#' # After exporting from Gephi: File > Export > Graph File > .gexf
#' g <- read_gephi_export("my_project.gexf")
#' igraph::vcount(g)
#' igraph::ecount(g)
#' igraph::vertex_attr_names(g)
#'
#' # GraphML export
#' g <- read_gephi_export("my_project.graphml")
#' }
#'
#' @export
read_gephi_export <- function(file, directed = NULL) {

  if (!file.exists(file)) {
    rlang::abort(paste0("File not found: ", file))
  }

  ext <- tolower(tools::file_ext(file))

  if (ext == "gephi") {
    rlang::abort(paste(
      "`.gephi` files use Gephi's internal binary format and cannot be",
      "parsed directly. In Gephi, use File -> Export -> Graph File",
      "and export as `.gexf` or `.graphml`, then pass that file here."
    ))
  }

  if (!ext %in% c("gexf", "graphml", "xml")) {
    rlang::abort(paste0(
      "Unsupported file extension '.", ext, "'. ",
      "Expected .gexf, .graphml, or .xml."
    ))
  }

  if (ext == "gexf") {
    g <- .read_gexf(file, directed)
  } else {
    g <- igraph::read_graph(file, format = "graphml")
    if (!is.null(directed)) {
      g <- .apply_directed(g, directed)
    }
  }

  n_v     <- igraph::vcount(g)
  n_e     <- igraph::ecount(g)
  v_attrs <- igraph::vertex_attr_names(g)
  e_attrs <- igraph::edge_attr_names(g)

  message(sprintf(
    "Loaded %s: %d vertices, %d edges (%s)\n  Vertex attrs: %s\n  Edge attrs:   %s",
    basename(file), n_v, n_e,
    if (igraph::is_directed(g)) "directed" else "undirected",
    if (length(v_attrs) > 0L) paste(v_attrs, collapse = ", ") else "(none)",
    if (length(e_attrs) > 0L) paste(e_attrs, collapse = ", ") else "(none)"
  ))

  g
}


# ── Internal: parse GEXF via xml2 ────────────────────────────────────────────

#' @noRd
.read_gexf <- function(file, directed_override = NULL) {

  if (!requireNamespace("xml2", quietly = TRUE)) {
    rlang::abort(
      "Package `xml2` is required to read GEXF files. Install it with: install.packages(\"xml2\")"
    )
  }

  doc  <- xml2::read_xml(file)
  # Strip namespaces for simpler XPath
  xml2::xml_ns_strip(doc)

  graph_node <- xml2::xml_find_first(doc, ".//graph")
  if (is.na(graph_node)) {
    rlang::abort("No <graph> element found in GEXF file.")
  }

  # Directionality from file
  edge_type <- tolower(xml2::xml_attr(graph_node, "defaultedgetype"))
  is_directed_file <- !identical(edge_type, "undirected")
  is_directed <- if (!is.null(directed_override)) directed_override else is_directed_file

  # ── Attribute declarations ─────────────────────────────────────────────────
  .parse_attr_decls <- function(class) {
    nodes <- xml2::xml_find_all(doc, paste0(".//attributes[@class='", class, "']/attribute"))
    if (length(nodes) == 0L) return(list())
    ids    <- xml2::xml_attr(nodes, "id")
    titles <- xml2::xml_attr(nodes, "title")
    types  <- xml2::xml_attr(nodes, "type")
    setNames(as.list(types), ids)   # id -> type
  }

  node_attr_decls <- .parse_attr_decls("node")
  edge_attr_decls <- .parse_attr_decls("edge")

  # ── Nodes ──────────────────────────────────────────────────────────────────
  node_nodes <- xml2::xml_find_all(doc, ".//nodes/node")

  node_ids    <- xml2::xml_attr(node_nodes, "id")
  node_labels <- xml2::xml_attr(node_nodes, "label")
  node_labels[is.na(node_labels)] <- node_ids[is.na(node_labels)]

  # viz attributes
  sizes <- vapply(node_nodes, function(n) {
    v <- xml2::xml_attr(xml2::xml_find_first(n, ".//size"), "value")
    if (is.na(v)) NA_real_ else as.numeric(v)
  }, numeric(1))

  pos_x <- vapply(node_nodes, function(n) {
    v <- xml2::xml_attr(xml2::xml_find_first(n, ".//position"), "x")
    if (is.na(v)) NA_real_ else as.numeric(v)
  }, numeric(1))

  pos_y <- vapply(node_nodes, function(n) {
    v <- xml2::xml_attr(xml2::xml_find_first(n, ".//position"), "y")
    if (is.na(v)) NA_real_ else as.numeric(v)
  }, numeric(1))

  vertices_df <- data.frame(
    name  = node_ids,
    label = node_labels,
    x     = pos_x,
    y     = pos_y,
    size  = sizes,
    stringsAsFactors = FALSE
  )

  # Extra node attvalues
  if (length(node_attr_decls) > 0L) {
    for (attr_id in names(node_attr_decls)) {
      vals <- vapply(node_nodes, function(n) {
        av <- xml2::xml_find_first(n, paste0(".//attvalue[@for='", attr_id, "']"))
        if (is.na(av)) NA_character_ else xml2::xml_attr(av, "value")
      }, character(1))
      vertices_df[[attr_id]] <- vals
    }
  }

  # ── Edges ──────────────────────────────────────────────────────────────────
  edge_nodes <- xml2::xml_find_all(doc, ".//edges/edge")

  if (length(edge_nodes) == 0L) {
    edges_df <- data.frame(from = character(0), to = character(0),
                           weight = numeric(0), stringsAsFactors = FALSE)
  } else {
    from_ids <- xml2::xml_attr(edge_nodes, "source")
    to_ids   <- xml2::xml_attr(edge_nodes, "target")
    weights  <- suppressWarnings(as.numeric(xml2::xml_attr(edge_nodes, "weight")))
    weights[is.na(weights)] <- 1.0

    edges_df <- data.frame(from = from_ids, to = to_ids, weight = weights,
                           stringsAsFactors = FALSE)

    # Extra edge attvalues
    if (length(edge_attr_decls) > 0L) {
      for (attr_id in names(edge_attr_decls)) {
        vals <- vapply(edge_nodes, function(e) {
          av <- xml2::xml_find_first(e, paste0(".//attvalue[@for='", attr_id, "']"))
          if (is.na(av)) NA_character_ else xml2::xml_attr(av, "value")
        }, character(1))
        edges_df[[attr_id]] <- vals
      }
    }

    # For undirected files forced to directed: add reverse edges
    if (is_directed && !is_directed_file) {
      rev_df   <- edges_df
      rev_df$from <- edges_df$to
      rev_df$to   <- edges_df$from
      edges_df <- rbind(edges_df, rev_df)
    }
  }

  igraph::graph_from_data_frame(
    d        = edges_df,
    directed = is_directed,
    vertices = vertices_df
  )
}


# ── Internal: apply directed override ────────────────────────────────────────

#' @noRd
.apply_directed <- function(g, directed) {
  if (directed && !igraph::is_directed(g)) {
    igraph::as.directed(g, mode = "arbitrary")
  } else if (!directed && igraph::is_directed(g)) {
    igraph::as.undirected(g, mode = "collapse")
  } else {
    g
  }
}
