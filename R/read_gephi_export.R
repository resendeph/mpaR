#' Read a Gephi export file (GEXF or GraphML)
#'
#' @description
#' Reads a graph exported from Gephi in GEXF (`.gexf`) or GraphML
#' (`.graphml` / `.xml`) format and returns an \code{igraph} object.
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
#' @return An \code{igraph} object with all node and edge attributes preserved
#'   from the export file.
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
      "parsed directly. In Gephi, use File → Export → Graph File",
      "and export as `.gexf` or `.graphml`, then pass that file here."
    ))
  }

  if (!ext %in% c("gexf", "graphml", "xml")) {
    rlang::abort(paste0(
      "Unsupported file extension '.", ext, "'. ",
      "Expected .gexf, .graphml, or .xml."
    ))
  }

  fmt <- if (ext == "gexf") "gexf" else "graphml"

  g <- igraph::read_graph(file, format = fmt)

  # Override directionality if requested
  if (!is.null(directed)) {
    if (directed && !igraph::is_directed(g)) {
      g <- igraph::as.directed(g, mode = "arbitrary")
    } else if (!directed && igraph::is_directed(g)) {
      g <- igraph::as.undirected(g, mode = "collapse")
    }
  }

  n_v   <- igraph::vcount(g)
  n_e   <- igraph::ecount(g)
  v_attrs <- igraph::vertex_attr_names(g)
  e_attrs <- igraph::edge_attr_names(g)

  message(sprintf(
    "Loaded %s: %d vertices, %d edges (%s)\n  Vertex attrs: %s\n  Edge attrs:   %s",
    basename(file),
    n_v, n_e,
    if (igraph::is_directed(g)) "directed" else "undirected",
    if (length(v_attrs) > 0L) paste(v_attrs, collapse = ", ") else "(none)",
    if (length(e_attrs) > 0L) paste(e_attrs, collapse = ", ") else "(none)"
  ))

  g
}
