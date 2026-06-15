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
