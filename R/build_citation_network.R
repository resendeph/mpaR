#' Build a citation network from fetched bibliographic data
#'
#' @description
#' Converts a data frame of scholarly works (as returned by [fetch_openalex()]
#' or similar functions) into a directed \code{igraph} object representing the
#' citation network.
#'
#' An edge A \eqn{\to} B means "paper A cites paper B". Under mpaR's
#' citation-network convention (see [classify_nodes()]):
#' \itemize{
#'   \item Old foundational papers accumulate in-edges and become
#'     \code{"source"} nodes.
#'   \item Recent papers not yet cited by others become \code{"terminal"} nodes.
#' }
#'
#' After building the network, pass it to [check_dag()] to verify acyclicity,
#' then to [traversal_weights()] and [mpa()] to run Main Path Analysis.
#'
#' @param data A data frame as returned by [fetch_openalex()], containing at
#'   least the columns \code{id} and \code{referenced_works}.
#' @param closed Logical. If \code{TRUE} (default), only citation links between
#'   papers present in \code{data} are included, producing a self-contained
#'   network. If \code{FALSE}, all referenced works become nodes even if they
#'   do not appear in \code{data}.
#'
#' @return A directed \code{igraph} object. Vertex attributes include
#'   \code{name} (the paper ID), \code{title}, \code{doi}, \code{year},
#'   \code{first_author}, \code{journal}, and \code{cited_by_count} where
#'   available.
#'
#' @seealso [fetch_openalex()], [check_dag()], [traversal_weights()], [mpa()]
#'
#' @examples
#' \dontrun{
#' papers <- fetch_openalex("main path analysis", n = 200)
#' g_raw  <- build_citation_network(papers)
#'
#' # Check for cycles and remove if needed
#' check_dag(g_raw)
#' g_dag <- igraph::delete_edges(g_raw, mpaR:::.find_back_edges(g_raw))
#'
#' # Run MPA
#' g      <- traversal_weights(g_dag)
#' result <- mpa(g)
#' plot_mpa(g, result$global, scope = "full")
#' }
#'
#' @export
build_citation_network <- function(data, closed = TRUE) {

  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) {
    rlang::abort("`data` must be a non-empty data frame from fetch_openalex() or similar.")
  }

  missing_cols <- setdiff(c("id", "referenced_works"), names(data))
  if (length(missing_cols) > 0L) {
    rlang::abort(sprintf(
      "Required column(s) missing from `data`: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  if (!is.list(data$referenced_works)) {
    rlang::abort("`referenced_works` must be a list column (each element a character vector).")
  }

  all_ids <- data$id

  # --- Build edge list --------------------------------------------------------
  edge_list <- lapply(seq_len(nrow(data)), function(i) {
    refs <- data$referenced_works[[i]]
    if (length(refs) == 0L) return(NULL)
    if (closed) refs <- refs[refs %in% all_ids]
    if (length(refs) == 0L) return(NULL)
    data.frame(from = data$id[i], to = refs, stringsAsFactors = FALSE)
  })

  edge_df <- do.call(rbind, Filter(Negate(is.null), edge_list))

  if (is.null(edge_df) || nrow(edge_df) == 0L) {
    hint <- if (closed)
      " Try closed = FALSE to include references outside the dataset."
    else
      ""
    rlang::abort(paste0("No citation links found in the data.", hint))
  }

  # --- Vertex attribute table -------------------------------------------------
  attr_cols <- intersect(
    c("id", "title", "doi", "year", "first_author",
      "journal", "abstract", "cited_by_count"),
    names(data)
  )
  vattr <- data[, attr_cols, drop = FALSE]

  # When closed = FALSE, add stub rows for external referenced works
  extra_ids <- setdiff(unique(c(edge_df$from, edge_df$to)), vattr$id)
  if (length(extra_ids) > 0L) {
    stub <- data.frame(
      id             = extra_ids,
      title          = NA_character_,
      doi            = NA_character_,
      year           = NA_integer_,
      first_author   = NA_character_,
      journal        = NA_character_,
      abstract       = NA_character_,
      cited_by_count = NA_integer_,
      stringsAsFactors = FALSE
    )
    stub <- stub[, intersect(names(stub), attr_cols), drop = FALSE]
    vattr <- rbind(vattr, stub)
  }

  # --- Build igraph -----------------------------------------------------------
  g <- igraph::graph_from_data_frame(
    d        = edge_df,
    directed = TRUE,
    vertices = vattr
  )

  message(sprintf(
    "Citation network: %d nodes, %d edges%s.",
    igraph::vcount(g),
    igraph::ecount(g),
    if (closed) " (closed)" else " (open - includes external references)"
  ))

  g
}
