#' Extract main paths from every component of a disconnected network
#'
#' @description
#' Real-world citation networks (e.g. cross-sector patent networks) often
#' consist of many weakly connected components rather than a single giant
#' component. \code{component_paths()} decomposes the network into its weakly
#' connected components, runs [traversal_weights()] and [main_path()] on each
#' one independently, and returns the combined result.
#'
#' Components smaller than \code{min_size} vertices are silently dropped,
#' keeping only technologically significant sub-networks, similarly to the
#' key-route selection strategy of Liu et al. (2019).
#'
#' @param x An \code{igraph} DAG or a data frame / matrix edge list.
#' @param type Main-path extraction strategy passed to [main_path()]:
#'   \code{"global"} (default), \code{"local"}, or \code{"key_route"}.
#' @param weight Traversal weight: \code{"SPC"} (default), \code{"SPLC"}, or
#'   \code{"SPNP"}.
#' @param k Number of key-route seed edges per component. Only used when
#'   \code{type = "key_route"}. Defaults to \code{1L}.
#' @param min_size Integer. Components with fewer than \code{min_size} vertices
#'   are excluded. Defaults to \code{3L}.
#'
#' @return A named list of \code{igraph} subgraphs, one per retained component,
#'   named \code{"component_1"}, \code{"component_2"}, etc. (ordered by
#'   decreasing component size). Each subgraph contains only the main-path
#'   vertices and edges for that component, with the traversal weight as an
#'   edge attribute.
#'
#'   The full decomposition summary is attached as attribute
#'   \code{"component_summary"}: a data frame with columns
#'   \code{component}, \code{n_vertices}, \code{n_edges}, and
#'   \code{retained}.
#'
#' @references
#' Liu, J. S., Chen, H. H., Ho, M. H. C., & Li, Y. C. (2012). Citations
#' networks as a research tool. *Journal of the American Society for
#' Information Science and Technology*, **63**(9).
#'
#' Liu, P., Guo, Q., Yet, F., & Chen, X. (2019). Few-shot learning with
#' key-route main paths. *Scientometrics*, **121**(3), 1437–1451.
#'
#' @examples
#' library(igraph)
#'
#' # Two disconnected chains: 1->2->3  and  4->5->6->7
#' el <- data.frame(
#'   from = c(1, 2, 4, 5, 6),
#'   to   = c(2, 3, 5, 6, 7)
#' )
#'
#' paths <- component_paths(el, type = "global", weight = "SPC")
#' length(paths)          # 2 components
#' attr(paths, "component_summary")
#'
#' # Inspect the larger component
#' igraph::as_edgelist(paths[["component_1"]])
#'
#' @export
component_paths <- function(x,
                            type     = c("global", "local", "key_route"),
                            weight   = c("SPC", "SPLC", "SPNP"),
                            k        = 1L,
                            min_size = 3L) {

  type   <- match.arg(type)
  weight <- match.arg(weight)
  g      <- .to_dag(x)

  # ── Decompose into weakly connected components ────────────────────────────
  membership <- igraph::components(g, mode = "weak")$membership
  comp_ids   <- sort(unique(membership))

  # Build summary and order by size (largest first)
  comp_sizes <- tabulate(membership)[comp_ids]
  ord        <- order(comp_sizes, decreasing = TRUE)
  comp_ids   <- comp_ids[ord]
  comp_sizes <- comp_sizes[ord]

  summary_rows <- vector("list", length(comp_ids))
  results      <- vector("list", length(comp_ids))
  retained_n   <- 0L

  for (i in seq_along(comp_ids)) {
    cid    <- comp_ids[i]
    v_ids  <- which(membership == cid)
    sub_g  <- igraph::induced_subgraph(g, v_ids)

    n_v <- igraph::vcount(sub_g)
    n_e <- igraph::ecount(sub_g)
    keep <- n_v >= min_size

    summary_rows[[i]] <- data.frame(
      component  = i,
      n_vertices = n_v,
      n_edges    = n_e,
      retained   = keep,
      stringsAsFactors = FALSE
    )

    if (!keep) next

    # Compute weights + extract path for this component
    sub_w  <- traversal_weights(sub_g, method = weight)
    sub_mp <- tryCatch(
      main_path(sub_w, type = type, weight = weight, k = k),
      error = function(e) NULL
    )

    if (!is.null(sub_mp) && igraph::ecount(sub_mp) > 0L) {
      retained_n <- retained_n + 1L
      results[[i]] <- sub_mp
    }
  }

  # Filter out NULLs and name
  keep_idx <- !vapply(results, is.null, logical(1))
  results  <- results[keep_idx]
  names(results) <- paste0("component_", seq_along(results))

  summary_df <- do.call(rbind, summary_rows)
  attr(results, "component_summary") <- summary_df

  if (length(results) == 0L) {
    message(
      "No components with >= ", min_size, " vertices were found. ",
      "Try lowering `min_size`."
    )
  } else {
    message(sprintf(
      "Retained %d component(s) out of %d total (min_size = %d).",
      length(results), length(comp_ids), min_size
    ))
  }

  results
}
