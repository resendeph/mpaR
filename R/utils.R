#' Convert input to an igraph DAG, attaching a standardised vertex name attribute.
#'
#' Accepts either an \pkg{igraph} graph object or a data frame with at least two
#' columns (first two columns = \code{from}, \code{to}).  Additional columns are
#' treated as edge attributes.
#'
#' @param x An \code{igraph} object or a data frame / matrix.
#' @param directed Logical; force directed graph when coercing from data frame.
#' @return A directed \code{igraph} object whose vertices carry a
#'   \code{"name"} attribute.
#' @keywords internal
#' @noRd
.to_dag <- function(x, directed = TRUE) {
  if (inherits(x, "igraph")) {
    g <- x
  } else if (is.data.frame(x) || is.matrix(x)) {
    df <- as.data.frame(x)
    if (ncol(df) < 2L) {
      rlang::abort("`x` must have at least two columns (from, to).")
    }
    g <- igraph::graph_from_data_frame(df, directed = directed)
  } else {
    rlang::abort(
      paste0(
        "`x` must be an igraph object or a data frame / matrix, ",
        "not <", class(x)[1L], ">."
      )
    )
  }

  if (!igraph::is_directed(g)) {
    rlang::abort("The graph must be directed.")
  }
  if (!igraph::is_dag(g)) {
    rlang::abort(
      "The graph must be a directed acyclic graph (DAG). ",
      "Check for cycles with `igraph::is_dag()`."
    )
  }

  # Ensure vertex names exist
  if (is.null(igraph::V(g)$name)) {
    igraph::V(g)$name <- as.character(seq_len(igraph::vcount(g)))
  }

  g
}


#' Return a topological ordering of vertex indices (1-based).
#' @keywords internal
#' @noRd
.topo_order <- function(g) {
  igraph::topo_sort(g, mode = "out")
}


#' Identify source vertices (in-degree 0) and sink vertices (out-degree 0).
#' Returns a list with integer vectors \code{sources} and \code{sinks}.
#' @keywords internal
#' @noRd
.sources_sinks <- function(g) {
  indeg  <- igraph::degree(g, mode = "in")
  outdeg <- igraph::degree(g, mode = "out")
  list(
    sources = which(indeg  == 0L),
    sinks   = which(outdeg == 0L)
  )
}


#' Compute the forward/backward path-count vectors shared by
#' \code{traversal_weights()}, \code{edge_weights()}, and \code{node_weights()}.
#'
#' \code{f[v]}  = # paths from any global source to \code{v}.
#' \code{fa[v]} = 1 + sum of fa over predecessors of v; paths from any ancestor
#'   of \code{v} (including \code{v} itself).
#' \code{b[v]}  = # paths from \code{v} to any global sink.
#' \code{ba[v]} = 1 + sum of ba over successors of v; paths from \code{v} to any
#'   descendant (including \code{v} itself).
#'
#' @param g An \code{igraph} DAG (already validated by \code{.to_dag()}).
#' @return A list with numeric vectors \code{f}, \code{fa}, \code{b}, \code{ba},
#'   each indexed by vertex id.
#' @keywords internal
#' @noRd
.path_counts <- function(g) {
  L      <- .dag_layers(g)
  n      <- L$n
  from_v <- L$from_v
  to_v   <- L$to_v

  # Forward pass: f (paths from sources), fa (paths from ancestors incl. self)
  f  <- numeric(n); fa <- numeric(n)
  f[L$sources] <- 1.0; fa[L$sources] <- 1.0
  for (lay in seq_len(L$maxlayer)) {
    vs <- which(L$layer_of == lay)
    es <- unlist(L$in_e[vs], use.names = FALSE)
    if (length(es) == 0L) next
    tv  <- to_v[es]; fv <- from_v[es]
    ag  <- rowsum(cbind(f[fv], fa[fv]), group = tv, reorder = FALSE)
    idx <- as.integer(rownames(ag))
    f[idx]  <- ag[, 1L]
    fa[idx] <- 1.0 + ag[, 2L]
  }

  # Backward pass: b (paths to sinks), ba (paths to descendants incl. self)
  b  <- numeric(n); ba <- numeric(n)
  b[L$sinks] <- 1.0; ba[L$sinks] <- 1.0
  if (L$maxlayer >= 1L) for (lay in (L$maxlayer - 1L):0L) {
    vs <- which(L$layer_of == lay)
    es <- unlist(L$out_e[vs], use.names = FALSE)
    if (length(es) == 0L) next
    fv  <- from_v[es]; tv <- to_v[es]
    ag  <- rowsum(cbind(b[tv], ba[tv]), group = fv, reorder = FALSE)
    idx <- as.integer(rownames(ag))
    b[idx]  <- ag[, 1L]
    ba[idx] <- 1.0 + ag[, 2L]
  }

  list(f = f, fa = fa, b = b, ba = ba)
}


#' Build the shared DAG structure used by all MPA routines.
#'
#' Constructs edge-id adjacency lists with base-R \code{split()} (one pass, no
#' per-vertex igraph calls) and a longest-path topological layering (Kahn
#' peeling): \code{layer_of[v]} is the length of the longest path from any
#' source to \code{v}.  Because every arc goes from a strictly lower layer to a
#' higher one, all vertices in a layer can be relaxed together, which lets the
#' DP routines run one vectorised step per layer instead of one per vertex.
#' @return list(n, m, from_v, to_v, in_e, out_e, sources, sinks, layer_of,
#'   maxlayer)
#' @keywords internal
#' @noRd
.dag_layers <- function(g) {
  n  <- igraph::vcount(g)
  m  <- igraph::ecount(g)
  el <- igraph::as_edgelist(g, names = FALSE)
  from_v <- as.integer(el[, 1L])
  to_v   <- as.integer(el[, 2L])

  out_e <- split(seq_len(m), factor(from_v, levels = seq_len(n)))
  in_e  <- split(seq_len(m), factor(to_v,   levels = seq_len(n)))
  indeg   <- tabulate(to_v,   nbins = n)
  outdeg  <- tabulate(from_v, nbins = n)
  sources <- which(indeg  == 0L)
  sinks   <- which(outdeg == 0L)

  layer_of  <- integer(n)
  processed <- logical(n)
  remaining <- indeg
  q <- sources; processed[q] <- TRUE
  cur <- 0L
  repeat {
    es <- unlist(out_e[q], use.names = FALSE)
    if (length(es) == 0L) break
    remaining <- remaining - tabulate(to_v[es], nbins = n)
    nxt <- which(remaining == 0L & !processed)
    if (length(nxt) == 0L) break
    cur <- cur + 1L
    layer_of[nxt]  <- cur
    processed[nxt] <- TRUE
    q <- nxt
  }

  list(n = n, m = m, from_v = from_v, to_v = to_v, in_e = in_e, out_e = out_e,
       sources = sources, sinks = sinks, layer_of = layer_of, maxlayer = cur)
}


#' Forward and backward longest-path DP on a layered DAG.
#'
#' Returns best cumulative weights and predecessor-edge pointers in both
#' directions.  Ties are broken toward the smallest edge id (stable sort), which
#' reproduces the classic single-path trace.
#' @return list(dp_fwd, pe_fwd, dp_bwd, pe_bwd)
#' @keywords internal
#' @noRd
.longest_paths <- function(L, w) {
  n <- L$n; from_v <- L$from_v; to_v <- L$to_v

  dp_fwd <- rep(-Inf, n); pe_fwd <- integer(n); dp_fwd[L$sources] <- 0
  for (lay in seq_len(L$maxlayer)) {
    vs <- which(L$layer_of == lay)
    es <- unlist(L$in_e[vs], use.names = FALSE)
    if (length(es) == 0L) next
    val <- dp_fwd[from_v[es]] + w[es]; tv <- to_v[es]
    o   <- order(tv, -val); es <- es[o]; tv <- tv[o]; val <- val[o]
    keep <- !duplicated(tv)
    dp_fwd[tv[keep]] <- val[keep]; pe_fwd[tv[keep]] <- es[keep]
  }

  dp_bwd <- rep(-Inf, n); pe_bwd <- integer(n); dp_bwd[L$sinks] <- 0
  if (L$maxlayer >= 1L) for (lay in (L$maxlayer - 1L):0L) {
    vs <- which(L$layer_of == lay)
    es <- unlist(L$out_e[vs], use.names = FALSE)
    if (length(es) == 0L) next
    val <- dp_bwd[to_v[es]] + w[es]; fv <- from_v[es]
    o   <- order(fv, -val); es <- es[o]; fv <- fv[o]; val <- val[o]
    keep <- !duplicated(fv)
    dp_bwd[fv[keep]] <- val[keep]; pe_bwd[fv[keep]] <- es[keep]
  }

  list(dp_fwd = dp_fwd, pe_fwd = pe_fwd, dp_bwd = dp_bwd, pe_bwd = pe_bwd)
}


#' Trace v -> sink using backward predecessor edges (pe_bwd).
#' @keywords internal
#' @noRd
.trace_fwd <- function(pe_bwd, v, to_v) {
  edges <- integer(0)
  repeat {
    eid <- pe_bwd[v]
    if (eid == 0L) break
    edges <- c(edges, eid)
    v <- to_v[eid]
  }
  edges
}


#' Version-safe edge-induced subgraph (igraph 2.1 renamed the function).
#' @keywords internal
#' @noRd
.sub_edges <- function(g, eids) {
  f <- tryCatch(getExportedValue("igraph", "subgraph_from_edges"),
                error = function(e) NULL)
  if (is.null(f)) f <- getExportedValue("igraph", "subgraph.edges")
  f(g, eids, delete.vertices = TRUE)
}
