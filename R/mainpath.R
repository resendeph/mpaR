#' Extract the main path(s) from a traversal-weighted DAG
#'
#' @description
#' Given a DAG that already carries edge-traversal weights (see
#' [traversal_weights()]), extracts the main path(s) using one of three
#' strategies:
#'
#' * **`"global"`** – the single source-to-sink path whose cumulative edge
#'   weight is highest (longest-path dynamic programming on the DAG).
#' * **`"local"`** – a greedy forward search from *every* source: at each node
#'   follow the highest-weight outgoing edge until a sink is reached.  Returns
#'   the union of all such paths.
#' * **`"key_route"`** – selects the top-\eqn{k} highest-weight edges (the
#'   *key routes*), then, for each key-route edge, finds the best incoming path
#'   from a source and the best outgoing path to a sink; the union of all these
#'   extended paths forms the key-route main path.
#'
#' ## Broadening the path with \code{threshold}
#'
#' The classic algorithms above produce a single, narrow path.  The
#' \code{threshold} argument relaxes this to include near-optimal edges,
#' producing a wider subgraph that captures alternative routes through the
#' network.  Set \code{threshold} to a value in \code{(0, 1)}:
#'
#' * **`"global"`** – runs both a forward and a backward dynamic-programming
#'   pass.  An edge \eqn{(u \to v, w)} is included if the best source-to-sink
#'   path *through* that edge has total weight
#'   \eqn{\ge threshold \times dp^*}, where \eqn{dp^*} is the optimal total
#'   weight.  At \code{threshold = 0.8}, all edges on paths within 80 \% of
#'   optimal are included.
#'
#' * **`"local"`** – at each node, follows every outgoing edge whose weight is
#'   \eqn{\ge threshold \times \max(\text{outgoing weights})}, instead of only
#'   the single best.  Branches are explored via BFS until all reachable sinks
#'   are reached.
#'
#' * **`"key_route"`** – expands the seed set to all edges with weight
#'   \eqn{\ge threshold \times w_k}, where \eqn{w_k} is the \eqn{k}-th
#'   highest weight.  Useful when you want to pick \eqn{k} seeds but also
#'   catch edges of similar importance.
#'
#' @param g An \code{igraph} DAG with at least one of the edge attributes
#'   \code{SPC}, \code{SPLC}, or \code{SPNP} (produced by
#'   [traversal_weights()]).
#' @param type Character; one of \code{"global"} (default), \code{"local"}, or
#'   \code{"key_route"}.
#' @param weight Character; which traversal-weight edge attribute to use.
#'   Defaults to the first of \code{SPC}, \code{SPLC}, \code{SPNP} found on
#'   \code{g}.
#' @param k Integer; number of key-route seed edges.  Only used when
#'   \code{type = "key_route"}.  Defaults to \code{1L}.
#' @param threshold Numeric in \code{(0, 1]}.  \code{1.0} (default) gives the
#'   classic algorithm.  Values below \code{1.0} broaden the path by including
#'   near-optimal edges — see the **Broadening** section above.
#'
#' @return An \code{igraph} subgraph containing only the vertices and edges
#'   that belong to the main path(s).  The subgraph retains the vertex
#'   \code{name} attribute and the original traversal-weight edge attribute.
#'
#' @seealso [mpa()] for a one-call wrapper, [traversal_weights()] to compute
#'   edge weights, [plot_mpa()] to visualise the result.
#'
#' @references
#' Hummon, N. P., & Doreian, P. (1989). Connectivity in a citation network:
#' The development of DNA theory. *Social Networks*, **11**(1), 39–63.
#' \doi{10.1016/0378-8733(89)90017-3}
#'
#' Garfield, E., Pudovkin, A. I., & Istomin, V. S. (2003). Why do we need
#' algorithmic historiography? *Journal of the American Society for Information
#' Science and Technology*, **54**(5), 400–412.
#'
#' @examples
#' library(igraph)
#'
#' # Small diamond-shaped network (two competing routes from 1 to 5)
#' el <- data.frame(
#'   from = c(1, 1, 2, 3, 2, 4),
#'   to   = c(2, 3, 4, 4, 5, 5)
#' )
#' g_w <- traversal_weights(el, method = "SPC")
#'
#' # --- Classic algorithms (threshold = 1.0, default) -----------------------
#'
#' # Global: single best source-to-sink path
#' mp_global <- main_path(g_w, type = "global", weight = "SPC")
#' igraph::as_edgelist(mp_global)
#' igraph::vcount(mp_global)   # narrow — just the optimal chain
#'
#' # Local: greedy from every source, union of all resulting paths
#' mp_local <- main_path(g_w, type = "local", weight = "SPC")
#' igraph::vcount(mp_local)
#'
#' # Key-route: extend from the single highest-weight edge
#' mp_kr <- main_path(g_w, type = "key_route", weight = "SPC", k = 1L)
#'
#' # Key-route with k = 3: seed from the top 3 edges
#' mp_kr3 <- main_path(g_w, type = "key_route", weight = "SPC", k = 3L)
#' igraph::vcount(mp_kr3)
#'
#' # --- Broadened paths (threshold < 1.0) -----------------------------------
#'
#' # Global, broadened: include all edges on paths >= 80% of optimal weight
#' mp_broad <- main_path(g_w, type = "global", weight = "SPC", threshold = 0.8)
#' igraph::vcount(mp_broad)   # more nodes than mp_global
#'
#' # Local, broadened: at each node follow edges within 90% of the local max
#' mp_local_broad <- main_path(g_w, type = "local", weight = "SPC",
#'                             threshold = 0.9)
#'
#' # Key-route, broadened: cast a wider seed net around k = 1
#' mp_kr_broad <- main_path(g_w, type = "key_route", weight = "SPC",
#'                          k = 1L, threshold = 0.7)
#'
#' # Compare path sizes as threshold decreases
#' sizes <- sapply(c(1.0, 0.9, 0.8, 0.7, 0.5), function(t) {
#'   igraph::vcount(main_path(g_w, type = "global", weight = "SPC",
#'                            threshold = t))
#' })
#' names(sizes) <- c("1.0", "0.9", "0.8", "0.7", "0.5")
#' print(sizes)
#'
#' @export
main_path <- function(g, type = c("global", "local", "key_route"),
                      weight = NULL, k = 1L, threshold = 1.0) {
  if (!inherits(g, "igraph")) {
    rlang::abort("`g` must be an igraph object.  Run traversal_weights() first.")
  }
  type <- match.arg(type)

  threshold <- as.numeric(threshold)
  if (is.na(threshold) || threshold <= 0 || threshold > 1) {
    rlang::abort("`threshold` must be a number in (0, 1].")
  }

  # Resolve weight attribute ---------------------------------------------------
  weight <- .resolve_weight(g, weight)
  w      <- igraph::edge_attr(g, weight)   # numeric vector, length = E

  # Dispatch -------------------------------------------------------------------
  edge_ids <- switch(
    type,
    global    = .global_main_path(g, w, threshold),
    local     = .local_main_path(g, w, threshold),
    key_route = .key_route_main_path(g, w, k, threshold)
  )

  igraph::subgraph.edges(g, eids = edge_ids, delete.vertices = TRUE)
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' @noRd
.resolve_weight <- function(g, weight) {
  available <- c("SPC", "SPLC", "SPNP")
  present   <- available[available %in% igraph::edge_attr_names(g)]

  if (is.null(weight)) {
    if (length(present) == 0L) {
      rlang::abort(
        "No traversal weight found on `g`. Run traversal_weights() first."
      )
    }
    return(present[1L])
  }

  if (!weight %in% igraph::edge_attr_names(g)) {
    rlang::abort(
      paste0("Edge attribute '", weight, "' not found on `g`.")
    )
  }
  weight
}




#' @noRd
#' Greedy forward BFS from every source; at each node includes all outgoing
#' edges with weight >= threshold * max_outgoing_weight.
.local_main_path <- function(g, w, threshold = 1.0) {
  ss     <- .sources_sinks(g)
  el     <- igraph::as_edgelist(g, names = FALSE)
  to_v   <- el[, 2L]

  edge_set  <- integer(0)
  visited_v <- logical(igraph::vcount(g))

  queue <- ss$sources
  while (length(queue) > 0L) {
    v     <- queue[[1L]]
    queue <- queue[-1L]
    if (visited_v[v]) next
    visited_v[v] <- TRUE

    out_e <- as.integer(igraph::incident(g, v, mode = "out"))
    if (length(out_e) == 0L) next   # sink

    max_w  <- max(w[out_e])
    cutoff <- threshold * max_w
    keep_e <- out_e[w[out_e] >= cutoff - 1e-10]

    edge_set <- union(edge_set, keep_e)
    queue    <- c(queue, to_v[keep_e])
  }

  edge_set
}



#' @noRd
#' Proper backward trace that also tracks the vertex.
#' prev_e: integer vector, prev_e[v] = edge id leading to v (0 = source)
#' from_v: integer vector mapping edge id -> from vertex
.trace_path <- function(prev_e, v, from_v = NULL) {
  if (is.null(from_v)) {
    # Reconstruct from_v is unavailable — caller must supply it.
    # This overload is called with from_v supplied; see below.
    return(integer(0))
  }
  edges <- integer(0)
  repeat {
    eid <- prev_e[v]
    if (eid == 0L) break
    edges <- c(eid, edges)
    v <- from_v[eid]
  }
  edges
}


# Re-define .global_main_path with from_v available in scope and threshold support
.global_main_path <- function(g, w, threshold = 1.0) {
  n   <- igraph::vcount(g)
  ord <- as.integer(.topo_order(g))
  ss  <- .sources_sinks(g)

  el     <- igraph::as_edgelist(g, names = FALSE)
  from_v <- el[, 1L]
  to_v   <- el[, 2L]

  # Forward DP: dp_fwd[v] = best cumulative weight to reach v from any source
  dp_fwd <- rep(-Inf, n)
  prev_e <- integer(n)
  dp_fwd[ss$sources] <- 0.0

  for (v in ord) {
    in_e <- as.integer(igraph::incident(g, v, mode = "in"))
    if (length(in_e) == 0L) next
    for (eid in in_e) {
      cand <- dp_fwd[from_v[eid]] + w[eid]
      if (cand > dp_fwd[v]) {
        dp_fwd[v] <- cand
        prev_e[v] <- eid
      }
    }
  }

  best_sink <- ss$sinks[which.max(dp_fwd[ss$sinks])]
  best_dp   <- dp_fwd[best_sink]

  # threshold == 1: classic single-path trace
  if (threshold >= 1.0) {
    return(.trace_path(prev_e, best_sink, from_v))
  }

  # threshold < 1: include all edges on near-optimal paths
  # Backward DP: dp_bwd[v] = best cumulative weight from v to any sink
  dp_bwd <- rep(-Inf, n)
  dp_bwd[ss$sinks] <- 0.0

  for (v in rev(ord)) {
    out_e <- as.integer(igraph::incident(g, v, mode = "out"))
    if (length(out_e) == 0L) next
    for (eid in out_e) {
      cand <- dp_bwd[to_v[eid]] + w[eid]
      if (cand > dp_bwd[v]) dp_bwd[v] <- cand
    }
  }

  # An edge (u->v, w[e]) is included if the best path through it is at least
  # threshold * best_dp.  Edges unreachable from any source or leading nowhere
  # have dp_fwd[u] or dp_bwd[v] == -Inf and are excluded automatically.
  cutoff   <- threshold * best_dp
  edge_set <- which(dp_fwd[from_v] + w + dp_bwd[to_v] >= cutoff - 1e-10)
  as.integer(edge_set)
}


.key_route_main_path <- function(g, w, k, threshold = 1.0) {
  k <- max(1L, as.integer(k))
  el     <- igraph::as_edgelist(g, names = FALSE)
  from_v <- el[, 1L]
  to_v   <- el[, 2L]
  ss     <- .sources_sinks(g)
  ord    <- as.integer(.topo_order(g))
  n      <- igraph::vcount(g)

  # Forward DP -----------------------------------------------------------------
  dp_fwd <- rep(-Inf, n); pe_fwd <- integer(n)
  dp_fwd[ss$sources] <- 0.0

  for (v in ord) {
    in_e <- as.integer(igraph::incident(g, v, mode = "in"))
    if (length(in_e) == 0L) next
    for (eid in in_e) {
      cand <- dp_fwd[from_v[eid]] + w[eid]
      if (cand > dp_fwd[v]) { dp_fwd[v] <- cand; pe_fwd[v] <- eid }
    }
  }

  # Backward DP ----------------------------------------------------------------
  dp_bwd <- rep(-Inf, n); pe_bwd <- integer(n)
  dp_bwd[ss$sinks] <- 0.0

  for (v in rev(ord)) {
    out_e <- as.integer(igraph::incident(g, v, mode = "out"))
    if (length(out_e) == 0L) next
    for (eid in out_e) {
      cand <- dp_bwd[to_v[eid]] + w[eid]
      if (cand > dp_bwd[v]) { dp_bwd[v] <- cand; pe_bwd[v] <- eid }
    }
  }

  # Helper: forward trace (v -> sink) using pe_bwd
  trace_fwd <- function(v) {
    edges <- integer(0)
    repeat {
      eid <- pe_bwd[v]
      if (eid == 0L) break
      edges <- c(edges, eid)
      v <- to_v[eid]
    }
    edges
  }

  # Seed edges: top-k by weight; threshold expands this to all edges within
  # threshold * w[k-th] — lets the user cast a wider net without increasing k.
  sorted_w <- sort(w, decreasing = TRUE)
  kth_w    <- sorted_w[min(k, length(w))]
  top_k    <- which(w >= threshold * kth_w - 1e-10)

  edge_set <- integer(0)
  for (seed_e in top_k) {
    u <- from_v[seed_e]
    v <- to_v[seed_e]
    edge_set <- union(edge_set, .trace_path(pe_fwd, u, from_v))
    edge_set <- union(edge_set, seed_e)
    edge_set <- union(edge_set, trace_fwd(v))
  }

  edge_set
}
