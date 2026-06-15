#' Extract the main path(s) from a traversal-weighted DAG
#'
#' @description
#' Given a DAG that already carries edge-traversal weights (see
#' [traversal_weights()]), extracts the main path(s) using one of three
#' strategies:
#'
#' * **`"global"`** – the single source-to-sink path whose cumulative edge
#'   weight is highest (longest-path DP on the DAG).
#' * **`"local"`** – a greedy forward search from *every* source: at each node
#'   follow the highest-weight outgoing edge until a sink is reached.  Returns
#'   the union of all such paths.
#' * **`"key_route"`** – selects the top-\eqn{k} highest-weight edges (the
#'   *key routes*), then, for each key-route edge, finds the best incoming path
#'   from a source and the best outgoing path to a sink; the union of all these
#'   extended paths forms the key-route main path.
#'
#' @param g An \code{igraph} DAG with at least one of the edge attributes
#'   \code{SPC}, \code{SPLC}, or \code{SPNP} (produced by
#'   [traversal_weights()]).
#' @param type Character; one of \code{"global"} (default), \code{"local"}, or
#'   \code{"key_route"}.
#' @param weight Character; which edge attribute to use as the weight.
#'   Defaults to the first of \code{SPC}, \code{SPLC}, \code{SPNP} found on
#'   \code{g}.
#' @param k Integer; number of key-route edges to seed from.  Only used when
#'   \code{type = "key_route"}.  Defaults to \code{1L}.
#'
#' @return An \code{igraph} subgraph containing only the vertices and edges
#'   that belong to the main path(s).  The subgraph carries a vertex attribute
#'   \code{"name"} and the original edge weight attribute.
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
#' el <- data.frame(
#'   from = c(1, 2, 2, 3, 4),
#'   to   = c(2, 3, 4, 5, 5)
#' )
#' g_w <- traversal_weights(el)
#' mp  <- main_path(g_w, type = "global")
#' igraph::as_edgelist(mp)
#'
#' @export
main_path <- function(g, type = c("global", "local", "key_route"),
                      weight = NULL, k = 1L) {
  if (!inherits(g, "igraph")) {
    rlang::abort("`g` must be an igraph object.  Run traversal_weights() first.")
  }
  type <- match.arg(type)

  # Resolve weight attribute ---------------------------------------------------
  weight <- .resolve_weight(g, weight)
  w      <- igraph::edge_attr(g, weight)   # numeric vector, length = E

  # Dispatch -------------------------------------------------------------------
  edge_ids <- switch(
    type,
    global    = .global_main_path(g, w),
    local     = .local_main_path(g, w),
    key_route = .key_route_main_path(g, w, k)
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
#' Longest-path DP on DAG; returns the edge IDs forming the optimal path.
.global_main_path <- function(g, w) {
  n   <- igraph::vcount(g)
  ord <- as.integer(.topo_order(g))
  ss  <- .sources_sinks(g)

  # dp[v]  = best cumulative weight to reach v from any source
  # prev_e[v] = edge id used to reach v on the best path
  dp     <- rep(-Inf, n)
  prev_e <- integer(n)      # 0 = no predecessor

  dp[ss$sources] <- 0.0

  el        <- igraph::as_edgelist(g, names = FALSE)
  from_v    <- el[, 1L]
  to_v      <- el[, 2L]

  for (v in ord) {
    in_e <- igraph::incident(g, v, mode = "in")
    if (length(in_e) == 0L) next     # source
    for (eid in as.integer(in_e)) {
      u    <- from_v[eid]
      cand <- dp[u] + w[eid]
      if (cand > dp[v]) {
        dp[v]     <- cand
        prev_e[v] <- eid
      }
    }
  }

  # Trace back from best sink
  best_sink <- ss$sinks[which.max(dp[ss$sinks])]
  .trace_path(prev_e, best_sink)
}


#' @noRd
#' Greedy forward search from every source; returns union of edge IDs.
.local_main_path <- function(g, w) {
  ss     <- .sources_sinks(g)
  el     <- igraph::as_edgelist(g, names = FALSE)
  to_v   <- el[, 2L]

  edge_set <- integer(0)

  for (src in ss$sources) {
    v <- src
    repeat {
      out_e <- as.integer(igraph::incident(g, v, mode = "out"))
      if (length(out_e) == 0L) break         # reached a sink
      best_e <- out_e[which.max(w[out_e])]
      edge_set <- union(edge_set, best_e)
      v <- to_v[best_e]
    }
  }

  edge_set
}


#' @noRd
#' Key-route: seed from top-k edges, extend to sources/sinks.
.key_route_main_path <- function(g, w, k) {
  k <- max(1L, as.integer(k))
  el     <- igraph::as_edgelist(g, names = FALSE)
  from_v <- el[, 1L]
  to_v   <- el[, 2L]
  ss     <- .sources_sinks(g)
  ord    <- as.integer(.topo_order(g))

  # Best weight reaching each vertex from any source (forward DP)
  n      <- igraph::vcount(g)
  dp_fwd <- rep(-Inf, n)
  pe_fwd <- integer(n)
  dp_fwd[ss$sources] <- 0.0

  for (v in ord) {
    in_e <- as.integer(igraph::incident(g, v, mode = "in"))
    if (length(in_e) == 0L) next
    for (eid in in_e) {
      cand <- dp_fwd[from_v[eid]] + w[eid]
      if (cand > dp_fwd[v]) {
        dp_fwd[v]  <- cand
        pe_fwd[v]  <- eid
      }
    }
  }

  # Best weight reaching any sink from each vertex (backward DP)
  dp_bwd <- rep(-Inf, n)
  pe_bwd <- integer(n)
  dp_bwd[ss$sinks] <- 0.0

  for (v in rev(ord)) {
    out_e <- as.integer(igraph::incident(g, v, mode = "out"))
    if (length(out_e) == 0L) next
    for (eid in out_e) {
      cand <- dp_bwd[to_v[eid]] + w[eid]
      if (cand > dp_bwd[v]) {
        dp_bwd[v]  <- cand
        pe_bwd[v]  <- eid
      }
    }
  }

  # Top-k edges by weight
  top_k   <- order(w, decreasing = TRUE)[seq_len(min(k, length(w)))]
  edge_set <- integer(0)

  for (seed_e in top_k) {
    u <- from_v[seed_e]
    v <- to_v[seed_e]
    # path from source to u
    edge_set <- union(edge_set, .trace_path(pe_fwd, u))
    # the seed edge itself
    edge_set <- union(edge_set, seed_e)
    # path from v to best sink
    edge_set <- union(edge_set, .trace_path_fwd(pe_bwd, v, to_v))
  }

  edge_set
}


#' @noRd
#' Trace backward from `v` using `prev_e` until prev_e[v] == 0.
.trace_path <- function(prev_e, v) {
  edges <- integer(0)
  repeat {
    eid <- prev_e[v]
    if (eid == 0L) break
    edges <- c(eid, edges)
    # Need to get the from-vertex; store mapping via closure isn't ideal,
    # so we pass edge-list separately in actual call sites.  Here we just
    # return the edge IDs; the caller resolves vertices.
    v <- .parent_vertex(prev_e, v)   # placeholder — replaced below
    break  # safety; real impl follows
  }
  edges
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


# Re-define .global_main_path with from_v available in scope
# (R's lexical scoping means we need to pass from_v explicitly)
.global_main_path <- function(g, w) {
  n   <- igraph::vcount(g)
  ord <- as.integer(.topo_order(g))
  ss  <- .sources_sinks(g)

  dp     <- rep(-Inf, n)
  prev_e <- integer(n)

  dp[ss$sources] <- 0.0

  el     <- igraph::as_edgelist(g, names = FALSE)
  from_v <- el[, 1L]
  to_v   <- el[, 2L]

  for (v in ord) {
    in_e <- as.integer(igraph::incident(g, v, mode = "in"))
    if (length(in_e) == 0L) next
    for (eid in in_e) {
      cand <- dp[from_v[eid]] + w[eid]
      if (cand > dp[v]) {
        dp[v]     <- cand
        prev_e[v] <- eid
      }
    }
  }

  best_sink <- ss$sinks[which.max(dp[ss$sinks])]
  .trace_path(prev_e, best_sink, from_v)
}


.key_route_main_path <- function(g, w, k) {
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

  top_k    <- order(w, decreasing = TRUE)[seq_len(min(k, length(w)))]
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
