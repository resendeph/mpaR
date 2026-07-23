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
#' @param k_range Optional integer vector \code{c(start, end)}; only used when
#'   \code{type = "key_route"}.  Seeds from the key routes ranked \code{start}
#'   to \code{end} by weight, instead of the top \code{k}.  This mirrors Pajek's
#'   ability to compute a slice of key routes (e.g. \code{c(5, 25)}), which is
#'   useful for isolating the contribution of mid-tier routes or excluding the
#'   dominant trunk.  Note that a slice which omits the top routes is a
#'   decomposition tool, not the canonical cumulative main path.  When supplied,
#'   \code{k_range} takes precedence over \code{k}.
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
#' \doi{10.1016/0378-8733(89)90017-8}
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
                      weight = NULL, k = 1L, threshold = 1.0,
                      k_range = NULL) {
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
    key_route = .key_route_main_path(g, w, k, threshold, k_range)
  )

  .sub_edges(g, edge_ids)
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




#' Greedy forward BFS from every source; at each node includes all outgoing
#' edges with weight >= threshold * max_outgoing_weight.
#' @noRd
.local_main_path <- function(g, w, threshold = 1.0) {
  L      <- .dag_layers(g)
  from_v <- L$from_v; to_v <- L$to_v; n <- L$n

  # local maximum outgoing weight per source vertex (vectorised)
  maxout <- rep(-Inf, n)
  mo     <- tapply(w, from_v, max)
  maxout[as.integer(names(mo))] <- as.numeric(mo)

  active  <- logical(n); active[L$sources] <- TRUE
  edge_in <- logical(length(w))
  # visit vertices in layered (topological) order so each node is decided
  # before any of its successors
  for (v in order(L$layer_of)) {
    if (!active[v]) next
    oe <- L$out_e[[v]]
    if (length(oe) == 0L) next   # sink
    keep <- oe[w[oe] >= threshold * maxout[v] - 1e-10]
    edge_in[keep] <- TRUE
    active[to_v[keep]] <- TRUE
  }
  which(edge_in)
}



#' Proper backward trace that also tracks the vertex.
#' `prev_e`: integer vector, `prev_e[v]` = edge id leading to `v` (0 = source).
#' `from_v`: integer vector mapping edge id to its from-vertex.
#' @noRd
.trace_path <- function(prev_e, v, from_v = NULL) {
  if (is.null(from_v)) {
    # Reconstruct from_v is unavailable - caller must supply it.
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


.global_main_path <- function(g, w, threshold = 1.0) {
  L  <- .dag_layers(g)
  lp <- .longest_paths(L, w)

  best_sink <- L$sinks[which.max(lp$dp_fwd[L$sinks])]

  # threshold == 1: classic single best source-to-sink path
  if (threshold >= 1.0) {
    return(.trace_path(lp$pe_fwd, best_sink, L$from_v))
  }

  # threshold < 1: include every edge on a path within threshold * optimal.
  # An edge (u->v) qualifies if dp_fwd[u] + w + dp_bwd[v] >= threshold * best.
  # Unreachable edges carry -Inf on one side and drop out automatically.
  best    <- lp$dp_fwd[best_sink]
  through <- lp$dp_fwd[L$from_v] + w + lp$dp_bwd[L$to_v]
  as.integer(which(through >= threshold * best - 1e-10))
}


.key_route_main_path <- function(g, w, k = 1L, threshold = 1.0, k_range = NULL) {
  L  <- .dag_layers(g)
  lp <- .longest_paths(L, w)
  from_v <- L$from_v; to_v <- L$to_v
  m   <- length(w)
  ord <- order(w, decreasing = TRUE)   # arcs from most to least important

  # Seed selection --------------------------------------------------------------
  #  * k_range = c(start, end): the key routes ranked start..end by weight
  #    (Pajek-style slice; a decomposition tool, not the cumulative main path).
  #  * otherwise top-k: all arcs with weight >= threshold * (k-th largest),
  #    so exact ties at the boundary are included.
  if (!is.null(k_range)) {
    r     <- as.integer(k_range)
    start <- max(1L, min(r)); end <- min(m, max(r))
    seeds <- ord[start:end]
  } else {
    k     <- max(1L, as.integer(k))
    kth_w <- w[ord[min(k, m)]]
    seeds <- which(w >= threshold * kth_w - 1e-10)
  }

  # Extend each seed edge back to a source and forward to a sink.
  edge_in <- logical(m)
  for (e in seeds) {
    edge_in[.trace_path(lp$pe_fwd, from_v[e], from_v)] <- TRUE
    edge_in[e] <- TRUE
    edge_in[.trace_fwd(lp$pe_bwd, to_v[e], to_v)] <- TRUE
  }
  which(edge_in)
}


#' Sweep the key-route main path over multiple k values in one pass
#'
#' @description
#' Computes the key-route main path for several values of \code{k} while sharing
#' a single forward/backward dynamic-programming pass.  The DP does not depend on
#' \code{k}, so this is dramatically faster than calling [main_path()] in a loop
#' when exploring how the path grows with \code{k} — the standard way to choose
#' \code{k} is to look for a plateau in the number of nodes and arcs as \code{k}
#' increases.
#'
#' @param g An \code{igraph} DAG carrying a traversal weight (see
#'   [traversal_weights()]).
#' @param weight Character; which weight attribute to use.  Defaults to the
#'   first of \code{SPC}, \code{SPLC}, \code{SPNP} present on \code{g}.
#' @param k_values Integer vector of \code{k} values to evaluate.
#' @param threshold Numeric in \code{(0, 1]}; broadens the seed set exactly as
#'   in [main_path()].
#' @param return_graphs Logical; if \code{TRUE}, the key-route subgraph for each
#'   \code{k} is attached as \code{attr(result, "graphs")} (a list aligned to
#'   the rows of the returned data frame).
#'
#' @return A data frame with columns \code{k}, \code{n_nodes}, \code{n_arcs}.
#'   For each \code{k}, these match \code{main_path(type = "key_route", k = k)}.
#'
#' @seealso [main_path()]
#'
#' @examples
#' library(igraph)
#' el <- data.frame(from = c(1,1,2,3,2,4), to = c(2,3,4,4,5,5))
#' g  <- traversal_weights(el, method = "SPC")
#' key_route_sweep(g, weight = "SPC", k_values = c(1, 2, 3))
#'
#' @export
key_route_sweep <- function(g, weight = NULL,
                            k_values = c(1, 10, 20, 30, 40, 50),
                            threshold = 1.0, return_graphs = FALSE) {
  if (!inherits(g, "igraph")) {
    rlang::abort("`g` must be an igraph object.  Run traversal_weights() first.")
  }
  weight <- .resolve_weight(g, weight)
  w      <- igraph::edge_attr(g, weight)
  m      <- length(w)

  L  <- .dag_layers(g)
  lp <- .longest_paths(L, w)
  from_v <- L$from_v; to_v <- L$to_v

  ord         <- order(w, decreasing = TRUE)
  sorted_wval <- w[ord]
  k_values    <- sort(unique(as.integer(k_values)))

  edge_in <- logical(m)
  n_nodes <- integer(length(k_values))
  n_arcs  <- integer(length(k_values))
  graphs  <- vector("list", length(k_values))

  # memoise source/sink traces so each seed's chain is walked at most once
  cb <- new.env(hash = TRUE, parent = emptyenv())
  cf <- new.env(hash = TRUE, parent = emptyenv())
  tb <- function(u) { key <- as.character(u); v <- cb[[key]]
                      if (is.null(v)) { v <- .trace_path(lp$pe_fwd, u, from_v); cb[[key]] <- v }; v }
  tf <- function(u) { key <- as.character(u); v <- cf[[key]]
                      if (is.null(v)) { v <- .trace_fwd(lp$pe_bwd, u, to_v); cf[[key]] <- v }; v }

  pos <- 0L                              # seed edges added so far (monotone in k)
  for (i in seq_along(k_values)) {
    k  <- k_values[i]
    bw <- sorted_wval[min(k, m)]         # k-th largest weight (threshold boundary)
    target <- sum(sorted_wval >= threshold * bw - 1e-10)
    while (pos < target) {
      pos <- pos + 1L
      e <- ord[pos]
      edge_in[c(tb(from_v[e]), e, tf(to_v[e]))] <- TRUE
    }
    inc_e      <- which(edge_in)
    n_arcs[i]  <- length(inc_e)
    n_nodes[i] <- length(unique(c(from_v[inc_e], to_v[inc_e])))
    if (return_graphs) graphs[[i]] <- .sub_edges(g, inc_e)
  }

  res <- data.frame(k = k_values, n_nodes = n_nodes, n_arcs = n_arcs)
  if (return_graphs) attr(res, "graphs") <- graphs
  res
}
