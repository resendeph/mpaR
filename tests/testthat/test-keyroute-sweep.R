library(igraph)

# Shared fixture: a small branchy DAG (single source 1, single sink 7) --------
#   1 -> 2, 1 -> 3, 2 -> 4, 2 -> 5, 3 -> 5, 3 -> 6, 4 -> 6, 4 -> 7, 5 -> 7, 6 -> 7
make_dag <- function(method = "SPC") {
  el <- data.frame(
    from = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 6),
    to   = c(2, 3, 4, 5, 5, 6, 6, 7, 7, 7)
  )
  traversal_weights(el, method = method)
}

sig <- function(sub) {
  if (igraph::ecount(sub) == 0L) return(character(0))
  e <- igraph::as_edgelist(sub, names = TRUE)
  sort(paste(e[, 1], e[, 2], sep = "|"))
}

# --- key_route_sweep --------------------------------------------------------
test_that("key_route_sweep matches main_path at every k", {
  g  <- make_dag()
  ks <- c(1L, 2L, 3L, 5L)
  sw <- key_route_sweep(g, weight = "SPC", k_values = ks)

  expect_s3_class(sw, "data.frame")
  expect_identical(names(sw), c("k", "n_nodes", "n_arcs"))
  expect_identical(sw$k, ks)

  for (i in seq_along(ks)) {
    mp <- main_path(g, type = "key_route", weight = "SPC", k = ks[i])
    expect_equal(sw$n_nodes[i], igraph::vcount(mp))
    expect_equal(sw$n_arcs[i],  igraph::ecount(mp))
  }
})

test_that("key_route_sweep counts are non-decreasing in k (threshold 1)", {
  g  <- make_dag()
  sw <- key_route_sweep(g, weight = "SPC", k_values = 1:8)
  expect_true(all(diff(sw$n_nodes) >= 0))
  expect_true(all(diff(sw$n_arcs)  >= 0))
})

test_that("key_route_sweep return_graphs aligns with the table and main_path", {
  g  <- make_dag()
  ks <- c(1L, 3L, 5L)
  sw <- key_route_sweep(g, weight = "SPC", k_values = ks, return_graphs = TRUE)
  gr <- attr(sw, "graphs")

  expect_length(gr, length(ks))
  for (i in seq_along(ks)) {
    expect_equal(igraph::vcount(gr[[i]]), sw$n_nodes[i])
    expect_equal(igraph::ecount(gr[[i]]), sw$n_arcs[i])
    mp <- main_path(g, type = "key_route", weight = "SPC", k = ks[i])
    expect_setequal(sig(gr[[i]]), sig(mp))
  }
})

test_that("key_route_sweep threshold broadens the path", {
  g  <- make_dag()
  s1 <- key_route_sweep(g, weight = "SPC", k_values = 3, threshold = 1.0)
  s2 <- key_route_sweep(g, weight = "SPC", k_values = 3, threshold = 0.7)
  expect_gte(s2$n_arcs, s1$n_arcs)
})

test_that("key_route_sweep validates its input", {
  expect_error(key_route_sweep(42), regexp = "igraph")
  g_bare <- igraph::graph_from_edgelist(
    matrix(c(1, 2, 2, 3), ncol = 2, byrow = TRUE), directed = TRUE
  )
  expect_error(key_route_sweep(g_bare), regexp = "traversal_weights")
})

# --- key_route k_range (Pajek-style slice) ----------------------------------
test_that("main_path key_route accepts k_range and returns a valid subgraph", {
  g  <- make_dag()
  mp <- main_path(g, type = "key_route", weight = "SPC", k_range = c(2, 4))
  expect_true(inherits(mp, "igraph"))
  # every edge of the slice is an edge of the original graph
  expect_true(all(sig(mp) %in% sig(g)))
})

test_that("k_range spanning all edges equals top-k over all edges", {
  g <- make_dag()
  m <- igraph::ecount(g)
  mp_range <- main_path(g, type = "key_route", weight = "SPC", k_range = c(1, m))
  mp_topk  <- main_path(g, type = "key_route", weight = "SPC", k = m)
  expect_setequal(sig(mp_range), sig(mp_topk))
})

test_that("k_range takes precedence over k", {
  g <- make_dag()
  m <- igraph::ecount(g)
  mp_k1   <- main_path(g, type = "key_route", weight = "SPC", k = 1)
  mp_prec <- main_path(g, type = "key_route", weight = "SPC", k = 1, k_range = c(1, m))
  mp_all  <- main_path(g, type = "key_route", weight = "SPC", k_range = c(1, m))
  # k is ignored when k_range is supplied
  expect_setequal(sig(mp_prec), sig(mp_all))
  expect_gte(igraph::ecount(mp_prec), igraph::ecount(mp_k1))
})

test_that("k_range is clamped to valid bounds without error", {
  g <- make_dag()
  expect_no_error(
    main_path(g, type = "key_route", weight = "SPC", k_range = c(0, 1000))
  )
})

