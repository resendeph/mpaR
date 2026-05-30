library(igraph)

# Shared fixture -------------------------------------------------------------
#
#   1 --> 2 --> 3 --> 5
#         |
#         2 --> 4 --> 5
#
# SPC:  1->2 = 2, 2->3 = 1, 2->4 = 1, 3->5 = 1, 4->5 = 1
# Global main path (SPC): any source-to-sink path; both have sum 1+1+1 = 3
# but edge 1->2 (SPC=2) is shared, so the path through it contributes more
# to total. Both paths have equal cumulative SPC (2+1+1 = 4 either way).
# Tie-breaking will follow which.max, so we just check structural properties.

make_weighted_dag <- function(method = "SPC") {
  el <- data.frame(from = c(1L, 2L, 2L, 3L, 4L),
                   to   = c(2L, 3L, 4L, 5L, 5L))
  traversal_weights(el, method = method)
}

# -----------------------------------------------------------------------
test_that("global main path is a source-to-sink path", {
  g  <- make_weighted_dag()
  mp <- main_path(g, type = "global", weight = "SPC")

  deg_in  <- igraph::degree(mp, mode = "in")
  deg_out <- igraph::degree(mp, mode = "out")

  sources <- sum(deg_in  == 0L)
  sinks   <- sum(deg_out == 0L)

  expect_equal(sources, 1L)
  expect_equal(sinks,   1L)
  # Path graph: every node has in-degree <= 1 and out-degree <= 1
  expect_true(all(deg_in  <= 1L))
  expect_true(all(deg_out <= 1L))
})


test_that("global main path subgraph contains only valid edges", {
  g      <- make_weighted_dag()
  mp     <- main_path(g, type = "global")
  mp_el  <- igraph::as_edgelist(mp, names = TRUE)

  valid_edges <- c("1->2", "2->3", "2->4", "3->5", "4->5")
  found_edges <- paste0(mp_el[, 1], "->", mp_el[, 2])
  expect_true(all(found_edges %in% valid_edges))
})


test_that("local main path covers all sources", {
  g      <- make_weighted_dag()
  mp     <- main_path(g, type = "local")
  # With a single source (vertex 1), local = same start vertex as global
  deg_in <- igraph::degree(mp, mode = "in")
  expect_equal(sum(deg_in == 0L), 1L)
})


test_that("key_route main path with k=1 returns a connected subgraph", {
  g  <- make_weighted_dag()
  mp <- main_path(g, type = "key_route", k = 1L)
  expect_true(igraph::is_connected(mp, mode = "weak"))
})


test_that("key_route with k > number of edges clips to all edges", {
  g  <- make_weighted_dag()
  # k=100 should not error; it just uses all edges as seeds
  expect_no_error(main_path(g, type = "key_route", k = 100L))
})


test_that("main_path errors without traversal weights", {
  g_bare <- igraph::graph_from_edgelist(
    matrix(c(1,2,2,3), ncol = 2, byrow = TRUE), directed = TRUE
  )
  expect_error(main_path(g_bare), regexp = "traversal_weights")
})


test_that("mpa() convenience wrapper returns same result as two-step call", {
  el <- data.frame(from = c(1L, 2L, 2L, 3L, 4L),
                   to   = c(2L, 3L, 4L, 5L, 5L))

  mp_wrapper   <- mpa(el, type = "global", weight = "SPC")
  g_w          <- traversal_weights(el, method = "SPC")
  mp_two_step  <- main_path(g_w, type = "global", weight = "SPC")

  expect_equal(
    sort(igraph::E(mp_wrapper)$SPC),
    sort(igraph::E(mp_two_step)$SPC)
  )
})


test_that("all three weight methods produce a valid global path", {
  el <- data.frame(from = c(1L, 2L, 2L, 3L, 4L),
                   to   = c(2L, 3L, 4L, 5L, 5L))
  for (w in c("SPC", "SPLC", "SPNP")) {
    g  <- traversal_weights(el, method = w)
    mp <- main_path(g, type = "global", weight = w)
    expect_true(igraph::vcount(mp) >= 2L,
                label = paste("vcount >= 2 for weight", w))
  }
})
