library(igraph)

# Helper: same well-understood DAG used in test-weights.R
#
#   1 --> 2 --> 3 --> 5
#               |
#         2 --> 4 --> 5
#
# Sources: {1}, Sinks: {5}
# Paths: 1->2->3->5  and  1->2->4->5
#
# f:  f[1]=1, f[2]=1, f[3]=1, f[4]=1, f[5]=2
# b:  b[1]=2, b[2]=2, b[3]=1, b[4]=1, b[5]=1
# fa: fa[1]=1, fa[2]=2, fa[3]=3, fa[4]=3, fa[5]=7
# ba: ba[1]=6, ba[2]=5, ba[3]=2, ba[4]=2, ba[5]=1

make_simple_el <- function() {
  data.frame(
    from = c(1L, 2L, 2L, 3L, 4L),
    to   = c(2L, 3L, 4L, 5L, 5L)
  )
}

# ── edge_weights() ──────────────────────────────────────────────────────────

test_that("edge_weights returns a data frame with from/to/SPC/SPLC/SPNP", {
  ew <- edge_weights(make_simple_el())
  expect_true(is.data.frame(ew))
  expect_setequal(names(ew), c("from", "to", "SPC", "SPLC", "SPNP"))
  expect_equal(nrow(ew), 5L)
})

test_that("edge_weights matches traversal_weights edge attributes", {
  el  <- make_simple_el()
  g   <- traversal_weights(el, method = "all")
  ew  <- edge_weights(el, method = "all")

  el_g       <- igraph::as_edgelist(g, names = TRUE)
  edge_names <- paste0(el_g[, 1], "->", el_g[, 2])
  ew_names   <- paste0(ew$from, "->", ew$to)

  spc_g  <- setNames(igraph::E(g)$SPC,  edge_names)
  spc_ew <- setNames(ew$SPC,            ew_names)

  expect_equal(spc_ew[names(spc_g)], spc_g, tolerance = 1e-9)
})

test_that("edge_weights respects method filtering", {
  ew <- edge_weights(make_simple_el(), method = "SPC")
  expect_true("SPC" %in% names(ew))
  expect_false("SPLC" %in% names(ew))
  expect_false("SPNP" %in% names(ew))
})

# ── node_weights() ──────────────────────────────────────────────────────────

test_that("node_weights returns a data frame with name/SPC/SPLC/SPNP", {
  nw <- node_weights(make_simple_el())
  expect_true(is.data.frame(nw))
  expect_setequal(names(nw), c("name", "SPC", "SPLC", "SPNP"))
  expect_equal(nrow(nw), 5L)
})

test_that("node-level SPC values are correct (paths-through-node)", {
  nw <- node_weights(make_simple_el(), method = "SPC")
  spc <- setNames(nw$SPC, nw$name)

  expect_equal(spc[["1"]], 2, tolerance = 1e-9)  # f[1]*b[1] = 1*2
  expect_equal(spc[["2"]], 2, tolerance = 1e-9)  # f[2]*b[2] = 1*2
  expect_equal(spc[["3"]], 1, tolerance = 1e-9)  # f[3]*b[3] = 1*1
  expect_equal(spc[["4"]], 1, tolerance = 1e-9)  # f[4]*b[4] = 1*1
  expect_equal(spc[["5"]], 2, tolerance = 1e-9)  # f[5]*b[5] = 2*1
})

test_that("node-level SPLC values are correct (paths-through-node)", {
  nw   <- node_weights(make_simple_el(), method = "SPLC")
  splc <- setNames(nw$SPLC, nw$name)

  expect_equal(splc[["1"]], 2, tolerance = 1e-9)  # fa[1]*b[1] = 1*2
  expect_equal(splc[["2"]], 4, tolerance = 1e-9)  # fa[2]*b[2] = 2*2
  expect_equal(splc[["3"]], 3, tolerance = 1e-9)  # fa[3]*b[3] = 3*1
  expect_equal(splc[["4"]], 3, tolerance = 1e-9)  # fa[4]*b[4] = 3*1
  expect_equal(splc[["5"]], 7, tolerance = 1e-9)  # fa[5]*b[5] = 7*1
})

test_that("node-level SPNP values are correct (paths-through-node)", {
  nw   <- node_weights(make_simple_el(), method = "SPNP")
  spnp <- setNames(nw$SPNP, nw$name)

  expect_equal(spnp[["1"]], 6,  tolerance = 1e-9)  # fa[1]*ba[1] = 1*6
  expect_equal(spnp[["2"]], 10, tolerance = 1e-9)  # fa[2]*ba[2] = 2*5
  expect_equal(spnp[["3"]], 6,  tolerance = 1e-9)  # fa[3]*ba[3] = 3*2
  expect_equal(spnp[["4"]], 6,  tolerance = 1e-9)  # fa[4]*ba[4] = 3*2
  expect_equal(spnp[["5"]], 7,  tolerance = 1e-9)  # fa[5]*ba[5] = 7*1
})

test_that("node_weights respects method filtering", {
  nw <- node_weights(make_simple_el(), method = "SPNP")
  expect_true("SPNP" %in% names(nw))
  expect_false("SPC" %in% names(nw))
  expect_false("SPLC" %in% names(nw))
})

test_that("node_weights accepts igraph input directly", {
  el <- make_simple_el()
  g  <- igraph::graph_from_data_frame(el, directed = TRUE)
  nw <- node_weights(g)
  expect_equal(nrow(nw), igraph::vcount(g))
})

test_that("non-DAG input is rejected by both functions", {
  g_cycle <- igraph::graph_from_edgelist(
    matrix(c(1, 2, 2, 3, 3, 1), ncol = 2, byrow = TRUE),
    directed = TRUE
  )
  expect_error(edge_weights(g_cycle), regexp = "DAG")
  expect_error(node_weights(g_cycle), regexp = "DAG")
})
