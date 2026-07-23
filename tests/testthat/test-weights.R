library(igraph)

# Helper: a small, well-understood DAG
#
#   1 --> 2 --> 3 --> 5
#               |
#         2 --> 4 --> 5
#
# Sources: {1}, Sinks: {5}
# Paths: 1->2->3->5  and  1->2->4->5

make_simple_dag <- function() {
  el <- data.frame(
    from = c(1L, 2L, 2L, 3L, 4L),
    to   = c(2L, 3L, 4L, 5L, 5L)
  )
  traversal_weights(el, method = "all")
}

# -----------------------------------------------------------------------
test_that("traversal_weights returns an igraph with SPC/SPLC/SPNP attributes", {
  g <- make_simple_dag()
  expect_true(inherits(g, "igraph"))
  expect_true("SPC"  %in% igraph::edge_attr_names(g))
  expect_true("SPLC" %in% igraph::edge_attr_names(g))
  expect_true("SPNP" %in% igraph::edge_attr_names(g))
})


test_that("SPC values are correct on simple DAG", {
  g      <- make_simple_dag()
  el     <- igraph::as_edgelist(g, names = TRUE)
  spc    <- igraph::E(g)$SPC

  # Name each SPC by "from->to"
  edge_names <- paste0(el[, 1], "->", el[, 2])
  spc_named  <- setNames(spc, edge_names)

  # Both paths pass through edge 1->2:  SPC = 2
  # (use [[ ]] so the name attribute is dropped and only the value is compared)
  expect_equal(spc_named[["1->2"]], 2, tolerance = 1e-9)
  # Only one path through 3->5 and one through 4->5: SPC = 1 each
  expect_equal(spc_named[["3->5"]], 1, tolerance = 1e-9)
  expect_equal(spc_named[["4->5"]], 1, tolerance = 1e-9)
  # Edges 2->3 and 2->4 each carry one path: SPC = 1
  expect_equal(spc_named[["2->3"]], 1, tolerance = 1e-9)
  expect_equal(spc_named[["2->4"]], 1, tolerance = 1e-9)
})


test_that("SPLC values are correct on simple DAG", {
  # SPLC(i->j) = fa[i] * b[j]
  # fa: fa[1]=1, fa[2]=2, fa[3]=3, fa[4]=3, fa[5]=7
  # b:  b[1]=2,  b[2]=2,  b[3]=1,  b[4]=1,  b[5]=1
  g          <- make_simple_dag()
  el         <- igraph::as_edgelist(g, names = TRUE)
  edge_names <- paste0(el[, 1], "->", el[, 2])
  splc_named <- setNames(igraph::E(g)$SPLC, edge_names)

  expect_equal(splc_named[["1->2"]], 1 * 2, tolerance = 1e-9)  # fa[1]*b[2] = 1*2 = 2
  expect_equal(splc_named[["2->3"]], 2 * 1, tolerance = 1e-9)  # fa[2]*b[3] = 2*1 = 2
  expect_equal(splc_named[["2->4"]], 2 * 1, tolerance = 1e-9)  # fa[2]*b[4] = 2*1 = 2
  expect_equal(splc_named[["3->5"]], 3 * 1, tolerance = 1e-9)  # fa[3]*b[5] = 3*1 = 3
  expect_equal(splc_named[["4->5"]], 3 * 1, tolerance = 1e-9)  # fa[4]*b[5] = 3*1 = 3
})


test_that("SPNP values are correct on simple DAG", {
  # SPNP(i->j) = fa[i] * ba[j]
  # fa: fa[1]=1, fa[2]=2, fa[3]=3, fa[4]=3
  # ba: ba[1]=6, ba[2]=5, ba[3]=2, ba[4]=2, ba[5]=1
  g          <- make_simple_dag()
  el         <- igraph::as_edgelist(g, names = TRUE)
  edge_names <- paste0(el[, 1], "->", el[, 2])
  spnp_named <- setNames(igraph::E(g)$SPNP, edge_names)

  expect_equal(spnp_named[["1->2"]], 1 * 5, tolerance = 1e-9)  # fa[1]*ba[2] = 1*5 = 5
  expect_equal(spnp_named[["2->3"]], 2 * 2, tolerance = 1e-9)  # fa[2]*ba[3] = 2*2 = 4
  expect_equal(spnp_named[["2->4"]], 2 * 2, tolerance = 1e-9)  # fa[2]*ba[4] = 2*2 = 4
  expect_equal(spnp_named[["3->5"]], 3 * 1, tolerance = 1e-9)  # fa[3]*ba[5] = 3*1 = 3
  expect_equal(spnp_named[["4->5"]], 3 * 1, tolerance = 1e-9)  # fa[4]*ba[5] = 3*1 = 3
})

test_that("SPNP >= SPLC >= SPC for all edges", {
  g <- make_simple_dag()
  expect_true(all(igraph::E(g)$SPNP >= igraph::E(g)$SPLC - 1e-9))
  expect_true(all(igraph::E(g)$SPLC >= igraph::E(g)$SPC  - 1e-9))
})


test_that("only selected methods are computed", {
  el <- data.frame(from = c(1,2), to = c(2,3))
  g  <- traversal_weights(el, method = "SPC")
  expect_true("SPC"  %in% igraph::edge_attr_names(g))
  expect_false("SPLC" %in% igraph::edge_attr_names(g))
  expect_false("SPNP" %in% igraph::edge_attr_names(g))
})


test_that("non-DAG input is rejected", {
  g_cycle <- igraph::graph_from_edgelist(
    matrix(c(1,2, 2,3, 3,1), ncol = 2, byrow = TRUE),
    directed = TRUE
  )
  expect_error(traversal_weights(g_cycle), regexp = "DAG")
})


test_that("undirected graph input is rejected", {
  g_und <- igraph::graph_from_edgelist(
    matrix(c(1,2, 2,3), ncol = 2, byrow = TRUE),
    directed = FALSE
  )
  expect_error(traversal_weights(g_und), regexp = "directed")
})


test_that("data frame input produces same result as igraph input", {
  el <- data.frame(from = c(1L, 2L, 2L, 3L, 4L),
                   to   = c(2L, 3L, 4L, 5L, 5L))
  g_from_df    <- traversal_weights(el)
  g_from_igraph <- traversal_weights(
    igraph::graph_from_data_frame(el, directed = TRUE)
  )
  expect_equal(sort(igraph::E(g_from_df)$SPC),
               sort(igraph::E(g_from_igraph)$SPC),
               tolerance = 1e-9)
})
