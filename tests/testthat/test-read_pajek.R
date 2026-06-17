library(igraph)

# ── Helpers ───────────────────────────────────────────────────────────────────

# Minimal .net with *Arcs
minimal_arcs <- function() {
  tmp <- tempfile(fileext = ".net")
  writeLines(c(
    "*Vertices 4",
    '1 "A"  0.1 0.2 0.0',
    '2 "B"  0.3 0.4 0.0',
    '3 "C"  0.5 0.6 0.0',
    '4 "D"  0.7 0.8 0.0',
    "*Arcs",
    "1 2 1",
    "2 3 2",
    "3 4 1"
  ), tmp)
  tmp
}

# Minimal .net with *Edges (undirected section)
minimal_edges <- function() {
  tmp <- tempfile(fileext = ".net")
  writeLines(c(
    "*Vertices 3",
    '1 "X"',
    '2 "Y"',
    '3 "Z"',
    "*Edges",
    "1 2 5",
    "2 3 3"
  ), tmp)
  tmp
}

# .net with *Arcslist
minimal_arcslist <- function() {
  tmp <- tempfile(fileext = ".net")
  writeLines(c(
    "*Vertices 4",
    "1",
    "2",
    "3",
    "4",
    "*Arcslist",
    "1 2 3",
    "2 4"
  ), tmp)
  tmp
}

# ── Tests ─────────────────────────────────────────────────────────────────────

test_that("read_pajek returns an igraph with correct vertex/edge count", {
  g <- read_pajek(minimal_arcs())
  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 4L)
  expect_equal(igraph::ecount(g), 3L)
})

test_that("vertex names are read correctly from quoted labels", {
  g <- read_pajek(minimal_arcs())
  expect_equal(sort(igraph::V(g)$name), c("A", "B", "C", "D"))
})

test_that("edge weights are stored correctly", {
  g     <- read_pajek(minimal_arcs())
  el    <- igraph::as_edgelist(g, names = TRUE)
  wts   <- igraph::E(g)$weight
  # edge B->C should have weight 2
  bc_w  <- wts[el[, 1] == "B" & el[, 2] == "C"]
  expect_equal(bc_w, 2)
})

test_that("xy coordinates are stored as vertex attributes", {
  g <- read_pajek(minimal_arcs())
  expect_false(any(is.na(igraph::V(g)$x)))
  expect_false(any(is.na(igraph::V(g)$y)))
})

test_that("*Edges section with directed=TRUE produces two directed edges per pair", {
  g <- read_pajek(minimal_edges(), directed = TRUE)
  expect_equal(igraph::ecount(g), 4L)   # 2 undirected * 2 directions
  expect_true(igraph::is_directed(g))
})

test_that("*Edges section with directed=FALSE produces undirected graph", {
  g <- read_pajek(minimal_edges(), directed = FALSE)
  expect_equal(igraph::ecount(g), 2L)
  expect_false(igraph::is_directed(g))
})

test_that("*Arcslist section is parsed correctly", {
  g <- read_pajek(minimal_arcslist())
  # 1->2, 1->3, 2->4 = 3 edges
  expect_equal(igraph::ecount(g), 3L)
})

test_that("vertices without labels fall back to integer id as name", {
  g <- read_pajek(minimal_arcslist())
  expect_equal(sort(igraph::V(g)$name), c("1", "2", "3", "4"))
})

test_that("weight_attr=NULL drops weight edge attribute", {
  g <- read_pajek(minimal_arcs(), weight_attr = NULL)
  expect_false("weight" %in% igraph::edge_attr_names(g))
})

test_that("error on missing file", {
  expect_error(read_pajek("nonexistent.net"), regexp = "not found")
})

test_that("sample_network_pajek.net loads without error", {
  net_file <- system.file("extdata", "sample_network_pajek.net", package = "mpaR")
  skip_if(net_file == "", "sample file not installed")
  g <- read_pajek(net_file)
  expect_equal(igraph::vcount(g), 50L)
  expect_true(igraph::ecount(g) > 0L)
})
