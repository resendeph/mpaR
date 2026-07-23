# mpaR 0.5.0

## Performance

* Rewrote the core dynamic-programming routines to be vectorised and layered.
  `traversal_weights()`, `node_weights()`, and all three `main_path()` types
  (global, local, key-route) now run one vectorised step per topological layer
  instead of one interpreted igraph call per vertex. On a 50,000-vertex /
  300,000-arc citation network the key-route extraction drops from tens of
  minutes to a few seconds, with identical results (verified against Pajek and
  by exhaustive path enumeration on small graphs).
* `read_pajek()` now parses arcs in a single vectorised pass. The previous
  version built one data frame per arc and was effectively O(E^2), taking
  minutes on large networks; it is now sub-second.

## New features

* `key_route_sweep()`: computes the key-route main path for many values of `k`
  in a single shared DP pass, returning a tidy `k` / `n_nodes` / `n_arcs` table
  (and optionally the subgraphs). This is the efficient way to choose `k` by
  looking for a plateau, replacing a slow loop over `main_path()`.
* `main_path(type = "key_route")` gains a `k_range = c(start, end)` argument to
  seed from a slice of key routes ranked `start` to `end` by weight, mirroring
  Pajek's key-route range option.

# mpaR 0.4.0

Initial CRAN release.

## Features

* Implements Main Path Analysis (MPA) as introduced by Hummon and Doreian
  (1989), computing traversal weights (SPC, SPLC, SPNP) for each edge of a
  directed acyclic graph (DAG) and extracting the global, local, and
  key-route main paths.
* `check_dag()` validates that a graph is acyclic and reports the
  back-edges responsible for any cycles.
* `classify_nodes()` labels vertices by role (source/terminal/user, or
  source/sink/user under the graph-theory convention).
* `check_scale_free()` fits a discrete power-law to the degree distribution
  and tests for scale-free behaviour.
* `edge_weights()` and `node_weights()` compute traversal weights at the
  edge and node level.
* Readers for Pajek (`read_pajek()`) and Gephi export
  (`read_gephi_export()`, supporting `.gexf` and `.graphml`) files.
* `plot_mpa()` visualizes main paths, with a `scope` argument for
  restricting the plot to a subgraph.
* Per-component path extraction for disconnected networks.
* Accepts `igraph` objects or edge-list data frames as input throughout.
