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
