#' Read a Pajek network file (.net)
#'
#' @description
#' Parses a Pajek `.net` file and returns a directed or undirected
#' \code{igraph} object.  The following Pajek sections are supported:
#'
#' * `*Vertices` — vertex ids, optional labels, and optional x/y/z
#'   coordinates stored as vertex attributes.
#' * `*Arcs` — directed edges with optional weights.
#' * `*Edges` — undirected edges with optional weights (converted to a
#'   directed graph by adding both orientations when \code{directed = TRUE}).
#' * `*Arcslist` / `*Edgeslist` — adjacency-list variants of the above.
#'
#' Lines beginning with `%` are treated as comments and ignored.
#'
#' @param file Path to a `.net` file.
#' @param directed Logical.  If \code{TRUE} (default), the returned graph is
#'   directed.  \code{*Arcs} become directed edges; \code{*Edges} are
#'   duplicated in both directions.  Set to \code{FALSE} to force an
#'   undirected graph.
#' @param weight_attr Character.  Name of the edge attribute used to store
#'   edge weights (default \code{"weight"}).  Set to \code{NULL} to drop
#'   weights.
#'
#' @return An \code{igraph} graph with:
#' \describe{
#'   \item{Vertex attributes}{\code{name} (label from file, or integer id if
#'     absent); \code{x}, \code{y}, \code{z} (coordinates, \code{NA} when not
#'     provided).}
#'   \item{Edge attribute}{\code{weight} (numeric; \code{1} when absent from
#'     the file).}
#' }
#'
#' @examples
#' net_file <- system.file("extdata", "sample_network_pajek.net",
#'                         package = "mpaR")
#' g <- read_pajek(net_file)
#' igraph::vcount(g)
#' igraph::ecount(g)
#' head(igraph::V(g)$name)
#'
#' @export
read_pajek <- function(file,
                       directed    = TRUE,
                       weight_attr = "weight") {

  if (!file.exists(file)) {
    rlang::abort(paste0("File not found: ", file))
  }

  raw   <- readLines(file, warn = FALSE)
  lines <- trimws(raw)

  # Drop comment lines (start with %)
  lines <- lines[!grepl("^%", lines)]
  # Drop blank lines
  lines <- lines[nchar(lines) > 0L]

  # ── Section splitting ──────────────────────────────────────────────────────
  # Section headers start with *  (case-insensitive)
  is_header <- grepl("^\\*", lines)
  headers   <- which(is_header)

  .section_lines <- function(name_pattern) {
    idx <- grep(name_pattern, lines, ignore.case = TRUE)
    if (length(idx) == 0L) return(character(0))
    start <- idx[1L] + 1L
    end   <- if (length(headers[headers > idx[1L]]) > 0L)
               min(headers[headers > idx[1L]]) - 1L
             else
               length(lines)
    if (start > end) return(character(0))
    lines[start:end]
  }

  # ── *Vertices ──────────────────────────────────────────────────────────────
  v_header <- grep("^\\*Vertices", lines, ignore.case = TRUE)
  if (length(v_header) == 0L) {
    rlang::abort("No *Vertices section found in the file.")
  }

  n_vertices <- as.integer(
    sub("^\\*Vertices\\s+", "", lines[v_header[1L]], ignore.case = TRUE)
  )

  v_lines <- .section_lines("^\\*Vertices")

  # Parse each vertex line:  ID  ["label"]  [x  y  z]
  vnames <- character(n_vertices)
  vx     <- rep(NA_real_, n_vertices)
  vy     <- rep(NA_real_, n_vertices)
  vz     <- rep(NA_real_, n_vertices)

  for (ln in v_lines) {
    # Extract quoted label if present
    label   <- NA_character_
    rest    <- ln
    q_match <- regmatches(rest, regexpr('"[^"]*"', rest))
    if (length(q_match) > 0L) {
      label <- gsub('"', '', q_match)
      rest  <- gsub('"[^"]*"', '', rest)
    }

    tokens <- strsplit(trimws(rest), "\\s+")[[1L]]
    tokens <- tokens[nchar(tokens) > 0L]
    id     <- as.integer(tokens[1L])

    vnames[id] <- if (!is.na(label) && label != "null") label else as.character(id)

    if (length(tokens) >= 4L) {
      vx[id] <- suppressWarnings(as.numeric(tokens[2L]))
      vy[id] <- suppressWarnings(as.numeric(tokens[3L]))
      vz[id] <- suppressWarnings(as.numeric(tokens[4L]))
    } else if (length(tokens) == 3L) {
      vx[id] <- suppressWarnings(as.numeric(tokens[2L]))
      vy[id] <- suppressWarnings(as.numeric(tokens[3L]))
    }
  }

  # Fill any un-set names with integer ids
  blank <- vnames == "" | is.na(vnames)
  vnames[blank] <- as.character(which(blank))

  # ── Edge parsing helper ────────────────────────────────────────────────────
  # Returns data.frame(from, to, weight) from raw edge lines
  # Vectorised: the whole block is parsed in one pass. The previous version
  # built one data.frame per line and rbind()ed them, which is ~O(E^2) and
  # takes minutes on large (100k+ arc) networks.
  .parse_edge_lines <- function(edge_lines, list_format = FALSE) {
    empty <- data.frame(from = integer(0), to = integer(0), weight = numeric(0))
    if (length(edge_lines) == 0L) return(empty)

    if (!list_format) {
      # Fast path: fixed 2 (from to) or 3 (from to weight) columns.
      nums <- suppressWarnings(scan(text = edge_lines, what = numeric(),
                                    quiet = TRUE))
      k    <- length(nums) / length(edge_lines)
      if (length(nums) > 0L && k == round(k) && k >= 2) {
        mm     <- matrix(nums, ncol = k, byrow = TRUE)
        weight <- if (k >= 3L) mm[, 3L] else rep(1.0, nrow(mm))
        weight[is.na(weight)] <- 1.0
        return(data.frame(from = as.integer(mm[, 1L]),
                          to   = as.integer(mm[, 2L]),
                          weight = weight, stringsAsFactors = FALSE))
      }
    }

    # List format, or ragged edge lines: split once, expand vectorised.
    tok   <- strsplit(trimws(edge_lines), "\\s+")
    from1 <- suppressWarnings(as.integer(vapply(tok, `[`, "", 1L)))
    if (list_format) {
      targets <- lapply(tok, function(x) suppressWarnings(as.integer(x[-1L])))
      len     <- lengths(targets)
      keep    <- len > 0L & !is.na(from1)
      if (!any(keep)) return(empty)
      data.frame(from   = rep(from1[keep], len[keep]),
                 to     = unlist(targets[keep], use.names = FALSE),
                 weight = 1.0, stringsAsFactors = FALSE)
    } else {
      to1 <- suppressWarnings(as.integer(vapply(tok, `[`, "", 2L)))
      w1  <- suppressWarnings(as.numeric(vapply(
               tok, function(x) if (length(x) >= 3L) x[3L] else NA_character_, "")))
      w1[is.na(w1)] <- 1.0
      keep <- !is.na(from1) & !is.na(to1)
      if (!any(keep)) return(empty)
      data.frame(from = from1[keep], to = to1[keep], weight = w1[keep],
                 stringsAsFactors = FALSE)
    }
  }

  # ── Collect all edge sections ──────────────────────────────────────────────
  arc_lines      <- .section_lines("^\\*Arcs$|^\\*Arc$")
  edge_lines     <- .section_lines("^\\*Edges$|^\\*Edge$")
  arcslist_lines <- .section_lines("^\\*Arcslist")
  edgelist_lines <- .section_lines("^\\*Edgeslist")

  el_parts <- list()

  if (length(arc_lines) > 0L)
    el_parts[["arcs"]] <- .parse_edge_lines(arc_lines)

  if (length(arcslist_lines) > 0L)
    el_parts[["arcslist"]] <- .parse_edge_lines(arcslist_lines, list_format = TRUE)

  if (length(edge_lines) > 0L) {
    df <- .parse_edge_lines(edge_lines)
    if (directed && nrow(df) > 0L) {
      # Duplicate in both directions for directed output
      df_rev <- data.frame(from = df$to, to = df$from, weight = df$weight,
                           stringsAsFactors = FALSE)
      df <- rbind(df, df_rev)
    }
    el_parts[["edges"]] <- df
  }

  if (length(edgelist_lines) > 0L) {
    df <- .parse_edge_lines(edgelist_lines, list_format = TRUE)
    if (directed && nrow(df) > 0L) {
      df_rev <- data.frame(from = df$to, to = df$from, weight = df$weight,
                           stringsAsFactors = FALSE)
      df <- rbind(df, df_rev)
    }
    el_parts[["edgeslist"]] <- df
  }

  if (length(el_parts) == 0L) {
    rlang::abort("No edge sections (*Arcs, *Edges, *Arcslist, *Edgeslist) found.")
  }

  el_all <- do.call(rbind, el_parts)

  # ── Build igraph ───────────────────────────────────────────────────────────
  vertices_df <- data.frame(
    name = vnames,
    x    = vx,
    y    = vy,
    z    = vz,
    stringsAsFactors = FALSE
  )

  # Replace integer ids with vertex names in edge list
  el_named <- data.frame(
    from   = vnames[el_all$from],
    to     = vnames[el_all$to],
    stringsAsFactors = FALSE
  )
  if (!is.null(weight_attr)) {
    el_named[[weight_attr]] <- el_all$weight
  }

  g <- igraph::graph_from_data_frame(
    d        = el_named,
    directed = directed,
    vertices = vertices_df
  )

  message(sprintf(
    "Parsed Pajek file: %d vertices, %d edges (%s)",
    igraph::vcount(g),
    igraph::ecount(g),
    if (directed) "directed" else "undirected"
  ))

  g
}
