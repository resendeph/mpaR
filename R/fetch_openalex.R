#' Fetch works from OpenAlex
#'
#' @description
#' Queries the OpenAlex API for scholarly works matching a search term, ORCID,
#' journal, or DOI. Returns a standardised data frame that can be passed
#' directly to [build_citation_network()].
#'
#' OpenAlex is a free, open database of scholarly works requiring no API key.
#' It covers approximately 250 million works and includes full reference lists,
#' making it suitable for constructing citation networks for Main Path Analysis.
#'
#' @param query Character string. The search term (keyword, phrase, etc.).
#'   Ignored when \code{orcid} or \code{doi} is supplied.
#' @param field Character; which part of the record to search. One of:
#'   \describe{
#'     \item{\code{"title_abstract"}}{Title and abstract (default).}
#'     \item{\code{"title"}}{Title only.}
#'   }
#' @param n Integer. Maximum number of works to retrieve. Defaults to
#'   \code{200}. Large values may result in slow API calls.
#' @param orcid Character. ORCID identifier (e.g.
#'   \code{"0000-0002-1234-5678"}). If supplied, retrieves all works by that
#'   author regardless of \code{query}.
#' @param doi Character vector. One or more DOIs to retrieve directly.
#' @param journal Character. Journal name to filter by (combined with
#'   \code{query} if both are supplied).
#' @param from_year Integer. Restrict results to works published from this
#'   year onwards.
#' @param to_year Integer. Restrict results to works published up to and
#'   including this year.
#' @param verbose Logical. If \code{TRUE} (default), print progress messages.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{\code{id}}{OpenAlex work ID (short form, e.g. \code{"W2741809807"}).}
#'   \item{\code{title}}{Paper title.}
#'   \item{\code{doi}}{DOI.}
#'   \item{\code{year}}{Publication year (integer).}
#'   \item{\code{first_author}}{Display name of the first author.}
#'   \item{\code{journal}}{Journal or source name.}
#'   \item{\code{abstract}}{Abstract text.}
#'   \item{\code{cited_by_count}}{Total citations received.}
#'   \item{\code{referenced_works}}{List column of character vectors: OpenAlex
#'     IDs of works cited by each paper. Used by [build_citation_network()] to
#'     construct edges.}
#' }
#'
#' @seealso [build_citation_network()]
#'
#' @references
#' Priem, J., Piwowar, H., & Orr, R. (2022). OpenAlex: A fully-open index of
#' the world's research works. \url{https://arxiv.org/abs/2205.01833}
#'
#' @examples
#' \dontrun{
#' # Keyword search (title + abstract)
#' papers <- fetch_openalex("main path analysis")
#'
#' # Title-only search, limit to 100 results
#' papers <- fetch_openalex("Hayek", field = "title", n = 100)
#'
#' # All works by a specific author (ORCID)
#' papers <- fetch_openalex(orcid = "0000-0002-1825-0097")
#'
#' # Restrict by year range
#' papers <- fetch_openalex("citation network",
#'                          from_year = 2010, to_year = 2023)
#'
#' # Full pipeline: fetch -> network -> MPA
#' papers <- fetch_openalex("main path analysis", n = 300)
#' g      <- build_citation_network(papers)
#' g      <- traversal_weights(g)
#' result <- mpa(g)
#' plot_mpa(g, result$global)
#' }
#'
#' @export
fetch_openalex <- function(query     = NULL,
                           field     = c("title_abstract", "title"),
                           n         = 200,
                           orcid     = NULL,
                           doi       = NULL,
                           journal   = NULL,
                           from_year = NULL,
                           to_year   = NULL,
                           verbose   = TRUE) {

  if (!requireNamespace("openalexR", quietly = TRUE)) {
    rlang::abort(paste0(
      "Package 'openalexR' is required for fetch_openalex(). ",
      "Install it with: install.packages('openalexR')"
    ))
  }

  field <- match.arg(field)

  # ── Build filter list ──────────────────────────────────────────────────────
  filter_list <- list()

  if (!is.null(orcid))   filter_list[["author.orcid"]] <- orcid
  if (!is.null(journal)) filter_list[["primary_location.source.display_name.search"]] <- journal

  if (!is.null(from_year) || !is.null(to_year)) {
    yr_from <- if (!is.null(from_year)) from_year else 1000L
    yr_to   <- if (!is.null(to_year))  to_year   else as.integer(format(Sys.Date(), "%Y"))
    filter_list[["publication_year"]] <- paste0(yr_from, "-", yr_to)
  }

  if (!is.null(doi)) {
    filter_list[["doi"]] <- paste(doi, collapse = "|")
  }

  # ── Search argument ────────────────────────────────────────────────────────
  search_arg <- NULL
  if (!is.null(query) && is.null(orcid) && is.null(doi)) {
    if (field == "title") {
      filter_list[["display_name.search"]] <- query
    } else {
      search_arg <- query
    }
  }

  # ── Call OpenAlex API ──────────────────────────────────────────────────────
  if (verbose) message("Querying OpenAlex...")

  fetch_args <- list(
    entity     = "works",
    filter     = if (length(filter_list) > 0L) filter_list else NULL,
    search     = search_arg,
    count_only = FALSE,
    verbose    = verbose
  )
  fetch_args <- fetch_args[!vapply(fetch_args, is.null, logical(1L))]

  raw <- tryCatch(
    do.call(openalexR::oa_fetch, fetch_args),
    error = function(e) {
      rlang::abort(paste0("OpenAlex API error: ", conditionMessage(e)))
    }
  )

  if (is.null(raw) || nrow(raw) == 0L) {
    if (verbose) message("No results returned from OpenAlex.")
    return(invisible(NULL))
  }

  if (nrow(raw) > n) {
    if (verbose) message(sprintf("Keeping %d of %d results.", n, nrow(raw)))
    raw <- raw[seq_len(n), , drop = FALSE]
  }

  if (verbose) message(sprintf("Retrieved %d works.", nrow(raw)))

  .standardise_oa(raw)
}


# ── Internal: standardise openalexR output ────────────────────────────────────

#' @noRd
.standardise_oa <- function(raw) {

  # First author display name
  first_author <- vapply(raw$authorships, function(auth) {
    if (is.null(auth) || length(auth) == 0L) return(NA_character_)
    first <- if (is.data.frame(auth)) auth[1L, ] else auth[[1L]]
    nm    <- if (is.data.frame(first)) first$au_display_name[1L]
             else first[["au_display_name"]]
    if (is.null(nm) || length(nm) == 0L) NA_character_ else as.character(nm[1L])
  }, character(1L))

  # Normalise referenced_works to list of character vectors
  # Strip full URL prefix for shorter, readable IDs
  clean_id <- function(x) {
    gsub("https://openalex.org/", "", as.character(x), fixed = TRUE)
  }

  ref_works <- lapply(raw$referenced_works, function(x) {
    if (is.null(x) || length(x) == 0L) character(0L) else clean_id(unlist(x))
  })

  ids <- clean_id(as.character(raw$id))

  out <- data.frame(
    id             = ids,
    title          = as.character(raw$display_name),
    doi            = .col_or_na(raw, "doi",              "character"),
    year           = .col_or_na(raw, "publication_year", "integer"),
    first_author   = first_author,
    journal        = .col_or_na(raw, "so",               "character"),
    abstract       = .col_or_na(raw, "ab",               "character"),
    cited_by_count = .col_or_na(raw, "cited_by_count",   "integer"),
    stringsAsFactors = FALSE
  )
  out$referenced_works <- ref_works
  out
}

#' @noRd
.col_or_na <- function(df, col, type) {
  if (col %in% names(df)) {
    switch(type,
      "character" = as.character(df[[col]]),
      "integer"   = as.integer(df[[col]]),
      df[[col]]
    )
  } else {
    switch(type,
      "character" = rep(NA_character_, nrow(df)),
      "integer"   = rep(NA_integer_,   nrow(df)),
      rep(NA, nrow(df))
    )
  }
}
