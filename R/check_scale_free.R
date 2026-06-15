#' Test whether a network has a scale-free degree distribution
#'
#' @description
#' Fits a discrete power-law distribution to the degree sequence of the graph
#' and reports whether the data are consistent with scale-free behaviour.
#' Scale-free networks are characterised by a degree distribution that follows
#' \eqn{P(k) \propto k^{-\gamma}}, where \eqn{\gamma} typically lies between
#' 2 and 3 for empirical citation networks.
#'
#' The function uses [igraph::fit_power_law()] (a maximum-likelihood estimator)
#' and optionally produces a log-log plot of the complementary cumulative
#' degree distribution (CCDF) with the fitted power-law overlaid.
#'
#' @param x An \code{igraph} graph or a data frame / matrix edge list.
#' @param mode Degree type to test: \code{"all"} (default, total degree),
#'   \code{"in"}, or \code{"out"}. For citation networks \code{"in"} (number
#'   of citations received) is usually of most interest.
#' @param xmin Numeric. Minimum degree value from which the power law is
#'   fitted. If \code{NULL} (default), the optimal \eqn{x_{\min}} is
#'   estimated automatically.
#' @param plot Logical. If \code{TRUE} (default), draw a log-log plot of the
#'   degree CCDF with the fitted power-law line.
#'
#' @return A list (invisibly) with elements:
#' \describe{
#'   \item{\code{alpha}}{Estimated power-law exponent \eqn{\gamma}.}
#'   \item{\code{xmin}}{The \eqn{x_{\min}} used for fitting.}
#'   \item{\code{KS_stat}}{Kolmogorov–Smirnov statistic.}
#'   \item{\code{KS_p}}{KS p-value. A large p-value (e.g. > 0.05) means the
#'     power-law cannot be rejected.}
#'   \item{\code{is_scale_free}}{Logical; \code{TRUE} if \code{KS_p > 0.05}
#'     and \eqn{2 \leq \gamma \leq 5}.}
#'   \item{\code{degree_sequence}}{Integer vector of degrees used.}
#' }
#'
#' @references
#' Barabási, A.-L. (2016). \emph{Network Science}. Cambridge University Press.
#'
#' Clauset, A., Shalizi, C. R., & Newman, M. E. J. (2009). Power-law
#' distributions in empirical data. \emph{SIAM Review}, \strong{51}(4),
#' 661–703. \doi{10.1137/070710111}
#'
#' @examples
#' library(igraph)
#'
#' # Barabási–Albert scale-free graph
#' g_sf <- igraph::sample_pa(500, directed = FALSE)
#' result <- check_scale_free(g_sf)
#' result$alpha
#' result$is_scale_free
#'
#' # Erdős–Rényi random graph (not scale-free)
#' g_er <- igraph::sample_gnp(500, p = 0.02)
#' check_scale_free(g_er, plot = FALSE)
#'
#' @export
check_scale_free <- function(x,
                             mode  = c("all", "in", "out"),
                             xmin  = NULL,
                             plot  = TRUE) {

  mode <- match.arg(mode)

  if (inherits(x, "igraph")) {
    g <- x
  } else if (is.data.frame(x) || is.matrix(x)) {
    g <- igraph::graph_from_data_frame(as.data.frame(x), directed = TRUE)
  } else {
    rlang::abort(paste0(
      "`x` must be an igraph object or a data frame / matrix, ",
      "not <", class(x)[1L], ">."
    ))
  }

  deg <- igraph::degree(g, mode = mode)
  deg <- deg[deg > 0L]   # power law only defined for k > 0

  if (length(deg) < 10L) {
    rlang::abort(
      "Too few non-zero-degree vertices to fit a power law (need >= 10)."
    )
  }

  # ── Fit ───────────────────────────────────────────────────────────────────
  fit_args <- list(x = deg, implementation = "plfit")
  if (!is.null(xmin)) fit_args$xmin <- xmin
  fit <- do.call(igraph::fit_power_law, fit_args)

  # igraph >= 2.0 returns an S4 object; use @ for slot access
  .s <- function(obj, nm) if (isS4(obj)) slot(obj, nm) else obj[[nm]]
  alpha        <- .s(fit, "alpha")
  xmin_used    <- .s(fit, "xmin")
  KS_stat      <- .s(fit, "KS.stat")
  KS_p         <- .s(fit, "KS.p")

  is_scale_free <- !is.na(KS_p) && KS_p > 0.05 && alpha >= 2 && alpha <= 5

  # ── Console summary ───────────────────────────────────────────────────────
  verdict <- if (is_scale_free) "CONSISTENT with scale-free" else
                                "NOT consistent with scale-free"
  message(sprintf(
    "Power-law fit (degree mode = '%s'):\n  alpha = %.3f  |  xmin = %g  |  KS stat = %.4f  |  KS p = %.4f\n  Verdict: %s",
    mode, alpha, xmin_used, KS_stat, KS_p, verdict
  ))

  # ── Plot ──────────────────────────────────────────────────────────────────
  if (plot) {
    .plot_ccdf(deg, alpha, xmin_used,
               mode_label = mode)
  }

  invisible(list(
    alpha           = alpha,
    xmin            = xmin_used,
    KS_stat         = KS_stat,
    KS_p            = KS_p,
    is_scale_free   = is_scale_free,
    degree_sequence = deg
  ))
}


# ── Internal: log-log CCDF plot ───────────────────────────────────────────────

#' @noRd
.plot_ccdf <- function(deg, alpha, xmin, mode_label) {
  tab    <- sort(unique(deg))
  ccdf   <- vapply(tab, function(k) mean(deg >= k), numeric(1))

  # Fitted line: P(K >= k) proportional to k^{-(alpha-1)} for k >= xmin
  k_fit   <- tab[tab >= xmin]
  norm_c  <- ccdf[tab == min(tab[tab >= xmin])]   # anchor at xmin
  ccdf_fit <- norm_c * (k_fit / xmin) ^ (-(alpha - 1))

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  graphics::plot(
    tab, ccdf,
    log  = "xy",
    pch  = 16,
    col  = "#555555",
    xlab = paste0("Degree (", mode_label, ")"),
    ylab = "P(K ≥ k)  [CCDF]",
    main = "Degree Distribution — Power-law fit",
    cex  = 0.7
  )
  graphics::lines(k_fit, ccdf_fit, col = "#E63946", lwd = 2)
  graphics::legend(
    "topright",
    legend = sprintf("γ = %.3f", alpha),
    col    = "#E63946",
    lwd    = 2,
    bty    = "n"
  )
  graphics::abline(v = xmin, lty = 2, col = "#AAAAAA")
}
