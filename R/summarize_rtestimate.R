#' Extract Rt estimation from a model fit
#' @param x Object to extract Rt from.
#' @param ... Additional arguments passed to methods.
#' @export
summarize_rtestimate <- function(x, ...) {
  UseMethod("summarize_rtestimate")
}

#' @rdname summarize_rtestimate
#' @importFrom cli cli_abort
#' @export
summarize_rtestimate.default <- function(x, ...) {
  cli::cli_abort("Your Rt method isn't supported yet. You should create a method.")
}

#' @rdname summarize_rtestimate
#' @export
#' @importFrom tibble tibble
#' @param level Confidence level for the confidence interval.
#' @param lambda The Poisson parameter (`cv_poisson_rt`).
summarize_rtestimate.cv_poisson_rt <- function(
    x, level = 0.95, lambda = "lambda.1se", ...) {

  if (!requireNamespace("rtestim", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg rtestim} package for this functionality.")
  }

  checkmate::assert_number(lambda, lower = 0)
  checkmate::assert_number(level, lower = 0, upper = 1)
  cb <- rtestim::confband(x, lambda = lambda, level = level, ...)
  tibble::tibble(
    Date = x$x,
    Rt_median = cb$fit,
    Rt_lb = cb[[2]], # danger
    Rt_ub = cb[[3]]
  )
}

#' @rdname summarize_rtestimate
#' @export
summarize_rtestimate.poisson_rt <- function(x, level = 0.95, lambda = NULL, ...) {

  if (!requireNamespace("rtestim", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg rtestim} package for this functionality.")
  }

  if (is.null(lambda)) {
    lambda <- 10^median(log10(x$lambda))
  }
  checkmate::assert_number(lambda, lower = 0)
  checkmate::assert_number(level, lower = 0, upper = 1)
  cb <- rtestim::confband(x, lambda = lambda, level = level, ...)
  tibble::tibble(
    Date = x$x,
    Rt_median = cb$fit,
    Rt_lb = cb[[2]], # danger
    Rt_ub = cb[[3]]
  )
}

#' @rdname summarize_rtestimate
#' @export
summarize_rtestimate.epinow <- function(x, level = 0.95, ...) {

  if (!requireNamespace("epinow2", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg epinow2} package for this functionality.")
  }
  checkmate::assert_number(level, lower = 0, upper = 1)

  res <- x$estimates$summarized |> dplyr::select()
}
