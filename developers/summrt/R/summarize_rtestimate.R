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
    x, level = 0.95, lambda, ...) {

  cb <- confband(x, lambda = lambda, level = level, ...)
  tibble::tibble(
    Date = x$x,
    Rt_median = fitted(x, lambda = lambda)
    Rt_lb = x$x,
    Rt_ub = x$x
  )
}
