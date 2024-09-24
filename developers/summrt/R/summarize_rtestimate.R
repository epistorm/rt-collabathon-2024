summarize_rtestimate <- function(x, ...) {
  UseMethod("summarize_rtestimate")
}

summarize_rtestimate.default <- function(x, ...) {
  cli::cli_abort("Your Rt method isn't supported yet. You should create a method.")
}

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
