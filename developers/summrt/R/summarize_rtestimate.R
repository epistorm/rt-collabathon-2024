summarize_rtestimate <- function(x, ...) {
  UseMethod("summarize_rtestimate")
}

summarize_rtestimate.default <- function(x, ...) {
  cli::cli_abort("Your Rt method isn't supported yet. You should create a method.")
}

summarize_rtestimate.cv_poisson_rt <- function(
    x, level = 0.95, lambda = "lambda.1se", ...) {

  if (!requireNamespace("rtestim", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg rtestim} package for this functionality.")
  }

  checkmate::assert_numeric(level, lower = 0, upper = 1, len = 1L)
  cb <- rtestim::confband(x, lambda = lambda, level = level, ...)
  alpha <- (1 - level) / 2
  tibble::tibble(
    Date = x$x,
    Rt_median = cb$fit,
    Rt_lb = cb[[2]], # danger
    Rt_ub = cb[[3]]
  )
}

summarize_rtestimate.poisson_rt <- function(x, level = 0.95, lambda = NULL, ...) {

  if (!requireNamespace("rtestim", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg rtestim} package for this functionality.")
  }

  checkmate::assert_numeric(level, lower = 0, upper = 1, len = 1L)
  cb <- rtestim::confband(x, lambda = lambda, level = level, ...)
  alpha <- (1 - level) / 2
  tibble::tibble(
    Date = x$x,
    Rt_median = cb$fit,
    Rt_lb = cb[[2]], # danger
    Rt_ub = cb[[3]]
  )
}
