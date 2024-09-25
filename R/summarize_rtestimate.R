
#' Create a new summary object
#' @param date Integer vector. vector of index dates.
#' @param median Double vector. vector of median values.
#' @param lb Double vector. vector of lower bounds.
#' @param up Double vector. vector of upper bounds.
#' @param package String. Name of the package.
#' @export 
new_summarize <- function(
  date, median, lb, up, package
) {

  # Asserting the types
  checkmate::assert_integer(date)
  checkmate::assert_double(median)
  checkmate::assert_double(lb)
  checkmate::assert_double(up)
  checkmate::assert_string(package)

  # Checking the length
  len_date <- length(date)
  len_median <- length(median)
  len_lb <- length(lb)
  len_up <- length(up)
  if (len_date != len_median || len_date != len_lb || len_date != len_up) {
    stop("The length of the date, median, lb, and up should be the same.")
  }

  structure(
    list(
      date = date,
      median = median,
      lb = lb,
      up = up,
      package = package
    ),
    class = "summrt_summary"
  )
}

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
#' @importFrom stats median
#' @export
summarize_rtestimate.poisson_rt <- function(x, level = 0.95, lambda = NULL, ...) {

  if (!requireNamespace("rtestim", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg rtestim} package for this functionality.")
  }

  if (is.null(lambda)) {
    lambda <- 10^stats::median(log10(x$lambda))
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

  # res <- x$estimates$summarized |> dplyr::select()
}

#' @export 
#' @rdname summarize_rtestimate
summarize_rtestimate.estimate_R <- function(x, ...) {
  if (!requireNamespace("EpiEstim", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg EpiEstim} package for this functionality.")
  }
  checkmate::assert_number(level, lower = 0, upper = 1)
  
  new_summarize(
    date = x$R$t_end,
    median = x$R$`Median(R)`,
    lb = x$R$`Quantile.0.025(R)`,
    up = x$R$`Quantile.0.975(R)`,
    package = "EpiEstim"
  )
}