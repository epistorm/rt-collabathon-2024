#' Create a new summary object
#'
#' Creates a new summary object for the `summrt` package while validating the input.
#'
#' @param date Integer vector. vector of index dates.
#' @param median Double vector. vector of median values.
#' @param lb Double vector. vector of lower bounds.
#' @param ub Double vector. vector of upper bounds.
#' @param package String. Name of the package.
#' @param notes String. Notes about the summary.
#' @export 
#' @return A list of class `summrt_summary`. with the following components:
#' - `estimates`: A tibble with the following columns:
#'   - `date`: Integer vector. vector of index dates.
#'   - `median`: Double vector. vector of median values.
#'   - `lb`: Double vector. vector of lower bounds.
#'   - `ub`: Double vector. vector of upper bounds.
#' - `package`: String. Name of the package.
#' - `notes`: String. Notes about the summary.
new_summrt <- function(
  date, median, lb, ub, package, notes
) {

  # Asserting the types
  checkmate::assert_integer(date)
  checkmate::assert_double(median)
  checkmate::assert_double(lb)
  checkmate::assert_double(ub)
  checkmate::assert_string(package)
  checkmate::assert_string(notes)

  # Checking the length
  len_date <- length(date)
  len_median <- length(median)
  len_lb <- length(lb)
  len_up <- length(ub)
  if (len_date != len_median || len_date != len_lb || len_date != len_up) {
    stop("The length of the date, median, lb, and ub should be the same.")
  }

  structure(
    list(
      estimates = tibble::tibble(
        date = date,
        median = median,
        lb = lb,
        ub = ub
      ),
      package = package,
      notes = notes
    ),
    class = "summrt_summary"
  )
}

#' Extract Rt estimation from a model fit
#' @param x Object to extract Rt from.
#' @param ... Additional arguments passed to methods.
#' @param notes String. Optional notes to add to the summary.
#' @export
summarize_rtestimate <- function(x, ..., notes = "") {
  UseMethod("summarize_rtestimate")
}

#' @rdname summarize_rtestimate
#' @importFrom cli cli_abort
#' @export
summarize_rtestimate.default <- function(x, ..., notes = "") {
  cli::cli_abort("Your Rt method isn't supported yet. You should create a method.")
}

#' @rdname summarize_rtestimate
#' @export
#' @importFrom tibble tibble
#' @param level Confidence level for the confidence interval.
#' @param lambda The Poisson parameter (`cv_poisson_rt`).
summarize_rtestimate.cv_poisson_rt <- function(
    x, level = 0.95, lambda = "lambda.1se", ...,
    notes = "cv_poisson_rt"
    ) {

  if (!requireNamespace("rtestim", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg rtestim} package for this functionality.")
  }

  checkmate::assert_number(lambda, lower = 0)
  checkmate::assert_number(level, lower = 0, upper = 1)
  cb <- rtestim::confband(x, lambda = lambda, level = level, ...)

  new_summrt(
    date = x$x,
    median = cb$fit,
    lb = cb[[2]], # danger
    ub = cb[[3]],
    package = "rtestim",
    notes = notes
  )
}

#' @rdname summarize_rtestimate
#' @importFrom stats median
#' @export
summarize_rtestimate.poisson_rt <- function(x, level = 0.95, lambda = NULL, ..., notes = "poisson_rt") {

  if (!requireNamespace("rtestim", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg rtestim} package for this functionality.")
  }

  if (is.null(lambda)) {
    lambda <- 10^stats::median(log10(x$lambda))
  }
  checkmate::assert_number(lambda, lower = 0)
  checkmate::assert_number(level, lower = 0, upper = 1)
  cb <- rtestim::confband(x, lambda = lambda, level = level, ...)
  
  new_summrt(
    date = x$x,
    median = cb$fit,
    lb = cb[[2]], 
    ub = cb[[3]],
    package = "rtestim",
    notes = notes
  )
}

#' @rdname summarize_rtestimate
#' @export
#' @importFrom stats quantile
summarize_rtestimate.epinow <- function(x, level = 0.95, ..., notes = "") {

  if (!requireNamespace("EpiNow2", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg EpiNow2} package for this functionality.")
  }

  y_extract <- rstan::extract(x$estimates$fit)$R
  t_max <- max(lubridate::ymd(x$estimates$observations$date), na.rm = TRUE)
  t_min <- min(lubridate::ymd(x$estimates$observations$date), na.rm = TRUE)
  t_length <- as.integer(t_max - t_min)
  
  return(new_summrt(
    date = c(0:t_length, (t_length + 1):(t_length + 7)),
    median = apply(y_extract, 2, stats::quantile, probs = 0.5),
    lb = apply(y_extract, 2, stats::quantile, probs = 0.025),
    ub = apply(y_extract, 2, stats::quantile, probs = 0.975),
    notes = notes
  ))
  
}

#' @export
#' @details The `estimate_R` method is for the `EpiEstim` package.
#' @rdname summarize_rtestimate
summarize_rtestimate.estimate_R <- function(x, ..., notes = "") {
  if (!requireNamespace("EpiEstim", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg EpiEstim} package for this functionality.")
  }
  
  new_summrt(
    date    = as.integer(x$R$t_end),
    median  = x$R$`Median(R)`,
    lb      = x$R$`Quantile.0.025(R)`,
    ub      = x$R$`Quantile.0.975(R)`,
    package = "EpiEstim",
    notes   = notes
  )
}

#' @export
#' @details The `Rt` method is for the `EpiLPS` package.
#' @rdname summarize_rtestimate
summarize_rtestimate.Rt <- function(x, ...) {
  if (!requireNamespace("EpiLPS", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg EpiLPS} package for this functionality.")
  }

  new_summrt(
    date    = x$RLPS$Time,
    median  = x$RLPS$Rq0.50,
    lb      = x$RLPS$Rq0.025,
    ub      = x$RLPS$Rq0.975,
    package = "EpiLPS"
  )
}
