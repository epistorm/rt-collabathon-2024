#' Corrupt an epidemic timeseries by dropping out points
#'
#' @param cases An integer vector of cases
#' @param p_dropout A double between 0 and 1 of the number of elements to remove
#' @param seed The random seed
#' @return An integer vector of cases with `p_dropout` cases removed`
#'
#' @export
apply_dropout <- function(cases, p_dropout, seed) {
  if (p_dropout < 0 || p_dropout > 1) {
    stop("arg `p_dropout` must be between 0 and 1")
  }

  withr::with_seed(seed, {
    # Choose indices to set to 0
    N <- length(cases)
    dropout <- rnbinom(N, size = 1, prob = p_dropout)
    
    # Apply indices
    cases[dropout] = 0
   })

  return(cases)
}
