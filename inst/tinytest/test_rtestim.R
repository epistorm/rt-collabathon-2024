# Example data
rtestim_obj <- readRDS(
  system.file("extdata", "rtestim_example.rds", package = "summrt")
)


## Check the cv method, string lambda choice
std_rtestim <- summarize_rtestimate(rtestim_obj, lambda = "lambda.min")

message("Check that names of rtestim are correct")
checkmate::expect_names(
  names(std_rtestim), must.include = c("estimates","package","notes")
)

message("Check that the date column is actually an integer")
expect_true(is.integer(std_rtestim$estimates$date))

message("Check that the package name is correct for rtestim")
expect_equal(std_rtestim$package, "rtestim")

message("Check that there are no NAs in median, lbs, ubs")
expect_true(all(!is.na(std_rtestim$estimates$median)))
expect_true(all(!is.na(std_rtestim$estimates$lb)))
expect_true(all(!is.na(std_rtestim$estimates$ub)))

## Check the cv method, numeric lambda choice
std_rtestim <- summarize_rtestimate(rtestim_obj, lambda = rtestim_obj$lambda[27])
message("Check that names of rtestim are correct")
checkmate::expect_names(
  names(std_rtestim), must.include = c("estimates","package","notes")
)

## Check the non-cv method, numeric lambda choice
std_rtestim <- summarize_rtestimate(
  rtestim_obj$full_fit,
  lambda = rtestim_obj$lambda[27]
)

message("Check that names of rtestim are correct")
checkmate::expect_names(
  names(std_rtestim), must.include = c("estimates","package","notes")
)

message("Check that the date column is actually an integer")
expect_true(is.integer(std_rtestim$estimates$date))

message("Check that the package name is correct for rtestim")
expect_equal(std_rtestim$package, "rtestim")

message("Check that there are no NAs in median, lbs, ubs")
expect_true(all(!is.na(std_rtestim$estimates$median)))
expect_true(all(!is.na(std_rtestim$estimates$lb)))
expect_true(all(!is.na(std_rtestim$estimates$ub)))
