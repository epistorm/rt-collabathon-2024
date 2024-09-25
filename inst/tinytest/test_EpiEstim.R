# Example data 
EpiEstim_obj <- readRDS(
  system.file("extdata", "EpiEstim_example.rds",
              package = "summrt")
)

std_epiestim <- summarize_rtestimate(EpiEstim_obj)

message("Check that names of EpiEstim are correct")
checkmate::expect_names( names(std_epiestim), 
                         must.include = c("estimates", 
                                          "package", 
                                          "notes"))

message("Check that the date column is actually an integer")
expect_true(is.integer(std_epiestim$estimates$date))

message("Check that the package name is correct for EpiEstim")
expect_equal(std_epiestim$package, "EpiEstim")

message("Check that there are no NAs in median, lbs, ubs")
expect_true(all(!is.na(std_epiestim$estimates$median)))
expect_true(all(!is.na(std_epiestim$estimates$lb)))
expect_true(all(!is.na(std_epiestim$estimates$ub)))


