# Example data 
EpiNow2_obj <- readRDS(
  system.file("extdata", "EpiNow2_example.rds",
            package = "summrt")
)

std_epinow2 <- summarize_rtestimate(EpiNow2_obj)

message("Check that names of EpiNow2 are correct")
checkmate::expect_names( names(std_epinow2), 
                         must.include = c("estimates", 
                                          "package", 
                                          "notes"))

message("Check that the date column is actually an integer")
expect_true(is.integer(std_epinow2$estimates$date))

message("Check that the package name is correct for EpiNow2")
expect_equal(std_epinow2$package, "EpiNow2")

message("Check that there are no NAs in median, lbs, ubs")
expect_true(all(!is.na(std_epinow2$estimates$median)))
expect_true(all(!is.na(std_epinow2$estimates$lb)))
expect_true(all(!is.na(std_epinow2$estimates$ub)))


