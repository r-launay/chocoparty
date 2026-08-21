test_that("correct output", {
  data("choco")
  expect_s3_class(mean_per_group(choco, "company_location", "rating"), "data.frame")
  expect_equal(nrow(mean_per_group(choco, "company_location", "rating")),
               length(unique(choco$company_location)))
})
