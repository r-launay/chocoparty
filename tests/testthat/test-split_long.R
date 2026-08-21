test_that("correct output", {
  data("choco")
  expect_s3_class(separate_longer(choco, "ingredients", "rating", ','), "data.frame")
  expect_true(nrow(separate_longer(choco, "company_location", "rating", ",")) !=
               length(unique(choco$ingredients)))
})
