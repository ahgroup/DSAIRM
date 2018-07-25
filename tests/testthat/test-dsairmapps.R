context("test-dsairmapps.R")

test_that("dsairmapp() returns character string", {
  expect_true( is.character(DSAIRM::dsairmapps()) )
})
