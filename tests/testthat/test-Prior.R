test_that("Priors work as expected", {
  x <- prior_normal(4, 10)
  expect_equal(initialValues(x), 4)
  expect_equal(as.character(x), "normal(4, 10);")

  x <- prior_normal(4, 10, 20)
  expect_equal(initialValues(x), 20)
  expect_equal(as.character(x), "normal(4, 10);")
})
