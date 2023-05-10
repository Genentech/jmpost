test_that("LinkRandomSlope smoke tests", {
  linkobject <- LinkRandomSlope(
    link_lm_phi = prior_normal(0, 5, init = 0.01)
  )


  expect_equal(
    initialValues(linkobject),
    list("link_lm_phi" = 0.01)
  )


  expect_true(
    is(as.StanModule(linkobject), "StanModule")
  )


  expect_error(
    LongitudinalRandomSlope(lm_rs_pp = prior_normal(0, 1)),
    "unused argument \\(lm_rs_pp"
  )
})
