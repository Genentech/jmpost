library(survival)


x <- data.frame(
  vpt = c("b", "a", "c"),
  varm = c("a", "a", "a"),
  vstudy = c("s1", "s1", "s1"),
  vtime = c(10, 20, 30),
  vevent = c(1, 1, 1)
)

test_that("Error Handling", {
  x2 <- x
  x2$vpt <- "b"

  expect_error(
    {
      DataSurvival(
        data = x2,
        formula = Surv(vtime, vevent) ~ 1,
        subject = "vpt",
        arm = "varm",
        study = "vstudy"
      )
    },
    "Only 1 survival observation"
  )
})



test_that("as.list returns expected values", {
  x <- data.frame(
    vpt = c("b", "a", "c", "d", "e"),
    varm = c("b", "a", "a", "b", "b"),
    vstudy = c("s1", "s1", "s1", "s1", "s1"),
    vtime = c(10, 20, 30, 25, 15),
    vevent = c(1, 1, 0, 1, 0)
  )

  df <- DataSurvival(
    data = x,
    formula = Surv(vtime, vevent) ~ 1,
    subject = "vpt",
    arm = "varm",
    study = "vstudy"
  )

  res <- as.list(df)

  expect_equal(res$Nind_dead, 3)
  expect_equal(res$dead_ind_index, c(1, 2, 4))
  expect_equal(res$n_arms, 2)
  expect_equal(res$Nind, 5)
  expect_equal(res$Times, x$vtime)
  expect_equal(res$study_index, rep(1, 5))
  expect_equal(res$arm_index, c(2, 1, 1, 2, 2))
  expect_equal(res$n_index_per_arm, c(2, 3))
  expect_equal(res$index_per_arm, c(2, 3, 1, 4, 5))
})
