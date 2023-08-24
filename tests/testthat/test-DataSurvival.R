
test_that("Error Handling", {

    x2 <- data.frame(
        vpt = c("b", "b", "c"),
        varm = c("a", "a", "a"),
        vstudy = c("s1", "s1", "s1"),
        vtime = c(10, 20, 30),
        vevent = c(1, 1, 1)
    )

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

test_that("DataSurvival being rendered to list is as expected for simple inputs", {

    x <- data.frame(
        vpt = c("b", "a", "c", "d", "e"),
        varm = c("a2", "a1", "a1", "a2", "a2"),
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
    expect_equal(res$Times, c(20, 10, 30, 25, 15))
    expect_equal(res$pt_study_index, rep(1, 5))
    expect_equal(res$pt_arm_index, c(1, 2, 1, 2, 2))
})
