test_that("DataLongitudinal being rendered to list is as expected for simple inputs", {
    x <- data.frame(
        vpt = c("b", "a", "a", "b", "c", "a"),
        vtime = c(10, 20, 15, 5, 25, 35),
        voutcome = c(2, 1, 4, 5, 6, 2)
    )

    dl <- DataLongitudinal(
        data = x,
        formula = voutcome ~ vtime,
        subject = "vpt",
        threshold = 3
    )

    li <- as.list(dl)

    expect_equal(li$Yobs, x$voutcome)
    expect_equal(li$ind_index, c(2, 1, 1, 2, 3, 1))
    expect_equal(li$obs_y_index, c(3, 4, 5))
    expect_equal(li$cens_y_index, c(1, 2, 6))
    expect_equal(li$Nta_cens_y, 3)
    expect_equal(li$Nta_obs_y, 3)
    expect_equal(li$Nta_total, 6)
    expect_equal(li$Tobs, x$vtime)
})
