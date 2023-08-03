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
    expect_identical(li$n_lm_time_grid, 201L)
    expect_length(li$lm_time_grid, 201L)
})


test_that("time_grid is rejected if in an invalid format", {
    x <- data.frame(
        vpt = c("b", "a", "a", "b", "c", "a"),
        vtime = c(10, 20, 15, 5, 25, 35),
        voutcome = c(2, 1, 4, 5, 6, 2)
    )

    # First check that it doesn't error when used properly
    result <- tryCatch(
        {
            DataLongitudinal(
                data = x,
                formula = voutcome ~ vtime,
                subject = "vpt",
                threshold = 5,
                time_grid = c(2, 4)
            )
            0
        },
        error = \(e) e$message,
        warning = \(w) w$message,
        message = \(m) m$message
    )
    expect_equal(result, 0)

    # Define combinations that we would expect to cause an error
    values <- list(
        c(4, 2),
        c(4, Inf),
        c(NA_real_, 2),
        c(2, 2)
    )

    for (value in values) {
        expect_error(
            DataLongitudinal(
                data = x,
                formula = voutcome ~ vtime,
                subject = "vpt",
                threshold = 5,
                time_grid = value
            ),
            "`time_grid` needs to be finite"
        )
    }
})
