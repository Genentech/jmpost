test_that("DataLongitudinal being rendered to list is as expected for simple inputs", {

    expected_vars <- c(
        "Nta_total", "Nta_obs_y", "Nta_cens_y", "ind_index", "obs_y_index",
        "cens_y_index", "Yobs", "Tobs", "Ythreshold",
        "n_mat_inds_obs_y", "w_mat_inds_obs_y", "v_mat_inds_obs_y", "u_mat_inds_obs_y",
        "n_mat_inds_cens_y", "w_mat_inds_cens_y", "v_mat_inds_cens_y", "u_mat_inds_cens_y",
        "n_mat_inds_all_y", "w_mat_inds_all_y", "v_mat_inds_all_y", "u_mat_inds_all_y"
    )

    x <- data.frame(
        vpt = factor(c("b", "a", "a", "b", "c", "a"), levels = c("b", "c", "a")),
        vtime = c(10, 20, 15, 5, 25, 35),
        voutcome = c(2, 1, 4, 5, 6, 2)
    )

    dl <- DataLongitudinal(
        data = x,
        formula = voutcome ~ vtime,
        threshold = 3
    )

    li <- as_stan_list(
        dl,
        subject_var = "vpt"
    )

    expect_equal(names(li), expected_vars)
    expect_equal(li$Nta_total, 6)
    expect_equal(li$Nta_obs_y, 3)
    expect_equal(li$Nta_cens_y, 3)
    expect_equal(li$ind_index, c(1, 3, 3, 1, 2, 3))
    expect_equal(li$obs_y_index, c(3, 4, 5))
    expect_equal(li$cens_y_index, c(1, 2, 6))
    expect_equal(li$Yobs, x$voutcome)
    expect_equal(li$Tobs, x$vtime)
    expect_equal(li$Ythreshold, 3)
})


test_that("DataSurvival print method works as expected", {

    expect_snapshot({
        x <- data.frame(
            vpt = factor(c("b", "a", "a", "b", "c", "a"), levels = c("b", "c", "a")),
            vtime = c(10, 20, 15, 5, 25, 35),
            voutcome = c(2, 1, 4, 5, 6, 2)
        )

        dl <- DataLongitudinal(
            data = x,
            formula = voutcome ~ vtime,
            threshold = 3
        )
        print(dl)
    })

    expect_snapshot({
        x <- data.frame(
            vpt = factor(c("b", "a", "a", "b", "c", "a"), levels = c("b", "c", "a")),
            vtime = c(10, 20, 15, 5, 25, 35),
            voutcome = c(2, 1, 4, 5, 6, 2)
        )

        dl <- DataLongitudinal(
            data = x,
            formula = voutcome ~ vtime
        )
        print(dl)
    })

})
