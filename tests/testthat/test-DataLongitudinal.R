test_that("DataLongitudinal being rendered to list is as expected for simple inputs", {

    expected_vars <- c(
        "n_tumour_all", "n_tumour_obs", "n_tumour_cens", "subject_tumour_index", "subject_tumour_index_obs",
        "subject_tumour_index_cens", "tumour_value", "tumour_time", "tumour_value_lloq",
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
    expect_equal(li$n_tumour_all, 6)
    expect_equal(li$n_tumour_obs, 3)
    expect_equal(li$n_tumour_cens, 3)
    expect_equal(li$subject_tumour_index, c(1, 3, 3, 1, 2, 3))
    expect_equal(li$subject_tumour_index_obs, c(3, 4, 5))
    expect_equal(li$subject_tumour_index_cens, c(1, 2, 6))
    expect_equal(li$tumour_value, x$voutcome)
    expect_equal(li$tumour_time, x$vtime)
    expect_equal(li$tumour_value_lloq, 3)
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
