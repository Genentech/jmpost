
test_that("longitudinal_plot() works as expected", {
    set.seed(3023)

    dat_a <- dplyr::tibble(
        time = seq(0, 100, by = 5),
        median = rnorm(length(time), 0, 14) + 40 + 5 * time,
        lower = median * 0.8,
        upper = median * 1.2,
        group = "a"
    )

    dat_ob_a <- dplyr::tibble(
        time = c(15, 25, 45, 75),
        Yob = 40 + 5 * time,
        group = "a"
    )

    dat_b <- dplyr::tibble(
        time = seq(0, 100, by = 5),
        median = rnorm(length(time), 0, 3) + 150 - 2 * time,
        lower = median * 0.8,
        upper = median * 1.2,
        group = "b"
    )

    dat_ob_b <- dplyr::tibble(
        time = c(25, 75, 100),
        Yob = 150 - 2 * time,
        group = "b"
    )

    dat <- dplyr::bind_rows(dat_a, dat_b)
    dat_ob <- dplyr::bind_rows(dat_ob_a, dat_ob_b)

    p1 <- longitudinal_plot(
        data = dat,
        data_obs = dat_ob,
        add_ci = TRUE
    )

    vdiffr::expect_doppelganger(
        "longitudinal_plot with ci",
        p1
    )

    p2 <- longitudinal_plot(
        data = dat,
        data_obs = dat_ob,
        add_ci = FALSE
    )

    vdiffr::expect_doppelganger(
        "longitudinal_plot without ci",
        p2
    )
})
