

test_that("Test that LongitudinalQuantities works as expected", {
    ensure_test_data_1()

    expected_column_names <- c("median", "lower", "upper", "time", "group")

    times <- c(-50, -10, 0, 10, 20, 50, 200, 300)
    longsamps <- LongitudinalQuantities(
        test_data_1$jsamples,
        c("pt_001", "pt_002"),
        times
    )
    preds <- summary(longsamps)
    expect_equal(nrow(preds), length(times) * 2)
    expect_equal(length(unique(preds$group)), 2)
    expect_equal(names(preds), expected_column_names)
    expect_equal(preds$type, NULL)


    longsamps <- LongitudinalQuantities(test_data_1$jsamples, time_grid = c(-30, 10, 20, 200, 300))
    preds <- summary(longsamps)
    expect_equal(nrow(preds), 5 * nrow(test_data_1$dat_os)) # 4 timepoints for each subject in the OS dataset
    expect_equal(names(preds), expected_column_names)
    expect_equal(unique(preds$group), test_data_1$dat_os$pt)


    longsamps <- LongitudinalQuantities(test_data_1$jsamples, c("pt_001", "pt_003"))
    preds <- preds <- summary(longsamps)
    expect_equal(nrow(preds), 2 * 201) # 201 default time points for 2 subjects
    expect_equal(names(preds), expected_column_names)
})


test_that("LongitudinalQuantities does not support group aggregation", {
    expect_error(
        LongitudinalQuantities(
            test_data_1$jsamples,
            groups = list("a" = c("pt_011", "pt_012"))
        ),
        regexp = "not 'list'"
    )
})



test_that("autoplot.LongitudinalQuantities works as expected", {

    samps <- LongitudinalQuantities(test_data_1$jsamples, c("pt_011", "pt_061"))
    p <- autoplot(
        samps,
        conf.level = FALSE
    )
    dat <- summary(samps)
    expect_length(p$layers, 2)
    expect_equal(p$labels$y, expression(y))
    expect_true(inherits(p$facet, "FacetWrap"))
    expect_equal(dat, p$layers[[1]]$data)
    expect_true(inherits(p$layers[[1]]$geom, "GeomLine"))
    expect_true(inherits(p$layers[[2]]$geom, "GeomPoint"))
    expect_equal(ggplot_build(p)$layout$layout |> nrow(), 2)


    samps <- LongitudinalQuantities(
        test_data_1$jsamples,
        groups = c("pt_011", "pt_061", "pt_001", "pt_002"),
        time_grid = c(10, 20, 50, 200)
    )
    p <- autoplot(samps)
    dat <- summary(samps)
    expect_length(p$layers, 3)
    expect_equal(p$labels$y, expression(y))
    expect_true(inherits(p$facet, "FacetWrap"))
    expect_equal(dat, p$layers[[1]]$data)
    expect_true(inherits(p$layers[[1]]$geom, "GeomLine"))
    expect_true(inherits(p$layers[[3]]$geom, "GeomPoint"))
    expect_true(inherits(p$layers[[2]]$geom, "GeomRibbon"))
    expect_equal(ggplot_build(p)$layout$layout |> nrow(), 4)
})




test_that("LongitudinalQuantities print method works as expected", {

    expect_snapshot({
        ptgroups <- c("pt_011", "pt_061", "pt_001", "pt_002")
        times <- seq(0, 100, by = 10)
        samps_p1 <- LongitudinalQuantities(test_data_1$jsamples, ptgroups, times)
        print(samps_p1)
    })
    expect_snapshot({
        ptgroups <- c("pt_011", "pt_061")
        samps_p2 <- LongitudinalQuantities(test_data_1$jsamples, ptgroups)
        print(samps_p2)
    })
})





# The idea of this test is that we compare the medians of the generated quantities
# against the true values in the dataset
# As the model is perfect it should be an extremely high value meaning we are recovering
# the correct quantities
test_that("LongitudinalQuantities can recover known results", {

    set.seed(101)
    ensure_test_data_1()
    longsamps <- LongitudinalQuantities(
        test_data_1$jsamples,
        time_grid = c(-100, -50, -10, 0, 1, 100, 200, 250, 300, 350)
    )

    dat_sum <- dplyr::tibble(summary(longsamps)) |>
        dplyr::rename(pt = group)

    dat_all <- test_data_1$dat_lm |>
        dplyr::left_join(dat_sum, by = c("pt", "time")) |>
        dplyr::select(pt, time, sld, median) |>
        dplyr::group_by(pt) |>
        dplyr::summarise(correl = cor(sld, median))

    expect_true(all(dat_all$correl > 0.99))
})


test_that("LongitudinalQuantities correctly subsets patients and rebuilds correct value for each sample", {
    set.seed(101)
    ensure_test_data_1()
    times <- c(-100, 0, 1, 100, 200)

    longsamps <- LongitudinalQuantities(
        test_data_1$jsamples,
        groups = c("pt_010", "pt_011", "pt_099"),
        time_grid = times
    )

    map_me <- function(time, mat) {
        dplyr::tibble(time = as.data.frame(mat[, 1] + mat[, 2] * time)[, 1])
    }

    vars_10 <- c("lm_rs_ind_intercept[10]", "lm_rs_ind_rnd_slope[10]")
    mat1 <- test_data_1$jsamples@results$draws(vars_10, format = "draws_matrix")
    dat1 <- dplyr::bind_rows(lapply(times, map_me, mat = mat1))

    vars_11 <- c("lm_rs_ind_intercept[11]", "lm_rs_ind_rnd_slope[11]")
    mat2 <- test_data_1$jsamples@results$draws(vars_11, format = "draws_matrix")
    dat2 <- dplyr::bind_rows(lapply(times, map_me, mat = mat2))

    vars_99 <- c("lm_rs_ind_intercept[99]", "lm_rs_ind_rnd_slope[99]")
    mat3 <- test_data_1$jsamples@results$draws(vars_99, format = "draws_matrix")
    dat3 <- dplyr::bind_rows(lapply(times, map_me, mat = mat3))

    vec_actual <- as.data.frame(longsamps)[["values"]]
    vec_expected <- c(dat1$time, dat2$time, dat3$time)

    # cmdstanr rounds the generated samples to 6 s.f.
    # Stan then uses these rounded samples when calculating the generated quantiles
    # The generated quantities themselves are then rounded to 6 s.f. when being stored on disk
    # This makes direct comparison of values (even with rounding or tolerance) impossible
    # Instead we just test for an extremely high correlation
    # For reference even changing a single number in one of the vectors from say 34 to 35
    # is enough to cause this test to fail
    expect_gt(cor(vec_actual, vec_expected), 0.9999999999)
})
