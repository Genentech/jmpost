
ensure_test_data_1()

test_that("Test that LongitudinalQuantities works as expected", {

    expected_column_names <- c("median", "lower", "upper", "time", "group")

    longsamps <- LongitudinalQuantities(
        test_data_1$jsamples,
        c("pt_00001", "pt_00002"),
        c(10, 20, 200, 300)
    )
    preds <- summary(longsamps)
    expect_equal(nrow(preds), 8)
    expect_equal(length(unique(preds$group)), 2)
    expect_equal(names(preds), expected_column_names)
    expect_equal(preds$type, NULL)


    longsamps <- LongitudinalQuantities(test_data_1$jsamples, time_grid = c(10, 20, 200, 300))
    preds <- summary(longsamps)
    expect_equal(nrow(preds), 4 * nrow(test_data_1$dat_os)) # 4 timepoints for each subject in the OS dataset
    expect_equal(names(preds), expected_column_names)
    expect_equal(unique(preds$group), test_data_1$dat_os$pt)


    longsamps <- LongitudinalQuantities(test_data_1$jsamples, c("pt_00001", "pt_00003"))
    preds <- preds <- summary(longsamps)
    expect_equal(nrow(preds), 2 * 201) # 201 default time points for 2 subjects
    expect_equal(names(preds), expected_column_names)
})


test_that("LongitudinalQuantities does not support group aggregation", {
    expect_error(
        LongitudinalQuantities(
            test_data_1$jsamples,
            groups = list("a" = c("pt_00011", "pt_00012"))
        ),
        regexp = "not 'list'"
    )
})



test_that("autoplot.LongitudinalQuantities works as expected", {

    samps <- LongitudinalQuantities(test_data_1$jsamples, c("pt_00011", "pt_00061"))
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
        groups = c("pt_00011", "pt_00061", "pt_00001", "pt_00002"),
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
        ptgroups <- c("pt_00011", "pt_00061", "pt_00001", "pt_00002")
        times <- seq(0, 100, by = 10)
        samps_p1 <- LongitudinalQuantities(test_data_1$jsamples, ptgroups, times)
        print(samps_p1)
    })
    expect_snapshot({
        ptgroups <- c("pt_00011", "pt_00061")
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
    longsamps <- LongitudinalQuantities(
        test_data_1$jsamples,
        time_grid = c(1, 100, 200, 250, 300, 350)
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
