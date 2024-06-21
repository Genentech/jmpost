
## Code to re-run this from commandline
# JMPOST_GRAPH_SNAPSHOT=TRUE \
# NOT_CRAN=TRUE \
# Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-survival_plot.R')"

snap_dir <- file.path(testthat::test_path(), "_snaps", "survival_plot")

test_that("survival_plot works as expected", {

    set.seed(38132)
    define_data <- function(i, group) {
        n <- 120
        dplyr::tibble(
            e_time = rexp(n, 1 / i),
            c_time = rexp(n, 1 / 200),
            event = ifelse(e_time <= c_time, 1, 0),
            time = ifelse(e_time <= c_time, e_time, c_time),
            group = group
        ) |> dplyr::select(time, event, group)
    }

    dat <- dplyr::bind_rows(
        define_data(100, "A"),
        define_data(75, "B"),
        define_data(50, "C")
    )

    mod <- survival::survreg(
        survival::Surv(time, event) ~ group,
        dist = "exponential",
        data = dat
    )

    preds <- predict(
        mod,
        type = "response",
        se.fit = TRUE,
        newdata = data.frame(group = c("A", "B", "C"))
    )

    med <- preds$fit
    lci <- preds$fit - preds$se.fit * 1.96
    uci <- preds$fit + preds$se.fit * 1.96

    times <- seq(0, 400, by = 30)

    get_data <- function(i, group) {
        data.frame(
            time = times,
            median = pexp(times, 1 / med[i], lower.tail = FALSE),
            lower = pexp(times, 1 / lci[i], lower.tail = FALSE),
            upper = pexp(times, 1 / uci[i], lower.tail = FALSE),
            group = group
        )
    }

    res <- dplyr::bind_rows(
        get_data(1, "A"),
        get_data(2, "B"),
        get_data(3, "C")
    )

    testthat::expect_no_error({
        p1 <- survival_plot(
            res,
            add_ci = TRUE,
            add_wrap = TRUE,
            kmdf = NULL,
            y_label = expression(frac(1, 2) + S(t^2)),
            x_label = expression(thd[3])
        )
    })


    announce_snapshot_file(file.path(snap_dir, "wrap-ci.svg"))
    if (is_graph_snapshot_enabled()) {
        vdiffr::expect_doppelganger(
            "wrap-ci",
            p1
        )
    }

    testthat::expect_no_error({
        p2 <- survival_plot(
            res,
            add_ci = FALSE,
            add_wrap = FALSE,
            kmdf = NULL,
            y_label = expression(frac(1, 2) + S(t^2)),
            x_label = expression(thd[3])
        )
    })

    announce_snapshot_file(file.path(snap_dir, "nowrap-noci.svg"))
    if (is_graph_snapshot_enabled()) {
        vdiffr::expect_doppelganger(
            "nowrap-noci",
            p2
        )
    }

    testthat::expect_no_error({
        p3 <- survival_plot(
            res,
            add_ci = FALSE,
            add_wrap = FALSE,
            kmdf = dat,
            y_label = expression(frac(1, 2) + S(t^2)),
            x_label = expression(thd[3])
        ) +
            theme(legend.position = "bottom") +
            scale_y_continuous(trans = "sqrt")
    })

    announce_snapshot_file(file.path(snap_dir, "nowrap-noci-km-ggplot2.svg"))
    if (is_graph_snapshot_enabled()) {
        vdiffr::expect_doppelganger(
            "nowrap-noci-km-ggplot2",
            p3
        )
    }
})
