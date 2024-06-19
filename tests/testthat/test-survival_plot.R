

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

    p1 <- survival_plot(
        res,
        add_ci = TRUE,
        add_wrap = TRUE,
        kmdf = NULL,
        y_label = expression(frac(1, 2) + S(t^2)),
        x_label = expression(thd[3])
    )

    if (is_graph_snapshot_enabled()) {
        vdiffr::expect_doppelganger(
            "survival_plot with wrap and ci",
            p1
        )
    }


    p2 <- survival_plot(
        res,
        add_ci = FALSE,
        add_wrap = FALSE,
        kmdf = NULL,
        y_label = expression(frac(1, 2) + S(t^2)),
        x_label = expression(thd[3])
    )

    if (is_graph_snapshot_enabled()) {
        vdiffr::expect_doppelganger(
            "survival_plot with no wrap and no ci",
            p2
        )
    }


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

    if (is_graph_snapshot_enabled()) {
        vdiffr::expect_doppelganger(
            "survival_plot-no wrap + no ci + km + ggplot2 ",
            p3
        )
    }
})
