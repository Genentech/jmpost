
test_that("brierScore(SurvivalQuantities) returns same results as survreg", {

    #
    # As we are using a survival-only model with weakly informative priors
    # the point estimates model should be very similar to that of the frequentist
    # model implemented by `survreg`.
    # As a result the Brier Scores generated for the 2 models should be near
    # identical.
    # In all this acts as an integration test to show that all of our functions
    # related to extracting the predicted values & the time & event data are working
    # as expected.
    #
    ensure_test_data_1()

    dat_os <- test_data_1$dat_os
    mp <- test_data_1$jsamples

    ### Get our internal bayesian estimate
    t_grid <- c(1, 25, 60, 425, 750)
    sq <- SurvivalQuantities(
        mp,
        grid = GridFixed(times = t_grid),
        type = "surv"
    )
    bs_survquant <- brierScore(sq)


    ### Get estimates from frequentist model
    mod <- survival::survreg(
        survival::Surv(time, event) ~ cov_cont + cov_cat,
        data = dat_os,
        dist = "exponential"
    )
    lambda <- exp(-predict(mod, type = "lp"))

    p_times <- rep(t_grid, each = length(lambda))
    p_lambda <- rep(lambda, times.out = length(t_grid))

    pred_mat <- matrix(
        nrow = length(lambda),
        ncol = length(t_grid),
        pexp(p_times, rate = p_lambda)
    )

    bs_survreg <- brier_score(
        t = t_grid,
        times = dat_os$time,
        events = dat_os$event,
        pred_mat = pred_mat
    )

    expect_equal(
        round(bs_survquant, 3),
        round(bs_survreg, 3)
    )
})


test_that("brier score weight matrix is correctly calculated", {
    # nolint start
    # Manual by-hand calculations to show what the expected values are
    # and then comparing them to the actual values our code generates
    # For:
    #     t = 4
    #     ti = 1, 5, 10, 20
    #     ev = 1, 0 , 1, 1
    #   ti > t | ti <= t & e |   value
    # ---------|-------------|------------
    #      F   |    T        |  1 / G(ti)
    #      T   |    F        |  1 / G(t)
    #      T   |    F        |  1 / G(t)
    #      T   |    F        |  1 / G(t)

    # For:
    #     t = 11
    #     ti = 1, 5, 10, 20
    #     ev = 1, 0, 1,  1
    #   ti > t | ti <= t & e |   value
    # ---------|-------------|------------
    #      F   |    T        |  1 / G(ti)
    #      F   |    F        |  0
    #      F   |    T        |  1 / G(ti)
    #      T   |    F        |  1 / G(t)

    # Set arbitrary mock values for G(t) and G(ti) so that we
    # can isolate any issues to the bs_get_weights() function
    # rather than the reverse_km() functions. 
    mock_g_t <- c(2, 3)
    mock_g_ti <- c(4, 5, 6, 7)
    expected <- matrix(c(
        4, 4,
        2, 0,
        2, 6,
        2, 3
    ), byrow = TRUE, ncol = 2)


    replacefun <- function(t, ...) {
        x <- if (length(t) == length(mock_g_t)) {
            list(surv = 1 / mock_g_t) # G(t)
        } else if (length(t) == length(mock_g_ti)) {
            list(surv = 1 / mock_g_ti) # G(ti)
        } else {
            stop("something went wrong")
        }
        x
    }
    # nolint end
    actual <- testthat::with_mocked_bindings(
        code = {
            times <-  c(1, 5, 10, 20)
            events <- c(1, 0, 1, 1)
            t <- c(4, 11)
            bs_get_weights(t, times, events)
        },
        reverse_km_event_first = replacefun,
        reverse_km_cen_first = replacefun
    )
    expect_equal(actual, expected)
})


test_that("bs_get_squared_dist() works as expected", {
    t <- c(5, 11)
    ti <- c(1, 5, 11, 13)
    ev <- c(1, 0, 1, 1)
    pred_mat <- matrix(c(
        0.1, 0.2,
        0.3, 0.4,
        0.5, 0.6,
        0.7, 0.8
    ), byrow = TRUE, ncol = length(t))

    # nolint start
    # ti <= 5 & ev=1  |  ti <= 11 & ev=1
    #        T        |         T
    #        F        |         F
    #        F        |         T
    #        F        |         F
    # nolint end
    indicator_mat <- matrix(c(
        1, 1,
        0, 0,
        0, 1,
        0, 0
    ), byrow = TRUE, ncol = length(t))
    expected <- (indicator_mat - pred_mat)^2
    actual <- bs_get_squared_dist(
        t = t,
        times = ti,
        events = ev,
        pred_mat = pred_mat
    )
    expect_equal(actual, expected)
})


test_that("reverse_km_event_first() and reverse_km_cen_first() work as expected", {
    n <- 60
    set.seed(300)
    ## Using rounding to ensure some ties
    idat <- dplyr::tibble(
        times_real = round(rexp(n, 1 / 20)) + 1,
        cen = round(rexp(n, 1 / 30)) + 1,
        events = dplyr::if_else(times_real <= cen, 1, 0),
        times = dplyr::if_else(events == 1, times_real, cen),
        events_cen = 1 - events
    ) |>
        dplyr::sample_frac(1)

    new_times <- c(0, 1, 5, 20, 40, 40, 30, 200)

    extract_prodlim <- function(mod, new_times) {
        ## Prodlim won't predict values beyond max time (unlike survfit) so we
        ## replace any values > max time with the value for max time
        new_times_pl <- new_times
        new_times_pl[new_times_pl > max(idat$times)] <- max(idat$times)

        smod <- summary(mod, times = new_times_pl)

        ## Despite putting duplicate time values into `times` prodlim still
        ## only returns a single estimate so we need to duplicate that estimate
        index <- findInterval(new_times_pl, smod$time)
        expected <- list(
            t = new_times,
            surv = smod$surv[index]
        )
    }

    actual <- reverse_km_event_first(
        t = new_times,
        times = idat$times,
        events = idat$events
    )

    mod <- prodlim::prodlim(
        survival::Surv(times, events) ~ 1,
        data = idat,
        reverse = TRUE
    )

    expect_equal(
        actual,
        extract_prodlim(mod, new_times)
    )


    actual <- reverse_km_cen_first(
        t = new_times,
        times = idat$times,
        events = idat$events
    )

    mod <- prodlim::prodlim(
        survival::Surv(times, 1 - events) ~ 1,
        data = idat,
        reverse = FALSE
    )

    expect_equal(
        actual,
        extract_prodlim(mod, new_times)
    )
})
