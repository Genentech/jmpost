

# TODO - docs
# TODO - tests
reverse_km_event_first <- function(t, times, events) {
    assert_numeric(t, any.missing = FALSE, finite = TRUE)
    assert_numeric(times, any.missing = FALSE, finite = TRUE)
    assert_numeric(events, any.missing = FALSE, finite = TRUE)
    assert_that(
        length(times) == length(events),
        all(events == 1 | events == 0)
    )
    events_cen <- 1 - events
    ord <- order(times, events_cen)
    times <- times[ord]
    events <- events[ord]
    events_cen <- events_cen[ord]
    cs_events <- cumsum(events)
    cs_censor <- cumsum(events_cen)

    g_times <- unique(times)
    g_n_events_cen <- tapply(events_cen, times, sum)
    g_cs_events_cen <- tapply(cs_censor, times, max)
    g_cs_events <- tapply(cs_events, times, max)
    g_is_cen <- tapply(events_cen, times, max)

    nrisk <- length(times) - g_cs_events - g_cs_events_cen + g_n_events_cen
    surv_interval <- 1 - g_n_events_cen / nrisk
    surv_interval <- ifelse(nrisk == 0, 1, surv_interval)
    surv <- cumprod(surv_interval)

    ct <- g_times[which(g_is_cen == 1)]
    sv <- surv[which(g_is_cen == 1)]

    names(surv) <- NULL
    names(ct) <- NULL
    names(sv) <- NULL
    list(
        t = t,
        surv = c(1, sv)[findInterval(t, ct) + 1]
    )
}


# TODO - docs
# TODO - tests
reverse_km_cen_first <- function(t, times, events) {
    assert_numeric(t, any.missing = FALSE, finite = TRUE)
    assert_numeric(times, any.missing = FALSE, finite = TRUE)
    assert_numeric(events, any.missing = FALSE, finite = TRUE)
    assert_that(
        length(times) == length(events),
        all(events == 1 | events == 0)
    )
    dat <- data.frame(
        times = times,
        events = events,
        cen_events = 1 - events
    )
    mod <- survival::survfit(
        survival::Surv(times, cen_events) ~ 1,
        data = dat
    )
    preds <- summary(mod, times = t, extend = TRUE)$surv

    assert_that(
        length(preds) == length(t)
    )
    list(
        t = t,
        surv = preds[order(order(t))]
    )
}



# TODO - docs
# TODO - tests
brier_score <- function(
    t,
    times,
    events,
    pred_mat,
    maintain_cen_order = TRUE,
    event_offset = TRUE
) {

    square_diff_mat <- bs_get_squared_dist(
        t = t,
        times = times,
        events = events,
        pred_mat = pred_mat
    )

    weight_mat <- bs_get_weights(
        t = t,
        times = times,
        events = events,
        event_offset = event_offset,
        maintain_cen_order = maintain_cen_order
    )

    # the following is a computational shortcut for diag(A %*% B)
    # as we don't want to compute the off-diagonal entries of the
    # matrix multiplication
    x <- colSums(weight_mat * square_diff_mat)
    names(x) <- t
    return(x / length(times))
}


# TODO - docs
# TODO - tests
bs_get_squared_dist <- function(t, times, events, pred_mat) {

    expected_mat <- mapply(
        \(ti, event) (ti <= t) * event * 1,
        ti = times,
        event = events,
        SIMPLIFY = FALSE
    ) |>
        unlist() |>
        matrix(ncol = length(t), byrow = TRUE)

    assert_that(
        nrow(pred_mat) == nrow(expected_mat),
        ncol(pred_mat) == ncol(expected_mat)
    )

    (expected_mat - pred_mat)^2
}


# TODO - docs
# TODO - tests
bs_get_weights <- function(
    t,
    times,
    events,
    event_offset = TRUE,
    maintain_cen_order = TRUE
) {

    n_col <- length(t)
    n_row <- length(times)

    reverse_km <- if (maintain_cen_order) reverse_km_event_first else reverse_km_cen_first
    offset <- if (event_offset) -.Machine$double.eps^(1 / 2) else 0

    censor_dist_t <- reverse_km(t, times, events)
    weight_mat_t <- 1 / matrix(
        rep(censor_dist_t$surv, n_row),
        nrow = n_row,
        byrow = TRUE
    )

    censor_dist_ti <- reverse_km(times + offset, times, events)
    weight_mat_ti <- 1 / matrix(
        rep(censor_dist_ti$surv, n_col),
        nrow = n_row
    )

    indicator_mat_t <- mapply(
        \(ti) (ti > t) * 1,
        ti = times,
        SIMPLIFY = FALSE
    ) |>
        unlist() |>
        matrix(ncol = n_col, byrow = TRUE)

    indicator_mat_ti <- mapply(
        \(ti, event) (ti <= t) * event * 1,
        ti = times,
        event = events,
        SIMPLIFY = FALSE
    ) |>
        unlist() |>
        matrix(ncol = n_col, byrow = TRUE)

    assert_that(
        all(indicator_mat_t + indicator_mat_ti <= 1)
    )

    weight_mat_t[indicator_mat_t == 0] <- 0
    weight_mat_ti[indicator_mat_ti == 0] <- 0

    (weight_mat_t + weight_mat_ti)
}
