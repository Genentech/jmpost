

#' Re-used documentation for Brier Score components
#'
#' @param t (`numeric`)\cr timepoints to calculate the desired quantity at.
#' @param times (`numeric`)\cr observed times.
#' @param events (`numeric`)\cr event indicator for `times`. Either 1 for an event or 0 for censor.
#' @param event_offset (`logical`)\cr If `TRUE` then \eqn{G(T_i)} is evaluated at \eqn{G(T_i-)}.
#' Setting this as `TRUE` mirrors the implementation of the `{pec}` package.
#' @param maintain_cen_order (`logical`)\cr If `TRUE` then, in the case of ties,
#' censor times are always considered
#' to have occurred after the event times when calculating the "reverse Kaplan-Meier" for the
#' IPCW estimates. Setting this to `TRUE` mirrors the implementation of the `{prodlim}`
#' package.
#' @param ... not used.
#'
#' @name Brier-Score-Shared
#' @keywords internal
NULL




#' Reverse Kaplan-Meier
#'
#' @inheritParams Brier-Score-Shared
#' @description
#' Calculates the survival estimates of the censoring distribution
#' using the Kaplan-Meier estimate.
#' This is primarily used in the calculation of the IPCW estimates.
#'
#' @details
#' With regards to ties between censor and event times; the standard
#' approach is to regard events as occurring before censors. However,
#' when modelling the censoring distribution we are regarding the
#' censors as "events" so which should come first in the case of ties?
#'
#' The `reverse_km_event_first()` function maintains the rule
#' that events always come first even if we are regarding the censors
#' as "events". This matches the implementation of
#' `prodlim::prodlim(..., reverse = TRUE)`.
#'
#' The `reverse_km_cen_first()` function provides the alternative
#' implementation assuming that in the case of ties the censor "events"
#' come before the event "censors". This is essentially a thin wrapper
#' around `survival::survfit(Surv(time, 1 - event), ...)`
#'
#' @name reverse_km
#' @keywords internal
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


#' @rdname reverse_km
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
    preds <- summary(mod, times = t[order(t)], extend = TRUE)$surv

    assert_that(
        length(preds) == length(t)
    )
    list(
        t = t,
        surv = preds[order(order(t))]
    )
}



#' Brier Score
#'
#' @inheritParams Brier-Score-Shared
#'
#' @description
#' Implements the Brier Score as detailed in \insertCite{blanche2015}{jmpost}
#'
#' @details
#' - `bs_get_squared_dist()` - implements the squared distance part
#' of the formula.
#' - `bs_get_weights()` - implements the IPCW weighting
#'
#' @references
#' \insertAllCited{}
#'
#' @keywords internal
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
    x / length(times)
}


#' @rdname brier_score
bs_get_squared_dist <- function(t, times, events, pred_mat) {

    assert_numeric(
        times,
        finite = TRUE,
        any.missing = FALSE
    )
    assert_numeric(
        t,
        finite = TRUE,
        any.missing = FALSE
    )
    assert_numeric(
        events,
        finite = TRUE,
        any.missing = FALSE,
        lower = 0,
        upper = 1
    )
    assert_matrix(
        pred_mat,
        any.missing = FALSE,
        nrows = length(times),
        ncols = length(t)
    )
    assert_that(
        length(events) == length(times)
    )


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


#' @rdname brier_score
bs_get_weights <- function(
    t,
    times,
    events,
    event_offset = TRUE,
    maintain_cen_order = TRUE
) {
    assert_numeric(
        times,
        finite = TRUE,
        any.missing = FALSE
    )
    assert_numeric(
        t,
        finite = TRUE,
        any.missing = FALSE
    )
    assert_numeric(
        events,
        finite = TRUE,
        any.missing = FALSE,
        lower = 0,
        upper = 1
    )
    assert_flag(event_offset, na.ok = FALSE, null.ok = FALSE)
    assert_flag(maintain_cen_order, na.ok = FALSE, null.ok = FALSE)
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
