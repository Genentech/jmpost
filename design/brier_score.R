
source(here::here("./local/brier_score_george.R"))

library(survival)
library(dplyr)
library(assertthat)
library(testthat)


# Returns a (n, k) matrix
# n = number of subjects in data
# k = number of timepoints we want to calculate score at
get_pred_mat <- function(idat, t) {
    mod <- survreg(Surv(times, events) ~ 1, data = idat)
    b <- exp(coef(mod))
    a <- 1 / mod$scale
    matrix(
        pweibull(rep(t, length(idat$times)), shape = a, scale = b),
        ncol = length(t),
        byrow = TRUE
    )
}


brier_score <- function(t, times, events, pred_mat) {

    # dat <- data.frame(
    #     times = times,
    #     events = events,
    #     index = seq_along(times)
    # )

    # dat <- dat[order(dat$times, -dat$events),, drop = FALSE]
    # dat[order(dat$index), ]

    t_mat <- matrix(
        rep(t, length(times)),
        ncol = length(t),
        byrow = TRUE
    )

    time_mat <- matrix(
        rep(times, length(t)),
        ncol = length(t),
        byrow = FALSE
    )

    event_mat <- matrix(
        rep(events, length(t)),
        ncol = length(t),
        byrow = FALSE
    )

    expected_mat <- (time_mat <= t_mat) * event_mat

    square_diff_mat <- (expected_mat - pred_mat)^2


    mod <- survfit(Surv(times, 1 - events) ~ 1)
    smod_t <- summary(mod, times = t, extend = TRUE)
    surv_cen_t <- smod_t$surv[order(order(t))]

    weight_mat_t <- 1 / matrix(
        rep(surv_cen_t, length(times)),
        nrow = length(t)
    )

    smod_ti <- summary(mod, times = times, extend = TRUE)
    surv_cen_ti <- smod_ti$surv[order(order(times, -events))]

    weight_mat_ti <- 1 / matrix(
        rep(surv_cen_ti, length(t)),
        nrow = length(t),
        byrow = TRUE
    )

    indicator_mat_t <- t((time_mat > t_mat) * 1)
    indicator_mat_ti <- t((time_mat <= t_mat) * event_mat * 1)

    assert_that(
        all(indicator_mat_t + indicator_mat_ti <= 1)
    )

    weight_mat_t[indicator_mat_t == 0] <- 0
    weight_mat_ti[indicator_mat_ti == 0] <- 0

    weight_mat <- (weight_mat_t + weight_mat_ti) / length(times)

    # Computational shortcut for diag(A %*% B)
    x <- colSums(t(weight_mat) * square_diff_mat)
    names(x) <- t
    browser()
    return(x)
}



n <- 60
set.seed(300)
idat <- tibble(
    times_real = rexp(n, 1 / 20),
    cen = rexp(n, 1 / 40),
    events = if_else(times_real <= cen, 1, 0),
    times = if_else(events == 1, times_real, cen),
    cen_event = 1 - events
) |>
    mutate(times = round(times) + 1) |>
    arrange(times, -events)

idat2 <- idat |> sample_frac(1)
idat |> print(n = 999)
t <- c(10, 20, 50)


brier_score(
    t,
    idat$times,
    idat$events,
    get_pred_mat(idat, t)
)
brier_score(
    t,
    idat2$times,
    idat2$events,
    get_pred_mat(idat, t)
)
brier_score(
    rev(t),
    idat2$times,
    idat2$events,
    get_pred_mat(idat, rev(t))
)


BS(
    t,
    idat$times,
    idat$events,
    get_pred_mat(idat, t),
    cause = 1,
    compute.iid = FALSE
)$print.tab[, "BS"]

BS(
    t,
    idat2$times,
    idat2$events,
    get_pred_mat(idat, t),
    cause = 1,
    compute.iid = FALSE
)$print.tab[, "BS"]

BS(
    rev(t),
    idat2$times,
    idat2$events,
    get_pred_mat(idat, t),
    cause = 1,
    compute.iid = FALSE
)$print.tab[, "BS"]

library(pec)
library(timeROC)


actual <- brier_score(t, times, events, pred_mat)
pre_e <- BS(t, times, events, pred_mat, cause = 1, compute.iid = FALSE)
expected <- e2$print.tab[, "BS"]
names(expected) <- NULL

expect_equal(actual, expected)

x2 <- c(1, 2, 3)
x <- c(5, 2, 6)

c(2, 5, 6)[c(2, 1, 3)]

c(2, 5, 6)
c(2, 1, 3)
x[order(x)][order(x)]




weights <- pec::ipcw(
    Surv(times, events) ~ 1,
    data = idat,
    method = "marginal",
    times = idat$times,
    subjectTimes = t,
    subjectTimesLag = 1
)
weights$IPCW.subjectTimes



mod <- survfit(Surv(times, 1-events) ~ 1, data = idat)
smod <- summary(mod, times = idat$times, extend = TRUE)
smod$surv

idat |> print(n = 20)

pec::ipcw
pec:::ipcw.marginal
subjectTimesLag <- 1
formula <- Surv(times, events) ~ 1
formula <- update.formula(formula, "~1")
fit <- prodlim::prodlim(formula, data = idat, reverse = TRUE)
prodlim::predictSurvIndividual(fit, lag = 0)


all(round(fit$surv,12) == round(mod$surv,12))


?survfit.formula

library(prodlim)
dat <- SimSurv(30)
pfit <- prodlim(Surv(time, status) ~ 1, data = dat)
pfit <- prodlim(Hist(time, status) ~ 1, data = dat, reverse = TRUE) ## same thing
sfit <- survfit(Surv(time, 1-status) ~ 1, data = dat, conf.type = "plain")
##  same result for the survival distribution function



library(dplyr)
library(prodlim)
library(survival)

n <- 60
set.seed(300)
## Using rounding to ensure some ties
idat <- tibble(
    times_real = round(rexp(n, 1 / 20)) + 1,
    cen = round(rexp(n, 1 / 40)) + 1,
    events = if_else(times_real <= cen, 1, 0),
    times = if_else(events == 1, times_real, cen)
)

## Using survfit
mod <- survfit(Surv(times, 1 - events) ~ 1, data = idat)

## Using reverse=TRUE option
pfit <- prodlim(Surv(times, events) ~ 1, data = idat, reverse = TRUE)
all(round(pfit$surv, 12) == round(mod$surv, 12))   # Does not match

## Using 1-events as done for survfit
pfit <- prodlim(Surv(times, 1 - events) ~ 1, data = idat)
all(round(pfit$surv, 12) == round(mod$surv, 12))   #  Matches the same

