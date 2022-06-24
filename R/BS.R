# Expected Brier score estimator iid representation
#' @export
# {{{ Inpus ;
# pred : prediction, e.g. P(T<t,cause=1|history) A MATRIX
# timepoints : vector of time points for which we aim to compute the iid representations
# times : the vector of observed time-to-event
# status : 1=uncensored, 0=censored
# cause : cause for which we aim to compute the expected Brier score estimator
# }}}

# {{{ Outputs :
# matrix with iid representation for all time points
# }}}


# {{{ Functions
# main function
BS <- function(timepoints, times, status, pred, cause = 1, compute.iid = TRUE) {
    # define useful objects
    n <- length(times)
    n_times <- length(timepoints)
    timepoints <- timepoints[order(timepoints)]
    times_names <- paste("t=", timepoints, sep = "")
    # output initialisation
    BS <- rep(NA, n_times)
    CumInci <- rep(NA, n_times)
    surv <- rep(NA, n_times)
    Stats <- matrix(NA, nrow = n_times, ncol = 4)
    hit1_all <- matrix(NA, nrow = n, ncol = n_times)
    hit2_all <- matrix(NA, nrow = n, ncol = n_times)
    epsilon_i <- matrix(NA, nrow = n, ncol = n_times)
    # adds name to outputs
    names(BS) <- times_names
    names(CumInci) <- times_names
    names(surv) <- times_names
    colnames(Stats) <- c("Cases", "survivor at t", "Other events at t", "Censored at t")
    rownames(Stats) <- times_names
    colnames(epsilon_i) <- times_names
    colnames(hit1_all) <- times_names
    colnames(hit2_all) <- times_names
    # Fit a simple KM

    # we need to order to use the ipcw() function of the pec package
    # browser()
    order_T <- order(times)
    times <- times[order_T]
    delta <- status[order_T]
    pred <- pred[order_T, , drop = FALSE]
    # compute KM weights
    weights <- ipcw(Surv(failure_time, status) ~ 1,
                    data = data.frame(failure_time = times, status = as.numeric(delta != 0)),
                    method = "marginal", times = timepoints, subjectTimes = times, subjectTimesLag = 1
    )
    Mat_data <- cbind(times, delta, as.numeric(delta == 0))
    colnames(Mat_data) <- c("T", "delta", "indic_Cens")
    # computate weights of cases
    Weights_cases_all <- 1 / (weights$IPCW.subjectTimes * n)
    # compute KM censoring estimator iid representation
    if (compute.iid) {
        MatInt0TcidhatMCksurEff <- Compute.iid.KM(times, delta != 0)
    }
    # loop on all time points
    for (t in 1:n_times) {
        Cases <- (Mat_data[, "T"] <= timepoints[t] & Mat_data[, "delta"] == cause)
        Controls_1 <- (Mat_data[, "T"] > timepoints[t])
        Controls_2 <- (Mat_data[, "T"] <= timepoints[t] & Mat_data[, "delta"] != cause & Mat_data[, "delta"] != 0)
        # compute weights
        Weights_controls_1 <- rep(1 / (weights$IPCW.times[t] * n), times = n)
        Weights_cases <- Weights_cases_all
        Weights_controls_2 <- Weights_cases_all
        Weights_cases[!Cases] <- 0
        Weights_controls_1[!Controls_1] <- 0
        Weights_controls_2[!Controls_2] <- 0
        # compute outputs
        CumInci[t] <- c(sum(Weights_cases))
        surv[t] <- c(sum(Weights_controls_1))
        Stats[t, ] <- c(sum(Cases), sum(Controls_1), sum(Controls_2), n - sum(Cases) - sum(Controls_1) - sum(Controls_2))
        hit1_all[, t] <- (Weights_controls_1 * ((pred[, t])^2)) * n
        hit2_all[, t] <- (Weights_cases * ((1 - pred[, t])^2) + Weights_controls_2 * ((pred[, t])^2)) * n
        BS[t] <- (sum(hit1_all[, t]) + sum(hit2_all[, t])) / n

        if (compute.iid) {
            # compute
            Int0tdMCsurEffARisk <- MatInt0TcidhatMCksurEff[max(which(Mat_data[, "T"] <= timepoints[t])), ]
            # browser()
            epsilon_i[, t] <- hit1_all[, t] + hit2_all[, t] - BS[t] + mean(hit1_all[, t]) * Int0tdMCsurEffARisk + colMeans(MatInt0TcidhatMCksurEff * hit2_all[, t])
        }
    }
    # compute mean and sd of iid representation
    sd_all <- rep(NA, n_times)
    mean_all <- rep(NA, n_times)
    if (compute.iid) {
        sd_all <- apply(epsilon_i, 2, sd) / sqrt(n)
        mean_all <- apply(epsilon_i, 2, mean)
    }
    # compute a table to print
    print.tab <- cbind(Stats, BS, sd_all, mean_all)
    colnames(print.tab) <- c(colnames(Stats), "BS", "sd", "mean_iid")
    # compute the computation time
    out <- list(
        BS = BS,
        iid = epsilon_i,
        sd = sd_all,
        res = (hit1_all + hit2_all),
        CumulativeIncidence = CumInci,
        survProb = surv,
        n = n,
        Stats = Stats,
        print.tab = print.tab,
        timepoints = timepoints
    )
    class(out) <- "ipcwEBS"
    out
}

