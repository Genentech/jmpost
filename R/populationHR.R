#' Calculate Population Hazard Ratios
#'
#' Calculates hazard ratios marginalised over subject specific random effects using the
#' approach proposed by \insertCite{oudenhoven2020marginal}{jmpost}.
#'
#' @param object ([`JointModelSamples`]) \cr samples as drawn from a Joint Model.
#' @param hr_formula (`formula`) \cr defines the terms to include in the hazard ratio calculation.
#' By default this uses the right side of the formula used in the survival model.
#' Set to `NULL` not include any terms
#' @param baseline (`formula`) \cr terms to model baseline hazard using variable `time`.
#' Default is a B-spline from [splines]: `~bs(time, df = 10)`
#' @param quantiles (`numeric`) \cr vector of two values in (0, 1) for calculating quantiles from log hazard ratio
#' distributions.
#'
#' @references \insertAllCited{}
#'
#' @returns A list containing a summary of parameter distributions as a `data.frame` and a
#' matrix containing the parameter estimates for each sample.
#' @export
#' @importFrom splines bs
populationHR <- function(
    object,
    hr_formula = object@data@survival@formula,
    baseline = ~ bs(time, df = 10),
    quantiles = c(0.025, 0.975)
) {
    assert_class(object, "JointModelSamples")
    assert_formula(hr_formula)
    assert_formula(baseline)
    assert_numeric(quantiles, lower = 0, upper = 1, any.missing = FALSE, unique = TRUE)
    if (!"time" %in% all.vars(baseline)) stop("baseline formula should include a time term.")

    # Extract the variable names used in the data
    subject_var <- object@data@subject@subject
    arm_var <- object@data@subject@arm
    long_time_var <- all.vars(delete.response(terms(object@data@longitudinal@formula)))
    surv_time_var <- all.vars(object@data@survival@formula[[2]][[2]])
    surv_covs <- all.vars(delete.response(terms(hr_formula)))
    if (!all(surv_covs %in% colnames(object@data@survival@data))) {
        stop("All variables in hr_formula must be in survival data")
    }


    marginal_formula <- stats::reformulate(c(
        attr(terms(baseline), "term.labels"),
        attr(terms(hr_formula), "term.labels")
    ))

    # Get the survival quantities at the observed longitudinal and survival times for each patient:
    times_df <- rbind(
        stats::setNames(object@data@longitudinal@data[, c(subject_var, long_time_var)], c("subject", "time")),
        stats::setNames(object@data@survival@data[, c(subject_var, surv_time_var)], c("subject", "time"))
    )
    times_df <- times_df[order(times_df$subject, times_df$time), ]
    times_df <- times_df[times_df$time > 0, ]
    times_df <- times_df[!duplicated.data.frame(times_df), ]

    # Generate samples of the log hazard log h_i(t) for each patient i at these time points t.
    grid_spec <- split(times_df$time, times_df$subject)
    log_haz_samples <- SurvivalQuantities(
        object,
        grid = GridManual(grid_spec),
        type = "loghaz"
    )@quantities@quantities |> t()

    # Construct \tilde{X} in paper's notation with one row per patient's time
    W_df <- dplyr::left_join(
        times_df,
        stats::setNames(object@data@survival@data[, c(subject_var, surv_covs)], c("subject", surv_covs)),
        by = "subject"
    )
    W_mat <- model.matrix(marginal_formula, W_df)

    # As model matrix contains baseline and covariates, we don't need the intercept term
    # but we want factor variables encoded relative to the intercept
    W_mat <- W_mat[, colnames(W_mat) != "(Intercept)", drop = FALSE]

    estimates <- stats::lm.fit(x = W_mat, y = log_haz_samples)$coefficients
    tidy_res <- apply(estimates, 1, function(x) {
        quantiles <- stats::quantile(x, probs = quantiles)
        c(mean = mean(x), median = median(x), quantiles)
    }) |>
        t() |>
        data.frame()

    list(summary = tidy_res, estimates)
}
