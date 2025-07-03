#' Calculate Population Hazard Ratios
#'
#' Calculates hazard ratios marginalised over subject specific random effects
#'
#' @param object ([`JointModelSamples`]) \cr samples as drawn from a Joint Model.
#' @param HR_terms (`character`) \cr to include in the hazard ratio calculation.
#' By default this include the terms used in the survival model.
#' Set to `NULL` not include any terms
#' @param arm (`logical`) \cr calculate hazard ratio for `arm`.
#' @param baseline (`string`) \cr terms to model baseline hazard using variable `"time"`.
#' Default is a B-spline from [splines]: `"bs(time, df=10)"`
#'
#' @returns A matrix
#'
#' @importFrom splines bs

PopulationHR <- function(object,
                         HR_terms = attr(terms(object@data@survival@formula), "term.labels"),
                         arm = TRUE,
                         baseline = "bs(time, df=10)") {
    # Extract the variable names used in the data
    subject_var <- object@data@subject@subject
    arm_var <- object@data@subject@arm
    long_time_var <- all.vars(delete.response(terms(object@data@longitudinal@formula)))
    surv_time_var <- all.vars(object@data@survival@formula[[2]][[2]])
    surv_covs <- all.vars(delete.response(terms(object@data@survival@formula)))

    population_terms <- unique(c(
        baseline,
        if (arm) arm_var else NULL,
        HR_terms
    ))

    marginal_formula <- reformulate(population_terms)

    # Get the survival quantities at the observed longitudinal times
    # and survival event times for each patient:
    times_df <- rbind(
        object@data@longitudinal@data |>
            select(subject = {{subject_var}}, time = {{long_time_var}}),
        object@data@survival@data |>
            select(subject = {{subject_var}}, time = {{surv_time_var}})
    ) |>
        filter(time > 0)

    # Generate samples of the log hazard log h_i(t) for each patient i at these time points t.
    grid_spec <- split(times_df$time, times_df$subject)
    logH <- SurvivalQuantities(
        object,
        grid = GridManual(grid_spec),
        type = "loghaz"
    )@quantities@quantities |> t()

    # Construct \tilde{X} in paper's notation with one row per patient's time
    W_df <- times_df |>
        select(subject, time) |>
        left_join(
            object@data@subject@data |>
                select(subject = {{subject_var}}, arm = {{arm_var}}),
            by = "subject"
        ) |>
        left_join(
            object@data@survival@data |>
                select(subject = {{subject_var}}, {{surv_covs}}),
            by = "subject"
        )

    W_mat <- model.matrix(marginal_formula, W_df)
    # As model matrix contains baseline and covariates, we don't need the intercept term
    # but we want factor variables encoded relative to the intercept
    W_mat <- W_mat[, colnames(W_mat) != "(Intercept)", drop = FALSE]

    estimates <- lm.fit(x = W_mat, y = logH)$coefficients
    tidy_res <- apply(estimates, 1, function(x) {
        c(mean = mean(x),
          median = median(x),
          quantile(x, c(0.025, 0.975))
        )
        }) |> t()

    list(tidy_res, estimates)
}

