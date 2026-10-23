# Synthetic posterior draws exercise extraction without Stan compilation.
make_shrinkage_samples <- function(
    variables,
    longitudinal = NULL,
    survival = FALSE
) {
    subject_data <- data.frame(
        subject = paste0("S", 1:4),
        arm = c("A", "B", "A", "B"),
        study = c("X", "X", "Y", "Y"),
        age = c(50, 60, 55, 65),
        time = c(1, 2, 3, 4),
        event = c(1, 0, 1, 0)
    )
    subject <- DataSubject(subject_data, "subject", "arm", "study")
    survival_data <- if (survival) {
        DataSurvival(subject_data, survival::Surv(time, event) ~ age + arm)
    }
    model <- JointModel(
        longitudinal = longitudinal,
        survival = if (survival) SurvivalWeibullPH()
    )
    values <- array(
        seq_len(10 * length(variables)) / (10 * length(variables)),
        dim = c(5, 2, length(variables)),
        dimnames = list(NULL, NULL, variables)
    )
    draws <- posterior::as_draws_array(values)
    .JointModelSamples(
        model = model,
        data = DataJoint(subject, survival = survival_data),
        results = structure(
            list(draws = function() draws),
            class = "CmdStanMCMC"
        )
    )
}

# Return summaries in reverse order to exercise matching by Stan variable.
make_coefficient_samples <- function(
    variables,
    longitudinal = NULL,
    survival = FALSE
) {
    fit <- make_shrinkage_samples(variables, longitudinal, survival)
    draws <- fit@results$draws()
    fit@results$summary <- function(variables, ...) {
        posterior::summarise_draws(
            subset(draws, variable = rev(variables)),
            ...
        )
    }
    fit
}
