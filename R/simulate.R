#' Simulate Patients from Posterior Predictive Distribution
#' @param object A [JointModelSamples] object
#' @param newdata A data frame containing data in the same format as
#'  the `object@data@survival@data`. Importantly, it should contain the same covariates and factor levels
#'  as the variables used in the survival formula `object@data@survival@formula` and
#'  the same columns used for `study`, `id`, and `arm`.
#' @param ... Unused.
#' @param times Times to simulate SLD.
#' @param time_max (`number`)\cr the maximum time to simulate to.
#' @param time_step (`number`)\cr the time interval between evaluating the log-hazard function.
#' @param lambda_censor (`number`)\cr the censoring rate, as the parameter of an exponential distribution.
#' @param scaled_variance Should variance be scaled by the expected value. Must be set the same as was used for
#' model fitting.
#' @param seed Ignored.
#' @param nsim Ignored.
#' @details Simulates a set of patients based on the covariates of those
#' used in the model fit or from `newdata`, which must contain the
#' same column names and factor levels.
#'
#' @exportS3Method stats::simulate
#' @export
simulate.JointModelSamples <- function(object,
                                       newdata = NULL,
                                       ...,
                                       times = c(0, 10, 50, 100),
                                       time_max = 2000,
                                       time_step = 1,
                                       lambda_censor = 1 / 3000,
                                       scaled_variance = TRUE,
                                       seed = NULL,
                                       nsim = NULL) {
    subj_data <- if (is.null(newdata)) {
        dplyr::left_join(
            object@data@subject@data[, c(
                object@data@subject@subject,
                object@data@subject@arm,
                object@data@subject@study
            )],
            object@data@survival@data[, c(
                object@data@subject@subject,
                all.vars(delete.response(terms(object@data@survival@formula)))
            )],
            by = object@data@subject@subject
        )
    } else {
        newdata
    }

    subject_data <- DataSubject(
        subj_data,
        subject = object@data@subject@subject,
        arm = object@data@subject@arm,
        study = object@data@subject@study
    )


    n_patients <- nrow(subject_data@data)

    requiredVars <- list(
        longitudinal = lapply(object@model@survival@parameters@parameters, function(x) x@name),
        survival = lapply(object@model@longitudinal@parameters@parameters, function(x) x@name),
        link = lapply(object@model@link@components, function(x) x@key)
    )

    draws <- object@results$draws(variables = unlist(requiredVars), format = "draws_matrix")
    draw_id <- sample.int(nrow(draws), n_patients, replace = FALSE)

    long_models <- list()
    surv_models <- list()
    for (i in seq.int(n_patients)) {
        draw <- draws[draw_id[i], ]
        long_models[[i]] <- createLongitudinalSimObject(object@model@longitudinal,
            draw,
            times = times,
            scaled_variance = scaled_variance
        )
        surv_models[[i]] <- createSurvivalSimObject(
            object@model@survival,
            draw,
            lambda_censor = lambda_censor,
            time_step = time_step,
            time_max = time_max
        )
    }

    SimJointDataResults(
        subject = subject_data,
        surv_formula = object@data@survival@formula,
        longitudinal = long_models,
        survival = surv_models
    )
}

# Longitudinal Sim Object constructors --------
#' @noRd
createLongitudinalSimObject <- function(object, draw, ...) {
    UseMethod("createLongitudinalSimObject")
}

#' Get draws value by name
#' @param draws matrix
#' @param name character to match column names of `draws` with [startsWith]
#' @return A vector of matching values or `NULL` if no match.
#' @noRd
get_vars <- function(draws, name) {
    result <- as.numeric(draws[, startsWith(colnames(draws), name)])
    if (length(result) == 0) {
        NULL
    } else {
        result
    }
}

#' @exportS3Method
createLongitudinalSimObject.LongitudinalRandomSlope <- function(object, draw, ...) {
    args <- list(...)

    args$intercept <- get_vars(draw, "lm_rs_intercept")
    args$slope_mu <- get_vars(draw, "lm_rs_slope_mu")
    args$slope_sigma <- get_vars(draw, "lm_rs_slope_sigma")
    args$sigma <- get_vars(draw, "lm_rs_sigma")
    args$link_dsld <- get_vars(draw, "link_dsld")
    args$link_identity <- get_vars(draw, "link_identity")
    args$scaled_variance <- NULL # not defined for random slope
    do.call(SimLongitudinalRandomSlope, args)
}

#' @exportS3Method
createLongitudinalSimObject.LongitudinalSteinFojo <- function(object, draw, ...) {
    args <- list(...)
    args$sigma <- get_vars(draw, "lm_sf_sigma")
    args$mu_s <- get_vars(draw, "lm_sf_mu_ks")
    args$mu_g <- get_vars(draw, "lm_sf_mu_kg")
    args$mu_b <- get_vars(draw, "lm_sf_mu_bsld")
    args$omega_s <- get_vars(draw, "lm_sf_omega_ks")
    args$omega_g <- get_vars(draw, "lm_sf_omega_kg")
    args$omega_b <- get_vars(draw, "lm_sf_omega_bsld")
    args$link_dsld <- get_vars(draw, "link_dsld")
    args$link_ttg <- get_vars(draw, "link_ttg")
    args$link_identity <- get_vars(draw, "link_identity")
    args$link_growth <- get_vars(draw, "link_growth")
    args$link_shrinkage <- get_vars(draw, "link_shrinkage")
    do.call(SimLongitudinalSteinFojo, args)
}

#' @exportS3Method
createLongitudinalSimObject.LongitudinalGSF <- function(object, draw, ...) {
    args <- list(...)
    args$sigma <- get_vars(draw, "lm_gsf_sigma")
    args$mu_s <- get_vars(draw, "lm_gsf_mu_ks")
    args$mu_g <- get_vars(draw, "lm_gsf_mu_kg")
    args$mu_b <- get_vars(draw, "lm_gsf_mu_bsld")
    args$mu_phi <- get_vars(draw, "lm_gsf_mu_phi")
    args$omega_s <- get_vars(draw, "lm_gsf_omega_ks")
    args$omega_g <- get_vars(draw, "lm_gsf_omega_kg")
    args$omega_b <- get_vars(draw, "lm_gsf_omega_bsld")
    args$omega_phi <- get_vars(draw, "lm_gsf_omega_phi")
    args$link_dsld <- get_vars(draw, "link_dsld")
    args$link_ttg <- get_vars(draw, "link_ttg")
    args$link_identity <- get_vars(draw, "link_identity")
    args$link_growth <- get_vars(draw, "link_growth")
    args$link_shrinkage <- get_vars(draw, "link_shrinkage")

    do.call(SimLongitudinalGSF, args)
}

#' @exportS3Method
createLongitudinalSimObject.LongitudinalClaretBruno <- function(object, draw, ...) {
    args <- list(...)
    args$sigma <- get_vars(draw, "lm_clbr_sigma")
    args$mu_b <- get_vars(draw, "lm_clbr_mu_b")
    args$mu_g <- get_vars(draw, "lm_clbr_mu_g")
    args$mu_c <- get_vars(draw, "lm_clbr_mu_c")
    args$mu_p <- get_vars(draw, "lm_clbr_mu_p")
    args$omega_b <- get_vars(draw, "lm_clbr_omega_b")
    args$omega_g <- get_vars(draw, "lm_clbr_omega_g")
    args$omega_c <- get_vars(draw, "lm_clbr_omega_c")
    args$omega_p <- get_vars(draw, "lm_clbr_omega_p")
    args$link_dsld <- get_vars(draw, "link_dsld")
    args$link_ttg <- get_vars(draw, "link_ttg")
    args$link_identity <- get_vars(draw, "link_identity")
    args$link_growth <- get_vars(draw, "link_growth")
    args$link_shrinkage <- get_vars(draw, "link_shrinkage")
    do.call(SimLongitudinalClaretBruno, args)
}


# Survival Sim Object constructors --------

#' @noRd
createSurvivalSimObject <- function(object, draw, ...) {
    UseMethod("createSurvivalSimObject")
}

#' @exportS3Method
createSurvivalSimObject.SurvivalWeibullPH <- function(object, draw, ...) {
    args <- list(...)
    args$lambda <- get_vars(draw, "sm_weibull_ph_lambda")
    args$gamma <- get_vars(draw, "sm_weibull_ph_gamma")

    result <- do.call(SimSurvivalWeibullPH, args)
    result@beta_os_cov <- get_vars(draw, "beta_os_cov")
    result
}

#' @exportS3Method
createSurvivalSimObject.SurvivalExponential <- function(object, draw, ...) {
    args <- list(...)
    args$lambda <- get_vars(draw, "sm_exp_lambda")

    result <- do.call(SimSurvivalExponential, args)
    result@beta_os_cov <- get_vars(draw, "beta_os_cov")
    result
}

#' @exportS3Method
createSurvivalSimObject.SurvivalGamma <- function(object, draw, ...) {
    lapply(object@model@survival@parameters@parameters, function(x) x@name)
    lapply(object@model@link@components, function(x) x@key)
    args <- list(...)
    args$k <- get_vars(draw, "sm_gamma_k")
    args$theta <- get_vars(draw, "sm_gamma_theta")

    result <- do.call(SimSurvivalGamma, args)
    result@beta_os_cov <- get_vars(draw, "beta_os_cov")
    result
}

#' @exportS3Method
createSurvivalSimObject.SurvivalLogLogistic <- function(object, draw, ...) {
    lapply(object@model@survival@parameters@parameters, function(x) x@name)
    lapply(object@model@link@components, function(x) x@key)
    args <- list(...)
    args$a <- get_vars(draw, "sm_loglogis_a")
    args$b <- get_vars(draw, "sm_loglogis_b")

    result <- do.call(SimSurvivalLogLogistic, args)
    result@beta_os_cov <- get_vars(draw, "beta_os_cov")
    result
}


sampleSubjectsFromObs <- function(object, subjects_df, covs_matrix) {
    subjects_df$log_haz_cov <- rowSums(covs_matrix[, -1] * t(sapply(object, function(x) x@beta_os_cov)))
    subjects_df |>
        dplyr::mutate(survival = stats::runif(dplyr::n())) |>
        dplyr::mutate(chazard_limit = -log(.data$survival)) |>
        dplyr::mutate(time_cen = stats::rexp(dplyr::n(), sapply(object, function(x) x@lambda_censor)))
}

# Simulate patient function ------
SimJointDataResults <- function(subject,
                                surv_formula,
                                longitudinal,
                                survival,
                                .silent = FALSE) {
    # take hazard windows from first element. All have the same
    # @time_step and @time_max parameters
    hazard_evaluation_info <- hazardWindows(survival[[1]])

    group_counts <- as.data.frame(table(subject@data[, c(subject@study, subject@arm)]))

    n_group <- group_counts$Freq
    arms <- group_counts[, 2]
    studies <- group_counts[, 1]
    n_subjects <- sum(n_group)
    n_times <- length(hazard_evaluation_info$midpoint)

    baseline <- data.frame(
        subject = subject@data[[subject@subject]],
        study = subject@data[[subject@study]],
        arm = subject@data[[subject@arm]]
    )

    cov_cols <- cbind(
        subject = subject@data[[subject@subject]],
        subject@data[, all.vars(delete.response(terms(surv_formula)))]
    )


    os_baseline <- sampleSubjectsFromObs(
        survival,
        subjects_df = baseline,
        model.matrix(delete.response(terms(surv_formula)), subject@data)
    )


    lm_baseline <- dplyr::bind_rows(
        lapply(seq.int(longitudinal), function(i) sampleSubjects(longitudinal[[i]], subjects_df = baseline[i, ]))
    )

    lm_dat_no_obvs <- dplyr::cross_join(baseline, data.frame(time = longitudinal[[1]]@times)) |>
        dplyr::left_join(lm_baseline, by = c("subject", "study", "arm"))


    lm_dat <- dplyr::bind_rows(
        .mapply(
            sampleObservations,
            dots = list(times_df = split(lm_dat_no_obvs, ~subject), object = longitudinal),
            MoreArgs = NULL
        )
    )

    hazard_eval_df <- dplyr::tibble(
        subject = rep(lm_baseline$subject, each = n_times),
        arm = rep(lm_baseline$arm, each = n_times),
        study = rep(lm_baseline$study, each = n_times),
        midpoint = rep(as.double(hazard_evaluation_info$midpoint), times = n_subjects),
        time = rep(as.double(hazard_evaluation_info$upper), times = n_subjects),
        width = rep(as.double(hazard_evaluation_info$width), times = n_subjects)
    )

    lm_link_dat <- sampleObservations(
        longitudinal[[1]],
        dplyr::left_join(hazard_eval_df, lm_baseline, by = c("subject", "study", "arm"))
    )[, c("subject", "study", "arm", "log_haz_link", "time", "width", "midpoint")]

    lm_link_dat <- dplyr::bind_rows(
        .mapply(
            sampleObservations,
            dots = list(
                times_df = split(
                    dplyr::left_join(hazard_eval_df, lm_baseline, by = c("subject", "study", "arm")),
                    ~subject
                ),
                object = longitudinal
            ),
            MoreArgs = NULL
        )
    )


    os_eval_df <- lm_link_dat |>
        dplyr::left_join(os_baseline, by = c("subject", "study", "arm"))

    withCallingHandlers(
        os_dat <- .mapply(
            sampleObservations,
            dots = list(times_df = split(os_eval_df, ~subject), object = survival),
            MoreArgs = NULL
        ) |> dplyr::bind_rows(),
        message = function(e) {
            if (!.silent) message(e)
            invokeRestart("muffleMessage")
        }
    )
    os_dat <- dplyr::left_join(os_dat, cov_cols, by = "subject")

    lm_dat2 <- lm_dat |>
        dplyr::left_join(dplyr::select(os_dat, "subject", os_time = "time"), by = "subject") |>
        dplyr::mutate(observed = (.data$time <= .data$os_time)) |>
        dplyr::arrange(dplyr::pick(c("subject", "time")))

    assert_that(
        length(unique(os_dat$subject)) == length(os_dat$subject),
        length(os_dat$subject) == n_subjects,
        all(os_dat$time >= 0),
        all(os_dat$event %in% c(0, 1)),
        msg = "Assumptions for the Survival data are not met (please report this issue)"
    )

    assert_that(
        nrow(lm_dat2) == n_subjects * length(longitudinal[[1]]@times),
        length(unique(lm_dat2$subject)) == n_subjects,
        msg = "Assumptions for the Longitudinal data are not met (please report this issue)"
    )

    # TODO rename study/arm/subject back to originals
    return(
        .SimJointData(
            survival = os_dat,
            longitudinal = lm_dat2[, c("subject", "arm", "study", "time", "sld", "observed")]
        )
    )
}
