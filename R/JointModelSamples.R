#' @include JointModel.R
NULL

# JointModelSamples-class ----

#' `JointModelSamples`
#'
#' Contains samples from a [`JointModel`].
#'
#' @slot model (`JointModel`)\cr the original model.
#' @slot data (`list`)\cr data input.
#' @slot init (`list`)\cr initial values.
#' @slot results (`CmdStanMCMC`)\cr the results from [sampleStanModel()].
#'
#' @aliases JointModelSamples
#' @export
.JointModelSamples <- setClass(
    "JointModelSamples",
    slots = c(
        model = "JointModel",
        data = "list",
        init = "list",
        results = "ANY"
    )
)


#' @rdname generateQuantities
#' @param patients (`character`)\cr explicit vector of patient IDs for whom the
#' generated quantities should be extracted
#' @param time_grid_lm (`numeric`)\cr grid of time points to use for providing samples
#'   of the longitudinal model fit functions. If `NULL`, will be taken as a sequence of
#'   201 values from 0 to the maximum observed event time.
#' @param time_grid_sm (`numeric`)\cr grid of time points to use for providing samples
#'   of the survival model fit functions. If `NULL`, will be taken as a sequence of
#'   201 values from 0 to the maximum observed event time.
#' @export
setMethod(
    f = "generateQuantities",
    signature = c(object = "JointModelSamples"),
    definition = function(object, patients, time_grid_lm, time_grid_sm, ...) {
        data <- object@data
        data[["n_lm_time_grid"]] <- length(time_grid_lm)
        data[["lm_time_grid"]] <- time_grid_lm
        data[["n_sm_time_grid"]] <- length(time_grid_sm)
        data[["sm_time_grid"]] <- time_grid_sm
        data[["n_pt_select_index"]] <- length(patients)
        data[["pt_select_index"]] <- data$pt_to_ind[patients]
        stanObject <- object@model@stan
        stanObject_data <- merge(
            stanObject,
            StanModule("base/generated_quantities_data.stan")
        )

        model <- compileStanModel(stanObject_data)

        devnull <- utils::capture.output(
            results <- model$generate_quantities(
                data = data,
                fitted_params = object@results$draws()
            )
        )
        return(results)
    }
)


# longitudinal-JointModelSamples ----

#' @rdname longitudinal
#'
#' @param patients (`character` or `NULL`)\cr optional subset of patients for
#' which the longitudinal fit samples should be extracted, the default `NULL`
#' meaning all patients.
#'
#' @param time_grid (`numeric`)\cr grid of time points to use for providing samples
#'   of the longitudinal model fit functions. If `NULL`, will be taken as a sequence of
#'   201 values from 0 to the maximum observed event time.
#'
#' @export
setMethod(
    f = "longitudinal",
    signature = c(object = "JointModelSamples"),
    definition = function(object, patients = NULL, time_grid = NULL, ...) {

        data <- object@data
        time_grid <- expand_time_grid(time_grid, max(data[["Tobs"]]))
        patients <- expand_patients(patients, names(object@data$pt_to_ind))
        gq <- generateQuantities(
            object,
            patients = patients,
            time_grid_lm = time_grid,
            time_grid_sm = numeric(0)
        )

        y_fit_at_grid_samples <- gq$draws(format = "draws_matrix")
        y_fit_samples <- object@results$draws("Ypred", format = "draws_matrix")

        results <- list()
        for (this_pt_ind in seq_along(patients)) {
            this_pt <- patients[this_pt_ind]
            this_result <- list()
            this_y_fit_names <- sprintf(
                "y_fit_at_time_grid[%i,%i]",
                this_pt_ind,
                seq_along(time_grid)
            )
            this_result$samples <- y_fit_at_grid_samples[, this_y_fit_names, drop = FALSE]
            this_result$summary <- data.frame(
                time = time_grid,
                samples_median_ci(this_result$samples)
            )
            for_this_pt <- which(data$ind_index == data$pt_to_ind[this_pt])
            this_fit <- samples_median_ci(y_fit_samples[, for_this_pt, drop = FALSE])
            this_result$observed <- data.frame(
                t =  data$Tobs[for_this_pt],
                y = data$Yobs[for_this_pt],
                this_fit
            )
            results[[this_pt]] <- this_result
        }
        .LongitudinalSamples(results)
    }
)


# survival-JointModelSamples ----

#' @rdname survival
#'
#' @param patients (`character` or `NULL`)\cr optional subset of patients for
#' which the survival function samples should be extracted, the default `NULL`
#' meaning all patients.
#'
#' @param time_grid (`numeric`)\cr grid of time points to use for providing samples
#' of the survival model fit functions. If `NULL`, will be taken as a sequence of
#' 201 values from 0 to the maximum observed event time.
#'
#' @export
setMethod(
    f = "survival",
    signature = c(object = "JointModelSamples"),
    definition = function(object, patients = NULL, time_grid = NULL, ...) {

        data <- object@data
        time_grid <- expand_time_grid(time_grid, max(data[["Times"]]))
        patients <- expand_patients(patients, names(object@data$pt_to_ind))
        gq <- generateQuantities(
            object,
            patients = patients,
            time_grid_lm = numeric(0),
            time_grid_sm = time_grid
        )

        log_surv_at_grid_samples <- gq$draws(format = "draws_matrix")
        log_surv_at_obs_samples <- object@results$draws(
            "log_surv_fit_at_obs_times",
            format = "draws_matrix"
        )

        results <- list()
        for (this_pt_ind in seq_along(patients)) {
            this_pt <- patients[this_pt_ind]
            this_result <- list()
            patient_ind <- object@data$pt_to_ind[this_pt]
            this_surv_fit_names <- sprintf(
                "log_surv_fit_at_time_grid[%i,%i]",
                this_pt_ind,
                seq_along(time_grid)
            )
            this_result$samples <- exp(log_surv_at_grid_samples[, this_surv_fit_names, drop = FALSE])
            this_result$summary <- data.frame(
                time = time_grid,
                samples_median_ci(this_result$samples)
            )
            this_result$observed <- data.frame(
                t = data$Times[patient_ind],
                death = (patient_ind %in% object@data$dead_ind_index),
                samples_median_ci(exp(log_surv_at_obs_samples[, patient_ind, drop = FALSE]))
            )
            rownames(this_result$observed) <- this_pt
            results[[this_pt]] <- this_result
        }
        .SurvivalSamples(results)
    }
)
