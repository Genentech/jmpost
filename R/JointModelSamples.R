#' @include JointModel.R
#' @include SurvivalQuantities.R
NULL

setOldClass("CmdStanMCMC")

# JointModelSamples-class ----

#' `JointModelSamples`
#'
#' Contains samples from a [`JointModel`].
#'
#' @slot model (`JointModel`)\cr the original model.
#' @slot data (`list`)\cr data input.
#' @slot results (`CmdStanMCMC`)\cr the results from [sampleStanModel()].
#'
#' @aliases JointModelSamples
#' @export
.JointModelSamples <- setClass(
    "JointModelSamples",
    slots = c(
        model = "JointModel",
        data = "DataJoint",
        results = "CmdStanMCMC"
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
        data <- as.list(object@data)
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

        data <- as.list(object@data)
        time_grid <- expand_time_grid(time_grid, max(data[["Tobs"]]))
        patients <- expand_patients(patients, names(data$pt_to_ind))
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





#TODO - Documentation
setMethod(
    f = "extractSurvivalQuantities",
    signature = "JointModelSamples",
    definition = function(
        object,
        patients = NULL,
        time_grid = NULL,
        type = c("surv", "haz", "loghaz", "cumhaz")
    ) {
        type <- match.arg(type)

        data <- as.list(object@data)
        patients <- decompose_patients(patients, names(data$pt_to_ind))

        time_grid <- expand_time_grid(time_grid, max(data[["Times"]]))

        gq <- generateQuantities(
            object,
            patients = patients$unique_values,
            time_grid_lm = numeric(0),
            time_grid_sm = time_grid
        )

        quantities_raw <- extract_survival_quantities(gq, type)

        quantities <- lapply(
            patients$indexes,
            average_samples_by_index,
            time_index = seq_along(time_grid),
            quantities = quantities_raw
        )
        SurvivalQuantities(
            quantities = quantities,
            groups = patients$groups,
            type = type,
            time_grid = time_grid,
            data = object@data
        )
    }
)


#' Summarise Quantities By Group
#'
#' This function takes a [posterior::draws_matrix()] (matrix of cmdstanr sample draws) and calculates
#' summary statistics (median / lower ci / upper ci) for selected columns.
#' A key feature is that it allows for columns to be aggregated together (see details).
#'
#' @param subject_index (`numeric`)\cr Which subject indices to extract from `quantities`.
#' See details.
#'
#' @param time_index (`numeric`)\cr Which time point indices to extract from `quantities`.
#' See details.
#'
#' @param quantities ([`posterior::draws_matrix`])\cr A matrix of sample draws.
#' See details.
#'
#' @details
#' It is assumed that `quantities` consists of the cartesian product
#' of subject indices and time indices. That is, if the matrix contains 4 subjects and 3 time
#' points then it should have 12 columns.
#' It is also assumed that each column of `quantities` are named as:
#' ```
#' "quantity[x,y]"
#' ```
#' Where
#' - `x` is the subject index
#' - `y` is the time point index
#'
#' The resulting `data.frame` that is created will have 1 row per value of `time_index` where
#' each row represents the summary statistics for that time point.
#'
#' Note that if multiple values are provided for `subject_index` then the pointwise average
#' will be calculated for each time point by taking the mean across the specified subjects
#' at that time point.
#'
#' @return
#' #TODO - update documentation
#'
#' @keywords internal
average_samples_by_index <- function(subject_index, time_index, quantities) {
    assert_that(
        is.numeric(subject_index),
        is.numeric(time_index),
        length(time_index) == length(unique(time_index)),
        inherits(quantities, "draws_matrix")
    )
    stacked_quantities <- array(dim = c(
        nrow(quantities),
        length(time_index),
        length(subject_index)
    ))
    for (ind in seq_along(subject_index)) {
        quantity_index <- sprintf(
            "quantity[%i,%i]",
            subject_index[ind],
            time_index
        )
        stacked_quantities[, , ind] <- quantities[, quantity_index]
    }
    apply(
        stacked_quantities,
        c(1, 2),
        mean,
        simplify = TRUE
    )
}



#' Extract Survival Quantities
#'
#' Utility function to extract generated quantities from a [cmdstanr::CmdStanGQ] object.
#' Multiple quantities are generated by default so this is a convenience function to extract
#' the desired ones and return them them as a user friendly [posterior::draws_matrix] object
#'
#' @param gq (`CmdStanGQ`) \cr A [cmdstanr::CmdStanGQ] object created by [generateQuantities]
#' @inheritParams SurvivalQuantities-Joint
#' @keywords internal
extract_survival_quantities <- function(gq, type = c("surv", "haz", "loghaz", "cumhaz")) {
    type <- match.arg(type)
    assert_that(
        inherits(gq, "CmdStanGQ")
    )
    meta <- switch(type,
        surv = list("log_surv_fit_at_time_grid", exp),
        cumhaz = list("log_surv_fit_at_time_grid", \(x) -x),
        haz = list("log_haz_fit_at_time_grid", exp),
        loghaz = list("log_haz_fit_at_time_grid", identity)
    )
    result <- gq$draws(meta[[1]], format = "draws_matrix")
    result_transformed <- meta[[2]](result)
    cnames <- colnames(result_transformed)
    colnames(result_transformed) <- gsub(meta[[1]], "quantity", cnames)
    result_transformed
}
