#' @include Grid.R
#' @include generics.R
NULL


#' @rdname Grid-Dev
.GridPopulation <- setClass(
    "GridPopulation",
    contains = "Grid",
    slots = c(
        "times" = "numeric_or_NULL",
        "newdata" = "data.frame_or_NULL"
    )
)

#' @rdname Grid-Functions
#' @export
GridPopulation <- function(times = NULL, newdata = NULL) {
    .GridPopulation(
        times = times,
        newdata = newdata
    )
}

setValidity(
    "GridPopulation",
    function(object) {
        if (!is.null(object@newdata) && anyDuplicated(names(object@newdata))) {
            return("Column names in `newdata` must be unique")
        }
        TRUE
    }
)

#' @rdname Quant-Dev
#' @export
as.QuantityGenerator.GridPopulation <- function(
    object,
    data,
    model = NULL,
    ...
) {
    assert_class(data, "DataJoint")
    data_list <- as.list(data)
    validate_time_grid(object@times)

    n_times <- length(object@times)
    subject_vars <- extractVariableNames(data@subject)
    subject_data <- as.data.frame(harmonise(data@subject))
    longitudinal_model <- if (is(model, "JointModel")) {
        model@longitudinal
    } else {
        model
    }

    if (!is.null(object@newdata)) {
        profiles <- object@newdata
    } else {
        required_covariates <- if (is(longitudinal_model, "LongitudinalRandomSlopeCov")) {
            unique(c(
                all.vars(longitudinal_model@mu_formula),
                all.vars(longitudinal_model@slope_mu_formula)
            ))
        } else if (is(longitudinal_model, "LongitudinalSteinFojoCov")) {
            .stein_fojo_cov_population_variables(longitudinal_model)
        } else {
            character()
        }
        additional_covariates <- setdiff(
            required_covariates,
            c(subject_vars$arm, subject_vars$study)
        )
        assert_that(
            length(additional_covariates) == 0,
            msg = paste0(
                "`GridPopulation(newdata = ...)` must define the additional ",
                "longitudinal covariates: ",
                paste(additional_covariates, collapse = ", ")
            )
        )

        population_indexes <- match(
            paste(data_list$pop_arm_index, data_list$pop_study_index),
            paste(data_list$subject_arm_index, data_list$subject_study_index)
        )
        profiles <- subject_data[population_indexes, , drop = FALSE]
    }

    required_columns <- c(subject_vars$arm, subject_vars$study)
    if (is(longitudinal_model, "LongitudinalRandomSlopeCov")) {
        required_columns <- unique(c(
            required_columns,
            all.vars(longitudinal_model@mu_formula),
            all.vars(longitudinal_model@slope_mu_formula)
        ))
    } else if (is(longitudinal_model, "LongitudinalSteinFojoCov")) {
        required_columns <- unique(c(
            required_columns,
            .stein_fojo_cov_population_variables(longitudinal_model)
        ))
    }
    missing_columns <- setdiff(required_columns, names(profiles))
    assert_that(
        length(missing_columns) == 0,
        msg = sprintf(
            "`GridPopulation(newdata = ...)` is missing: %s",
            paste(missing_columns, collapse = ", ")
        )
    )

    n_quant <- nrow(profiles)
    expanded_indexes <- rep(seq_len(n_quant), n_times)
    expanded_profiles <- profiles[expanded_indexes, , drop = FALSE]
    rownames(expanded_profiles) <- NULL

    QuantityGeneratorPopulation(
        times = rep(object@times, each = n_quant),
        arms = as.character(expanded_profiles[[subject_vars$arm]]),
        studies = as.character(expanded_profiles[[subject_vars$study]]),
        newdata = expanded_profiles
    )
}

#' @rdname Quant-Dev
#' @export
as.QuantityCollapser.GridPopulation <- function(
    object,
    data,
    model = NULL,
    ...
) {
    assert_class(data, "DataJoint")
    generator <- as.QuantityGenerator(object, data, model = model)
    subject_vars <- extractVariableNames(data@subject)
    profile_columns <- if (!is.null(object@newdata)) {
        names(object@newdata)
    } else {
        c(subject_vars$arm, subject_vars$study)
    }
    groups <- vapply(
        seq_len(nrow(generator@newdata)),
        function(index) {
            values <- vapply(
                generator@newdata[index, profile_columns, drop = FALSE],
                as.character,
                character(1)
            )
            paste(sprintf("%s=%s", names(values), values), collapse = "; ")
        },
        character(1)
    )
    QuantityCollapser(
        times = generator@times,
        groups = groups,
        indexes = as.list(seq_along(generator@times))
    )
}


#' @export
#'
#' @returns This is just a placeholder which will error when used.
as.list.GridPopulation <- function(x, data, ...) {
    stop("`as.list()` is not implemented for `GridPopulation` objects")
}


#' @rdname coalesceGridTime
#' @export
coalesceGridTime.GridPopulation <- function(object, times, ...) {
    if (is.null(object@times)) {
        object <- GridPopulation(
            times = times,
            newdata = object@newdata
        )
    }
    object
}
