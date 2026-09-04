#' @include generics.R
#' @include Grid.R
NULL

#' @rdname Quant-Dev
.QuantityGeneratorPopulation <- setClass(
    "QuantityGeneratorPopulation",
    contains = "QuantityGenerator",
    slots = c(
        "times" = "numeric",
        "studies" = "character_or_NULL",
        "arms" = "character_or_NULL",
        "newdata" = "data.frame_or_NULL"
    )
)


#' @rdname Quant-Dev
QuantityGeneratorPopulation <- function(
    times,
    studies = NULL,
    arms = NULL,
    newdata = NULL
) {
    .QuantityGeneratorPopulation(
        times = times,
        studies = studies,
        arms = arms,
        newdata = newdata
    )
}


setValidity(
    "QuantityGeneratorPopulation",
    function(object) {
        if (length(object@times) != length(object@arms)) {
            return("Length of `times` and `arms` must be equal")
        }
        if (length(object@times) != length(object@studies)) {
            return("Length of `times` and `studies` must be equal")
        }
        if (
            !is.null(object@newdata) &&
                length(object@times) != nrow(object@newdata)
        ) {
            return("Length of `times` and rows in `newdata` must be equal")
        }
        return(TRUE)
    }
)


#' @rdname as_stan_list.QuantityGenerator
#' @export
as_stan_list.QuantityGeneratorPopulation <- function(
    object,
    data,
    model = NULL,
    ...
) {
    assert_that(
        is(data, "DataJoint")
    )
    ret <- list()
    data_list <- as.list(data)
    ret[["gq_times"]] <- object@times
    ret[["gq_n_quant"]] <- length(object@arms)
    ret[["gq_long_pop_arm_index"]] <- data_list$arm_to_index[object@arms]
    ret[["gq_long_pop_study_index"]] <- data_list$study_to_index[object@studies]

    # Sanity checks
    assert_that(
        length(ret[["gq_long_pop_arm_index"]]) ==
            length(ret[["gq_long_pop_study_index"]]),
        length(ret[["gq_long_pop_study_index"]]) == length(ret[["gq_times"]]),
        length(ret[["gq_long_pop_study_index"]]) == ret[["gq_n_quant"]],
        all(!is.na(ret[["gq_long_pop_arm_index"]])),
        all(!is.na(ret[["gq_long_pop_study_index"]]))
    )

    longitudinal_model <- if (is(model, "JointModel")) {
        model@longitudinal
    } else {
        model
    }
    if (is(longitudinal_model, "LongitudinalRandomSlopeCov")) {
        assert_that(
            !is.null(object@newdata) &&
                nrow(object@newdata) == ret[["gq_n_quant"]],
            msg = paste0(
                "Population quantities for `LongitudinalRandomSlopeCov` ",
                "require `GridPopulation(newdata = ...)`"
            )
        )
        ret <- append(
            ret,
            .random_slope_cov_population_stan_data(
                longitudinal_model,
                data@subject,
                object@newdata
            )
        )
    } else if (is(longitudinal_model, "LongitudinalSteinFojoCov")) {
        assert_that(
            !is.null(object@newdata) &&
                nrow(object@newdata) == ret[["gq_n_quant"]],
            msg = paste0(
                "Population quantities for `LongitudinalSteinFojoCov` ",
                "require `GridPopulation(newdata = ...)`"
            )
        )
        ret <- append(
            ret,
            .stein_fojo_cov_population_stan_data(
                longitudinal_model,
                data@subject,
                object@newdata
            )
        )
    }
    return(ret)
}
