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
    ret <- append(
        ret,
        gq_population_stan_data(
            object,
            model = longitudinal_model,
            data = data
        )$data
    )
    return(ret)
}


#' @rdname gq_population_stan_data
#' @export
#' @rawNamespace S3method(gq_population_stan_data,QuantityGeneratorPopulation)
gq_population_stan_data.QuantityGeneratorPopulation <- function(
    object,
    model,
    data = NULL,
    ...
) {
    # Note: We need to specify `model` here as second argument in order
    # to dispatch the appropriate method based on the class of `model`.
    UseMethod("gq_population_stan_data", model)
}
