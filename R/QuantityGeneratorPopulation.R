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
        "arms" = "character_or_NULL"
    )
)


#' @rdname Quant-Dev
QuantityGeneratorPopulation <- function(times, studies = NULL, arms = NULL) {
    .QuantityGeneratorPopulation(
        times = times,
        studies = studies,
        arms = arms
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
        return(TRUE)
    }
)


#' @rdname as_stan_list.QuantityGenerator
#' @export
as_stan_list.QuantityGeneratorPopulation <- function(object, data, ...) {
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
        length(ret[["gq_long_pop_arm_index"]]) == length(ret[["gq_long_pop_study_index"]]),
        length(ret[["gq_long_pop_study_index"]]) == length(ret[["gq_times"]]),
        length(ret[["gq_long_pop_study_index"]]) == ret[["gq_n_quant"]],
        all(!is.na(ret[["gq_long_pop_arm_index"]])),
        all(!is.na(ret[["gq_long_pop_study_index"]]))
    )
    return(ret)
}
