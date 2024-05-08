#' @include generics.R
#' @include Grid.R
NULL


#' @rdname Quant-Dev
.QuantityGeneratorSubject <- setClass(
    "QuantityGeneratorSubject",
    contains = "QuantityGenerator",
    slots = c(
        "times" = "numeric",
        "subjects" = "character_or_NULL"
    )
)


#' @rdname Quant-Dev
QuantityGeneratorSubject <- function(times, subjects = NULL) {
    .QuantityGeneratorSubject(
        times = times,
        subjects = subjects
    )
}


setValidity(
    "QuantityGeneratorSubject",
    function(object) {
        if (length(object@times) != length(object@subjects)) {
            return("Length of `times` and `subjects` must be equal")
        }
        return(TRUE)
    }
)


#' @rdname as_stan_list.QuantityGenerator
#' @export
as_stan_list.QuantityGeneratorSubject <- function(object, data, ...) {
    assert_that(
        is(data, "DataJoint")
    )
    ret <- list()
    data_list <- as.list(data)
    ret[["gq_pt_index"]] <- data_list$subject_to_index[as.character(object@subjects)]
    ret[["gq_n_quant"]] <- length(object@subjects)
    ret[["gq_times"]] <- object@times

    # dummy pop indexes in order for stan code to actualy compile. In this setting
    # this matrix isn't actually used so doesn't matter what these values are
    # but don't want to have to burden individual longitudinal models with the
    # conditional logic to check if they are generating population quantities or not
    ret[["gq_long_pop_arm_index"]] <- rep(1, length(object@subjects))
    ret[["gq_long_pop_study_index"]] <- rep(1, length(object@subjects))

    # Sanity checks
    assert_that(
        length(ret[["gq_times"]]) == ret[["gq_n_quant"]],
        all(object@subjects %in% names(data_list$subject_to_index))
    )
    return(ret)
}
