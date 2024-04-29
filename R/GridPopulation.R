#' @include Grid.R
#' @include generics.R
NULL


#' @rdname Grid-Dev
.GridPopulation <- setClass(
    "GridPopulation",
    contains = "Grid",
    slots = c(
        "times" = "numeric_or_NULL"
    )
)

#' @rdname Grid-Functions
#' @export
GridPopulation <- function(times = NULL) {
    .GridPopulation(
        times = times
    )
}

#' @rdname Quant-Dev
#' @export
as.QuantityGenerator.GridPopulation <- function(object, data, ...) {

    assert_class(data, "DataJoint")
    data_list <- as.list(data)
    validate_time_grid(object@times)

    n_times <- length(object@times)
    n_quant <- length(data_list$pop_study_index)

    QuantityGenerator(
        times = rep(object@times, each = n_quant),
        arms = rep(data_list$pop_arm_index, n_times),
        studies = rep(data_list$pop_study_index, n_times)
    )
}

#' @rdname Quant-Dev
#' @export
as.QuantityCollapser.GridPopulation <- function(object, data, ...) {
    assert_class(data, "DataJoint")
    data_list <- as.list(data)
    generator <- as.QuantityGenerator(object, data)
    QuantityCollapser(
        times = generator@times,
        groups = sprintf(
            "arm=%s; study=%s",
            names(data_list$arm_to_index)[generator@arms],
            names(data_list$study_to_index)[generator@studies]
        ),
        indexes = as.list(seq_along(generator@times))
    )
}


#' @export
as.list.GridPopulation <- function(x, data, ...) {
    stop("`as.list()` is not implemented for `GridPopulation` objects")
}


#' @rdname coalesceGridTime
#' @export
coalesceGridTime.GridPopulation <- function(object, times, ...) {
    if (is.null(object@times)) {
        object <- GridPopulation(
            times = times
        )
    }
    object
}
