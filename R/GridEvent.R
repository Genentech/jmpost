#' @include Grid.R
#' @include generics.R
NULL

#' @rdname Grid-Dev
.GridEvent <- setClass(
    "GridEvent",
    contains = "Grid",
    slots = c(
        "subjects" = "character_or_NULL"
    )
)


#' @rdname Grid-Functions
#' @export
GridEvent <- function(subjects = NULL) {
    .GridEvent(
        subjects = subjects
    )
}

#' @rdname Quant-Dev
#' @export
as.QuantityGenerator.GridEvent <- function(object, data, ...) {
    assert_class(data, "DataJoint")
    assert_that(
        !is.null(data@survival),
        msg = "Survival data must have been provided to `DataJoint()` in order to use `GridEvent()`"
    )
    data_list <- as.list(data)
    subjects <- unlist(as.list(object, data = data), use.names = FALSE)
    event_times <- data_list$event_times[data_list$subject_to_index[subjects]]
    QuantityGeneratorSubject(
        times = event_times,
        subjects = subjects
    )
}

#' @rdname Quant-Dev
#' @export
as.QuantityCollapser.GridEvent <- function(object, data, ...) {
    generator <- as.QuantityGenerator(object, data)
    QuantityCollapser(
        times = generator@times,
        groups = generator@subjects,
        indexes = as.list(seq_along(generator@times))
    )
}

#' @export
as.list.GridEvent <- function(x, data, ...) {
    subjects_to_list(x@subjects, data)
}
