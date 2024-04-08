
#' @include Grid.R
#' @include generics.R
NULL


#' @rdname Grid-Dev
.GridEven <- setClass(
    "GridEven",
    contains = "Grid",
    slots = c(
        "subjects" = "character",
        "length.out" = "numeric"
    )
)


#' @rdname Grid-Functions
#' @export
GridEven <- function(subjects = NULL, length.out = 30) {
    .GridEven(
        subjects = subjects,
        length.out = length.out
    )
}


setValidity(
    "GridEven",
    function(object) {
        if (length(object@length.out) != 1 || all(object@length.out <= 0)) {
            return("The `length.out` argument must be a positive integer")
        }
    }
)


#' @rdname Quant-Dev
#' @export
as.QuantityGenerator.GridEven <- function(object, data, ...) {
    assert_class(data, "DataJoint")
    data_list <- as.list(data)
    subjects <- unlist(as.list(object, data = data), use.names = FALSE)
    assert_that(
        all(subjects %in% names(data_list$subject_to_index)),
        msg = "All subject names must be in the `DataSubject` object"
    )

    spec <- lapply(
        subjects,
        \(sub) {
            subject_index <- data_list$subject_to_index[[sub]]
            time_index <- data_list$subject_tumour_index == subject_index
            subject_times <- data_list$tumour_time[time_index]
            seq(min(subject_times), max(subject_times), length.out = object@length.out)
        }
    )
    names(spec) <- subjects

    as.QuantityGenerator(
        GridManual(spec),
        data = data
    )
}


#' @rdname Quant-Dev
#' @export
as.QuantityCollapser.GridEven <- function(object, data, ...) {
    generator <- as.QuantityGenerator(object, data)
    QuantityCollapser(
        times = generator@times,
        groups = generator@subjects,
        indexes = as.list(seq_along(generator@times))
    )
}


#' @export
as.list.GridEven <- function(x, data, ...) {
    subjects_to_list(x@subjects, data)
}
