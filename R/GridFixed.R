#' @include Grid.R
#' @include generics.R
NULL


#' @rdname Grid-Dev
.GridFixed <- setClass(
    "GridFixed",
    contains = "Grid",
    slots = c(
        "subjects" = "character_or_NULL",
        "times" = "numeric_or_NULL"
    )
)

#' @rdname Grid-Functions
#' @export
GridFixed <- function(subjects = NULL, times = NULL) {
    .GridFixed(
        subjects = subjects,
        times = times
    )
}

#' @rdname Quant-Dev
#' @export
as.QuantityGenerator.GridFixed <- function(object, data, ...) {
    assert_class(data, "DataJoint")
    data_list <- as.list(data)
    subjects <- unlist(as.list(object, data = data), use.names = FALSE)
    time_grid <- expand_time_grid(object@times, max(data_list[["tumour_time"]]))

    pt_times <- expand.grid(
        pt = subjects,
        time = time_grid,
        stringsAsFactors = FALSE
    )

    QuantityGenerator(
        times = pt_times$time,
        subjects = pt_times$pt
    )
}

#' @rdname Quant-Dev
#' @export
as.QuantityCollapser.GridFixed <- function(object, data, ...) {
    generator <- as.QuantityGenerator(object, data)
    QuantityCollapser(
        times = generator@times,
        groups = generator@subjects,
        indexes = as.list(seq_along(generator@times))
    )
}


#' @export
as.list.GridFixed <- function(x, data, ...) {
    expand_null_subs_to_all_subs(x, data, ...)
}
