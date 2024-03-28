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
    time_grid <- expand_time_grid(object@times, max(data_list[["Tobs"]]))

    pt_times <- expand.grid(
        pt = subjects,
        time = time_grid,
        stringsAsFactors = FALSE
    )

    .QuantityGenerator(
        times = pt_times$time,
        subjects = pt_times$pt
    )
}

#' @rdname Quant-Dev
#' @export
as.QuantityCollapser.GridFixed <- function(object, data, ...) {
    generator <- as.QuantityGenerator(object, data)
    .QuantityCollapser(
        times = generator@times,
        groups = generator@subjects,
        indexes = as.list(seq_along(generator@times))
    )
}


#' @export
as.list.GridFixed <- function(x, data, ...) {
    data_list <- as.list(data)
    subjects <- if (is.null(x@subjects)) {
        subs <- as.list(names(data_list$pt_to_ind))
        names(subs) <- names(data_list$pt_to_ind)
        subs
    } else {
        subs <- as.list(x@subjects)
        names(subs) <- x@subjects
        subs
    }
    assert_that(
        identical(
            unlist(subjects, use.names = FALSE),
            unique(unlist(subjects, use.names = FALSE))
        )
    )
    subjects
}
