#' @include Grid.R
#' @include GridFixed.R
#' @include generics.R
NULL

#' @rdname Grid-Dev
.GridGrouped <- setClass(
    "GridGrouped",
    contains = "Grid",
    slots = c(
        "groups" = "list",
        "times" = "numeric_or_NULL"
    )
)

#' @rdname Grid-Functions
#' @export
GridGrouped <- function(groups, times = NULL) {
    .GridGrouped(
        groups = groups,
        times = times
    )
}


setValidity(
    "GridGrouped",
    function(object) {
        if (!all(vapply(object@groups, is.character, logical(1)))) {
            return("Each element of `groups` must be a character vector")
        }
        gnames <- names(object@groups)
        gnames <- gnames[!is.na(gnames) & gnames != ""]
        if (length(gnames) != length(object@groups)) {
            return("Each element of `groups` must be named")
        }
        return(TRUE)
    }
)


#' @rdname Quant-Dev
#' @export
as.QuantityGenerator.GridGrouped <- function(object, data, ...) {
    assert_class(data, "DataJoint")
    data_list <- as.list(data)
    subjects_unique <- unique(unlist(object@groups))
    assert_that(
        all(subjects_unique %in% names(data_list$subject_to_index))
    )
    as.QuantityGenerator(
        GridFixed(
            times = object@times,
            subjects = subjects_unique
        ),
        data = data
    )
}


#' @rdname Quant-Dev
#' @export
as.QuantityCollapser.GridGrouped <- function(object, data, ...) {
    assert_class(data, "DataJoint")
    data_list <- as.list(data)
    assert_that(
        all(unique(unlist(object@groups)) %in% names(data_list$subject_to_index))
    )

    validate_time_grid(object@times)

    group_grid <- expand.grid(
        group = names(object@groups),
        time = object@times,
        stringsAsFactors = FALSE
    )

    generator <- as.QuantityGenerator(object, data)

    select_indexes <- mapply(
        function(group, time) {
            correct_subject <- generator@subjects %in% object@groups[[group]]
            correct_time <- generator@times == time
            seq_along(correct_time)[correct_subject & correct_time]
        },
        group_grid$group,
        group_grid$time,
        SIMPLIFY = FALSE
    )
    names(select_indexes) <- NULL

    QuantityCollapser(
        times = group_grid$time,
        groups = group_grid$group,
        indexes = select_indexes
    )
}

#' @export
as.list.GridGrouped <- function(x, ...) {
    x@groups
}

#' @rdname coalesceGridTime
#' @export
coalesceGridTime.GridGrouped <- function(object, times, ...) {
    if (is.null(object@times)) {
        object <- GridGrouped(
            groups = object@groups,
            times = times
        )
    }
    object
}
