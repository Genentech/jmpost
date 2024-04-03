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
    patients_unique <- unique(unlist(object@groups))
    assert_that(
        all(patients_unique %in% names(data_list$pt_to_ind))
    )
    as.QuantityGenerator(
        GridFixed(
            times = object@times,
            subjects = patients_unique
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
        all(unique(unlist(object@groups)) %in% names(data_list$pt_to_ind))
    )
    time_grid <- expand_time_grid(object@times, max(data_list[["tumour_time"]]))

    group_grid <- expand.grid(
        group = names(object@groups),
        time = time_grid,
        stringsAsFactors = FALSE
    )

    generator <- as.QuantityGenerator(object, data)

    select_indexes <- mapply(
        function(group, time) {
            correct_pt <- generator@subjects %in% object@groups[[group]]
            correct_time <- generator@times == time
            seq_along(correct_time)[correct_pt & correct_time]
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
