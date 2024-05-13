#' @include Grid.R
#' @include generics.R
NULL


#' @rdname Grid-Dev
.GridPrediction <- setClass(
    "GridPrediction",
    contains = "Grid",
    slots = c(
        "times" = "numeric_or_NULL",
        "newdata" = "data.frame",
        "params" = "list"
    )
)
#' @rdname Grid-Functions
#' @export
GridPrediction <- function(times = NULL, newdata, params = NULL) {
    .GridPrediction(
        times = times,
        params = params,
        newdata = newdata
    )
}

#' @rdname Quant-Dev
#' @export
as.QuantityGenerator.GridPrediction <- function(object, data, ...) {

    assert_class(data, "DataJoint")
    data_list <- as.list(data)
    validate_time_grid(object@times)

    n_times <- length(object@times)
    n_obs <- nrow(object@newdata)
    newdata <- object@newdata
    if (!is.null(newdata[["..new_subject.."]])) {
        stop("newdata must not contain a column named '..new_subject..'")
    }
    newdata[["..new_subject.."]] <- sprintf(
        "new_subject_%i",
        seq_len(nrow(newdata))
    )

    QuantityGeneratorPrediction(
        times = rep(object@times, each = n_obs),
        newdata = replicate(newdata, n = n_times, simplify = FALSE) |> dplyr::bind_rows(),
        params = object@params
    )
}

#' @rdname Quant-Dev
#' @export
as.QuantityCollapser.GridPrediction <- function(object, data, ...) {
    assert_class(data, "DataJoint")
    data_list <- as.list(data)
    generator <- as.QuantityGenerator(object, data)
    QuantityCollapser(
        times = generator@times,
        groups = generator@newdata[["..new_subject.."]],
        indexes = as.list(seq_along(generator@times))
    )
}


#' @export
as.list.GridPrediction <- function(x, data, ...) {
    stop("`as.list()` is not implemented for `GridPrediction` objects")
}


#' @rdname coalesceGridTime
#' @export
coalesceGridTime.GridPrediction <- function(object, times, ...) {
    if (is.null(object@times)) {
        object <- GridPrediction(
            times = times,
            newdata = object@newdata,
            params = object@params
        )
    }
    object
}
