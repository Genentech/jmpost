
#' @include Grid.R
#' @include generics.R
NULL


#' @rdname Grid-Dev
.GridManual <- setClass(
    "GridManual",
    contains = "Grid",
    slots = c(
        "spec" = "list"
    )
)


#' @rdname Grid-Functions
#' @export
GridManual <- function(spec) {
    .GridManual(
        spec = spec
    )
}


setValidity(
    "GridManual",
    function(object) {
        subject_names <- names(object@spec)
        subject_names_valid <- subject_names[!is.na(subject_names) & subject_names != ""]
        if (length(subject_names_valid) != length(object@spec)) {
            return("Each element of `subjects` must be named")
        }
        for (times in object@spec) {
            if (!is.numeric(times)) {
                return("Each element of `spec` must be a numeric vector")
            }
            if (length(times) != length(unique(times))) {
                return("Each time vector per subject must be unique")
            }
        }
        return(TRUE)
    }
)


#' @rdname Quant-Dev
#' @export
as.QuantityGenerator.GridManual <- function(object, data, ...) {
    assert_class(data, "DataJoint")
    data_list <- as.list(data)
    assert_that(
        all(names(object@spec) %in% names(data_list$subject_to_index)),
        msg = "All subject names must be in the `DataSubject` object"
    )
    lens <- vapply(object@spec, length, numeric(1))
    .QuantityGenerator(
        times = unlist(object@spec, use.names = FALSE),
        subjects = rep(names(object@spec), lens)
    )
}


#' @rdname Quant-Dev
#' @export
as.QuantityCollapser.GridManual <- function(object, data, ...) {
    generator <- as.QuantityGenerator(object, data)
    QuantityCollapser(
        times = generator@times,
        groups = generator@subjects,
        indexes = as.list(seq_along(generator@times))
    )
}


#' @export
as.list.GridManual <- function(x, data, ...) {
    subs <- as.list(names(x@spec))
    names(subs) <- names(x@spec)
    subs
}
