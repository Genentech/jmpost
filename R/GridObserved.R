#' @include Grid.R
#' @include generics.R
NULL

#' @rdname Grid-Dev
.GridObserved <- setClass(
    "GridObserved",
    contains = "Grid",
    slots = c(
        "subjects" = "character_or_NULL"
    )
)


#' @rdname Grid-Functions
#' @export
GridObserved <- function(subjects = NULL) {
    .GridObserved(
        subjects = subjects
    )
}

#' @rdname Quant-Dev
#' @export
as.QuantityGenerator.GridObserved <- function(object, data, ...) {
    assert_class(data, "DataJoint")
    data_list <- as.list(data)
    subjects <- unlist(as.list(object, data = data), use.names = FALSE)
    unique_visits <- tapply(data_list$Tobs, data_list$ind_index, unique)
    patient_visits <- unique_visits[data_list$pt_to_ind[subjects]]
    visit_lengths <- vapply(patient_visits, length, numeric(1))
    .QuantityGenerator(
        times = unlist(patient_visits, use.names = FALSE),
        subjects = rep(subjects, visit_lengths)
    )
}

#' @rdname Quant-Dev
#' @export
as.QuantityCollapser.GridObserved <- function(object, data, ...) {
    generator <- as.QuantityGenerator(object, data)

    QuantityCollapser(
        times = generator@times,
        groups = generator@subjects,
        indexes = as.list(seq_along(generator@times))
    )
}

#' @export
as.list.GridObserved <- function(x, data, ...) {
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
