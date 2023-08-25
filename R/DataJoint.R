#' @include DataSurvival.R
#' @include DataLongitudinal.R
NULL

# DataJoint-class ----

#' `DataJoint`
#'
#' The `DataJoint` class handles combining data from a [`DataSurvival`] object and a
#' [`DataLongitudinal`] object.
#'
#' @slot survival (`DataSurvival`)\cr object created by [DataSurvival()].
#' @slot longitudinal (`DataLongitudinal`)\cr object created by [DataLongitudinal()].
#'
#' @exportClass DataJoint
.DataJoint <- setClass(
    Class = "DataJoint",
    representation = list(
        survival = "DataSurvival",
        longitudinal = "DataLongitudinal"
    )
)

# DataJoint-constructors ----

#' @rdname DataJoint-class
#'
#' @param survival (`DataSurvival`)\cr object created by [DataSurvival()].
#' @param longitudinal (`DataLongitudinal`)\cr object created by [DataLongitudinal()].
#'
#' @details
#'
#' - `as.list(x)`, `as(x, "list")`: Coerces x into a list of data components required
#' for fitting a [JointModel()]. See the vignette (TODO) for more details.
#'
#' @export
DataJoint <- function(survival, longitudinal) {
    .DataJoint(
        survival = survival,
        longitudinal = longitudinal
    )
}

# DataJoint-validity ----

setValidity(
    Class = "DataJoint",
    method = function(object) {
        lm <- as.data.frame(object@longitudinal)
        lvars <- extractVariableNames(object@longitudinal)
        os <- as.data.frame(object@survival)
        ovars <- extractVariableNames(object@survival)

        if (!all(as.character(lm[[lvars$pt]]) %in% as.character(os[[ovars$pt]]))) {
            return("There are subjects in the longitudinal data that do not exist in the survival data")
        }
        if (!all(as.character(os[[ovars$pt]]) %in% as.character(lm[[lvars$pt]]))) {
            return("There are subjects in the survival data that do not exist in the longitudinal data")
        }
    }
)

# DataJoint-as.list ----

#' @rdname as.list
setMethod(
    "as.list",
    signature = "DataJoint",
    definition = function(x) {
        append(
            as.list(x@survival),
            as.list(x@longitudinal)
        )
    }
)

# coerce-DataJoint,list ----

#' @rdname as.list
#'
#' @name coerce-DataJoint-list-method
#' @aliases coerce,DataJoint,list-method
setAs(
    from = "DataJoint",
    to = "list",
    def = function(from) as.list(from)
)


# TODO - Document function
# TODO - test function
setMethod(
    f = "subset",
    signature = "DataJoint",
    definition = function(x, patients) {
        data <- as.list(x)
        dat <- data.frame(
            time = data[["Times"]],
            event = as.numeric(seq_along(data[["Times"]]) %in% data[["dead_ind_index"]]),
            patient = names(data[["pt_to_ind"]])
        )
        subset_and_add_grouping(dat, patients)
    }
)

# TODO - Document function
# TODO - test function
subset_and_add_grouping <- function(dat, groupings) {
    groupings <- decompose_patients(groupings, names(data$pt_to_ind))$patients_list
    dat_subset_list <- lapply(
        seq_along(groupings),
        \(i) {
            dat_reduced <- dat[dat$patient %in% groupings[[i]], , drop = FALSE]
            dat_reduced[["group"]] <- names(groupings)[[i]]
            dat_reduced
        }
    )
    Reduce(rbind, dat_subset_list)
}
