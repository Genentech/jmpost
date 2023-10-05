#' @include DataSurvival.R
#' @include DataLongitudinal.R
NULL


#' Re-used documentation for `DataJoint`
#'
#' @param object ([`DataJoint`]) \cr Survival and Longitudinal Data.
#' @param x ([`DataJoint`]) \cr Survival and Longitudinal Data.
#' @param ... Not Used.
#'
#' @name DataJoint-Shared
#' @keywords internal
NULL



# DataJoint-class ----

#' @title
#' Joint Data Object and Constructor Function
#'
#' @description
#' The `DataJoint` class handles combining data from a [`DataSurvival`] object and a
#' [`DataLongitudinal`] object.
#'
#' @slot survival (`DataSurvival`)\cr See Argument for details.
#' @slot longitudinal (`DataLongitudinal`)\cr See Argument for details.
#'
#'
#' @param survival (`DataSurvival`)\cr object created by [DataSurvival()].
#' @param longitudinal (`DataLongitudinal`)\cr object created by [DataLongitudinal()].
#'
#' @family DataObjects
#' @family DataJoint
#' @export DataJoint
#' @exportClass DataJoint
.DataJoint <- setClass(
    Class = "DataJoint",
    representation = list(
        survival = "DataSurvival",
        longitudinal = "DataLongitudinal"
    )
)

#' @rdname DataJoint-class
DataJoint <- function(survival, longitudinal) {
    .DataJoint(
        survival = survival,
        longitudinal = longitudinal
    )
}

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

#' `DataJoint` -> `list`
#'
#' @inheritParams DataJoint-Shared
#' @description
#' Coerces  [`DataJoint`] into a `list` of data components required
#' for fitting a [`JointModel`]. See the vignette (TODO) for more details.
#' @family DataJoint
#' @seealso [as.list.DataSurvival()], [as.list.DataLongitudinal()]
#' @export
as.list.DataJoint <- function(x, ...) {
    append(
        as.list(x@survival),
        as.list(x@longitudinal)
    )
}



#' Subsetting `DataJoint` as a `data.frame`
#'
#' @param x (`DataJoint`) \cr A [DataJoint][DataJoint-class] object created by [DataJoint()]
#' @param patients (`character` or `list`)\cr the patients that you wish to subset the `data.frame`
#' to contain. See details.
#' @param ... Not used.
#'
#' @description
#'
#' Coerces the object into a `data.frame` containing just event times and status
#' filtering for specific patients. If `patients` is a list then an additional variable `group` will be added
#' onto the dataset specifying which group the row belongs to.
#'
#' @examples
#' \dontrun{
#' pts <- c("PT1", "PT3", "PT4")
#' subset(x, pts)
#'
#' groups <- list(
#'     "g1" = c("PT1", "PT3", "PT4"),
#'     "g2" = c("PT2", "PT3")
#' )
#' subset(x, groups)
#' }
#' @family DataJoint
#' @export
subset.DataJoint <- function(x, patients, ...) {
    data <- as.list(x)
    dat <- data.frame(
        time = data[["Times"]],
        event = as.numeric(seq_along(data[["Times"]]) %in% data[["dead_ind_index"]]),
        patient = names(data[["pt_to_ind"]])
    )
    subset_and_add_grouping(dat, patients)
}



#' `subset_and_add_grouping`
#'
#' @param dat (`data.frame`) \cr Must have a column called `patient` which corresponds to the
#' values passed to `groupings`
#' @param groupings (`character` or `list`)\cr the patients that you wish to subset the dataset
#' to contain. If `groupings` is a list then an additional variable `group` will be added
#' onto the dataset specifying which group the row belongs to.
#'
#' @details
#' Example of usage
#' ```
#' pts <- c("PT1", "PT3", "PT4")
#' subset_and_add_grouping(dat, pts)
#'
#' groups <- list(
#'     "g1" = c("PT1", "PT3", "PT4"),
#'     "g2" = c("PT2", "PT3")
#' )
#' subset_and_add_grouping(dat, groups)
#' ```
#'
#' @keywords internal
subset_and_add_grouping <- function(dat, groupings) {
    groupings <- decompose_patients(groupings, dat$patient)$groups
    dat_subset_list <- lapply(
        seq_along(groupings),
        \(i) {
            dat_reduced <- dat[dat$patient %in% groupings[[i]], , drop = FALSE]
            dat_reduced[["group"]] <- names(groupings)[[i]]
            dat_reduced
        }
    )
    x <- Reduce(rbind, dat_subset_list)
    row.names(x) <- NULL
    x
}


#' Extract Observed Longitudinal Values
#'
#' Utility function to extract the observed longitudinal values from a [`DataJoint`] object
#' @param object ([`DataJoint`])\cr The data used to fit a [`JointModel`]
#' @return A data.frame with the following columns
#' - `subject` (`character`)\cr The subject identifier
#' - `time` (`numeric`)\cr The time at which the observation occured
#' - `Yob` (`numeric`)\cr The observed value
#' @keywords internal
extract_observed_values <- function(object) {
    assert_that(inherits(object, "DataJoint"))
    data <- as.list(object)
    x <- data.frame(
        subject = names(data$pt_to_ind)[data$ind_index],
        time = data$Tobs,
        Yob = data$Yobs
    )
    row.names(x) <- NULL
    x
}
