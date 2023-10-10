#' @include DataSurvival.R
#' @include DataLongitudinal.R
#' @include DataSubject.R
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

setClassUnion("DataLongitudinal_or_NULL", c("DataLongitudinal", "NULL"))
setClassUnion("DataSurvival_or_NULL", c("DataSurvival", "NULL"))


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
#' @family DataObjects
#' @family DataJoint
#' @export DataJoint
#' @exportClass DataJoint
.DataJoint <- setClass(
    Class = "DataJoint",
    representation = list(
        subject = "DataSubject",
        survival = "DataSurvival_or_NULL",
        longitudinal = "DataLongitudinal_or_NULL"
    )
)

#' @param survival (`DataSurvival`)\cr object created by [DataSurvival()].
#' @param longitudinal (`DataLongitudinal`)\cr object created by [DataLongitudinal()].
#' @rdname DataJoint-class
DataJoint <- function(subject, survival = NULL, longitudinal = NULL) {

    subject_suited <- suit_up(subject)
    vars <- extractVariableNames(subject)
    subject_var <- vars$subject
    subject_ord <- as.character(as.data.frame(subject_suited)[[vars$subject]])

    survival_suited <- suit_up(
        survival,
        subject_var = subject_var,
        subject_ord = subject_ord
    )

    longitudinal_suited <- suit_up(
        longitudinal,
        subject_var = subject_var,
        subject_ord = subject_ord
    )

    .DataJoint(
        subject = subject_suited,
        survival = survival_suited,
        longitudinal = longitudinal_suited
    )
}



setValidity(
    Class = "DataJoint",
    method = function(object) {
        # TODO
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
#' @family as_stan_list
#' @export
as_stan_list.DataJoint <- function(object, ...) {

    vars <- extractVariableNames(object@subject)
    subject_var <- vars$subject
    as_stan_list(object@subject) |>
        append(as_stan_list(object@survival)) |>
        append(as_stan_list(
            object@longitudinal,
            subject_var = subject_var
        ))
}

#' @rdname as_stan_list.DataJoint
as.list.DataJoint <- function(x, ...) {
    as_stan_list(x, ...)
}



#' Subsetting `DataJoint` as a `data.frame`
#'
#' @param x (`DataJoint`) \cr object created by [DataJoint()].
#' @param patients (`character` or `list`)\cr patients that you wish to subset the `data.frame`
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
#' @param dat (`data.frame`) \cr must have a column called `patient` which corresponds to the
#' values passed to `groupings`.
#' @param groupings (`character` or `list`)\cr patients that you wish to subset the dataset
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
#' @param object ([`DataJoint`])\cr data used to fit a [`JointModel`].
#' @return A data.frame with the following columns
#' - `subject` (`character`)\cr The subject identifier
#' - `time` (`numeric`)\cr The time at which the observation occurred
#' - `Yob` (`numeric`)\cr The observed value
#' @keywords internal
extract_observed_values <- function(object) {
    assert_class(object, "DataJoint")
    data <- as.list(object)
    x <- data.frame(
        subject = names(data$pt_to_ind)[data$ind_index],
        time = data$Tobs,
        Yob = data$Yobs
    )
    row.names(x) <- NULL
    x
}
