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
#' @slot subject (`DataSubject`)\cr See Argument for details.
#' @slot survival (`DataSurvival`)\cr See Argument for details.
#' @slot longitudinal (`DataLongitudinal`)\cr See Argument for details.
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

#' @param subject (`DataSubject`)\cr object created by [DataSubject()].
#' @param survival (`DataSurvival`)\cr object created by [DataSurvival()].
#' @param longitudinal (`DataLongitudinal`)\cr object created by [DataLongitudinal()].
#' @rdname DataJoint-class
DataJoint <- function(subject, survival = NULL, longitudinal = NULL) {

    subject_suited <- harmonise(subject)
    vars <- extractVariableNames(subject)
    subject_var <- vars$subject
    subject_ord <- levels(as.data.frame(subject_suited)[[vars$subject]])

    survival_suited <- harmonise(
        survival,
        subject_var = subject_var,
        subject_ord = subject_ord
    )

    longitudinal_suited <- harmonise(
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
        vars <- extractVariableNames(object@subject)
        subject_var <- vars$subject
        subject_ord <- as.character(as.data.frame(object@subject)[[vars$subject]])
        if (!is.null(object@survival)) {
            survival_df <- as.data.frame(object@survival)
            if (!subject_var %in% names(survival_df)) {
                return(sprintf("Unable to find `%s` in `survival`", sujbect_var))
            }
            if (!all(survival_df[[subject_var]] %in% subject_ord)) {
                return("There are subjects in `survival` that are not in `subject`")
            }
            if (!nrow(survival_df) == length(unique(survival_df[[subject_var]]))) {
                return("There are duplicate subjects in `survival`")
            }
        }
        if (!is.null(object@longitudinal)) {
            long_df <- as.data.frame(object@longitudinal)
            if (!subject_var %in% names(long_df)) {
                return(sprintf("Unable to find `%s` in `longitudinal`", sujbect_var))
            }
            if (!all(long_df[[subject_var]] %in% subject_ord)) {
                return("There are subjects in `longitudinal` that are not in `subject`")
            }
        }
        subject_df <- as.data.frame(object@subject)
        if (!subject_var %in% names(subject_df)) {
            return(sprintf("Unable to find `%s` in `subject`", sujbect_var))
        }
        if (!nrow(subject_df) == length(unique(subject_df[[subject_var]]))) {
            return("There are duplicate subjects in `subject`")
        }
        return(TRUE)
    }
)


# DataJoint-as.list ----




#' Data Object -> `list`
#'
#' @param object (`DataSubject` or `DataLongitudinal` or `DataSurvival`) \cr
#' data object to convert to a `list`.
#' @param x (`DataSubject` or `DataLongitudinal` or `DataSurvival`) \cr
#' data object to convert to a `list`.
#' @param subject_var (`character`) \cr the name of the variable
#' containing the subject identifier.
#' @param ... not used.
#'
#' @description
#' Coerces a data object into a `list` of data components required
#' for fitting a [`JointModel`]. See the "Extending jmpost" vignette for more details.
#'
#' @name as_stan_list.DataObject
#' @family as_stan_list
#' @family DataJoint
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

#' @rdname as_stan_list.DataObject
#' @export
as.list.DataJoint <- function(x, ...) {
    as_stan_list(x, ...)
}



#' Subsetting `DataJoint` as a `data.frame`
#'
#' @param x (`DataJoint`) \cr object created by [DataJoint()].
#' @param subjects (`character` or `list`)\cr subjects that you wish to subset the `data.frame`
#' to contain. See details.
#' @param ... Not used.
#'
#' @description
#'
#' Coerces the object into a `data.frame` containing just event times and status
#' filtering for specific subjects If `subjects` is a list then an additional variable `group` will be added
#' onto the dataset specifying which group the row belongs to.
#'
#' @examples
#' \dontrun{
#' subjects <- c("SUB1", "SUB2", "SUB3", "SUB4")
#' subset(x, subjects)
#'
#' groups <- list(
#'     "g1" = c("SUB1", "SUB3", "SUB4"),
#'     "g2" = c("SUB2", "SUB3")
#' )
#' subset(x, groups)
#' }
#' @family DataJoint
#' @export
subset.DataJoint <- function(x, subjects, ...) {
    data <- as.list(x)
    dat <- data.frame(
        time = data[["event_times"]],
        event = as.numeric(seq_along(data[["event_times"]]) %in% data[["subject_event_index"]]),
        subject = names(data[["subject_to_index"]])
    )
    subset_and_add_grouping(dat, subjects)
}



#' `subset_and_add_grouping`
#'
#' @param dat (`data.frame`) \cr must have a column called `subject` which corresponds to the
#' values passed to `groupings`.
#' @param groupings (`character` or `list`)\cr subjects that you wish to subset the dataset
#' to contain. If `groupings` is a list then an additional variable `group` will be added
#' onto the dataset specifying which group the row belongs to.
#'
#' @details
#' Example of usage
#' ```
#' subjects <- c("SUB1", "SUB2", "SUB3", "SUB4")
#' subset_and_add_grouping(dat, subjects)
#'
#' groups <- list(
#'     "g1" = c("SUB1", "SUB3", "SUB4"),
#'     "g2" = c("SUB2", "SUB3")
#' )
#' subset_and_add_grouping(dat, groups)
#' ```
#'
#' @keywords internal
subset_and_add_grouping <- function(dat, groupings) {
    groupings <- decompose_subjects(groupings, dat$subject)$groups
    dat_subset_list <- lapply(
        seq_along(groupings),
        \(i) {
            dat_reduced <- dat[dat$subject %in% groupings[[i]], , drop = FALSE]
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
        subject = names(data$subject_to_index)[data$subject_tumour_index],
        time = data$tumour_time,
        Yob = data$tumour_value
    )
    row.names(x) <- NULL
    x
}

#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "DataJoint",
    definition = function(object) {
        string_survival <- if (is.null(object@survival)) {
            "      Survival-Data Object:\n          Not Specified"
        } else {
            as_print_string(object@survival, indent = 5)
        }

        string_longitudinal <- if (is.null(object@longitudinal)) {
            "      Longitudinal-Data Object:\n          Not Specified"
        } else {
            as_print_string(object@longitudinal, indent = 5)
        }

        template <- c(
            "Joint-Data Object Containing:",
            "",
            as_print_string(object@subject, indent = 5),
            "",
            string_survival,
            "",
            string_longitudinal
        )
        cat("\n", paste(template, collapse = "\n"), "\n\n")
    }
)
