

#' Re-used documentation for `DataSubject`
#'
#' @param object ([`DataSubject`]) \cr subject-level data.
#' @param x ([`DataSubject`]) \cr subject-level data.
#' @param ... Not Used.
#'
#' @name DataSubject-Shared
#' @keywords internal
NULL



#' Subject Data Object and Constructor Function
#'
#' The [`DataSubject`] class handles the processing of the subject data for
#' fitting a [`JointModel`].
#'
#' @slot data (`data.frame`)\cr the subject-level data.
#' @slot subject (`character`)\cr the name of the variable containing the subject identifier.
#' @slot arm (`character`)\cr the name of the variable containing the arm identifier.
#' @slot study (`character`)\cr the name of the variable containing the study identifier.
#'
#' @family DataObjects
#' @family DataSubject
#' @exportClass DataSubject
#' @export DataSubject
.DataSubject <- setClass(
    Class = "DataSubject",
    representation = list(
        data = "data.frame",
        subject = "character",
        arm = "character",
        study = "character"
    )
)


#' @param data (`data.frame`)\cr the subject-level data.
#' @param subject (`character`)\cr the name of the variable containing the subject identifier.
#' @param arm (`character`)\cr the name of the variable containing the arm identifier.
#' @param study (`character`)\cr the name of the variable containing the study identifier.
#' @rdname DataSubject-class
#' @export
DataSubject <- function(data, subject, arm, study) {
    vars <- c(subject, arm, study)
    vars_frm_chr <- paste0("~ ", paste(vars, collapse = " + "))
    .DataSubject(
        data = remove_missing_rows(data, as.formula(vars_frm_chr)),
        subject = subject,
        arm = arm,
        study = study
    )
}

setValidity(
    "DataSubject",
    method = function(object) {
        if (length(object@subject) != 1) {
            return("`subject` must be a length 1 character")
        }
        if (length(object@arm) != 1) {
            return("`arm` must be a length 1 character")
        }
        if (length(object@study) != 1) {
            return("`study` must be a length 1 character")
        }
        if (nrow(object@data) == 0) {
            return("`data` should not have 0 rows")
        }
        sbj <- object@data[[object@subject]]
        if (!(is(sbj, "character") | is(sbj, "factor"))) {
            return("`data[[subject]]` should be of type character or factor")
        }
    }
)



#' @inheritParams DataSubject-Shared
#' @inherit extractVariableNames description title
#'
#' @returns
#' A list with the following named elements:
#' - `subject` (`character`)\cr the name of the variable containing the subject identifier.
#' - `arm` (`character`)\cr the name of the variable containing the arm identifier.
#' - `study` (`character`) \cr the name of the variable containing the study identifier.
#' @family DataSubject
#' @family extractVariableNames
#' @keywords internal
extractVariableNames.DataSubject <- function(object) {
    list(
        subject = object@subject,
        arm = object@arm,
        study = object@study
    )
}


#' @rdname as_stan_list
#' @family DataSubject
#' @export
as_stan_list.DataSubject <- function(x) {
    df <- as.data.frame(harmonise(x))
    vars <- extractVariableNames(x)
    list(
        Nind = nrow(df),
        n_studies = length(unique(df[[vars$study]])),
        n_arms = length(unique(df[[vars$arm]])),
        pt_study_index = as.numeric(df[[vars$study]]),
        pt_arm_index = as.numeric(df[[vars$arm]]),
        pt_to_ind = stats::setNames(
            seq_len(nlevels(df[[vars$subject]])),
            levels(df[[vars$subject]])
        )
    )
}

#' @rdname as_stan_list
#' @export
as.list.DataSubject <- function(x, ...) {
    as_stan_list(x, ...)
}


#' `DataSubject` -> `data.frame`
#'
#' @inheritParams DataSubject-Shared
#'
#' @description
#' Converts a [`DataSubject`] object into a `data.frame`.
#' The subject variable is cast to factor.
#' @family DataSubject
#' @export
as.data.frame.DataSubject <- function(x, ...) {
    x <- x@data
    rownames(x) <- NULL
    x
}



#' @rdname harmonise
harmonise.DataSubject <- function(object, ...) {
    data <- as.data.frame(object)
    vars <- extractVariableNames(object)
    assert_that(
        vars$subject %in% names(data),
        vars$arm %in% names(data),
        vars$study %in% names(data)
    )
    assert_character(
        as.character(data[[vars$subject]]),
        any.missing = FALSE,
        unique = TRUE
    )
    data[[vars$subject]] <- factor(data[[vars$subject]])
    data[[vars$arm]] <- factor(data[[vars$arm]])
    data[[vars$study]] <- factor(data[[vars$study]])
    data <- data[order(data[[vars$subject]]), ]
    DataSubject(
        data = data,
        subject = object@subject,
        arm = object@arm,
        study = object@study
    )
}
