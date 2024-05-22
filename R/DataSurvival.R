#' @include generics.R
#' @include utilities.R
NULL

#' Re-used documentation for `DataSurvival`
#'
#' @param object ([`DataSurvival`]) \cr Survival Data.
#' @param x ([`DataSurvival`]) \cr Survival Data.
#' @param ... Not Used.
#'
#' @name DataSurvival-Shared
#' @keywords internal
NULL



# DataSurvival-class ----

#' Survival Data Object and Constructor Function
#'
#' The [`DataSurvival`] class handles the processing of the survival data
#' for fitting a [`JointModel`].
#'
#' @slot data (`data.frame`)\cr See Arguments for details.
#' @slot formula (`formula`)\cr See Arguments for details.
#'
#' @family DataObjects
#' @family DataSurvival
#' @exportClass DataSurvival
#' @export DataSurvival
.DataSurvival <- setClass(
    Class = "DataSurvival",
    representation = list(
        data = "data.frame",
        formula = "formula"
    )
)

#' @param data (`data.frame`)\cr the observed time-to-event data.
#' @param formula (`formula`)\cr of the form `Surv(time, event) ~ cov1 + cov2 + ...`.
#'   See [survival::Surv()] for more details, though note that this package only supports right censoring.
#' @rdname DataSurvival-class
DataSurvival <- function(data, formula) {
    .DataSurvival(
        data = remove_missing_rows(data, formula),
        formula = formula
    )
}

setValidity(
    "DataSurvival",
    method = function(object) {
        dat <- object@data
        x <- extractVariableNames(object)
        dnames <- names(dat)
        if (nrow(object@data) == 0) {
            return("`data` should not have 0 rows")
        }
        if (length(x$frm) != 3) {
            return("`formula` should be a 2 sided formula")
        }
        LHS <- as.character(x$frm[[2]][[1]])
        if (!(identical(LHS, "Surv") || identical(LHS, c("::", "survival", "Surv")))) {
            return("The LHS of `formula` should be a call to survival::Surv()")
        }
        for (v in c(x$time, x$event)) {
            if (! v %in% dnames) {
                return(sprintf("Variable %s is not in `data`", x$subject))
            }
        }
        return(TRUE)
    }
)






#' @inheritParams DataSurvival-Shared
#' @inherit extractVariableNames description title
#'
#' @returns
#' A list with the following named elements:
#' - `frm` (`formula`)\cr a symbolic description of the survival model to be fitted
#' - `time` (`character`)\cr  The name of the variable containing the event time
#' - `event` (`character`) \cr  The name of the variable containing the event status
#' @export
#' @family DataSurvival
#' @family extractVariableNames
#' @keywords internal
extractVariableNames.DataSurvival <- function(object) {
    list(
        frm = object@formula,
        time = as.character(object@formula[[2]][[2]]),
        event = as.character(object@formula[[2]][[3]])
    )
}


#' `DataSurvival` -> `data.frame`
#'
#' @inheritParams DataSurvival-Shared
#'
#' @description
#' Converts a [`DataSurvival`] object into a `data.frame`.
#' The subject variable is cast to factor.
#' @family DataSurvival
#' @export
as.data.frame.DataSurvival <- function(x, ...) {
    x <- x@data
    rownames(x) <- NULL
    x
}



#' @rdname as_stan_list.DataObject
#' @family DataSurvival
#' @export
as_stan_list.DataSurvival <- function(object, ...) {
    df <- as.data.frame(object)
    vars <- extractVariableNames(object)

    design_mat <- stats::model.matrix(vars$frm, data = df)
    remove_index <- grep("(Intercept)", colnames(design_mat), fixed = TRUE)
    design_mat <- design_mat[, -remove_index, drop = FALSE]
    rownames(design_mat) <- NULL

    # Parameters for efficient integration of hazard function -> survival function
    gh_parameters <- statmod::gauss.quad(
        n = getOption("jmpost.gauss_quad_n"),
        kind = getOption("jmpost.gauss_quad_kind")
    )

    model_data <- list(
        n_subject_event = sum(df[[vars$event]]),
        subject_event_index = which(df[[vars$event]] == 1),
        event_times = df[[vars$time]],
        p_os_cov_design = ncol(design_mat),
        os_cov_design = design_mat,
        n_nodes = length(gh_parameters$nodes),
        nodes = gh_parameters$nodes,
        weights = gh_parameters$weights
    )
    return(model_data)
}

#' @rdname as_stan_list.DataObject
#' @export
as.list.DataSurvival <- function(x, ...) {
    as_stan_list(x, ...)
}

#' @rdname harmonise
harmonise.DataSurvival <- function(object, subject_var, subject_ord, ...) {

    data <- as.data.frame(object)

    assert_string(subject_var, na.ok = FALSE)
    assert_character(subject_ord, any.missing = FALSE)
    assert_that(
        subject_var %in% names(data),
        msg = sprintf("Subject variable `%s` not found in `survival`", subject_var)
    )
    assert_that(
        all(data[[subject_var]] %in% subject_ord),
        msg = "There are subjects in `survival` that are not present in `subjects`"
    )
    assert_that(
        all(subject_ord %in% data[[subject_var]]),
        msg = "There are subjects in `subjects` that are not present in `survival`"
    )

    data[[subject_var]] <- factor(
        as.character(data[[subject_var]]),
        levels = subject_ord
    )

    data_ord <- data[order(data[[subject_var]]), ]

    DataSurvival(
        data = data_ord,
        formula = object@formula
    )
}



#' `DataSurvival` -> Printable `Character`
#'
#' Converts [`DataSurvival`] object into a printable string.
#' @inheritParams DataSurvival-Shared
#' @family DataSurvival
#' @param indent (`numeric`)\cr how much white space to prefix the print string with.
#' @keywords internal
#' @export
as_print_string.DataSurvival <- function(object, indent = 1, ...) {
    template <- c(
        "Survival-Data Object:",
        "    # of Rows     = %d",
        "    # of Columns  = %d",
        "    # of Events   = %d",
        "    Formula       = %s"
    )
    pad <- rep(" ", indent) |> paste(collapse = "")
    template_padded <- paste(pad, template)
    vars <- extractVariableNames(object)
    sprintf(
        paste(template_padded, collapse = "\n"),
        nrow(object@data),
        ncol(object@data),
        sum(object@data[[vars$event]]),
        Reduce(paste, deparse(vars$frm))
    )
}

#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "DataSurvival",
    definition = function(object) {
        string <- as_print_string(object)
        cat("\n", string, "\n\n")
    }
)

#' Build design matrix for prediction data
#'
#' @description
#' This function takes a `DataSurvival` object and a `data.frame` object and generates
#' a design matrix for the `data.frame` that has the identical structure to the
#' design matrix of the `DataSurvival` object.
#'
#' This is used for predicting new data using a model that was trained on a different
#' original data source
#'
#' @param olddata ([`DataSurvival`]) \cr The original data to be used as a template for the new data
#' @param newdata ([`data.frame`]) \cr The new data to be used to generate the design matrix
#' @importFrom stats .checkMFClasses terms delete.response model.frame model.matrix
#' @importFrom survival coxph
#' @keywords internal
mirror_design_matrix <- function(olddata, newdata) {
    frm <- as_formula(olddata)
    # Dummy model to generate a bunch of meta information that we can use to
    # re-construct the design matrix
    model <- coxph(data = as.data.frame(olddata), formula = frm)
    model_terms <- delete.response(terms(model))
    model_frame <- model.frame(
        model_terms,
        newdata,
        xlev = model$xlevels
    )
    if (
        !is.null(data_classes <- attr(model_terms, "dataClasses"))) {
        .checkMFClasses(data_classes, model_frame)
    }
    design_mat <- model.matrix(
        model_terms,
        model_frame,
        contrasts.arg = model$contrasts
    )
    remove_index <- grep("(Intercept)", colnames(design_mat), fixed = TRUE)
    design_mat <- design_mat[, -remove_index, drop = FALSE]
    rownames(design_mat) <- NULL
    design_mat
}


#' @export
as_formula.DataSurvival <- function(x, ...) {
    vars <- extractVariableNames(x)
    vars$frm
}
