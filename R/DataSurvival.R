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
#' The [`DataSurvival`] class handles the processing of the survival data for fitting a Joint Model.
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
        if (as.character(x$frm[[2]][[1]]) != "Surv") {
            return("The LHS of `formula` should be a call to survival::Surv()")
        }
        for (v in c(x$time, x$event)) {
            if (! v %in% dnames) {
                return(sprintf("Variable %s is not in `data`", x$pt))
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



#' `DataSurvival` -> `list`
#' @inheritParams DataSurvival-Shared
#' @description
#' Coerces  [`DataSurvival`] into a `list` of data components required
#' for fitting a [`JointModel`]. See the vignette (TODO) for more details.
#' @family DataSurvival
#' @family as_stan_list
#' @export
as_stan_list.DataSurvival <- function(object, ...) {
    df <- as.data.frame(object)
    vars <- extractVariableNames(object)

    design_mat <- stats::model.matrix(vars$frm, data = df)
    remove_index <- grep("(Intercept)", colnames(design_mat), fixed = TRUE)
    design_mat <- design_mat[, -remove_index, drop = FALSE]
    rownames(design_mat) <- NULL

    # Parameters for efficient integration of hazard function -> survival function
    gh_parameters <- statmod::gauss.quad(n = 15, kind = "legendre")

    model_data <- list(
        Nind_dead = sum(df[[vars$event]]),
        dead_ind_index = which(df[[vars$event]] == 1),
        Times = df[[vars$time]],
        p_os_cov_design = ncol(design_mat),
        os_cov_design = design_mat,
        n_nodes = length(gh_parameters$nodes),
        nodes = gh_parameters$nodes,
        weights = gh_parameters$weights
    )
    return(model_data)
}


# TODO - docs
suit_up.DataSurvival <- function(object, subject_var, subject_ord, ...) {

    data <- as.data.frame(object)

    assert_string(subject_var, na.ok = FALSE)
    assert_character(subject_ord, any.missing = FALSE)
    assert_that(
        subject_var %in% names(data),
        msg = sprintf("Subject variable `%s` not found in `survival`", subject_var)
    )
    assert_that(
        all(data[[subject_var]] %in% subject_ord),
        msg = "There are subjects `survival` that are not present in `subjects`"
    )
    assert_that(
        all(subject_ord %in% data[[subject_var]]),
        msg = "There are subjects `subjects` that are not present in `survival`"
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
