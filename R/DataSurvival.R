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
#' @param data (`data.frame`)\cr the observed time-to-event data.
#' @param formula (`formula`)\cr of the form `Surv(time, event) ~ cov1 + cov2 + ...`.
#'   See [survival::Surv()] for more details, though note that this package only supports right censoring.
#' @param subject (`character`)\cr the name of the subject identifier variable.
#' @param arm (`character`)\cr the name of the treatment arm variable.
#' @param study (`character`)\cr the name of the study variable.
#'
#' @slot data (`data.frame`)\cr See Arguments for details.
#' @slot formula (`formula`)\cr See Arguments for details.
#' @slot subject (`character`)\cr See Arguments for details.
#' @slot arm (`character`)\cr See Arguments for details.
#' @slot study (`character`)\cr See Arguments for details.
#'
#' @family DataObjects
#' @family DataSurvival
#' @exportClass DataSurvival
#' @export DataSurvival
.DataSurvival <- setClass(
    Class = "DataSurvival",
    representation = list(
        data = "data.frame",
        formula = "formula",
        subject = "character",
        arm = "character",
        study = "character"
    )
)

#' @rdname DataSurvival-class
DataSurvival <- function(data, formula, subject, arm, study) {
    .DataSurvival(
        data = remove_missing_rows(data, formula, c(subject, arm, study)),
        formula = formula,
        subject = subject,
        arm = arm,
        study = study
    )
}

setValidity(
    "DataSurvival",
    method = function(object) {

        dat <- object@data
        x <- extractVariableNames(object)

        dnames <- names(dat)

        if (length(x$frm) != 3) {
            return("`formula` should be a 2 sided formula")
        }
        if (as.character(x$frm[[2]][[1]]) != "Surv") {
            return("The LHS of `formula` should be a call to survival::Surv()")
        }
        if (length(x$pt) != 1) {
            return("`subject` should be a length 1 character vector")
        }
        if (length(x$arm) != 1) {
            return("`arm` should be a length 1 character vector")
        }
        if (length(x$study) != 1) {
            return("`study` should be a length 1 character vector")
        }
        for (v in c(x$pt, x$arm, x$study, x$time, x$event)) {
            if (! v %in% dnames) {
                return(sprintf("Variable %s is not in `data`", x$pt))
            }
        }
        if (length(dat[[x$pt]]) != length(unique(dat[[x$pt]]))) {
            return("Only 1 survival observation per subject is supported")
        }
        return(TRUE)
    }
)




#' @inheritParams DataSurvival-Shared
#' @inherit extractVariableNames description title
#'
#' @returns
#' A list with the following named elements:
#' - `arm` (`character`)\cr The name of the variable containing the treatment arm
#' - `study` (`character`)\cr The name of the variable containing the study identifier
#' - `pt` (`character`)\cr The name of the variable containing the subject identifier
#' - `frm` (`formula`)\cr a symbolic description of the survival model to be fitted
#' - `time` (`character`)\cr  The name of the variable containing the event time
#' - `event` (`character`) \cr  The name of the variable containing the event status
#' @family DataSurvival
#' @family extractVariableNames
#' @keywords internal
extractVariableNames.DataSurvival <- function(object) {
    list(
        arm = object@arm,
        study = object@study,
        pt = object@subject,
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
    df <- x@data
    vars <- extractVariableNames(x)
    df_fct <- df
    df_fct[[vars$arm]] <- factor(df_fct[[vars$arm]])
    df_fct[[vars$study]] <- factor(df_fct[[vars$study]])
    df_fct[[vars$pt]] <- pt_2_factor(df_fct[[vars$pt]])
    df_sorted <- df_fct[order(df_fct[[vars$pt]]), ]
    return(df_sorted)
}



#' `DataSurvival` -> `list`
#' @inheritParams DataSurvival-Shared
#' @description
#' Coerces  [`DataSurvival`] into a `list` of data components required
#' for fitting a [`JointModel`]. See the vignette (TODO) for more details.
#' @family DataSurvival
#' @export
as.list.DataSurvival <- function(x, ...) {
    df <- as.data.frame(x)

    vars <- extractVariableNames(x)

    design_mat <- stats::model.matrix(vars$frm, data = df)
    remove_index <- grep("(Intercept)", colnames(design_mat), fixed = TRUE)
    design_mat <- design_mat[, -remove_index, drop = FALSE]

    # Parameters for efficient integration of hazard function -> survival function
    gh_parameters <- statmod::gauss.quad(n = 15, kind = "legendre")

    model_data <- list(
        Nind = nrow(df),
        Nind_dead = sum(df[[vars$event]]),
        dead_ind_index = which(df[[vars$event]] == 1),
        Times = df[[vars$time]],
        p_os_cov_design = ncol(design_mat),
        os_cov_design = design_mat,
        n_nodes = length(gh_parameters$nodes),
        nodes = gh_parameters$nodes,
        weights = gh_parameters$weights,

        # survival dataset is sorted by pt, as such
        # pt_study_index[3] = study index for patient with factor-level 3
        # This allows for easy assumptions to be made in the longitudinal data
        # for ensuring alignment / consistency
        n_studies = length(unique(df[[vars$study]])),
        n_arms = length(unique(df[[vars$arm]])),
        pt_study_index = as.numeric(df[[vars$study]]),
        pt_arm_index = as.numeric(df[[vars$arm]])
    )

    return(model_data)
}
