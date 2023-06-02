
#' @include generics.R
NULL

#' @rdname DataSurvival
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


#' `DataSurvival`
#'
#' The [`DataSurvival`] class handles the processing of the survival data for fitting a Joint Model.
#'
#' @param data A `data.frame` object containing the observed survival/time to event data
#' @param formula A two sided `formula` of the form `Surv(time, event) ~ cov1 + cov2 + ...`. See [`survival::Surv()`] for more details though note that this package only supports right censoring
#' @param subject A length 1 `character` vector specifying the name of the subject identifier variable
#' @param arm A length 1 `character` vector specifying the name of the treatment arm variable
#' @param study A length 1 `character` vector specifying the name of the study variable
#'
#' @details
#'
#' ## Coercion
#' - `as.list(x)`, `as(x, "list")`: Coerces x into a list of data components required
#' for fitting a [`JointModel`].
#' See the vignette (TODO) for more details
#'
#' @seealso [`DataJoint`], [`DataLongitudinal`]
#'
#' @export
DataSurvival <- function(data, formula, subject, arm , study) {
    .DataSurvival(
        data = remove_missing_rows(data, formula, c(subject, arm, study)),
        formula = formula,
        subject = subject,
        arm = arm,
        study = study
    )
}

#' Survival Formula Specification
#'
#' See [survival::Surv()] for details.
#'
#' @returns An object of class `Surv`.
#'
#' @name Surv
#' @rdname Surv
#'
#' @importFrom survival Surv
#' @export Surv
NULL


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


#' @rdname extractVariableNames
#' @export
setMethod(
    "extractVariableNames",
    signature = "DataSurvival",
    definition = function(object, ...) {
        list(
            arm = object@arm,
            study = object@study,
            pt = object@subject,
            frm = object@formula,
            time = as.character(object@formula[[2]][[2]]),
            event = as.character(object@formula[[2]][[3]])
        )
    }
)


#' @export
setMethod(
    "as.data.frame",
    signature = "DataSurvival",
    definition = function(x, row.names, optional, ...) {
        as(x, "data.frame")
    }
)

#' @export
setAs(
    from = "DataSurvival",
    to = "data.frame",
    def = function(from) {
        df <- from@data
        vars <- extractVariableNames(from)
        df_fct <- df
        df_fct[[vars$arm]] <- factor(df_fct[[vars$arm]])
        df_fct[[vars$study]] <- factor(df_fct[[vars$study]])
        df_fct[[vars$pt]] <- pt_2_factor(df_fct[[vars$pt]])
        return(df_fct)
    }
)


#' @export
setMethod(
    "as.list",
    signature = "DataSurvival",
    definition = function(x, ...) {

        df <- as(x, "data.frame")
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
            study_index = as.numeric(df[[vars$study]]),
            arm_index = as.numeric(df[[vars$arm]])
        )

        arm_data <- get_arm_study_data(
            as.numeric(df[[vars$pt]]),
            as.numeric(df[[vars$arm]]),
            as.numeric(df[[vars$study]])
        )


        return(append(model_data, arm_data))
    }
)


#' `get_arm_study_data`
#'
#' @param pt a factor vector of subject identifiers
#' @param arm a factor vector of treatment arms. Must be same length as `pt`
#' @param study a factor vector of study identifiers. Must be same length as `pt`
#'
#' The function is purely a sub-component of `as.list(DataSurvival)`. Its
#' purpose is to extract key logic into its own function to enable unit tests
#'
#' @returns TODO
#'
#' @keywords internal
get_arm_study_data <- function(pt, arm, study) {

    assert_that(
        length(pt) == length(arm),
        length(arm) == length(study),
        msg = "`pt`, `arm` and `study` must all have the same length"
    )

    result <- list()

    u_arm_study <- unique(data.frame(study = study, arm = arm))

    # TODO - Assumption that arms are unique to studies
    assert_that(
        length(u_arm_study[["arm"]]) == length(unique(u_arm_study[["arm"]])),
        length(unique(arm)) == nrow(u_arm_study),
        msg = "Arms must be unique across Studies e.g. arm A can't be in Study-1 and Study-2"
    )

    u_arm_study_ord <- u_arm_study[order(u_arm_study[["arm"]]), ]

    n_per_arm <- vector("numeric", length = length(u_arm_study_ord[["arm"]]))
    index_per_arm <- vector("numeric", length = 0)

    uniq_arm <- u_arm_study_ord[["arm"]]
    for (i in seq_along(uniq_arm)) {
        arm_selected <- uniq_arm[[i]]
        index_per_arm <- c(index_per_arm, which(arm == arm_selected))
        n_per_arm[[i]] <- sum(arm == arm_selected)
    }

    result[["index_per_arm"]] <- index_per_arm
    result[["n_index_per_arm"]] <- n_per_arm
    result[["n_arms"]] <- nrow(u_arm_study)
    result[["arm_to_study_index"]] <- as.numeric(u_arm_study[["study"]])
    result[["n_studies"]] <- length(unique(u_arm_study[["study"]]))
    return(result)
}


#' `pt_2_factor`
#'
#' Converts subject identifiers to factors
#' If pt is already a factor it will re-level it to ensure the levels are in alphabetical order
#' This is to ensure that [`DataLongitudinal`] and [`DataSurvival`] use identical index numbers
#' for the subjects to ensure data alignment in the Joint Model
#'
#' @param pt a `character` or `factor` vector
#'
#' @returns a `factor` vector with the levels in alphabetical order
#' @keywords internal
pt_2_factor <- function(pt) {
    pt_char <- as.character(pt)
    pt_uniq <- unique(pt_char)
    pt_uniq_ord <- pt_uniq[order(pt_uniq)]
    factor(pt_char, levels = pt_uniq_ord)
}



