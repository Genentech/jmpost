#' @include generics.R
#' @include utilities.R
NULL

# DataSurvival-class ----

#' `DataSurvival`
#'
#' The [`DataSurvival`] class handles the processing of the survival data for fitting a Joint Model.
#'
#' @slot data (`data.frame`)\cr the observed time-to-event data. Note that
#'   observations that contain missing values in the required variables are removed
#'   in the slot.
#' @slot formula (`formula`)\cr of the form `Surv(time, event) ~ cov1 + cov2 + ...`.
#'   See [`survival::Surv()`] for more details, though note that this package only supports right censoring.
#' @slot subject (`string`)\cr the name of the subject identifier variable.
#' @slot arm (`string`)\cr the name of the treatment arm variable.
#' @slot study (`string`)\cr the name of the study variable.
#' @slot time_grid (`numeric`)\cr grid of time points to use for providing samples
#'   of the survival functions. If `NULL`, will be taken as a sequence of
#'   201 values from 0 to the maximum observed times.
#'
#' @seealso [`DataJoint`], [`DataLongitudinal`]
#'
#' @exportClass DataSurvival
.DataSurvival <- setClass(
    Class = "DataSurvival",
    representation = list(
        data = "data.frame",
        formula = "formula",
        subject = "character",
        arm = "character",
        study = "character",
        time_grid = "numeric_or_NULL"
    )
)

# DataSurvival-constructors ----

#' @rdname DataSurvival-class
#'
#' @param data (`data.frame`)\cr the observed time-to-event data.
#' @param formula (`formula`)\cr of the form `Surv(time, event) ~ cov1 + cov2 + ...`.
#'   See [`survival::Surv()`] for more details, though note that this package only supports right censoring.
#' @param subject (`string`)\cr the name of the subject identifier variable.
#' @param arm (`string`)\cr the name of the treatment arm variable.
#' @param study (`string`)\cr the name of the study variable.
#' @param time_grid (`numeric`)\cr grid of time points to use for providing samples
#'   of the longitudinal model fit functions. If `NULL`, will be taken as a sequence of
#'   201 values from 0 to the maximum observed times.
#'
#' @export
DataSurvival <- function(data, formula, subject, arm, study, time_grid = NULL) {
    .DataSurvival(
        data = remove_missing_rows(data, formula, c(subject, arm, study)),
        formula = formula,
        subject = subject,
        arm = arm,
        study = study,
        time_grid = time_grid
    )
}

# Surv ----

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
#' @keywords internal
NULL

# DataSurvival-validity ----

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
        if (!is.null(object@time_grid)) {
            if (!is.sorted(object@time_grid) ||
                any(duplicated(object@time_grid)) ||
                !all(is.finite(object@time_grid))) {
                return("`time_grid` needs to be finite, sorted, unique values numeric vector")
            }
        }
        return(TRUE)
    }
)

# extractVariableNames-DataSurvival ----

#' @rdname extractVariableNames
setMethod(
    "extractVariableNames",
    signature = "DataSurvival",
    definition = function(object) {
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

# as.data.frame-DataSurvival ----

#' @rdname as.data.frame
setMethod(
    "as.data.frame",
    signature = "DataSurvival",
    definition = function(x, row.names, optional, ...) {
        as(x, "data.frame")
    }
)

# coerce-DataSurvival,data.frame ----

#' @rdname as.data.frame
#'
#' @name coerce-DataSurvival-data.frame-method
#' @aliases coerce,DataSurvival,data.frame-method
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

# as.list-DataSurvival ----

#' @rdname as.list
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

        sm_time_grid <- if (is.null(x@time_grid)) {
            seq(from = 0, to = max(df[[vars$time]]), length = 201)
        } else {
            x@time_grid
        }

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
            arm_index = as.numeric(df[[vars$arm]]),
            sm_time_grid = sm_time_grid,
            n_sm_time_grid = length(sm_time_grid)
        )

        arm_data <- get_arm_study_data(
            as.numeric(df[[vars$pt]]),
            as.numeric(df[[vars$arm]]),
            as.numeric(df[[vars$study]])
        )

        return(append(model_data, arm_data))
    }
)

# get_arm_study_data ----

#' `get_arm_study_data`
#'
#' @param pt (`factor`)\cr subject identifiers.
#' @param arm (`factor`)\cr treatment arms of the same length as `pt`.
#' @param study (`factor`)\cr study identifiers of the same length as `pt`.
#'
#' The function is purely a sub-component of `as.list(DataSurvival)`. Its
#' purpose is to extract key logic into its own function to enable unit tests.
#'
#' @returns A list with `index_per_arm`, `n_index_per_arm`, `n_arms`,
#'   `arm_to_study_index`, and `n_studies`.
#'
#' @keywords internal
get_arm_study_data <- function(pt, arm, study) {

    assert_that(
        length(pt) == length(arm),
        length(arm) == length(study),
        msg = "`pt`, `arm` and `study` must all have the same length"
    )

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

    list(
        index_per_arm = index_per_arm,
        n_index_per_arm = n_per_arm,
        n_arms = nrow(u_arm_study),
        arm_to_study_index = as.numeric(u_arm_study[["study"]]),
        n_studies = length(unique(u_arm_study[["study"]]))
    )
}
