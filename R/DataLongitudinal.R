#' @include generics.R
#' @include utilities.R
NULL

#' Re-used documentation for `DataLongitudinal`
#'
#' @param object ([`DataLongitudinal`]) \cr Longitudinal Data.
#' @param x ([`DataLongitudinal`]) \cr Longitudinal Data.
#' @param ... Not Used.
#'
#' @name DataLongitudinal-Shared
#' @keywords internal
NULL

setClassUnion("numeric_or_NULL", c("numeric", "NULL"))

# DataLongitudinal-class ----

#' Longitudinal Data Object and Constructor Function
#'
#' The [`DataLongitudinal`] class handles the processing of the longitudinal data for fitting a Joint Model.
#'
#'
#' @slot data (`data.frame`)\cr See Arguments for details; Note that
#'   observations that contain missing values in the required variables are removed.
#' @slot formula (`formula`)\cr See Arguments for details
#' @slot threshold (`numeric`)\cr See Arguments for details
#'
#' @family DataObjects
#' @family DataLongitudinal
#' @export DataLongitudinal
#' @exportClass DataLongitudinal
.DataLongitudinal <- setClass(
    Class = "DataLongitudinal",
    representation = list(
        data = "data.frame",
        formula = "formula",
        threshold = "numeric_or_NULL"
    )
)


#' @param data (`data.frame`)\cr containing the observed longitudinal data.
#' @param formula (`formula`)\cr of the form `outcome ~ time`, and cannot contain any additional covariates.
#' @param threshold (`numeric`)\cr cut-off value to be used to declare an observation as censored
#'   (below detection limit).
#' @rdname DataLongitudinal-class
DataLongitudinal <- function(data, formula, threshold = NULL) {
    .DataLongitudinal(
        data = remove_missing_rows(data, formula),
        formula = formula,
        threshold = threshold
    )
}

setValidity(
    "DataLongitudinal",
    function(object) {
        if (!length(object@formula) == 3) {
            return("`formula` should be a 2 sided formula")
        }
        if (!length(object@formula[[3]]) == 1) {
            return("the RHS of `formula` should only have 1 value")
        }
        if (!length(object@threshold) <= 1) {
            return("`threshold` must be of length 1 or `NULL`")
        }
        vars <- extractVariableNames(object)
        vars$threshold <- NULL
        vars$frm <- NULL
        for (i in unlist(vars)) {
            if (! i %in% names(object@data)) {
                return(sprintf("Variable `%s` is not in data", i))
            }
        }
        return(TRUE)
    }
)




#' @rdname harmonise
harmonise.DataLongitudinal <- function(object, subject_var, subject_ord, ...) {
    data <- as.data.frame(object)
    vars <- extractVariableNames(object)
    assert_string(subject_var, na.ok = FALSE)
    assert_character(subject_ord, any.missing = FALSE)
    assert_that(
        subject_var %in% names(data),
        msg = sprintf("Subject variable `%s` not found in `longitudinal`", subject_var)
    )
    assert_that(
        all(data[[subject_var]] %in% subject_ord),
        msg = "There are subjects `longitudinal` that are not present in `subjects`"
    )
    assert_that(
        all(subject_ord %in% data[[subject_var]]),
        msg = "There are subjects `subjects` that are not present in `longitudinal`"
    )
    data[[subject_var]] <- factor(
        as.character(data[[subject_var]]),
        levels = subject_ord
    )
    data_re_ord <- order(
        data[[subject_var]],
        data[[vars$time]],
        data[[vars$outcome]]
    )
    data_ord <- data[data_re_ord, ]
    DataLongitudinal(
        data = data_ord,
        formula = object@formula,
        threshold = object@threshold
    )
}



#' `DataLongitudinal` -> `data.frame`
#'
#' @inheritParams DataLongitudinal-Shared
#'
#' @description
#' Converts a [`DataLongitudinal`] object into a `data.frame`.
#' The subject variable is cast to factor.
#' @family DataLongitudinal
#' @export
as.data.frame.DataLongitudinal <- function(x, ...) {
    x <- x@data
    rownames(x) <- NULL
    x
}




#' @inheritParams DataLongitudinal-Shared
#' @inherit extractVariableNames description title
#'
#' @returns
#' A list with the following named elements:
#' - `pt` (`character`)\cr The name of the variable containing the subject identifier
#' - `frm` (`formula`)\cr of the form `outcome ~ time`
#' - `time` (`character`)\cr The name of the variable containing the outcome time
#' - `outcome` (`character`)\cr The name of the variable containing the outcome values
#' - `threshold` (`numeric`)\cr cut-off value to be used to declare an observation as censored
#'   (below detection limit).
#' @family DataLongitudinal
#' @family extractVariableNames
extractVariableNames.DataLongitudinal <- function(object) {
    list(
        frm = object@formula,
        time = as.character(object@formula[[3]]),
        outcome = as.character(object@formula[[2]]),
        threshold = object@threshold
    )
}


#' @rdname as_stan_list.DataObject
#' @family DataLongitudinal
#' @export
as_stan_list.DataLongitudinal <- function(object, subject_var, ...) {

    df <- as.data.frame(object)
    vars <- extractVariableNames(object)

    assert_factor(df[[subject_var]])

    mat_sld_index <- stats::model.matrix(
        stats::as.formula(paste("~", subject_var)),
        data = df
    ) |>
        t()

    adj_threshold <- if (is.null(vars$threshold)) {
        -999999
    } else {
        vars$threshold
    }

    index_obs <- which(df[[vars$outcome]] >= adj_threshold)
    index_cen <- which(df[[vars$outcome]] < adj_threshold)

    sparse_mat_inds_all_y <- rstan::extract_sparse_parts(mat_sld_index)
    sparse_mat_inds_obs_y <- rstan::extract_sparse_parts(mat_sld_index[, index_obs])
    sparse_mat_inds_cens_y <- rstan::extract_sparse_parts(mat_sld_index[, index_cen])

    model_data <- list(
        Nta_total = nrow(df),

        # Number of individuals and tumor assessments.
        Nta_obs_y = length(index_obs),
        Nta_cens_y = length(index_cen),

        # Index vectors
        ind_index = as.numeric(df[[subject_var]]),
        obs_y_index = index_obs,
        cens_y_index = index_cen,

        Yobs = df[[vars$outcome]],
        Tobs = df[[vars$time]],
        Ythreshold = adj_threshold,

        # Sparse matrix parameters
        # Matrix of individuals x observed tumor assessments.
        n_mat_inds_obs_y = c(
            length(sparse_mat_inds_obs_y$w),
            length(sparse_mat_inds_obs_y$v),
            length(sparse_mat_inds_obs_y$u)
        ),
        w_mat_inds_obs_y = sparse_mat_inds_obs_y$w,
        v_mat_inds_obs_y = sparse_mat_inds_obs_y$v,
        u_mat_inds_obs_y = sparse_mat_inds_obs_y$u,

        # Matrix of individuals x censored tumor assessments.
        n_mat_inds_cens_y = c(
            length(sparse_mat_inds_cens_y$w),
            length(sparse_mat_inds_cens_y$v),
            length(sparse_mat_inds_cens_y$u)
        ),
        w_mat_inds_cens_y = sparse_mat_inds_cens_y$w,
        v_mat_inds_cens_y = sparse_mat_inds_cens_y$v,
        u_mat_inds_cens_y = sparse_mat_inds_cens_y$u,

        # Matrix of all individuals tumour assessments
        n_mat_inds_all_y = c(
            length(sparse_mat_inds_all_y$w),
            length(sparse_mat_inds_all_y$v),
            length(sparse_mat_inds_all_y$u)
        ),
        w_mat_inds_all_y = sparse_mat_inds_all_y$w,
        v_mat_inds_all_y = sparse_mat_inds_all_y$v,
        u_mat_inds_all_y = sparse_mat_inds_all_y$u

    )

    return(model_data)
}

#' @rdname as_stan_list.DataObject
#' @export
as.list.DataLongitudinal <- function(x, ...) {
    as_stan_list(x, ...)
}
