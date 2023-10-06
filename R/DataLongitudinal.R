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
#' @slot subject (`character`)\cr See Arguments for details
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
        subject = "character",
        threshold = "numeric_or_NULL"
    )
)


#' @param data (`data.frame`)\cr containing the observed longitudinal data.
#' @param formula (`formula`)\cr of the form `outcome ~ time`, and cannot contain any additional covariates.
#' @param subject (`character`)\cr the name of the subject identifier variable.
#' @param threshold (`numeric`)\cr cut-off value to be used to declare an observation as censored
#'   (below detection limit).
#' @rdname DataLongitudinal-class
DataLongitudinal <- function(data, formula, subject, threshold = NULL) {
    .DataLongitudinal(
        data = remove_missing_rows(data, formula, c(subject)),
        formula = formula,
        subject = subject,
        threshold = threshold
    )
}

setValidity(
    "DataLongitudinal",
    function(object) { #nolint
        if (length(object@subject) > 1) {
            return("`subject` should be a length 1 character vector or NULL")
        }
        if (!length(object@formula) == 3) {
            return("`formula` should be a 2 sided formula")
        }
        if (!length(object@formula[[3]]) == 1) {
            return("the RHS of `formula` should only have 1 value")
        }
        if (! object@subject %in% names(object@data)) {
            return("`subject` does not exist in `data`")
        }
        pt <- object@data[[object@subject]]
        if (!(is(pt, "character") | is(pt, "factor"))) {
            return("`data[[subject]]` should be of type character or factor")
        }
        return(TRUE)
    }
)







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
    df_fct <- x@data
    vars <- extractVariableNames(x)
    df_fct[[vars$pt]] <- pt_2_factor(df_fct[[vars$pt]])
    return(df_fct)
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
        pt = object@subject,
        frm = object@formula,
        time = as.character(object@formula[[3]]),
        outcome = as.character(object@formula[[2]]),
        threshold = object@threshold
    )
}


#' `DataLongitudinal` -> `list`
#' @inheritParams DataLongitudinal-Shared
#' @description
#' Coerces  [`DataLongitudinal`] into a `list` of data components required
#' for fitting a [`JointModel`]. See the vignette (TODO) for more details.
#' @family DataLongitudinal
#' @export
as.list.DataLongitudinal <- function(x, ...) {

    df <- as.data.frame(x)
    vars <- extractVariableNames(x)

    mat_sld_index <- stats::model.matrix(
        stats::as.formula(paste("~", vars$pt)),
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
        Yobs = df[[vars$outcome]],
        Tobs = df[[vars$time]],
        Ythreshold = adj_threshold,

        # Number of individuals and tumor assessments.
        Nta_obs_y = length(index_obs),
        Nta_cens_y = length(index_cen),

        # Index vectors
        ind_index = as.numeric(df[[vars$pt]]),
        pt_to_ind = stats::setNames(seq_len(nlevels(df[[vars$pt]])), levels(df[[vars$pt]])),
        obs_y_index = index_obs,
        cens_y_index = index_cen,

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
