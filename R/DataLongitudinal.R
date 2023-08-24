#' @include generics.R
#' @include utilities.R
NULL

setClassUnion("numeric_or_NULL", c("numeric", "NULL"))

# DataLongitudinal-class ----

#' `DataLongitudinal`
#'
#' The [`DataLongitudinal`] class handles the processing of the longitudinal data for fitting a Joint Model.
#'
#' @slot data (`data.frame`)\cr containing the observed longitudinal data. Note that
#'   observations that contain missing values in the required variables are removed
#'   in the slot.
#' @slot formula (`formula`)\cr of the form `outcome ~ time`, and cannot contain any additional covariates.
#' @slot subject (`string`)\cr the name of the subject identifier variable.
#' @slot threshold (`number`)\cr cut-off value to be used to declare an observation as censored
#'   (below detection limit).
#'
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

# DataLongitudinal-constructors ----

#' @rdname DataLongitudinal-class
#'
#' @param data (`data.frame`)\cr containing the observed longitudinal data.
#' @param formula (`formula`)\cr of the form `outcome ~ time`, and cannot contain any additional covariates.
#' @param subject (`string`)\cr the name of the subject identifier variable.
#' @param threshold (`number`)\cr cut-off value to be used to declare an observation as censored
#'   (below detection limit).
#'
#' @details
#'
#' - `as.list(x)`, `as(x, "list")`: Coerces x into a list of data components required
#' for fitting a [`JointModel`].
#' See the vignette (TODO) for more details
#' - `as.data.frame(x)`
#'
#' @seealso [`DataJoint`], [`DataSurvival`].
#'
#' @export
DataLongitudinal <- function(data, formula, subject, threshold = NULL) {
    .DataLongitudinal(
        data = remove_missing_rows(data, formula, c(subject)),
        formula = formula,
        subject = subject,
        threshold = threshold
    )
}

# DataLongitudinal-validity ----

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

# as.data.frame-DataLongitudinal ----

#' @rdname as.data.frame
setMethod(
    "as.data.frame",
    signature = "DataLongitudinal",
    definition = function(x, row.names, optional, ...) {
        df_fct <- x@data
        vars <- extractVariableNames(x)
        df_fct[[vars$pt]] <- pt_2_factor(df_fct[[vars$pt]])
        return(df_fct)
    }
)

# extractVariableNames-DataLongitudinal ----

#' @rdname extractVariableNames
setMethod(
    f = "extractVariableNames",
    signature = "DataLongitudinal",
    definition = function(object) {
        list(
            pt = object@subject,
            frm = object@formula,
            time = as.character(object@formula[[3]]),
            outcome = as.character(object@formula[[2]]),
            threshold = object@threshold
        )
    }
)

# as.list-DataLongitudinal ----

#' @rdname as.list
setMethod(
    f = "as.list",
    signature = "DataLongitudinal",
    definition = function(x) {

        df <- as.data.frame(x)
        vars <- extractVariableNames(x)

        mat_sld_index <- stats::model.matrix(
            stats::as.formula(paste("~", vars$pt)),
            data = df
        ) |>
            t()

        # TODO - Maybe reimplement this using a more robust approach than magic number
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
)
