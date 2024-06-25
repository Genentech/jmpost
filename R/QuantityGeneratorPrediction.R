#' @include generics.R
#' @include Grid.R
NULL

#' @rdname Quant-Dev
.QuantityGeneratorPrediction <- setClass(
    "QuantityGeneratorPrediction",
    contains = "QuantityGenerator",
    slots = c(
        "times" = "numeric",
        "newdata" = "data.frame",
        "params" = "list"
    )
)


#' @rdname Quant-Dev
QuantityGeneratorPrediction <- function(times, newdata = NULL, params = NULL) {
    .QuantityGeneratorPrediction(
        times = times,
        newdata = newdata,
        params = params
    )
}
setValidity(
    "QuantityGeneratorPrediction",
    function(object) {
        if (length(object@times) != nrow(object@newdata)) {
            return("Length of `times` and `newdata` must be equal")
        }
        return(TRUE)
    }
)




#' @rdname as_stan_list.QuantityGenerator
#' @export
as_stan_list.QuantityGeneratorPrediction <- function(object, data, model, ...) {
    assert_that(
        is(data, "DataJoint")
    )
    ret <- list()
    data_list <- as_stan_list(data)

    ret[["gq_times"]] <- object@times
    ret[["gq_n_quant"]] <- length(object@times)

    # Get a list of which longutidunal parameters need to be defined based
    # on the selected longitudinal model
    par_names <- getPredictionNames(model@longitudinal)
    for (nam in par_names) {
        assert_that(
            nam %in% names(object@params),
            msg = sprintf("Parameter '%s' not found in `params`", nam)
        )
        assert_that(
            is.numeric(object@params[[nam]]),
            msg = sprintf("Parameter '%s' must be numeric", nam)
        )
        assert_that(
            length(object@params[[nam]]) == 1,
            msg = sprintf("Parameter '%s' must be length 1", nam)
        )
    }

    par_vals <- object@params[par_names]
    if (length(par_vals) == 0) {
        par_vals <- 0
        par_names <- "null_model"
    }

    ret[["gq_n_par"]] <- length(par_names)

    # Replicate the longitudinal parameters so the same parameter values are used
    # for all observations that are being predicted
    ret[["gq_link_function_inputs"]] <- matrix(
        rep(unlist(par_vals), each = ret[["gq_n_quant"]]),
        ncol = ret[["gq_n_par"]],
        nrow = ret[["gq_n_quant"]]
    )

    # Create design matrix from new data ensuring that it has the same
    # structure as the original design matrix
    ret[["gq_os_cov_design"]] <- mirror_design_matrix(
        data@survival,
        object@newdata
    )

    # dummy pop indexes in order for stan code to actualy compile. In this setting
    # this matrix isn't actually used so doesn't matter what these values are
    # but don't want to have to burden individual longitudinal models with the
    # conditional logic to check if they are generating population quantities or not
    ret[["gq_long_pop_arm_index"]] <- rep(1, ret[["gq_n_quant"]])
    ret[["gq_long_pop_study_index"]] <- rep(1, ret[["gq_n_quant"]])


    # Sanity checks
    assert_that(
        nrow(ret[["gq_os_cov_design"]]) == ret[["gq_n_quant"]],
        ncol(ret[["gq_os_cov_design"]]) == data_list[["p_os_cov_design"]],
        all(!is.na(ret[["gq_link_function_inputs"]]))
    )

    return(ret)
}
