
#' Re-used documentation for `Quantities`
#'
#' @param x ([`Quantities`]) \cr generated quantities.
#' @param object ([`Quantities`]) \cr generated quantities.
#' @param time_grid (`numeric`)\cr sets the `time` variable.
#' Must be equal in length to `ncol(x)`.
#' @param type (`character`)\cr sets the `type` variable.
#' @param groups (`list`)\cr named `list`.
#' The element names are used to set the `group` variable.
#' @param conf.level (`numeric`) \cr confidence level of the interval.
#' @param ... not used.
#'
#' @keywords internal
#' @name Quantities-Shared
NULL


#' Generated Quantities Container
#'
#' A simple wrapper around a `list` to ensure each element complies
#' to specific formatting rules
#'
#' @keywords internal
#' @name Quantities-class
#' @family Quantities
.Quantities <- setClass(
    "Quantities",
    slots = list(
        "quantities" = "matrix",
        "times" = "numeric",
        "groups" = "character"
    )
)

#' @param x (`list`)\cr a `list` of `matrix` objects. Each matrix must have the exact
#' same dimensions.
#' @param todo todo
#' @rdname Quantities-class
Quantities <- function(quantities, times, groups) {
    .Quantities(
        quantities = quantities,
        times = times,
        groups = groups
    )
}

setValidity(
    Class = "Quantities",
    method = function(object) {
        if (length(object@times) != length(object@groups)) {
            return("Length of `times` must be equal to the length of `groups`")
        }
        if (length(object@times) != ncol(object@quantities)) {
            return("Length of `times` must be equal to the number of columns in `quantities`")
        }
        return(TRUE)
    }
)


#' Dimensions of `Quantitites`
#'
#' TODO
#'
#' @inheritParams Quantities-Shared
#' @keywords internal
#' @export
dim.Quantities <- function(x) {
    dim(x@quantities)
}


#' `Quantities` -> `data.frame`
#'
#' @inheritParams Quantities-Shared
#'
#' @keywords internal
#' @family Quantities
#' @export
as.data.frame.Quantities <- function(x, ...) {
    data.frame(
        group = rep(x@groups, each = nrow(x@quantities)),
        time = rep(x@times, each = nrow(x@quantities)),
        values = as.vector(x@quantities)
    )
}


#' summary
#'
#' @description
#' This method returns a summary statistic `data.frame` of the quantities. Note that this
#' is just an internal utility method in order to share common code between
#' [LongitudinalQuantities] and [SurvivalQuantities]
#'
#' @inheritParams Quantities-Shared
#'
#' @returns
#' A `data.frame` with the following variables:
#' - `median` (`numeric`) \cr the median value of the quantity.
#' - `lower` (`numeric`) \cr the lower CI value of the quantity.
#' - `upper` (`numeric`) \cr the upper CI value of the quantity.
#' - `time` (`numeric`) \cr the time point which the quantity is for.
#' - `group` (`character`) \cr which group the quantity belongs to.
#' - `type` (`character`) \cr what type of quantity is it.
#'
#' @keywords internal
#' @family Quantities
#' @export
summary.Quantities <- function(object, conf.level = 0.95, ...) {
    quantities_summarised <- samples_median_ci(
        object@quantities,
        level = conf.level
    )

    quantities_summarised$group <- object@groups
    quantities_summarised$time <- object@times
    quantities_summarised[, c("group", "time", "median", "lower", "upper")]
}


# TODO - Update docs
#' Extract and Average Quantities By Group Index
#'
#' This function takes a [posterior::draws_matrix()] (matrix of cmdstanr sample draws) and extracts
#' the specified columns and aggregates them by calculating the pointwise average.
#'
#' @param subject_index (`numeric`)\cr which subject indices to extract from `quantities`.
#' See details.
#'
#' @param time_index (`numeric`)\cr which time point indices to extract from `quantities`.
#' See details.
#'
#' @param quantities ([`posterior::draws_matrix`])\cr sample draws.
#' See details.
#'
#' @details
#' It is assumed that `quantities` consists of the cartesian product
#' of subject indices and time indices. That is, if the matrix contains 4 subjects and 3 time
#' points then it should have 12 columns.
#' It is also assumed that each column of `quantities` are named as:
#' ```
#' "quantity[x,y]"
#' ```
#' Where
#' - `x` is the subject index
#' - `y` is the time point index
#'
#' This function returns a `matrix` with 1 row per sample and 1 column per `time_index`.
#'
#' Note that if multiple values are provided for `subject_index` then the pointwise average
#' will be calculated for each time point by taking the mean across the specified subjects
#' at each time point.
#'
#' @keywords internal
collapse_quantities <- function(quantities_raw, collapser) {
    assert_class(quantities_raw, "matrix")
    assert_class(collapser, "QuantityCollapser")

    quantities <- matrix(
        NA,
        nrow = nrow(quantities_raw),
        ncol = length(collapser)
    )

    for (idx in seq_len(length(collapser))) {
        quantities[, idx] <- quantities_raw[
            ,
            collapser@indexes[[idx]],
            drop = FALSE
        ] |> rowMeans()
    }

    return(quantities)
}

#' Extract Survival Quantities
#'
#' Utility function to extract generated quantities from a [cmdstanr::CmdStanGQ] object.
#' Multiple quantities are generated by default so this is a convenience function to extract
#' the desired ones and return them them as a user friendly [posterior::draws_matrix] object
#'
#' @param gq (`CmdStanGQ`) \cr a [cmdstanr::CmdStanGQ] object created by [generateQuantities()].
#' @param type (`character`)\cr quantity to be generated.
#' Must be one of `surv`, `haz`, `loghaz`, `cumhaz`, `lm_identity`.
#' @keywords internal
extract_quantities <- function(gq, type = c("surv", "haz", "loghaz", "cumhaz", "lm_identity")) {
    type <- match.arg(type)
    assert_class(gq, "CmdStanGQ")
    meta <- switch(type,
        surv = list("log_surv_fit_at_time_grid", exp),
        cumhaz = list("log_surv_fit_at_time_grid", \(x) -x),
        haz = list("log_haz_fit_at_time_grid", exp),
        loghaz = list("log_haz_fit_at_time_grid", identity),
        lm_identity = list("y_fit_at_time_grid", identity)
    )
    result <- gq$draws(meta[[1]], format = "draws_matrix")
    result_transformed <- meta[[2]](result)
    cnames <- colnames(result_transformed)
    colnames(result_transformed) <- gsub(meta[[1]], "quantity", cnames)
    result_transformed
}


#' `Quantities` -> Printable `Character`
#'
#' Converts [`Quantities`] object into a printable string.
#' @inheritParams Quantities-Shared
#' @family Quantities
#' @keywords internal
#' @export
as_print_string.Quantities <- function(object, indent = 1, ...) {
    template <- c(
        "Quantities Object:",
        "    # of samples    = %d",
        "    # of quantities = %d"
    )
    pad <- rep(" ", indent) |> paste(collapse = "")
    template_padded <- paste(pad, template)
    sprintf(
        paste(template_padded, collapse = "\n"),
        nrow(object),
        ncol(object)
    )
}


#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "Quantities",
    definition = function(object) {
        string <- as_print_string(object)
        cat("\n", string, "\n\n")
    }
)
