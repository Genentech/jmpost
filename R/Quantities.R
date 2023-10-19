
#' Re-used documentation for `Quantities`
#'
#' @param x ([`Quantities`]) \cr generated quantities.
#' @param object ([`Quantities`]) \cr generated quantities.
#' @param time_grid (`numeric`)\cr sets the `time` variable.
#' Must be equal in length to `ncol(x)`.
#' @param type (`character`)\cr sets the `type` variable.
#' @param groups (`list`)\cr named `list`.
#' The element names are used to set the `group` variable.
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
    contains = "list"
)

#' @param x (`list`)\cr a `list` of `matrix` objects. Each matrix must have the exact
#' same dimensions.
#' @rdname Quantities-class
Quantities <- function(x) {
    .Quantities(x)
}

setValidity(
    Class = "Quantities",
    method = function(object) {
        if (length(object) == 0) {
            return("`Quantities` object should have at least 1 element")
        }
        for (element in object) {
            if (!inherits(element, "matrix")) {
                return("each element a `Quantites` object must be a matrix")
            }
        }
        num_rows <- vapply(object, nrow, numeric(1))
        num_cols <- vapply(object, ncol, numeric(1))

        if (length(unique(num_rows)) != 1 | num_rows[[1]] == 0) {
            return("All elements of a `Quantities` object must have the same non-zero number of rows")
        }
        if (length(unique(num_cols)) != 1 | num_cols[[1]] == 0) {
            return("All elements of a `Quantities` object must have the same non-zero number of columns")
        }
    }
)


#' Dimensions of `Quantitites`
#'
#' Returns the number of rows and columns within any given
#' element of the [Quantities] object. As each element is of the same
#' dimensionality this just returns a single number for the rows and columns
#' of all elements of the [Quantities]
#'
#' @inheritParams Quantities-Shared
#' @keywords internal
#' @export
dim.Quantities <- function(x) {
    num_rows <- vapply(x, nrow, numeric(1))
    num_cols <- vapply(x, ncol, numeric(1))
    u_num_rows <- unique(num_rows)
    u_num_cols <- unique(num_cols)
    assert_number(u_num_cols)
    assert_number(u_num_cols)
    return(c(u_num_rows, u_num_cols))
}


#' `Quantities` -> `data.frame`
#'
#' @inheritParams Quantities-Shared
#'
#' @keywords internal
#' @family Quantities
#' @export
as.data.frame.Quantities <- function(x, ..., time_grid, type, groups) {
    assert_that(length(time_grid) == ncol(x))
    quantities_df <- lapply(
        x,
        \(element) {
            n <- nrow(element)
            values <- as.vector(element)
            times <- rep(time_grid, each = n)
            type <- rep(type, each = length(time_grid) * n)
            assert_that(
                length(values) == length(times),
                length(times) == length(type)
            )
            data.frame(
                values = values,
                time = times,
                type = type,
                stringsAsFactors = FALSE
            )
        }
    )
    for (i in seq_along(quantities_df)) {
        quantities_df[[i]]["group"] <- names(groups)[[i]]
    }
    res <- Reduce(rbind, quantities_df)
    res[, c("values", "time", "group", "type")]
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
summary.Quantities <- function(object, time_grid, type, groups, conf.level = 0.95, ...) {

    assert_string(type)
    assert_that(
        ncol(object) == length(time_grid),
        is.list(groups),
        length(names(groups)) == length(groups),
        all(names(groups) != ""),
        all(!is.null(names(groups)))
    )

    quantities_summarised <- lapply(
        object,
        samples_median_ci,
        level = conf.level
    )

    for (i in seq_along(quantities_summarised)) {
        assert_that(nrow(quantities_summarised[[i]]) == length(time_grid))
        quantities_summarised[[i]][["time"]] <- time_grid
        quantities_summarised[[i]][["group"]] <- names(groups)[[i]]
        quantities_summarised[[i]][["type"]] <- type
    }
    Reduce(rbind, quantities_summarised)
}


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
average_samples_by_index <- function(subject_index, time_index, quantities) {
    assert_that(
        is.numeric(subject_index),
        is.numeric(time_index),
        length(time_index) == length(unique(time_index)),
        inherits(quantities, "draws_matrix")
    )
    stacked_quantities <- array(dim = c(
        nrow(quantities),
        length(time_index),
        length(subject_index)
    ))
    for (ind in seq_along(subject_index)) {
        quantity_index <- sprintf(
            "quantity[%i,%i]",
            subject_index[ind],
            time_index
        )
        stacked_quantities[, , ind] <- quantities[, quantity_index]
    }
    apply(
        stacked_quantities,
        c(1, 2),
        mean,
        simplify = TRUE
    )
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
        "    # of Elements  = %d",
        "    # of Rows      = %d",
        "    # of Columns   = %d"
    )
    pad <- rep(" ", indent) |> paste(collapse = "")
    template_padded <- paste(pad, template)
    sprintf(
        paste(template_padded, collapse = "\n"),
        length(object),
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
