#' Row Numbers of Data with Missing Variables
#'
#' @param df (`data.frame`)\cr input data.
#' @param formula (`formula` or `NULL`)\cr which variables to inspect for missingness, if `NULL`
#'   all variables are considered.
#'
#' @returns Numeric vector specifying which rows contain at least 1 missing observation
#'   in any of the inspected variables.
#'
#' @keywords internal
get_missing_rownumbers <- function(df, formula = NULL) {
    if (is.null(formula)) {
        formula <- ~ .
    }
    mdf <- stats::model.frame(formula, data = df, na.action = stats::na.pass)
    which(!stats::complete.cases(mdf))
}

#' Remove Rows with Missing Variables
#'
#' Removes any rows from a data set that contain missing values in the inspected
#' variables. Allows users to specify which variables to inspect for missing values
#' based on either a formula or a character vector of variable names.
#'
#' @param data (`data.frame`)\cr input data.
#' @param formula (`formula` or `NULL`)\cr which variables to inspect for missingness.
#' @param extra_vars (`character`)\cr additional variables to inspect for missingness.
#'
#' @returns The `data` after removing observations that contain missing values in the required variables.
#'   Note that additional variables not listed in `formula` or `extra_vars` are not dropped and may
#'   still contain missing values.
#'
#' @keywords internal
remove_missing_rows <- function(data, formula, extra_vars) {
    extra_vars <- paste(extra_vars, collapse = " + ")
    formula_update_string <- paste0(". ~ . + ", extra_vars)
    formula_update <- stats::update(formula, formula_update_string)

    missing_rows <- get_missing_rownumbers(data, formula_update)

    if (length(missing_rows) == 0) {
        return(data)
    }
    message(sprintf(
        "Note that %d observations were removed as one of more required variables contained missing values",
        length(missing_rows)
    ))
    data[-missing_rows, ]
}

#' Replicate Single Values in a List
#'
#' @param initial_values (`list`)\cr initial values with names.
#' @param sizes (`list`)\cr each size corresponds to an element in `initial_values`,
#'   matched by the names. An attribute `array` must be attached to each element,
#'   see [replace_with_lookup()].
#'
#' @returns A named list of values, with any single values in the `initial_values` list
#'   replicated according to the corresponding values in the `sizes` list.
#'   Even when the size is 1, the value is passed as an `array` if the corresponding
#'   attribute is `TRUE` in `sizes`.
#'
#' @note The resulting list has the same names as the original lists.
#'
#' @keywords internal
expand_initial_values <- function(initial_values, sizes) {
    assert_that(
        is.list(initial_values),
        msg = "`initial_values` must be a list"
    )
    assert_that(
        is.list(sizes),
        msg = "`sizes` must be a list"
    )
    assert_that(
        all(names(sizes) %in% names(initial_values)),
        all(names(initial_values) %in% names(sizes)),
        msg = "`initial_values` and `sizes` must have identical names"
    )

    for (name in names(initial_values)) {
        # Check for single values and replicate them according to sizes.
        if (length(initial_values[[name]]) == 1) {
            initial_values[[name]] <- rep(initial_values[[name]], sizes[[name]])
        }
        # Check for array handling.
        needs_array <- attr(sizes[[name]], "array")
        assert_that(
            is.flag(needs_array),
            msg = "each sizes element must have array flag attribute"
        )
        if (needs_array) {
            initial_values[[name]] <- as.array(initial_values[[name]])
        }
    }

    # Check that each element of initial_values has the same length as specified in sizes.
    for (name in names(initial_values)) {
        assert_that(
            length(initial_values[[name]]) == sizes[[name]],
            msg = "length of element in `initial_values` does not match specified size"
        )
    }

    initial_values
}

#' Replace Character Size by Looked Up Numbers
#'
#' @param sizes (`list`)\cr may include character elements that correspond to
#'   names in the data list.
#' @param data (`list`)\cr data containing numeric values.
#'
#' @returns A list of sizes with character elements in `sizes`
#'   replaced by their corresponding numeric values in `data`.
#'
#' @details An attribute `array` for each returned list element indicates
#'   whether the parameter needs to be handled
#'   as an array. This is the case when the size is larger than 1, or when
#'   the size was looked up in the `data`, because in that case it is flexible
#'   and hence is handled as an array in the Stan code.
#'
#' @note Each element in the final list of sizes must be a single number.
#'
#' @keywords internal
replace_with_lookup <- function(sizes, data) {

    assert_that(is.list(sizes), msg = "`sizes` must be a list")
    assert_that(is.list(data), msg = "`data` must be a list")

    for (idx in seq_along(sizes)) {
        val <- sizes[[idx]]
        if (is.character(val)) {
            assert_that(
                length(val) == 1,
                msg = "character elements of `sizes` must be strings"
            )
            assert_that(
                val %in% names(data),
                msg = sprintf("`%s` is not available in `data`", val)
            )
            new_val <- data[[val]]
            assert_that(
                is.number(new_val),
                msg = "Selected values from data must be single numbers"
            )
            sizes[[idx]] <- structure(new_val, array = TRUE)
        } else {
            assert_that(
                is.number(val),
                msg = "Existing values in sizes must be single numbers"
            )
            sizes[[idx]] <- structure(val, array = val > 1)
        }
    }
    sizes
}

#' Obtain Median and Credible Intervals from MCMC samples
#'
#' @param samples (`matrix`)\cr with samples in rows and parameters in columns.
#' @param level (`number`)\cr credibility level to use for the credible intervals.
#'
#' @returns A `data.frame` with columns `median`, `lower` and `upper`.
#' @export
#'
#' @examples
#' set.seed(123)
#' samples <- cbind(rnorm(100, 0, 1), rexp(100, 0.5), rpois(100, 5))
#' samples_median_ci(samples)
samples_median_ci <- function(samples, level = 0.95) {
    assert_that(is.matrix(samples))
    assert_that(is.number(level), level < 1, level > 0)

    samples_median <- apply(samples, MARGIN = 2L, FUN = stats::median)
    probs <- c((1 - level) / 2, (1 + level) / 2)
    samples_ci <- t(apply(samples, MARGIN = 2L, FUN = stats::quantile, probs = probs))
    colnames(samples_ci) <- c("lower", "upper")
    as.data.frame(cbind(
        median = samples_median,
        samples_ci
    ))
}

#' `pt_2_factor`
#'
#' Converts subject identifiers to factors.
#' If `pt` is already a factor it will re-level it to ensure the levels are in alphabetical order.
#' This is to ensure that [`DataLongitudinal`] and [`DataSurvival`] use identical index numbers
#' for the subjects to ensure data alignment in the joint model.
#'
#' @param pt (`character` or `factor`)\cr subject identifiers.
#'
#' @returns A `factor` with the levels in alphabetical order.
#'
#' @keywords internal
pt_2_factor <- function(pt) {
    pt_char <- as.character(pt)
    pt_uniq <- unique(pt_char)
    pt_uniq_ord <- pt_uniq[order(pt_uniq)]
    factor(pt_char, levels = pt_uniq_ord)
}



#' `decorated_render`
#'
#' Simple wrapper around [jinjar::render()] that provides some additional default
#' variables about the system (avoids each call to jinjar having to specify them)
#' @param ... Arguments passed onto [jinjar::render()]
#' @returns See [jinjar::render()]
#' @keywords internal
decorated_render <- function(...) {
    jinjar::render(
        ...,
        machine_double_eps = sqrt(.Machine$double.eps),
        machine_double_neg_eps = sqrt(.Machine$double.neg.eps)
    )
}


is_windows <- function() {
    sysname <- Sys.info()["sysname"]
    return(sysname == "Windows")
}

#' `expand_time_grid`
#'
#' This function expands a given time grid by setting a default grid if one hasn't been provided
#' and then verifying it's properties.
#' The grid must be finite, sorted, and contain unique values.
#'
#' @param time_grid (`numeric` or `NULL`)\cr A vector of times which quantities will be
#' evaluated at. If NULL, a default grid will be created as a length 201 vector spanning
#' from 0 to `max_time`.
#' @param max_time (`numeric``)\cr Specifies the maximum time to be used in creating the default grid.
#' @return Returns the expanded time_grid.
#' @examples
#' expand_time_grid(time_grid = c(0, 5, 10), max_time = 20)
#' expand_time_grid(time_grid = NULL, max_time = 50)
#' @keywords internal
expand_time_grid <- function(time_grid, max_time) {
    default_grid <- seq(from = 0, to = max_time, length = 201)
    if (is.null(time_grid)) {
        time_grid <- default_grid
    }
    assert_that(
        !any(is.na(time_grid)),
        is.numeric(time_grid),
        !is.null(time_grid),
        !is.unsorted(time_grid),
        !any(duplicated(time_grid)),
        all(is.finite(time_grid)),
        msg = "`time_grid` needs to be finite, sorted, unique valued numeric vector"
    )
    return(time_grid)
}



#' `expand_patients`
#'
#' This function checks and expands a given patients vector.
#' The input vector must be unique and contain only values
#' as specified by `all_pts`
#'
#' @param patients (`character` or `NULL`)\cr Character vector representing the patients.
#' If NULL, it will be set to the value of `all_pts`.
#' @param all_pts (`character`)\cr Character vector representing all possible patients.
#' @return Returns the expanded `patients` vector.
#' @examples
#' expand_patients(
#'     patients = c("patient1", "patient2"),
#'     all_pts = c("patient1", "patient2", "patient3")
#' )
#' expand_patients(
#'     patients = NULL,
#'     all_pts = c("patient1", "patient2", "patient3")
#' )
#' @keywords internal
expand_patients <- function(patients, all_pts) {
    assert_that(
        is.character(all_pts),
        msg = "`all_pts` must be a character vector"
    )
    if (is.null(patients)) {
        patients <- unique(all_pts)
    }
    assert_that(
        is.character(patients),
        all(patients %in% all_pts),
        !any(duplicated(patients)),
        msg = "`patients` should be a unique character vector containing only values from the original df"
    )
    return(patients)
}
