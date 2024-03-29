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
remove_missing_rows <- function(data, formula, extra_vars = NULL) {
    if (!is.null(extra_vars)) {
        extra_vars <- paste(extra_vars, collapse = " + ")
        formula_update_string <- paste0(". ~ . + ", extra_vars)
        formula <- stats::update(formula, formula_update_string)
    }

    missing_rows <- get_missing_rownumbers(data, formula)

    if (length(missing_rows) == 0) {
        return(data)
    }
    message(sprintf(
        "Note that %d observations were removed as one of more required variables contained missing values",
        length(missing_rows)
    ))
    data_reduced <- data[-missing_rows, ]
    rownames(data_reduced) <- NULL
    data_reduced
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
#' @keywords internal
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
    time_grid
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



#' Decompose Patients into Relevant Components
#'
#' This function takes in a character vector or list of patients and decomposes it into a
#' structured format.
#'
#' The primary use of this function is to correctly setup indexing variables for
#' predicting survival quantities (see [SurvivalQuantities()])
#'
#' @param patients (`character` or `list`)\cr patient identifiers. If `NULL` will be set to `all_pts`.
#'
#' @param all_pts (`character`)\cr the set of allowable patient identifiers.
#' Will cause an error if any value of `patients` is not in this vector.
#'
#' @return A list containing three components:
#' - `groups`: (`list`)\cr each element of the list is a character vector
#' specifying which patients belong to a given "group" where the "group" is the element name
#' - `unique_values`: (`character`)\cr vector of the unique patients within `patients`
#' - `indexes`: (`list`)\cr each element is a named and is a numeric index vector
#' that maps the values of `grouped` to `unique_values`
#' @examples
#' \dontrun{
#' result <- decompose_patients(c("A", "B"), c("A", "B", "C", "D"))
#' result <- decompose_patients(
#'     list("g1" = c("A", "B"), "g2" = c("B", "C")),
#'     c("A", "B", "C", "D")
#' )
#' }
#' @seealso [expand_patients()], [SurvivalQuantities()]
#' @keywords internal
decompose_patients <- function(patients, all_pts) {
    if (is.character(patients) || is.null(patients)) {
        patients <- expand_patients(patients, all_pts)
        names(patients) <- patients
        patients <- as.list(patients)
    }
    patients <- lapply(
        patients,
        expand_patients,
        all_pts = all_pts
    )
    assert_that(
        is.list(patients),
        length(unique(names(patients))) == length(patients),
        all(vapply(patients, is.character, logical(1)))
    )
    patients_vec_unordered <- unique(unlist(patients))
    patients_vec <- patients_vec_unordered[order(patients_vec_unordered)]
    patients_lookup <- stats::setNames(seq_along(patients_vec), patients_vec)
    patients_index <- lapply(
        patients,
        \(x) {
            z <- patients_lookup[x]
            names(z) <- NULL
            z
        }
    )
    list(
        groups = patients,
        unique_values = patients_vec,
        indexes = patients_index
    )
}

is_cmdstanr_available <- function() {
    requireNamespace("cmdstanr", quietly = TRUE)
}
