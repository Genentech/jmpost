#' `get_missing_rownumbers`
#'
#' Returns which row numbers contain at least 1 missing observation any variables
#'
#' @param df A data.frame
#' @param formula A formula specifying which variables to inspect for missingness, if null
#' all variables are considered
#'
#' @keywords internal
get_missing_rownumbers <- function(df, formula = NULL) {
    if (is.null(formula)) {
        formula <- ~ .
    }
    mdf <- model.frame(formula, data = df, na.action = na.pass)
    missing_rows <- which(!complete.cases(mdf))
    return(missing_rows)
}


#' `remove_missing_rows`
#'
#' Removes any rows from a dataset that contain missing values. Allows users to specify which variables
#' to inspect for missing values based on either a formula or a character vector of variable names.
#'
#' @param data A data.frame
#' @param formula A formula specifying which variables to inspect for missingness
#' @param extra_vars A character vector specifying which variables to inspect for missingness
#'
#' @returns `data` after removing observations that contain missing values in the required variables
#' Note that additional variables not listed in `formula` or `extra_vars` are not dropped and may
#' still contain missing values
#'
#' @keywords internal
remove_missing_rows <- function(data, formula, extra_vars) {
    extra_vars <- paste(extra_vars, collapse = " + ")
    formula_update_string <- paste0(". ~ . + ", extra_vars)
    formula_update <- update(formula, formula_update_string)

    missing_rows <- get_missing_rownumbers(data, formula_update)

    if (length(missing_rows) == 0 ) {
        return(data)
    }
    message(sprintf(
        "Note that %d observations were removed as one of more required variables contained missing values",
        length(missing_rows)
    ))
    return(data[-missing_rows, ])
}





#' Replicate single values in a list based on specified sizes
#'
#' This function takes a list of initial values and a corresponding list of sizes
#' and replicates any single values in the initial values list according to the
#' corresponding values in the sizes list. The resulting list has the same names
#' as the original lists.
#'
#' @param initial_values A named list of initial values.
#' @param sizes A named list of sizes, where each size corresponds to an element in initial_values.
#'
#' @return A named list of values, with any single values in the initial_values list
#' replicated according to the corresponding values in the sizes list.
#'
#' @keywords internal
expand_initial_values <- function(initial_values, sizes){
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

    # Check for single values in initial_values and replicate them according to sizes
    for (name in names(initial_values)) {
        if (length(initial_values[[name]]) == 1) {
            initial_values[[name]] <- rep(initial_values[[name]], sizes[[name]])
        }
    }

    # Check that each element of initial_values has the same length as specified in sizes
    for (name in names(initial_values)) {
        assert_that(
            length(initial_values[[name]]) == sizes[[name]],
            msg = "length of element in `initial_values` does not match specified size"
        )
    }

    return(initial_values)
}



#' replace_with_lookup
#'
#' This function takes a list of sizes and a list of data and returns a modified list
#' of sizes where each character element is replaced by the corresponding numeric value
#' from the data list. Each element of size must be a length one numeric after the lookup
#'
#' @param sizes A list of sizes, which may include character elements that correspond to
#' names in the data list.
#' @param data A list of data containing numeric values.
#'
#' @return A list of sizes with character elements replaced by their corresponding
#' numeric values.
#'
#' @keywords internal
replace_with_lookup <- function(sizes, data) {

    assert_that( is.list(sizes), msg = "`sizes` must be a list")
    assert_that( is.list(data), msg = "`data` must be a list")

    for ( idx in seq_along(sizes)) {
        val <- sizes[[idx]]
        if (is.character(val)) {
            assert_that(
                length(val) == 1,
                msg = "character elements of `sizes` must be length 1"
            )
            assert_that(
                val %in% names(data),
                msg = sprintf("`%s` is not available in `data`")
            )
            new_val <- data[[val]]
            assert_that(
                length(new_val) == 1,
                is.numeric(new_val),
                msg = "Selected values from data must be length 1 numerics"
            )
            sizes[[idx]] <- new_val
        }

        assert_that(
            is.numeric(sizes[[idx]]),
            length(sizes[[idx]]) == 1,
            msg = "All elements of `sizes` must be length 1 numerics after lookup"
        )
    }
    return(sizes)
}

#' Obtain Median and Credible Intervals from MCMC samples
#'
#' @param samples (`matrix`)\cr with samples in rows and parameters in columns.
#' @param level (`number`)\cr credibility level to use for the credible intervals.
#'
#' @return A `data.frame` with columns `median`, `lower` and `upper`.
#' @export
#'
#' @examples
#' set.seed(123)
#' samples <- cbind(rnorm(100, 0, 1), rexp(100, 0.5), rpois(100, 5))
#' samples_median_ci(samples)
samples_median_ci <- function(samples, level = 0.95) {
    assert_that(is.matrix(samples))
    assert_that(is.number(level), level < 1, level > 0)

    samples_median <- apply(samples, MARGIN = 2L, FUN = median)
    probs <- c((1 - level) / 2, (1 + level) / 2)
    samples_ci <- t(apply(samples, MARGIN = 2L, FUN = quantile, probs = probs))
    colnames(samples_ci) <- c("lower", "upper")
    as.data.frame(cbind(
        median = samples_median,
        samples_ci
    ))
}
