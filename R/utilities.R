


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

