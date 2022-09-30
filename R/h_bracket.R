
#' h_bracket
#'
#' Generic function that creates a character string oriented by curly brackets
#'
#' @param x An Object
#' @export
h_bracket <- function(x) {
    if (any(nchar(x) >= 1, length(x) > 1)) {
        str <- paste0(
            "{\n",
            paste0("    ", str_map(x), collapse = "\n"),
            "\n}\n"
        )
    } else {
        str <- NULL
    }
    return(str)
}


#' Function removes spaces and add 4 spaces after change lines
#' @export
str_map <- function(i) {
    i <- gsub("^\\s*", "", i)
    i <- gsub("\n\\s*", "\n    ", i)
    print(i)
}
