
#' h_bracket
#'
#' Generic function that creates a character string oriented by curly brackets
#'
#' @param x An Object
#' @export
h_bracket <- function(x) {
    if (any(nchar(x) >= 1, length(x) > 1)) {
        paste0(
            "{\n",
            paste0(x, collapse = "\n"),
            "\n}\n"
        )
    }
}
