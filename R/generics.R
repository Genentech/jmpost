

#' Merge
#'
#' Generic function to collapse two similar objects into a single combined object
#'
#' @param x An Object
#' @param y An Object with identical class to `x`
#' @export
setGeneric(
    "merge",
    function(x, y) standardGeneric("merge")
)




#' h_bracket
#'
#' Generic function that creates a character string oriented by curly brackets
#'
#' @param x An Object
#' @export
setGeneric(
    "h_bracket",
    function(x) standardGeneric("h_bracket")
)

