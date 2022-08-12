#' @rdname h_bracket
#' @export
setMethod(
    f = "h_bracket",
    signature = "character",
    definition = function(x) {
        if (any(nchar(x) >= 1, length(x) > 1)) paste0("{\n", paste0(x, collapse = "\n"), "\n}\n")
    }
)

#' @rdname h_bracket
#' @export
setMethod(
    f = "h_bracket",
    signature = "list",
    definition = function(x) {
        paste0(paste0(x, collapse = "\n"), "\n")
    }
)
