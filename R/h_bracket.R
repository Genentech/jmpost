#' @rdname h_bracket
#' @export
setMethod(
    f = "h_bracket",
    signature = "character",
    definition = function(x) {
       if(nchar(x) >= 1) paste0("{\n", paste0(x, collapse = "\n"), "\n}\n")
    }
)

#' @rdname h_bracket
#' @export
setMethod(
    f = "h_bracket",
    signature = "list",
    definition = function(x) {
        model_text <- paste0(x, collapse = "\n")
        paste0( model_text, "\n", "target+=sum(log_lik);\n")

    }
)
