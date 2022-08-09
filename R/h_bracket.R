#' @rdname h_bracket
#' @export
setMethod(
    f = "h_bracket",
    signature = "character",
    definition = function(x) {
        paste0("{\n", x, "\n}\n")
    }
)

#' @rdname h_bracket
#' @export
setMethod(
    f = "h_bracket",
    signature = "list",
    definition = function(x) {
        model_text <- paste0(x, collapse = "\n")
        model_text <- paste0( model_text, "\n", "target+=sum(log_lik);\n")


        paste0("{\n", model_text, "\n}\n")
    }
)
