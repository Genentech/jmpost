#' StanAll class object
#' @slot functions Character, the functions part of a stan model.
#' @slot data Character, the data part of a stan model.
#' @slot parameters Character, the parameters part of a stan model.
#' @slot transformed_parameters Character, the transformed_parameters part of a stan model.
#' @slot model Character, the model part of a stan model.
#' @slot priors List, the priors part of a stan model.
#' @slot generated_quantities Character, the generated_quantities part of a stan model.
#' @slot includes Character
#' @slot inits List with the initial values of the stan model.
#' @exportClass StanModule
StanModule <- setClass(
    "StanModule",
    representation(
        functions = "character",
        data = "character",
        parameters = "character",
        transformed_parameters = "character",
        priors = "list",
        generated_quantities = "character",
        inits = "list"
    )
)


#' read_stan returns stan code as a character.
#'
#' @param file Character, either the absolute path of a stan file, or the name of the stan
#' file in the package directory or the stan code as a string.
#' @importFrom fs is_file
#' @export
read_stan <- function(string) {
    system_file <- system.file("stanparts", string, package = "jmpost")
    if (file.exists(string)) {
        out <- readLines(string)
    } else if (file.exists(system_file) & !dir.exists(system_file)) {
        out <- readLines(system_file)
    } else {
        out <- string
    }
    return(out)
}


#' @importFrom assertthat assert_that
#' @importFrom assertthat validate_that
#' @rdname StanModule-class
#' @export
setMethod(
    f = "initialize",
    signature = "StanModule",
    definition = function(.Object,
                          ...,
                          functions = "",
                          data = "",
                          parameters = "",
                          transformed_parameters = "",
                          generated_quantities = "",
                          priors = list(),
                          inits = list()) {
        assert_that(
            is.character(functions),
            is.character(data),
            is.character(parameters),
            is.character(transformed_parameters),
            is.character(generated_quantities),
            msg = paste(
                "`Functions`, `data`, `parameters`, `transformed_parameters` and",
                "`generated_quantities` must be character vectors"
            )
        )

        assert_that(
            is.list(priors),
            is.list(inits),
            msg = "`Priors` and `inits` must be lists"
        )


        callNextMethod(
            .Object,
            ...,
            functions = vapply(
                X = functions,
                FUN = read_stan,
                FUN.VALUE = character(1),
                USE.NAMES = FALSE
            ),
            data = vapply(
                X = data,
                FUN = read_stan,
                FUN.VALUE = character(1),
                USE.NAMES = FALSE
            ),
            parameters = vapply(
                X = parameters,
                FUN = read_stan,
                FUN.VALUE = character(1),
                USE.NAMES = FALSE
            ),
            transformed_parameters = vapply(
                X = transformed_parameters,
                FUN = read_stan,
                FUN.VALUE = character(1),
                USE.NAMES = FALSE
            ),
            generated_quantities = vapply(
                X = generated_quantities,
                FUN = read_stan,
                FUN.VALUE = character(1),
                USE.NAMES = FALSE
            ),
            priors = priors,
            inits = inits
        )
    }
)


#' Convert a StanModule object into stan code
#'
#' Collapses a StanModule object down into a single string inserting the required block
#' fences
#' i.e. `data { ... }`
#'
#' @param x A `StanModule` object
#' @export
setMethod(
    f = "as.character",
    signature = "StanModule",
    definition = function(x) {
        block_map <- list(
            functions = "functions",
            data = "data",
            parameters = "parameters",
            transformed_parameters = "transformed parameters",
            priors = "model",
            generated_quantities = "generated quantities"
        )

        block_strings <- lapply(
            names(block_map),
            function(id) {
                char <- slot(x, id)
                if (!is.character(char) || length(char) > 1) {
                    char <- paste0(char, collapse = "\n")
                }
                if (nchar(char) >= 1) {
                    return(sprintf("\n%s {\n%s\n}\n", block_map[[id]], char))
                } else {
                    return("")
                }
            }
        )
        return(paste0(block_strings, collapse = ""))
    }
)




#' @rdname merge
#' @export
setMethod(
    f = "merge",
    signature = c("StanModule", "StanModule"),
    definition = function(x, y) {
        pars <- c(
            "functions", "data", "parameters",
            "transformed_parameters", "generated_quantities"
        )

        args <- lapply(
            pars,
            function(par) remove_blank_strings(c(slot(x, par), slot(y, par)))
        )

        names(args) <- pars

        args$priors <- append(x@priors, y@priors)
        args$inits <- append(x@inits, y@inits)

        do.call(StanModule, args)
    }
)


#' Removes blank strings from a string vector
#'
#' Function removes blank strings from a string vector
#' If all strings are blank then it will just return a single blank
#'
#' @param x a vector of string
remove_blank_strings <- function(x) {
    if (all(x == "")) {
        return("")
    }
    return(x[!x == ""])
}
