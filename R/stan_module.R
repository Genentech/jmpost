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
#' @export
StanModule <- setClass(
    "StanModule",slots = list(functions = "character",
                              data = "character",
                              parameters = "character",
                              transformed_parameters = "character",
                              model = "character",
                              priors = "list",
                              generated_quantities = "character",
                              inits = "list")

)


#' read_stan returns stan code as a character.
#'
#' @param string Character, either the absolute path of a stan file, or the name of the stan
#' file in the package directory or the stan code as a string.
#' @export
read_stan <- function(string) {
    system_file <- system.file("stanparts", string, package = "jmpost")
    if (is_file(string)) {
        out <- read_file(string)
    } else if (is_file(system_file)) {
        out <- read_file(system_file)
    } else {
        out <- string
    }
    return(out)
}


setValidity("StanModule", function(object) {
    if (length(object@priors) > 0) {
        if (is.null(names(object@priors)) == FALSE | all(names(object@priors) != "")
            ){
            "`Priors` must have names"
            } else {TRUE}
} else{
        TRUE
      }
})


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
                          model = "",
                          generated_quantities = "",
                          priors = list(),
                          inits = list()) {

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
            model = vapply(
                X = model,
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

#' model_prep
#'
#' Populates the model section of a StanModule
#'
#' @param x A StanModule object
#' @export
model_prep <- function(x) {
    if (length(x@priors) > 0) {
        tmp_priors <- as.list(
            paste(names(x@priors), "~", x@priors)
        )

        x@model <- paste0(
            paste0(tmp_priors, collapse = "\n"),
            "\n",
            x@model
        )
    }

    x
}



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
        y <- model_prep(x)

        block_map <- list(
            functions = "functions",
            data = "data",
            parameters = "parameters",
            transformed_parameters = "transformed parameters",
            model = "model",
            generated_quantities = "generated quantities"
        )


        block_strings <- lapply(
            names(block_map),
            function(id) {
                char <- slot(y, id)
                if (any(nchar(char) >= 1, length(char) > 1)) {
                    return(paste(block_map[[id]], h_bracket(char)))
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


#' Read entire file as a single string
#'
#' Simple utility function to read in a file as 1
#' continous string
#'
#' @param filename Location of file to read in
#'
read_file <- function(filename) {
    paste0(readLines(filename), collapse = "\n")
}


#' Is string a valid file
#'
#' A utility function to check if a string is a valid file or not.
#' Used to help address short comings of file.exists that will return TRUE
#' for a directory as well as a file
#'
#' @param filename A character string
#' @importFrom assertthat assert_that
is_file <- function(filename = NULL) {
    if (is.null(filename)) {
        return(FALSE)
    }
    assert_that(
        is.character(filename),
        length(filename) == 1,
        msg = "`filename` must be a length 1 character"
    )
    if (is.na(filename)) {
        return(FALSE)
    }
    return(file.exists(filename) & !dir.exists(filename))
}
