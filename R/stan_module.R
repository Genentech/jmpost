
STAN_BLOCKS <- list(
    functions = "functions",
    data = "data",
    transformed_data = "transformed data",
    parameters = "parameters",
    transformed_parameters = "transformed parameters",
    model = "model",
    generated_quantities = "generated quantities"
)



# TODO - print method for stanmodule


# TODO - Documentation
#' StanAll class object
.StanModule <- setClass(
    Class = "StanModule",
    slots = list(
        functions = "character",
        data = "character",
        transformed_data = "character",
        parameters = "character",
        transformed_parameters = "character",
        model = "character",
        generated_quantities = "character",
        priors = "list",
        inits = "list"
    )
)


#' @export
StanModule <- function(
    x = "",
    priors = list(),
    inits = list(),
    ...
) {
    assert_that(
        is.character(x),
        length(x) == 1,
        msg = "`x` must be either a length 1 character vector"
    )
    code <- read_stan(x)
    code_fragments <- as_stan_fragments(code)
    .StanModule(
        functions = code_fragments$functions,
        data = code_fragments$data,
        transformed_data = code_fragments$transformed_data,
        parameters =  code_fragments$parameters,
        transformed_parameters = code_fragments$transformed_parameters,
        model = code_fragments$model,
        generated_quantities = code_fragments$generated_quantities,
        priors = priors,
        inits = inits,
        ...
    )
}



# TODO - Update validity
# setValidity(
#     Class = "StanModule",
#     function(object) {
#         msg <- NULL
#         priors <- object@priors
#         priors_names <- names(object@priors)
#         priors_names <- priors_names[priors_names != "" & !is.na(priors_names)]
#         if (length(priors) != length(priors_names)) {
#             msg <- c(msg, "`Priors` must have names")
#         }
#         return(msg)
#     }
# )






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
        as_stan_file(
            functions = x@functions,
            data = x@data,
            transformed_data = x@transformed_data,
            parameters = x@parameters,
            transformed_parameters = x@transformed_parameters,
            model = x@model,
            generated_quantities = x@generated_quantities
        )
    }
)


#' @export
setMethod(
    f = "merge",
    signature = c("StanModule", "StanModule"),
    definition = function(x, y, ...) {
        stan_fragments <- lapply(
            names(STAN_BLOCKS),
            \(par) {
                new_string <- c(slot(x, par), slot(y, par))
                if (all(new_string == "")) {
                    return("")
                }
                return(new_string)
            }
        )
        names(stan_fragments) <- names(STAN_BLOCKS)
        stan_code <- do.call(as_stan_file, stan_fragments)
        StanModule(
            x = stan_code,
            priors = append(x@priors, y@priors),
            inits = append(x@inits, y@inits)
        )
    }
)





#' @export
setMethod(
    f = "show",
    signature = "StanModule",
    definition = function(object) {
        print("StanModule Object")
    }
)




#' @export
setMethod(
    f = "as.list",
    signature = "StanModule",
    definition = function(x) {
        string <- as.character(x)
        li <- as_stan_fragments(string)
        for (block in names(STAN_BLOCKS)) {
            li[[block]] <- paste(li[[block]], collapse = "\n")
        }
        return(li)
    }
)


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
    if (nchar(filename) > 1000) {
        return(FALSE)
    }
    if (is.na(filename)) {
        return(FALSE)
    }
    return(file.exists(filename) & !dir.exists(filename))
}




#' read_stan returns stan code as a character.
#' TODO - Rework this, users shouldn't need to know about our inst-file structures
#'
#' @param string Character, either the absolute path of a stan file, or the name of the stan
#' file in the package directory or the stan code as a string.
#' @export
read_stan <- function(string) {
    # TODO - Make this only apply if in development environment
    local_inst_file <- file.path("inst", "stan", string)
    system_file <- system.file(file.path("stan", string), package = "jmpost")
    local_file <- string
    files <- c(local_file, local_inst_file, system_file)
    for (fi in files) {
        if (is_file(fi)) {
            return(readLines(fi))
        }
    }
    return(string)
}



# TODO - Documentation
as_stan_file <- function(
    functions = "",
    data = "",
    transformed_data = "",
    parameters = "",
    transformed_parameters = "",
    model = "",
    generated_quantities = ""
) {
    block_strings <- lapply(
        names(STAN_BLOCKS),
        function(id) {
            char <- get(id)
            if (any(nchar(char) >= 1)) {
                return(sprintf("%s {\n%s\n}\n\n", STAN_BLOCKS[[id]], paste(char, collapse = "\n")))
            } else {
                return("")
            }
        }
    )
    return(paste0(block_strings, collapse = ""))
}




# TODO - Documentation
# TODO - Check if R stan provides any parsers
# Function only works if code is in format 
# ```
# data {
#     <code>
# }
# model {
#     <code>
# }
# ```
# That is to say we do not support code in inline format i.e.
# ```
# data { <code> }
# model { <code> }
# ```
#' @import stringr
as_stan_fragments <- function(x) {
    code <- unlist(stringr::str_split(x, "\n"))
    results <- list()
    target <- NULL
    for (line in code) {
        for (block in names(STAN_BLOCKS)) {
            regex <- sprintf("^%s *\\{ *$", STAN_BLOCKS[[block]])
            if (stringr::str_detect(line, regex)) {
                target <- block
                line <- NULL
                break
            }
        }
        if(!is.null(target)) {
            results[[target]] <- c(results[[target]], line)
        }
    }
    
    # Remove trailing "}"
    for (block in names(results)) {
        block_length <- length(results[[block]])
        entry <- block_length
        while (entry >= 0) {
            line <- results[[block]][[entry]]
            if (stringr::str_detect(line, "^ *\\} *$")) {
                break
            }
            entry <- entry - 1
        }
        results[[block]] <- results[[block]][-seq(entry, block_length)]
    }
    
    # Add missings
    for (block in names(STAN_BLOCKS)) {
        if(is.null(results[[block]])) {
            results[[block]] <- ""
        }
    }
    return(results)
}


