#' @include generics.R
NULL

# STAN_BLOCKS ----

STAN_BLOCKS <- list(
    functions = "functions",
    data = "data",
    transformed_data = "transformed data",
    parameters = "parameters",
    transformed_parameters = "transformed parameters",
    model = "model",
    generated_quantities = "generated quantities"
)

# add_missing_stan_blocks ----

#' Add Missing Stan Blocks
#'
#' @param x (`list`)\cr list of Stan code blocks
#'
#' @return Amended list `x` such that all blocks in the global variable
#'   `STAN_BLOCKS` are contained.
#'
#' @keywords internal
add_missing_stan_blocks <- function(x) {
    # STAN_BLOCKS is defined as a global variable in StanModule.R
    # TODO - Make it an argument to the function
    for (block in names(STAN_BLOCKS)) {
        if (is.null(x[[block]])) {
            x[[block]] <- ""
        }
    }
    return(x)
}

# StanModule-class ----

#' `StanModule`
#'
#' @slot functions (`character`)\cr the `functions` block.
#' @slot data (`character`)\cr the `data` block.
#' @slot transformed_data (`character`)\cr the `transformed_data` block.
#' @slot parameters (`character`)\cr the `parameters` block.
#' @slot transformed_parameters (`character`)\cr the `transformed_parameters` block.
#' @slot model (`character`)\cr the `model` block.
#' @slot generated_quantities (`character`)\cr the `generated_quantities` block.
#' @slot priors (`list`)\cr the prior specifications.
#' @slot inits (`list`)\cr the initial values.
#'
#' @exportClass StanModule
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

# StanModule-constructors ----

#' @rdname StanModule-class
#'
#' @param x (`string`)\cr file name of the Stan code which should be parsed.
#' @param priors (`list`)\cr the prior specifications.
#' @param inits (`list`)\cr the initial values.
#' @param ... additional arguments passed to the constructor.
#'
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
        msg = "`x` must be a length 1 character vector"
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

# as.character-StanModule ----

#' @rdname as.character
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

# merge-StanModule,StanModule ----

#' @rdname merge
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

# compileStanModel-StanModule,character ----

#' @rdname compileStanModel
setMethod(
    f = "compileStanModel",
    signature = signature(object = "StanModule", exe_file = "character"),
    definition = function(object, exe_file) {
        if (!dir.exists(dirname(exe_file))) {
            dir.create(dirname(exe_file), recursive = TRUE)
        }
        x <- cmdstanr::cmdstan_model(
            stan_file = cmdstanr::write_stan_file(as.character(object)),
            exe_file = exe_file
        )
        invisible(x)
    }
)

# compileStanModel-StanModule,empty ----

#' @rdname compileStanModel
setMethod(
    f = "compileStanModel",
    signature = signature(object = "StanModule", exe_file = "empty"),
    definition = function(object, exe_file) {
        exe_file <- file.path(tempdir(), "model")
        invisible(compileStanModel(object, exe_file))
    }
)

# show-StanModule ----

#' @rdname show
setMethod(
    f = "show",
    signature = "StanModule",
    definition = function(object) {
        print("StanModule Object")
    }
)

# as.list-StanModule ----

#' @rdname as.list
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

# is_file ----

#' Is String a Valid File?
#'
#' A utility function to check if a string is a valid file or not.
#' Used to help address short comings of [file.exists()] that will return `TRUE`
#' for a directory as well as a file.
#'
#' @param filename (`string`)\cr file name.
#'
#' @keywords internal
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

# read_stan ----

#' Stan Code as Character
#'
#' @param string Character, either the absolute path of a stan file, or the name of the stan
#' file in the package directory or the stan code as a string.
#'
#' @export
read_stan <- function(string) {
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

# as_stan_file ----

#' Merging Code Blocks into Stan Code Character Vector
#'
#' @param functions (`character`)\cr code block.
#' @param data (`character`)\cr code block.
#' @param transformed_data (`character`)\cr code block.
#' @param parameters (`character`)\cr code block.
#' @param transformed_parameters (`character`)\cr code block.
#' @param model (`character`)\cr code block.
#' @param generated_quantities (`character`)\cr code block.
#'
#' @return Character vector of the complete Stan code.
#'
#' @keywords internal
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

# as_stan_fragments ----

#' Conversion of Character Vector into Stan Code Block List
#'
#' @param x (`character`)\cr the single Stan code vector.
#'
#' @return A list with the Stan code blocks.
#'
#' @details
#' Function only works if code is in format
#' ```
#' data {
#'     <code>
#' }
#' model {
#'     <code>
#' }
#' ```
#' That is to say we do not support code in inline format i.e.
#' ```
#' data { <code> }
#' model { <code> }
#' ```
#'
#' @keywords internal
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
        if (!is.null(target)) {
            results[[target]] <- c(results[[target]], line)
        }
    }

    # Remove trailing "}".
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

    # Add missings.
    for (block in names(STAN_BLOCKS)) {
        if (is.null(results[[block]])) {
            results[[block]] <- ""
        }
    }
    results
}
