#' @include generics.R
NULL

# STAN_BLOCKS ----

#' List of Stan Blocks
#'
#' @description
#' A list with 1 element per standard Stan program blocks.
#' This object is  mostly used internally as a reference for
#' what blocks are expected to exist within a given Stan program.
#'
#' @export
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
#' @param stan_blocks (`list`)\cr reference list of stan blocks.
#'
#' @return Amended list `x` such that all blocks in the global variable
#' `STAN_BLOCKS` are contained.
#'
#' @keywords internal
add_missing_stan_blocks <- function(x, stan_blocks = STAN_BLOCKS) {
    for (block in names(stan_blocks)) {
        if (is.null(x[[block]])) {
            x[[block]] <- ""
        }
    }
    return(x)
}

# StanModule-class ----

#' `StanModule` Object and Constructor Function
#'
#' @param x (`string`)\cr file path to a Stan program or a character vector
#' of Stan code to be parsed.
#' @param priors (`list`)\cr the prior specifications.
#' @param inits (`list`)\cr the initial values.
#' @param ... additional arguments passed to the constructor.
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
#' @export StanModule
#' @family StanModule
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
        parameters = code_fragments$parameters,
        transformed_parameters = code_fragments$transformed_parameters,
        model = code_fragments$model,
        generated_quantities = code_fragments$generated_quantities,
        priors = priors,
        inits = inits,
        ...
    )
}

# as.character-StanModule ----

#' `StanModule` -> `character`
#' @param x ([`StanModule`])\cr A stan program
#' @param ... Not Used.
#' @description
#' Converts a [`StanModule`] object into a valid Stan program file where each
#' line of the returned `character` vector represents a line of the program
#' @family StanModule
#' @export
as.character.StanModule <- function(x, ...) {
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



# merge-StanModule,StanModule ----

#' @param stan_blocks (`list`)\cr reference list of stan blocks.
#' @rdname merge
setMethod(
    f = "merge",
    signature = c("StanModule", "StanModule"),
    definition = function(x, y, stan_blocks = STAN_BLOCKS, ...) {
        stan_fragments <- lapply(
            names(stan_blocks),
            \(par) {
                new_string <- c(slot(x, par), slot(y, par))
                if (all(new_string == "")) {
                    return("")
                }
                return(new_string)
            }
        )
        names(stan_fragments) <- names(stan_blocks)
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
compileStanModel.StanModule <- function(object) {
    exe_dir <- getOption("jmpost.cache.dir")
    if (!dir.exists(exe_dir)) {
        dir.create(exe_dir, recursive = TRUE)
    }
    stan_code <- as.character(object)
    hash_name <- digest::digest(stan_code, "md5")
    exe_name <- paste0(
        "model_",
        hash_name,
        if (is_windows()) ".exe" else ""
    )
    exe_file <- file.path(exe_dir, exe_name)
    source_file <- cmdstanr::write_stan_file(
        code = stan_code,
        dir = exe_dir,
        basename = sprintf("model_%s.stan", hash_name)
    )

    # Suppress "model executable is up to date" message as
    # users are not in control of the cache so this message is meaningless
    withCallingHandlers(
        {
            x <- cmdstanr::cmdstan_model(
                stan_file = source_file,
                exe_file = exe_file,
                quiet = TRUE
            )
        },
        message = function(m) {
            if (m$message == "Model executable is up to date!\n") {
                invokeRestart("muffleMessage")
            }
        }
    )
    invisible(x)
}



# as.list-StanModule ----

#' `StanModule` -> `list`
#' @description
#' Returns a named list where each element of the list corresponds
#' to a Stan modelling block e.g. `data`, `model`, etc.
#' @param x ([`StanModule`])\cr A Stan Module
#' @param stan_blocks (`list`)\cr reference list of stan blocks.
#' @param ... Not Used.
#' @family StanModule
#' @export
as.list.StanModule <- function(x, stan_blocks = STAN_BLOCKS, ...) {
    string <- as.character(x)
    li <- as_stan_fragments(string)
    for (block in names(stan_blocks)) {
        li[[block]] <- paste(li[[block]], collapse = "\n")
    }
    return(li)
}

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
#' @param stan_blocks (`list`)\cr reference list of stan blocks.
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
    generated_quantities = "",
    stan_blocks = STAN_BLOCKS
) {
    block_strings <- lapply(
        names(stan_blocks),
        function(id) {
            char <- get(id)
            if (any(nchar(char) >= 1)) {
                return(sprintf("%s {\n%s\n}\n\n", stan_blocks[[id]], paste(char, collapse = "\n")))
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
#' @param stan_blocks (`list`)\cr reference list of stan blocks.
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
as_stan_fragments <- function(x, stan_blocks = STAN_BLOCKS) {
    code <- unlist(stringr::str_split(x, "\n"))

    errmsg <- paste(
        "There were problems parsing the `%s` block.",
        "Please consult the `Formatting Stan Files` section of the",
        "`Extending jmpost` vignette"
    )

    # Check to see if any block openings exist that have code on the same line
    # e.g.  `data { int i;}`. This is unsupported so we throw an error
    for (block in stan_blocks) {
        regex <- sprintf("^\\s*%s\\s*\\{\\s*[^\\s-]+", block)
        if (any(grepl(regex, code, perl = TRUE))) {
            stop(sprintf(errmsg, block))
        }
    }

    # We first look to identify the opening of a block e.g.  `data {`
    # We then regard all lines that follow as belonging to that block
    # until we see another block being opened e.g. `model{`
    results <- list()
    target <- NULL
    for (line in code) {
        for (block in names(stan_blocks)) {
            regex <- sprintf("^\\s*%s\\s*\\{\\s*$", stan_blocks[[block]])
            if (stringr::str_detect(line, regex)) {
                target <- block
                line <- NULL
                break
            }
        }
        if (!is.null(target)) {
            # This is memory inefficient but given the relatively small size of
            # stan files its regarded as a acceptable simplification to ease the
            # code burden
            results[[target]] <- c(results[[target]], line)
        }
    }

    # Loop over each block to remove trailing "}".
    for (block in names(results)) {
        block_length <- length(results[[block]])
        # The following processing is only required if the block actually has content
        if (block_length == 1 && results[[block]] == "") {
            next
        }
        has_removed_char <- FALSE
        # Walk backwards to find the closing `}` that corresponds to the `<block> {`
        for (index in rev(seq_len(block_length))) {
            line <- results[[block]][[index]]
            # This code will exit the for loop as soon as it hits the closing `}`
            # thus if we ever see a line that ends in text/numbers it means
            # somethings gone wrong
            if (stringr::str_detect(line, "[\\w\\d]+\\s*$")) {
                stop(sprintf(errmsg, block))
            }
            if (stringr::str_detect(line, "\\}\\s*$")) {
                new_line <- stringr::str_replace(line, "\\s*\\}\\s*$", "")
                # If the line is now blank after removing the closing `}` then drop the line
                keep_offset <- if (nchar(new_line) == 0) -1 else 0
                # Only keep lines from the start of the block to the closing `}`
                # this is to ensure we drop blank lines that were between the end
                # of the block and the start of the next
                keep_range <- seq_len(index + keep_offset)
                results[[block]][[index]] <- new_line
                results[[block]] <- results[[block]][keep_range]
                has_removed_char <- TRUE
                break
            }
        }
        # If we haven't actually removed a closing `}` then something has gone wrong...
        if (!has_removed_char) {
            stop(sprintf(errmsg, block))
        }
    }

    # Add any missing blocks back in
    for (block in names(stan_blocks)) {
        if (is.null(results[[block]])) {
            results[[block]] <- ""
        }
    }
    results
}

#' `StanModule` -> Printable `Character`
#'
#' Converts [`StanModule`] object into a printable string.
#' @param object ([`StanModule`])\cr A stan program
#' @family StanModule
#' @param indent (`numeric`)\cr how much white space to prefix the print string with.
#' @keywords internal
#' @export
as_print_string.StanModule <- function(object, indent = 1, ...) {
    slots <- getSlots("StanModule")
    slots <- slots[!names(slots) %in% c("priors", "inits")]
    components <- Filter(\(block) all(slot(object, block) != ""), names(slots))

    template <- c(
        "StanModule Object with components:",
        paste("    ", components)
    )
    pad <- rep(" ", indent) |> paste(collapse = "")
    template_padded <- paste(pad, template)
    sprintf(
        paste(template_padded, collapse = "\n")
    )
}

#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "StanModule",
    definition = function(object) {
        string <- as_print_string(object)
        cat("\n", string, "\n\n")
    }
)
