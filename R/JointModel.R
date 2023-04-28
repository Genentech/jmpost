
#' @include StanModule.R
#' @include generics.R
#' @include ParameterList.R
NULL


.JointModel <- setClass(
    Class = "JointModel",
    slots = list(
        stan = "StanModule",
        parameters = "ParameterList"
    )
)


#' @export
JointModel <- function(longitudinal = NULL, survival = NULL, link = NULL) {

    longitudinal_linked <- addLink(longitudinal, link)

    parameters <- merge(
        getParameters(longitudinal_linked),
        getParameters(survival)
    )

    base_model <- paste0(read_stan("base/base.stan"), collapse = "\n")

    stan_full <- jinjar::render(
        .x = base_model,
        longditudinal = add_missing_stan_blocks(as.list(longitudinal_linked)),
        survival = add_missing_stan_blocks(as.list(survival)),
        priors = as.list(parameters),
        link_none = class(link)[[1]] == "LinkNone" | is.null(link)
    )

    full_plus_funs <- merge(
        StanModule("base/functions.stan"),
        StanModule(stan_full)
    )

    .JointModel(
        stan = full_plus_funs,
        parameters = parameters
    )
}



#' As character
#' @param x A `JointModel` object
#' @export
setMethod(
    f = "as.character",
    signature = "JointModel",
    definition = function(x) {
        as.character(x@stan)
    }
)


#' @export
setMethod(
    f = "write_stan",
    signature = "JointModel",
    definition = function(x, file_path) {
        fi <- file(file_path, open = "w")
        writeLines(as.character(x), fi)
        close(fi)
    }
)


setMethod(
    f = "compileStanModel",
    signature = "JointModel",
    definition = function(object, exe_file) {
        x <- compileStanModel(object@stan, exe_file)
        invisible(x)
    }
)



setMethod(
    f = "sampleStanModel",
    signature = "JointModel",
    definition = function(object, data, ..., exe_file = NULL) {
        
        args <- list(...)
        
        if (is(data, "DataJoint")) {
            args[["data"]] <- as.list(data)
        } else if (is(data, "list")) {
            args[["data"]] <- data
        } else {
            stop("`data` must either be a list or a DataJoint object")
        }
        
        if (!"init" %in% names(args)) {
            values_initial <- initialValues(object)
            values_sizes <- size(object@parameters)
            values_sizes_complete <- replace_with_lookup(values_sizes, args[["data"]])
            values_initial_expanded <- expand_initial_values(values_initial, values_sizes_complete)
            args[["init"]] <- function() values_initial_expanded
        }
        
        model <- compileStanModel(object, exe_file)
        do.call(model$sample, args)
    }
)


#' @export
setMethod(
    f = "initialValues",
    signature = "JointModel",
    definition = function(object) {
        initialValues(object@parameters)
    }
)


add_missing_stan_blocks <- function(x) {
    # STAN_BLOCKS is defined as a global variable in stan_module.R
    # TODO - Make it an argument to the function
    for (block in names(STAN_BLOCKS)) {
        if (is.null(x[[block]])) {
            x[[block]] <- ""
        }
    }
    return(x)
}














