



# TODO - Documentation
.JointModel <- setClass(
    Class = "JointModel",
    slots = list(
        stan = "StanModule",
        inits = "numeric"
    )
)

# functions = paste0(stan_joint@functions, collapse = "\n"),
# data = paste0(stan_joint@data, collapse = "\n"),
# transformed_data = paste0(stan_joint@transformed_data, collapse = "\n"),
# parameters = paste0(stan_joint@parameters, collapse = "\n"),
# transformed_parameters = paste0(stan_joint@transformed_parameters, collapse = "\n"),
# model = paste0(stan_joint@model, collapse = "\n"),



#' @export
JointModel <- function(longitudinal_model = NULL, survival_model = NULL, link = NULL) {

    longitudinal_model_linked <- addLink(longitudinal_model, link)

    parameters <- merge(
        getParameters(longitudinal_model_linked),
        getParameters(survival_model)
    )

    base_model <- paste0(read_stan("base/base.stan"), collapse = "\n")

    stan_full <- jinjar::render(
        .x = base_model,
        longditudinal = add_missing_stan_blocks(as.list(longitudinal_model_linked)),
        survival = add_missing_stan_blocks(as.list(survival_model)),
        priors = as.list(parameters),
        link_none = class(link)[[1]] == "LinkNone" | is.null(link)
    )
    
    full_plus_funs <- merge(
        StanModule("base/functions.stan"),
        StanModule(stan_full)
    )
    
    .JointModel(
        stan = full_plus_funs,
        inits = getInits(parameters)
    )
}


#' as character joint model TODO
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
    definition = function(object, exe_file = NULL) {
        if (is.null(exe_file)) {
            exe_file = file.path(tempdir(), "model")
        }
        x <- cmdstanr::cmdstan_model(
            stan_file = cmdstanr::write_stan_file(as.character(object)),
            exe_file = exe_file
        )
        invisible(x)
    }
)


setMethod(
    f = "sampleStanModel",
    signature = "JointModel",
    definition = function(object, ..., exe_file = NULL) {
        
        args <- list(...)
        
        if ("chains" %in% names(args)) {
            nchains <- args[["chains"]] 
        } else {
            nchains <- 1
        }
        
        inits <- replicate(nchains, as.list(object@inits), simplify = FALSE)
        
        model <- compileStanModel(object, exe_file)
        model$sample(
            ...,
            init = inits
        )
    }
)


add_missing_stan_blocks <- function(x) {
    # STAN_BLOCKS is defined as a global variable in stan_module.R
    for (block in names(STAN_BLOCKS)) {
        if (is.null(x[[block]])) {
            x[[block]] <- ""
        }
    }
    return(x)
}

