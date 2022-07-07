

### This file outlines a general template for how the object interfaces should work



############
#
# Base Longditudinal Model
#
############


CustomLM <- setClass(
    "LongModel",
    representation = list("stan" = "StanModule")
)


setMethod(
    "get_link",
    "LongModel",
    definition = function(obj, ..., selected = c("ttg", "dsld")) {
        if ("ttg" %in% selected) {
            get_link_ttg(obj)
        }
        #rest of definition
    }
)


############
#
# Exponential Longditudinal Model
#
############

# Simple function to create a pre-populated StanModule object 
ExponentialStanModule <- function(
    data = "path/to/exp/code/data.stan",
    model = "path/to/exp/code/model.stan",
    functions = "path/to/exp/code/functions.stan",
    priors = list(...),
    inits = list(...)
) {
    StanModule(
        data = data,
        model = model,
        functions = functions,
        priors = priors,
        inits = inits
    )
}

# Default implementation
setMethod(
    "initalize",
    "ExponentialModel",
    definition = function(obj, ..., stan = ExponentialStanModule()) {
        callNextMethod(obj, ..., stan = stan)
    }
)

setMethod(
    "get_link_ttg",
    "ExponentialModel",
    definition = function(obj, ...) {
        HazardLink(
            stan = StanModule(
                # code implementation of the TTG
            )
        )
    }
)


setMethod(
    "get_link_dsld",
    "ExponentialModel",
    definition = function(obj, ...) {
        HazardLink(
            stan = StanModule(
                # code implementation of the dSLD
            )
        )
    }
)


#### Example of use


# Example of using pure defaults
lm <- ExponentialModel()


# Example of modifing specific aspects of the model code / priors / inits
exponential_stan_module <- ExponentialStanModule(
    priors = list(...),
    inits = list(...)
)
lm <- ExponentialModel(exponential_stan_module)


# rest of work flow
os <- OSModel()
link <- get_link(lm)
JointModel(lm, os, link)



############
#
# Custom user defined longditudinal model
#
############


CustomLM <- setClass(
    "CustomLM",
    contains = "LongModel"
)


setMethod(
    "get_link_ttg",
    "CustomLM",
    function() {
        HazardLink(
            stan = StanModule(
                # code implementation of the TTG for our custom long model
            )
        )
    }
)

lm <- CustomLM(StanModule(...))
link <- get_link(lm, selected = c("ttg"))
os <- OSModel()
jm <- JointModel(lm, os, link, path)




