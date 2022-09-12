cmdstan_mod <- R6::R6Class("CmdStanModel")
setOldClass("CmdStanModel")

#' @exportClass JointModel
JointModel <- setClass("JointModel",
    slots = c(cmdstan_mod = "CmdStanModel")
)



#' @importFrom cmdstanr write_stan_file
#' @importFrom cmdstanr cmdstan_model
#' @rdname StanModule-class
#' @param util Logical. Populates the .stan file with general functions.
#' @export
setMethod(
    f = "initialize",
    signature = "JointModel",
    definition = function(.Object,
                          ...,
                          long,
                          os,
                          util = TRUE,
                          name = "stan_model.stan") {
        mod <- merge(x = long, y = os)

        utils <- StanModule(functions = read_file(system.file("Utils", "utils.stan", package = "jmpost")))

        if(util == TRUE) mod <- merge(x = mod, y = utils)

        cmdstanr::write_stan_file(
            as.character(mod),
            basename = name,
            dir = paste0(system.file(package = "jmpost"), "/stanmodels")
        )

        callNextMethod(
            .Object,
            ...,
            cmdstan_mod = cmdstan_model(
                stan_file = system.file(
                    paste0("stanmodels/", name),
                    package = "jmpost"
                )
            )
        )
    }
)
