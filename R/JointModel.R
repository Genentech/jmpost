cmdstan_mod <- R6::R6Class("CmdStanModel")
setOldClass("CmdStanModel")

#' @exportClass JointModel
JointModel <- setClass("JointModel",
    slots = c(cmdstan_mod = "CmdStanModel")
)



#' @importFrom cmdstanr write_stan_file
#' @importFrom cmdstanr cmdstan_model
#' @rdname StanModule-class
#' @export
setMethod(
    f = "initialize",
    signature = "JointModel",
    definition = function(.Object,
                          ...,
                          long,
                          os,
                          name = "stan_model.stan") {
        mod <- joint(long = long, os = os)

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
