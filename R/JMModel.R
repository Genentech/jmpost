cmdstan_mod <- R6::R6Class("CmdStanModel")
setOldClass("CmdStanModel")

#' @exportClass jmModel
jmModel <- setClass("jmModel",
    slots = c(cmdstan_mod = "CmdStanModel")
)



#' @importFrom cmdstanr write_stan_file
#' @importFrom cmdstanr cmdstan_model
#' @rdname StanModule-class
#' @export
setMethod(
    f = "initialize",
    signature = "jmModel",
    definition = function(.Object,
                          ...,
                          Long,
                          Os,
                          name = "stan_model.stan") {
        mod <- jm_complete(Long = Long, Os = Os)

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
