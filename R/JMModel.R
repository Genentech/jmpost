cmdstan_mod <- R6::R6Class("CmdStanModel")
setOldClass("CmdStanModel")

#' @exportClass jmModel
jm_model <- setClass("jmModel",
                     slots = c(cmdstan_mod = "CmdStanModel")
)



#' @importFrom cmdstanr write_stan_file
#' @importFrom cmdstanr cmdstan_model
#' @rdname StanModule-class
#' @export
setMethod(
    f = "initialize",
    signature = "jmModel",
    definition = function(
        .Object,
        ...
    ) {

        cmdstanr::write_stan_file(
            as.character(object),
            basename = "stan_model.stan",
            dir = paste0(system.file(package = "jmpost"), "/stanmodels")
        )

        callNextMethod(
            .Object,
            ...,
            cmdstan_mod = cmdstan_model(
                stan_file = system.file(
                    "stanmodels/stan_model.stan",
                    package = "jmpost"
                )
            )
        )
    }
)
