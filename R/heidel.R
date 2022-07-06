

#' Heidelberger and Welch's convergence diagnostic
#' @param object A JMpost object
#' @importFrom coda heidel.diag
#' @importFrom coda geweke.diag
#' @importFrom coda raftery.diag
#' @importFrom coda gelman.diag
#' @importFrom cmdstanr as_mcmc.list
#' @export
jm_post_check <- function(object){

    mcmc_object <- as_mcmc.list(object@cmdstan_fit)

    list( "Heidel" = heidel.diag(mcmc_object),
          "Gewele" = geweke.diag(mcmc_object),
          "Raftery" = raftery.diag(mcmc_object),
          "Gelman" = ifelse( object@options@chains > 1, gelman.diag(mcmc_object), NA_character_))
    }

