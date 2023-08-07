
#' Construct a Log Hazard Function for the Weibull Model
#'
#' @param lambda (`number`)\cr the scale parameter.
#' @param gamma (`number`)\cr the shape parameter.
#'
#' @returns A function of `time` returning the log hazard.
#' @export
sim_os_weibull <- function(lambda, gamma) {
    function(time) {
        log(lambda) + log(gamma) + (gamma - 1) * log(time)
    }
}

#' Construct a Log Hazard Function for the Exponential Model
#'
#' @param lambda (`number`)\cr the rate parameter.
#'
#' @returns A function of `time` returning the log hazard.
#' @export
sim_os_exponential <- function(lambda) {
    function(time) {
        log(lambda)
    }
}

#' Construct a Log Hazard Function for the Log-Logistic Model
#'
#' @param lambda (`number`)\cr the inverse median parameter.
#' @param p (`number`)\cr the shape parameter.
#'
#' @returns A function of `time` returning the log hazard.
#' @export
sim_os_loglogistic <- function(lambda, p) {
    function(time) {
        c1 <- log(lambda) + log(p) + (p - 1) * (log(lambda) + log(time))
        c2 <- log(1 + (lambda * time)^p)
        return(c1 - c2)
    }
}
