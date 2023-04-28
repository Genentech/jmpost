
#' @include generics.R
NULL 

.Prior <- setClass(
    Class = "Prior",
    slots = c(
        "parameters" = "list",
        "repr" = "character",
        "init" = "numeric"
    )
)

#' @export
setMethod(
    f = "as.character",
    signature = "Prior",
    definition = function(x) {
        as(x, "character")
    }
)


#' @export
setAs(
    from = "Prior",
    to = "character",
    def = function(from) {
        glue::glue(from@repr, .envir = list2env(from@parameters))
    }
)


#' @export
prior_normal <- function(mu, sigma, init = mu) {
    .Prior(
        parameters = list(mu = mu, sigma = sigma),
        repr = "normal({mu}, {sigma});",
        init = init
    )
}


#' @export
prior_cauchy <- function(mu, sigma, init = mu) {
    .Prior(
        parameters = list(mu = mu, sigma = sigma),
        repr = "cauchy({mu}, {sigma});",
        init = init
    )
}


#' @export
prior_gamma <- function(alpha, beta, init = alpha/beta) {
    .Prior(
        parameters = list(alpha = alpha, beta = beta),
        repr = "gamma({alpha}, {beta});",
        init = init
    )
}

#' @export
prior_lognormal <- function(mu, sigma, init = exp(mu + (sigma^2)/2)) {
    .Prior(
        parameters = list(mu = mu, sigma = sigma),
        repr = "lognormal({mu}, {sigma});",
        init = init
    )
}


#' @export
prior_beta <- function(a, b, init = a/(a+b)) {
    .Prior(
        parameters = list(a = a, b = b),
        repr = "beta({a}, {b});",
        init = init
    )
}


#' @export
prior_none <- function(init = 0.00001) {
    .Prior(
        parameters = list(),
        repr = "",
        init = init
    )
}


#' @export 
setMethod(
    f = "initialValues",
    signature = "Prior",
    definition = function(object) object@init
)

