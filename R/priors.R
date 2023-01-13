

.Prior <- setClass(
    Class = "Prior",
    slots = c(
        "parameters" = "list",
        "repr" = "character"
    )
)

#' @export
setMethod(
    f = "as.character",
    signature = "Prior",
    definition = function(x) {
        glue::glue(x@repr, .envir = list2env(x@parameters))
    }
)


#' @export
prior_normal <- function(mu, sigma) {
    .Prior(
        parameters = list(mu = mu, sigma = sigma),
        repr = "normal({mu}, {sigma});"
    )
}


#' @export
prior_cauchy <- function(mu, sigma) {
    .Prior(
        parameters = list(mu = mu, sigma = sigma),
        repr = "cauchy({mu}, {sigma});"
    )
}


#' @export
prior_gamma <- function(alpha, beta) {
    .Prior(
        parameters = list(alpha = alpha, beta = beta),
        repr = "gamma({alpha}, {beta});"
    )
}


