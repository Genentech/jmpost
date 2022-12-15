

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
normal_prior <- function(mu, sigma) {
    .Prior(
        parameters = list(mu = mu, sigma = sigma),
        repr = "normal({mu}, {sigma});"
    )
}

#' @export
cauchy_prior <- function(mu, sigma) {
    .Prior(
        parameters = list(mu = mu, sigma = sigma),
        repr = "cauchy({mu}, {sigma});"
    )
}

#' @export
gamma_prior <- function(alpha, beta) {
    .Prior(
        parameters = list(alpha = alpha, beta = beta),
        repr = "gamma({alpha}, {beta});"
    )
}
