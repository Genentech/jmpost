
.onLoad <- function(libname, pkgname) {
    if (!"cmdstanr" %in% utils::installed.packages()[, "Package"]) {
        stop(c(
            "The `cmdstanr` package is not installed.",
            " Please note that this package is not available on CRAN.",
            " To install it, please follow the instructions at: https://mc-stan.org/cmdstanr/"
        ))
    }
    set_options()
}


# This only exists to silence the false positive R CMD CHECK warning about
# importing but not using the posterior package. posterior is a dependency
# of rcmdstan that we use a lot implicitly. Also we link to their documentation
# pages in ours
.never_run <- function() {
    posterior::as_draws()
}
