#' jmpost settings
#'
#' @description
#' Define settings that modify the behaviour of the `jmpost` package
#'
#' Each of the following are the name of options that can be set via:
#' ```
#' options(<option_name> = <value>)
#' ```
#'
#' ## `jmpost.prior_shrinkage`
#'
#' Default = `0.5`
#'
#' By default all initial values are drawn as random sample from the respective prior
#' distribution with a shrinkage factor towards the mean. That is:
#' ```
#' initial_value = prior_mean * prior_shrinkage + (1 - prior_shrinkage) * prior_sample
#' ```
#' This setting controls the shrinkage factor. A value of 0 means no shrinkage (i.e.
#' pure random draw) whilst a value of 1 means the initial value is just the mean.
#'
#' ## `jmpost.cache_dir`
#'
#' Default = `tempfile()`
#'
#' Directory to store compiled stan models in. If not set, a temporary directory is used for
#' the given R session. Can also be set via the environment variable `JMPOST_CACHE_DIR`.
#'
#'
#'
#' ## `jmpost.gauss_quad_n`
#'
#' Default = 15
#'
#' In most cases the survival function of the joint model does not have a closed form
#' and as such it is calculated by integrating the hazard function. `jmpost` estimates this
#' via Gaussian Quadrature, in particular it uses [`statmod::gauss.quad`] with
#' `kind = "legendre"` to create the nodes and weights.
#'
#' This option specifies the `n` argument in the call to [`statmod::gauss.quad`]. In general
#' higher values of `n` lead to better accuracy of the approximation but at the cost of
#' increased computational time.
#'
#' ## `jmpost.double_eps` and `jmpost.double_neg_eps`
#'
#' Default = `sqrt(.Machine$double.eps)` and `sqrt(.Machine$double.neg.eps)`
#'
#' These are the smallest positive floating-point numbers `x` which are used
#' in the parameter definitions as lower boundaries to prevent numerical issues
#' with the MCMC sampler in the beginning of the sampling process.
#' The default values are set to the square root of the machine epsilon for
#' double precision numbers, which is a common choice to balance numerical stability
#' and the ability to explore the parameter space effectively.
#'
#' @examples
#' \dontrun{
#' options(jmpost.prior_shrinkage = 0.5)
#' }
#' @name jmpost-settings
set_options <- function() {
    cache_dir <- Sys.getenv("JMPOST_CACHE_DIR")

    if (cache_dir == "" || is.null(cache_dir)) {
        cache_dir <- tempfile()
    }

    current_opts <- names(options())
    jmpost_opts <- list(
        jmpost.cache_dir = cache_dir,
        jmpost.prior_shrinkage = 0.5,
        jmpost.gauss_quad_n = 15,
        jmpost.double_eps = sqrt(.Machine$double.eps),
        jmpost.double_neg_eps = sqrt(.Machine$double.neg.eps)
    )
    for (opt in names(jmpost_opts)) {
        if (!opt %in% current_opts) {
            options(jmpost_opts[opt])
        }
    }
}
