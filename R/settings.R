


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
#' initial_value = prior_sample * prior_shrinkage + (1 - prior_shrinkage) * prior_mean
#' ```
#' This setting controls the shrinkage factor. A value of 0 means no shrinkage (i.e.
#' pure random draw) whilst a value of 1 means the initial value is just the mean.
#'
#' @examples
#' \dontrun{
#' options(jmpost.prior_shrinkage = 0.5)
#' }
#' @name jmpost-settings
NULL

if (is.null(getOption("jmpost.prior_shrinkage", default = NULL))) {
    options("jmpost.prior_shrinkage" = 0.5)
}
