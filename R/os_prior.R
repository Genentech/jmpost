#### os_model####
#' @export
os_prior <- function(p = "gamma(2, 0.5)",
                     `1/lambda` = "lognormal(0, 5)",
                     beta_os_cov = "normal(0, 5)") {
    as.list(environment())
}
