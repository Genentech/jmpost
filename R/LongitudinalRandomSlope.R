
#' @include LongitudinalModel.R
#' @include Link.R
NULL


.LongitudinalRandomSlope <- setClass(
    Class = "LongitudinalRandomSlope",
    contains = "LongitudinalModel"
)


#' @export
LongitudinalRandomSlope <- function(
    intercept = prior_normal(30, 4, init = 30),
    slope_mu = prior_normal(0, 10, init = 0.001),
    slope_sigma = prior_cauchy(0, 2.5, init = 0.001),
    sigma = prior_cauchy(0, 2.5, init = 0.001)
) {

    stan <- StanModule(
        x = "lm-random-slope/model.stan"
    )

    .LongitudinalRandomSlope(
        LongitudinalModel(
            stan = stan,
            parameters = ParameterList(
                Parameter(name = "lm_rs_intercept", prior = intercept, size = 1),
                Parameter(name = "lm_rs_slope", prior = slope_mu, size = "Nind"),
                Parameter(name = "lm_rs_sigma", prior = sigma, size = 1)
            )
        )
    )
}


.LinkRandomSlope <- setClass(
    Class = "LinkRandomSlope",
    contains = "Link"
)


#' @export
LinkRandomSlope <- function(
    link_lm_phi = prior_normal(0.2, 0.5, init = 0.02)
) {
    .LinkRandomSlope(
        Link(
            stan = StanModule(
                x = "lm-random-slope/links.stan"
            ),
            parameters = ParameterList(
                Parameter(name = "link_lm_phi", prior = link_lm_phi, size = 1)
            )
        )
    )
}





