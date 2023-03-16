



.LongitudinalRandomSlope <- setClass(
    Class = "LongitudinalRandomSlope",
    contains = "LongitudinalModel"
)


#' @export
LongitudinalRandomSlope <- function(
    intercept = Parameter(prior_normal(30, 4), init = 30),
    slope_mu = Parameter(prior_normal(0, 10), init = 0.001),
    slope_sigma = Parameter(prior_cauchy(0, 2.5), init = 0.001),
    sigma = Parameter(prior_cauchy(0, 2.5), init = 0.001)
) {

    stan <- StanModule(
        x = "lm-random-slope/model.stan"
    )
    
    .LongitudinalRandomSlope(
        LongitudinalModel(
            stan = stan,
            parameters = ParameterList(
                lm_rs_intercept = intercept,
                lm_rs_slope_mu = slope_mu,
                lm_rs_slope_sigma = slope_sigma,
                lm_rs_sigma = sigma
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
    link_lm_phi = Parameter(prior_normal(0.2, 0.5), init = 0.02)
) {
    .LinkRandomSlope(
        Link(
            stan = StanModule(
                x = "lm-random-slope/links.stan"
            ),
            parameters = ParameterList(
                link_lm_phi = link_lm_phi
            )
        )
    )
}





