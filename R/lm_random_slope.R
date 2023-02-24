



.LongitudinalRandomSlope <- setClass(
    Class = "LongitudinalRandomSlope",
    contains = "LongitudinalModel"
)

# TODO - Allow input to be with Prior or Parameter
#' @export
LongitudinalRandomSlope <- function(
    lm_rs_intercept = Parameter(prior_normal(30, 4), init = 30),
    lm_rs_slope = Parameter(prior_normal(0, 10), init = 0.001),
    lm_rs_sigma = Parameter(prior_cauchy(0, 2.5), init = 0.001)
) {

    stan <- StanModule(
        x = "lm-random-slope/model.stan"
    )
    
    .LongitudinalRandomSlope(
        LongitudinalModel(
            stan = stan,
            parameters = ParameterList(
                lm_rs_intercept = lm_rs_intercept,
                lm_rs_slope = lm_rs_slope,
                lm_rs_sigma = lm_rs_sigma
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





