#' @include Link.R
NULL

# LinkRandomSlope-class ----

#' `LinkRandomSlope`
#'
#' This class extends the general [`Link`] class for the [`LongitudinalRandomSlope`]
#' model.
#'
#' @exportClass LinkRandomSlope
.LinkRandomSlope <- setClass(
    Class = "LinkRandomSlope",
    contains = "Link"
)

# LinkRandomSlope-constructors ----

#' @rdname LinkRandomSlope-class
#'
#' @param link_lm_phi (`Prior`)\cr prior for the link coefficient for the
#'   random slope link contribution.
#'
#' @export
LinkRandomSlope <- function(
    link_lm_phi = prior_normal(0.2, 0.5)
) {
    .LinkRandomSlope(
        Link(
            name = "Random Slope",
            stan = StanModule(
                x = "lm-random-slope/links.stan"
            ),
            parameters = ParameterList(
                Parameter(name = "link_lm_phi", prior = link_lm_phi, size = 1)
            )
        )
    )
}
