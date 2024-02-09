#' @include StanModule.R
#' @include LongitudinalModel.R
#' @include ParameterList.R
#' @include LinkComponent.R
NULL


.Link <- setClass(
    Class = ".Link",
    slots = list(
        stan = "StanModule",
        parameters = "ParameterList"
    )
)

setValidity(
    Class = ".Link",
    method = function(object) {
        for (link in object@links) {
            if (!is.function(link)) {
                return("all elements of `links`` must be a function")
            }
        }
        return(TRUE)
    }
)

Link <- function(components, model) {
    if 
    .Link(links = links)
}




# JointModel(
#     survival = SurvivalExponential(),
#     longitudinal = LongitudinalGSF(),
#     link = list(
#         link_ttg(prior_normal()),
#         link_dlsd(prior_gamma()),
#         link_identity(prior_normal())
#     )
# )
