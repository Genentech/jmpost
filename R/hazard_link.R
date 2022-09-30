

#' Hazard Link Class
#'
#' Creates a Hazard Link object which combines a longditudinal model to
#' an OS model
#'
#' @slot stan A `StanModule` object as created by [StanModule()] that specifies any Stan code
#' required for the link / contribution
#' @slot contribution Stan code for what this link contributes to the log hazard in the OS model
#' @slot parameter The name of link parameter
#' @examples
#' HazardLink(
#'     parameters = "beta_ttg",
#'     contribution = "beta_ttg * ttg(phi)",
#'     stan = StanModule(
#'         functions = "real ttg(real phi) {phi^2 };"
#'     )
#' )
#' @export
HazardLink <- setClass(
    "HazardLink",
    representation = list(
        "stan" = "StanModule",
        "contribution" = "character",
        "parameters" = "character"
    )
)


#' @importFrom assertthat assert_that
#' @rdname StanModule-class
#' @export
setMethod(
    f = "initialize",
    signature = "HazardLink",
    definition = function(.Object, ..., stan = StanModule(), contribution, parameters) {

        assert_that(
            is.character(parameters),
            length(parameters) >= 1,
            msg = "`parameter` must be a character vector"
        )

        assert_that(
            is.character(contribution),
            length(contribution) == 1,
            msg = "`contribution` must be length 1 character vectors"
        )

        if (length(stan@parameters) == 0 || all(stan@parameters == "")) {
            stan@parameters <- sprintf("real %s;", parameters)
        }

        callNextMethod(
            .Object,
            ...,
            stan = stan,
            contribution = contribution,
            parameters = parameters
        )
    }
)


#' @rdname merge
#' @export
setMethod(
    f = "merge",
    signature = c("HazardLink", "HazardLink"),
    definition = function(x, y) {
        HazardLink(
            stan = merge(x@stan, y@stan),
            contribution = paste0(x@contribution, " + ", y@contribution),
            parameters = c(x@parameters, y@parameters)
        )
    }
)




#' get method for Hazard link
#' @rdname priors
#' @param object A `Hazardlink` object
#' @param value the character strings of the prior information for replacement
#' @export


setMethod(
    f = "priors",
    signature = "HazardLink",
    definition = function(object) {
        priors(object@stan)
    }
)


#' set method for Hazardlink:
#' @rdname extract-priors
#' @param object A `Hazardlink` object
#' @param value the character strings of the prior information for replacement
#' @export
#'
setReplaceMethod(
    f = "priors",
    signature = "HazardLink",
    definition = function(object, value) {
        priors(object@stan) <- value
        object
    }
)

