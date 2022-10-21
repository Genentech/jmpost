

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
    slots = list(
        stan = "StanModule",
        contribution = "character",
        parameters = "character"
    )
)

setValidity("HazardLink", function(object) {
    msg <- NULL
    if (length(object@parameters) == 0) {
        msg <- c(msg, "`parameters` must be specified")
    }
    if (any(object@parameters == "")) {
        msg <- c(msg, "`parameters` must not be blank")
    }
    if (!length(object@contribution) == 1) {
        msg <- c(msg, "`contribution` must be length 1 character vector")
    }
    return(msg)
})


#' @rdname StanModule-class
#' @export
setMethod(
    f = "initialize",
    signature = "HazardLink",
    definition = function(.Object, ..., stan = StanModule(), contribution, parameters) {


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



#' @rdname priors
#' @export
setMethod(
    f = "priors",
    signature = "HazardLink",
    definition = function(object) {
        priors(object@stan)
    }
)


#' @rdname priors
#' @export
setValidity("HazardLink", function(object) {
    if (is.contained(list(names(priors(object))),list("beta_dt","beta_ttg")))
        TRUE
    else
        "priors of the OsModel should contain : beta_dt, beta_ttg"
})



#' @rdname extract-priors
#' @export
setReplaceMethod(
    f = "priors",
    signature = "HazardLink",
    definition = function(object, value) {
        priors(object@stan) <- value
        validObject(object)
        object
    }
)


