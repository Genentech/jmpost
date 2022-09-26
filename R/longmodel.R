






#' `LongModel`
#'
#' This represents an abstract class for longditudinal models.
#' This class only exists for other concrete instatiations to inherit off of.
#'
#' Provides default implementation of the [getLink()] method.
#' Child classes should define for themselves [getLinkTTG()] and
#' [getLinkDSLD()] or alternative provide their own implementations
#' for [getLink()].
#'
#' @slot stan a `StanModule` object as created by [StanModule()]
#' @export
LongModel <- setClass(
    Class = "LongModel",
    representation = list(
        stan = "StanModule"
    )
)


#' Access Hazard Link
#'
#' @description
#'
#' Access the hazard link for a given longditudinal hazard model.
#'
#' * [getLinkTTG()] returns the TTG hazard link
#' * [getLinkDSLD()] returns the dSLD hazard link
#' * [getLink()] is a convience function to return a hazard link created by merging
#' other hazard link functions together
#'
#' @param object A `LongModel` object.
#'
#' @return A `HazardLink` object
#'
#' @export
setGeneric(
    name = "getLink",
    def = function(object, ...) standardGeneric("getLink")
)


#' @rdname getLink
#' @export
setGeneric(
    name = "getLinkTTG",
    def = function(object) standardGeneric("getLinkTTG")
)


#' @rdname getLink
#' @export
setGeneric(
    name = "getLinkDSLD",
    def = function(object) standardGeneric("getLinkDSLD")
)


#' @rdname LongModel-class
#' @export
setMethod(
    f = "getLinkTTG",
    signature = "LongModel",
    definition = function(object) {
        stop("TTG link is not defined for the base LongModel object")
    }
)

#' @rdname LongModel-class
#' @export
setMethod(
    f = "getLinkDSLD",
    signature = "LongModel",
    definition = function(object) {
        stop("dSLD link is not defined for the base LongModel object")
    }
)

#' @rdname LongModel-class
#' @export
setMethod(
    f = "getLink",
    signature = "LongModel",
    definition = function(object, selection = c("ttg", "dsld")) {

        if (length(selection) == 0) {
            return(NULL)
        }

        assert_that(
            is.character(selection),
            all(selection %in% c("ttg", "dsld")),
            msg = "`selection` must be a character vector with values of either 'ttg' or 'dsld'"
        )

        assert_that(
            length(selection) == length(unique(selection)),
            msg = "`selection` must contain unique values"
        )

        function_map <- list(
            "ttg" = getLinkTTG,
            "dsld" = getLinkDSLD
        )

        functions_selected <- function_map[selection]

        hazard_objects <- lapply(
            functions_selected,
            \(fun) fun(object)
        )

        hazard_link <- Reduce(
            \(x, y) merge(x, y),
            hazard_objects
        )

        return(hazard_link)
    }
)



#' get method for longmodel
setMethod(
    f = "priors",
    signature = list(object = "LongModel"),
    definition = function(object) {
        object@stan@priors
    }
)
#' set method for longmodel

setReplaceMethod(
    f = "priors",
    signature = "LongModel",
    definition = function(object,value) {
        priors(object@stan)<- value
        object
    }
)


