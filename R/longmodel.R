






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

### a function to check if a list is included in a predefined
is.contained=function(vec1,vec2){
    x=vector(length = length(vec1))
    for (i in 1:length(vec1)) {
        x[i] = vec1[i] %in% vec2
        if(length(which(vec1[i] %in% vec2)) == 0) vec2 else
            vec2=vec2[-match(vec1[i], vec2)]
    }
    y=all(x==T)
    return(y)
}


#' @rdname priors
#' @export
setValidity("LongModel", function(object) {
    if (is.contained(priors(object@stan),as.list(names(long_prior())))|is.null(priors(object@stan)))
        TRUE
    else
        "priors of the HarzardLink should contain : mean_mu_ks ,mean_mu_kg,
         mean_mu_phi,sd_mu_ks,sd_mu_kg,sd_mu_phi,mu_bsld, omega_bsld,
         omega_ks, omega_kg, omega_phi, sigma, eta_tilde_bsld,eta_tilde_ks, eta_tilde_kg, eta_tilde_phi,
         mu_ks[sld_par_shared],mu_kg[sld_par_shared], logit(mu_phi[sld_par_shared]) ,mu_ks[sld_par_separate],
         mu_kg[sld_par_separate],mu_phi[sld_par_separate]"
})

# else "the priors inputted is not a predefined long_prior" ??



#' @rdname priors
#' @export
setMethod(
    f = "priors",
    signature = list(object = "LongModel"),
    definition = function(object) {
        priors(object@stan)
    }
)



#' @rdname extract-priors
#' @export
setReplaceMethod(
    f = "priors",
    signature = "LongModel",
    definition = function(object, value) {
        priors(object@stan) <- value
        validObject(object)
    }
)
