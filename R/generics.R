

#' Merge
#'
#' Generic function to collapse two similar objects into a single combined object
#'
#' @param x An Object
#' @param y An Object to be merged with `x`
#' @export
setGeneric(
    "merge",
    function(x, y) standardGeneric("merge")
)


#' Parametrize TemplatedStanOs object with the selected Hazardlink
#' @param osmod TemplatedStanOs object
#' @param link HazardLink object
#' @export
setGeneric("parametrize", function(osmod, link) {
    standardGeneric("parametrize")
})





#' priors() get method for `LongModel`, `HazardLink` , `StanModule` or `OsModel`
#' @rdname priors
#' @param object A `LongModel`, `HazardLink` , `StanModule` or `OsModel` object
#' @examples
#' priors(object)
#' @export
setGeneric(
    name = "priors",
    def = function(object)
        standardGeneric("priors")
)

#' priors() replacement method for `LongModel`, `HazardLink` , `StanModule` or `OsModel` object
#' @rdname extract-priors
#' @param object A `LongModel`, `HazardLink` , `StanModule` and `OsModel` object
#' @param value the character strings of the prior information for replacement
#' @examples
#' priors(object)<-"lognormal(0,1);"
#' @export
setGeneric(
    name = "priors<-",
    def = function(object, value)
        standardGeneric("priors<-")
)


#' inits() get method for `LongModel`, `HazardLink` , `StanModule` or `OsModel`
#' @rdname inits
#' @param object A `LongModel`, `HazardLink` , `StanModule` or `OsModel` object
#' @examples
#' inits(object)
#' @export
setGeneric(
    name = "inits",
    def = function(object)
        standardGeneric("inits")
)

#' inits() replacement method for `LongModel`, `HazardLink` , `StanModule` or `OsModel` object
#' @rdname extract-inits
#' @param object A `LongModel`, `HazardLink` , `StanModule` and `OsModel` object
#' @param value the character strings of the inits information for replacement
#' @examples
#' inits(object)<-"mean_mu_ks = 60"
#' @export
setGeneric(
    name = "inits<-",
    def = function(object, value)
        standardGeneric("inits<-")
)
