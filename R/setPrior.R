
#priors() method for stanmodule, hazardlink , longmodel and osmodel

# getter method for LongModel:
setGeneric(name = "prior", def = function(object) standardGeneric("prior"))
#' get method for longmodel
setMethod(
    f = "prior",
    signature = list(object = "LongModel"),
    definition = function(object) {
        object@stan@priors
    }
)

#' get method for OsModel

setMethod(
    f = "prior",
    signature = list(object = "OsModel"),
    definition = function(object) {
        object@stan@priors
    }
)

#' get method for Hazard link

setMethod(
    f = "prior",
    signature = "HazardLink",
    definition = function(object) {
        object@stan@priors
    }
)

#' get method for stan module


setMethod(
    f = "prior",
    signature = list(object = "StanModule"),
    definition = function(object) {
        object@priors
    }
)



#' setter method for longmodel:
setGeneric(name = "prior<-", def = function(object, value) standardGeneric("prior<-"))

setReplaceMethod(
    f = "prior",
    signature = "LongModel",
    definition = function(object,value) {
        object@stan@priors<- value
        object
    }
)
#' setter method for Osmodel:

setReplaceMethod(
    f = "prior",
    signature = "OsModel",
    definition = function(object,value) {
        object@stan@priors<- value
        object
    }
)

#' setter method for Hazardlink:

setReplaceMethod(
    f = "prior",
    signature = "HazardLink",
    definition = function(object,value) {
        object@stan@priors<- value
        object
    }
)


#' setter method for StanModule:

setReplaceMethod(
    f = "prior",
    signature = "StanModule",
    definition = function(object,value) {
        object@priors<- value
        object
    }
)





#method for stanmodule, hazardlink , longmodel and osmodel

# setMethod(
#     f = "[[<-",
#     signature = c("PersonList"),
#     definition=function(x,i,j,value) {
#         x@datasetList[[i]] <- value
#         return(x)
#     })
