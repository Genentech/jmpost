
#' @include generics.R
#' @include Link.R
#' @include LongitudinalModel.R
#' @include SurvivalModel.R
#' @include StanModule.R
#' @include StanModel.R
#' @include Parameter.R
NULL


#######################
#
#  addLink
#

setMethod(
    "addLink",
    signature = c("LongitudinalModel", "NULL"),
    definition = function(x, y, ...) x
)

setMethod(
    "addLink",
    signature = c("NULL", "Link"),
    definition = function(x, y, ...) {
        stop("No Longitudinal model has been defined for the link function to be combined with")
    }
)

setMethod(
    "addLink",
    signature = c("NULL", "NULL"),
    definition = function(x, y, ...) NULL
)



#######################
#
#  getParameters
#



setMethod(
    f = "getParameters",
    signature = "NULL",
    definition = function(object) NULL
)



#######################
#
#  merge
#



setMethod(
    "merge",
    signature = c("StanModel", "NULL"),
    definition = function(x, y, ...) x@stan
)
setMethod(
    "merge",
    signature = c("NULL", "StanModel"),
    definition = function(x, y, ...) y@stan
)




setMethod(
    "merge",
    signature = c("StanModule", "NULL"),
    definition = function(x, y, ...) x
)
setMethod(
    "merge",
    signature = c("NULL", "StanModule"),
    definition = function(x, y, ...) y
)




setMethod(
    "merge",
    signature = c("ParameterList", "NULL"),
    definition = function(x, y, ...) x
)
setMethod(
    "merge",
    signature = c("NULL", "ParameterList"),
    definition = function(x, y, ...) y
)




setMethod(
    "merge",
    signature = c("NULL", "NULL"),
    definition = function(x, y, ...) NULL
)

