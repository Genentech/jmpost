#' @include generics.R
#' @include Link.R
#' @include LongitudinalModel.R
#' @include SurvivalModel.R
#' @include StanModule.R
#' @include StanModel.R
#' @include Parameter.R
NULL

# addLink ----

## addLink-LongitudinalModel,NULL ----

#' @rdname addLink
setMethod(
    "addLink",
    signature = c("LongitudinalModel", "NULL"),
    definition = function(x, y, ...) x
)

## addLink-NULL,Link ----

#' @rdname addLink
setMethod(
    "addLink",
    signature = c("NULL", "Link"),
    definition = function(x, y, ...) stop("LongitudinalModel needs to be defined")
)

## addLink-NULL,NULL ----

#' @rdname addLink
setMethod(
    "addLink",
    signature = c("NULL", "NULL"),
    definition = function(x, y, ...) NULL
)

# getParameters ----
#' @export
getParameters.default <- function(object) {
    if (missing(object) || is.null(object)) {
        return(NULL)
    }
    stop(sprintf("No default method implemented for getParameters(<%s>)", typeof(object)))
}


# merge ----

## merge-StanModel,NULL ----

#' @rdname merge
setMethod(
    "merge",
    signature = c("StanModel", "NULL"),
    definition = function(x, y, ...) x@stan
)

## merge-NULL,StanModel ----

#' @rdname merge
setMethod(
    "merge",
    signature = c("NULL", "StanModel"),
    definition = function(x, y, ...) y@stan
)

## merge-StanModule,NULL ----

#' @rdname merge
setMethod(
    "merge",
    signature = c("StanModule", "NULL"),
    definition = function(x, y, ...) x
)

## merge-NULL,StanModule ----

#' @rdname merge
setMethod(
    "merge",
    signature = c("NULL", "StanModule"),
    definition = function(x, y, ...) y
)

## merge-ParameterList,NULL ----

#' @rdname merge
setMethod(
    "merge",
    signature = c("ParameterList", "NULL"),
    definition = function(x, y, ...) x
)

## merge-NULL,ParameterList ----

#' @rdname merge
setMethod(
    "merge",
    signature = c("NULL", "ParameterList"),
    definition = function(x, y, ...) y
)

## merge-NULL,NULL ----

#' @rdname merge
setMethod(
    "merge",
    signature = c("NULL", "NULL"),
    definition = function(x, y, ...) NULL
)
