
#'  Log-logistic overall survival
#'
#' Description - Log-logistic overall survival model object creator
#'
#' @export
LogLogisticOs <- setClass(
    Class = "LogLogisticOs",
    contains = "OsModel"
)


#' LogLogisticOs object creator
#'
#' Creates a log-logistic overall survival templated object
#' @rdname LogLogisticOs-class
#' @importFrom assertthat assert_that
#' @export
setMethod(
    f = "initialize",
    signature = "LogLogisticOs",
    definition = function(.Object,
                          ..., stan = LogLogisticModule(), templated) {
        callNextMethod(
            .Object,
            ...,
            stan = stan@stan,
            templated = stan@templated
        )
    }
)






