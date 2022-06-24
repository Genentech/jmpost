




setGeneric("KM", function(object) {
  standardGeneric("KM")
})

#' Kaplan Meier kurve
#' @param object JMpost object
#' @importFrom survival survfit
#' @importFrom survival Surv
#' @export
setMethod("KM",
  signature(object = "JMpost"),
  value = "numeric",
  function(object) {


    fit <- survfit(Surv(
        object@data@data_os[, object@data@vars$time_survival, drop = TRUE],
        object@data@data_os[, object@data@vars$overall_survival_death, drop = TRUE]
    ) ~ 1)

    summary(fit, times = object@predictions)$surv
  }
)

