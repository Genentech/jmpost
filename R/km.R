#' @export




setGeneric("KM", function(object) {
  standardGeneric("KM")
})


#' @importFrom survival survfit
#' @importFrom survival Surv

setMethod("KM",
  signature(object = "JMpost"),
  value = "numeric",
  function(object) {
    rel_vals <- c(rep(FALSE, 770), rep(TRUE,89))

    fit <- survfit(Surv(
        object@data@data_os[!rel_vals, object@data@vars$AYR, drop = TRUE],
        object@data@data_os[!rel_vals, object@data@vars$overall_survival_death, drop = TRUE]
    ) ~ 1)

    summary(fit, times = object@predictions)$surv
  }
)

