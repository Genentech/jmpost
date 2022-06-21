#' @export

setGeneric("KM", function(object) {
  standardGeneric("KM")
})


setMethod("KM",
  signature(object = "JMpost"),
  value = "numeric",
  function(object) {
    rel_vals <- str_detect(object@data$ARM, "MO")
    fit <- survfit(Surv(
      object@data$AYR[!rel_vals],
      object@data$DEATH[!rel_vals]
    ) ~ 1)

    summary(fit, times = object@data$os_pred_times)$surv
  }
)
