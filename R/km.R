#' @export
#' @import survival



setGeneric("KM", function(jmdata) {
  standardGeneric("KM")
})


setMethod("KM",
  signature(jmdata = "JMdata"),
  value = "numeric",
  function(object) {
    rel_vals <- c(rep(FALSE, 770), rep(TRUE,89))

    fit <- survfit(Surv(
        jmdata@data_os[!rel_vals, jmdata@vars$AYR, drop = TRUE],
        jmdata@data_os[!rel_vals, jmdata@vars$overall_survival_death, drop = TRUE]
    ) ~ 1)

    summary(fit, times = seq(from = 0.001, to = 2, length = 100))$surv
  }
)
