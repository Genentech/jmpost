#' @export

setGeneric("scaled_brier", function(object, jmdata ) {
  standardGeneric("scaled_brier")
})


setMethod("scaled_brier",
  signature(object = "JMpost",
            jmdata = "JMdata"),
  value = "numeric",
  function(object, jmdata) {
    KMref <- KM(object)

    save_ind_unconditional_survival <- object@cmdstan_fit$draws("save_ind_unconditional_survival",
      format = "draws_matrix"
    )

    mean_sur_probs <- matrix(
      data = NA,
      ncol = 100,
      nrow = 9
    )

    for (i in c(1:9)) {
      for (k in 1:100) {
        temp_mean <- mean(save_ind_unconditional_survival[, i * k])
        mean_sur_probs[i, k] <- temp_mean
      }
    }


    brier <- BS(
      timepoints = object@predictions,
      times = jmdata@data_os[, jmdata@vars$longitudinal],
      status = jmdata@data_os[, jmdata@vars$overall_survival_death],
      pred = mean_sur_probs,
      cause = 1
    )

    1 - brier$BS / KMref
  }
)
