#' @export

setGeneric("scaled_brier", function(object) {
  standardGeneric("scaled_brier")
})


setMethod("scaled_brier",
  signature(object = "JMpost"),
  value = "numeric",
  function(object) {
    KMref <- KM(object)
    rel_vals <- str_detect(object@data$ARM, "MO")

    save_ind_unconditional_survival <- object@cmdstan_fit$draws("save_ind_unconditional_survival",
      format = "draws_matrix"
    )

    mean_sur_probs <- matrix(
      data = NA,
      ncol = object@data$n_times,
      nrow = object@data$n_pat
    )

    for (i in c(1:object@data$n_pat)) {
      for (k in 1:object@data$n_times) {
        temp_mean <- mean(save_ind_unconditional_survival[, i * k])
        mean_sur_probs[i, k] <- temp_mean
      }
    }


    brier <- BS(
      timepoints = object@data$os_pred_times,
      times = object@data$AYR[rel_vals],
      status = object@data$DEATH[rel_vals],
      pred = mean_sur_probs,
      cause = 1
    )

    1 - brier$BS / KMref
  }
)
