



setGeneric("inits_prep", def = function(object, data) {
    standardGeneric("inits_prep")
})

#' Initial values preperation
#' @export
#' @param object
#' @param data

setMethod("inits_prep",
          signature(
              object = "JMModel",
              data = "JMdata"),
          value = "JMModel",
          function(object, data){
              object@inits$mu_bsld <- object@inits$mu_bsld(data)
              object@inits$mu_ks <- object@inits$mu_ks(data)
              object@inits$mu_kg <- object@inits$mu_kg(data)
              object@inits$mu_phi <- object@inits$mu_phi(data)

              object@inits$eta_tilde_bsld <- object@inits$eta_tilde_bsld(data)
              object@inits$eta_tilde_ks <- object@inits$eta_tilde_ks(data)
              object@inits$eta_tilde_kg <- object@inits$eta_tilde_kg(data)
              object@inits$eta_tilde_phi <- object@inits$eta_tilde_phi(data)
              object@inits$beta_os_cov <- object@inits$beta_os_cov(data)
              object
          }
)
