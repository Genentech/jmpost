#' Time to growth function
#' @param psi_ks The srinkage rate parameter
#' @param psi_kg The growth rate parameter
#' @param psi_phi The percentage of cells responded to the treatment
#' @importFrom LaplacesDemon logit
#' @export

ttg <- function(psi_ks, psi_kg, psi_phi){
    num <- logit(psi_phi) + log(psi_ks / psi_kg)
    denom <- psi_ks + psi_kg
    num / denom
}
