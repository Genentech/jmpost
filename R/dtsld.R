#' Derivative of SLD
#' @param time Times to be predicted
#' @param psi_bsld Baseline parameter
#' @param psi_ks The srinkage rate parameter
#' @param psi_kg The growth rate parameter
#' @param psi_phi The percentage of cells responded to the treatment
#' @export


dtsld <- function(time, psi_bsld, psi_ks, psi_kg, psi_phi) {

    patient_dt <- matrix(data = NA, nrow = length(psi_bsld), ncol = length(time))

    for( i in 1:length(psi_bsld)){

        patient_dt[i,] <- psi_bsld[i] * (
            (1 - psi_phi[i]) * psi_kg[i] * exp(psi_kg[i] * time) -
                psi_phi[i] * psi_ks[i] * exp(-psi_ks[i] * time)
        )

    }
    patient_dt
}
