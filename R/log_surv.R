#' Log survival function
#' @param time
#' @param lambda
#' @param p
#' @param beta
#' @param gamma
#' @param psi_bsld
#' @param psi_ks The srinkage rate parameter
#' @param psi_kg The growth rate parameter
#' @param psi_phi The percentage of cells responded to the treatment
#' @param nodes
#' @param weights
#' @param beta_os_cov
#' @param os_cov_design
#' @importFrom LaplacesDemon logit
#' @export

log_surv <- function(time, lambda, p, beta, gamma, psi_bsld,
                     psi_ks, psi_kg, psi_phi, nodes, weights,
                     beta_os_cov, os_cov_design){

    nodes_time <- (nodes + 1) * (time / 2)
    nodes_time_hazard = exp(log_haz(time = nodes_time, lambda = lambda,
                                    p = p, beta = beta , gamma = gamma ,
                                    psi_bsld = psi_bsld,
                                    psi_ks = psi_ks,
                                    psi_kg = psi_kg,
                                    psi_phi = psi_phi,
                                    beta_os_cov = beta_os_cov,
                                    os_cov_design = os_cov_design))

    - (weights * nodes_time_hazard) * time / 2
}
