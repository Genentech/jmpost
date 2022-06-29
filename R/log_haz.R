#' Log hazard function
#' @param time
#' @param lambda
#' @param p
#' @param beta
#' @param gamma
#' @param psi_bsld
#' @param psi_ks The srinkage rate parameter
#' @param psi_kg The growth rate parameter
#' @param psi_phi The percentage of cells responded to the treatment
#' @param beta_os_cov
#' @param os_cov_design
#' @importFrom LaplacesDemon logit
#' @export


log_haz <- function(time, lambda, p, beta, gamma, psi_bsld,
                    psi_ks, psi_kg, psi_phi,
                    beta_os_cov, os_cov_design){
    res <- matrix(data = NA, ncol = length(psi_bsld), nrow = length(time))


    for( k in 1:length(psi_bsld)){
        for( i in 1:length(time)){

            log_base <- log_h0(time = time[i], lambda = lambda, p = p)
            dt <- dtsld(time[i], psi_bsld[k], psi_ks[k], psi_kg[k], psi_phi[k])
            dt_c <- dt[[1]]
            cov_contribution = (as.matrix(os_cov_design)[k,] %*% as.matrix(beta_os_cov))
            cov_contribution_c <- cov_contribution[,,drop = TRUE]

            ttg_contribution = ttg(psi_ks[k], psi_kg[k], psi_phi[k])

            res[i,k] <-  (log_base + beta * dt_c + gamma * ttg_contribution + cov_contribution_c)[[1]]
        }
    }
    res
}
