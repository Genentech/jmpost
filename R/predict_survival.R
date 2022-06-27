log_h0 <- function(time, lambda, p){
    lambda_t <- lambda * time
    log_num <- log(lambda) + log(p) + (p-1)* log(lambda_t)
    log_denom <- log1p(lambda_t^p)
    log_num - log_denom
}

dtsld <- function(time, psi_bsld, psi_ks, psi_kg, psi_phi) {

  psi_bsld_matrix <- do.call(rbind, replicate(nrow(time), psi_bsld, simplify = F))
  psi_ks_matrix <- do.call(rbind, replicate(nrow(time), psi_ks, simplify = F))
  psi_kg_matrix <- do.call(rbind, replicate(nrow(time), psi_kg, simplify = F))
  psi_phi_matrix <- do.call(rbind, replicate(nrow(time), psi_phi, simplify = F))


  psi_bsld_matrix %*% (
    (1 - psi_phi_matrix) %*% psi_kg_matrix %*% exp(psi_kg_matrix %*% time) -
      psi_phi_matrix %*% psi_ks_matrix %*% exp(-psi_ks_matrix %*% time)
    )
}


ttg <- function(psi_ks, psi_kg, psi_phi){
     num <- logit(psi_phi) + log(psi_ks / psi_kg)
     denom <- psi_ks + psi_kg
     num / denom
}


log_haz <- function(time, lambda, p, beta, gamma, psi_bsld,
                    psi_ks, psi_kg, psi_phi,
                    beta_os_cov, os_cov_design){
    log_base <- log_h0(time = time, lambda = lambda, p = p)
    dt <- dtsld(time, psi_bsld, psi_ks, psi_kg, psi_phi)
    cov_contribution = (os_cov_design * beta_os_cov)
    cov_contribution_matrix = do.call(rbind, replicate(nrow(time), cov_contribution, simplify = FALSE))
    ttg_contribution = ttg(psi_ks, psi_kg, psi_phi)
    ttg_contribution_matrix = do.call(rbind, replicate(nrow(time), ttg_contribution, simplify = FALSE))
    log_baseline + beta * dt + gamma * ttg_contribution_matrix + cov_contribution_matrix
}


log_surv <- function(time, lambda, p, beta, gamma, psi_bsld,
                     psi_ks, psi_kg, psi_phi, nodes, weights,
                     beta_os_cov, os_cov_design){
    pos_time <- time[time>0]
    n_time <- sum(pos_time)
    time_pos_index <- which(time > 0)
    nodes_time <- (nodes + 1) * (time[time_pos_index] / 2)
    nodes_time_hazard = exp(log_haz(nodes_time, lambda,
                                    p, beta, gamma,
                                    psi_bsld[time_positive_index],
                                    psi_ks[time_positive_index],
                                    psi_kg[time_positive_index],
                                    psi_phi[time_positive_index],
                                    beta_os_cov,
                                    os_cov_design[time_positive_index]))

   - (weights * nodes_time_hazard) * time[time_pos_index] / 2
}

# log_surv(time = my_time,
#          lambda = res[res$variable == "lambda", "mean", drop = TRUE],
#          p = res[res$variable == "p", "mean", drop = TRUE],
#          beta = res[res$variable == "beta_dt", "mean", drop = TRUE],
#          gamma = res[res$variable == "beta_ttg", "mean", drop = TRUE],
#          psi_bsld = res[str_detect(res$variable, "psi_bsld"), "mean"],
#          psi_ks = res[str_detect(res$variable, "psi_ks"), "mean"],
#          psi_kg = res[str_detect(res$variable, "psi_kg"), "mean"],
#          psi_phi = res[str_detect(res$variable, "psi_phi"), "mean"],
#          nodes = my_mjpost@data@data$nodes,
#          weights = my_mjpost@data@data$weights,
#          beta_os_cov = res[str_detect(res$variable, "beta_os_cov"), "mean"],
#          os_cov_design = my_mjpost@data@data$os_cov_design)

