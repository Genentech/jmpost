#' @export


link_arguments <- c(
  "TTG" = " real beta_ttg,",
  "dtSLD" = " real beta_dt,",
  "psi_kg" = "real beta_psi_kg"
)

link_calculation <- c(
  "TTG" = " row_vector[cols(time)] ttg_contribution = ttg(psi_ks, psi_kg, psi_phi);\n
    matrix[rows(time), cols(time)] ttg_contribution_matrix = rep_matrix(ttg_contribution, rows(time));",
  "dtSLD" = " matrix[rows(time), cols(time)] dt = dtsld(time, psi_bsld, psi_ks, psi_kg, psi_phi);",
  "psi_kg" = "matrix[rows(time), cols(psi_kg)] psi_kg_matrix = rep_matrix(psi_kg, rows(time));"
)

link_log_haz <- c(
  "TTG" = " beta_ttg * ttg_contribution_matrix",
  "dtSLD" = " beta_dt * dt",
  "psi_kg" = "beta_psi_kg * psi_kg_matrix"
)

beta_ttg_prior <- function(beta_ttg = "normal(0,5)") {
  as.list(environment())
}
beta_dt_prior <- function(beta_dt = "normal(0,5)") {
  as.list(environment())
}
beta_psi_kg_prior <- function(beta_psi_kg = "normal(0,5)") {
  as.list(environment())
}


link_pars <- c(
  "TTG" = "real beta_ttg;\n",
  "dtSLD" = "real beta_dt;\n ",
  "psi_kg" = "real beta_psi_kg;\n "
)


## Adjusted population log hazard function
link_pop_log_haz <- c(
  "TTG" = " + beta_ttg * pop_ttg",
  "dtSLD" = " + beta_dt * pop_dtsld",
  "psi_kg" = ""
)

beta_ttg_init <- function(init = 0) {
  list(beta_ttg = init)
}
beta_dt_init <- function(init = 0) {
  list(beta_dt = init)
}
beta_psi_kg_init <- function(init = 0) {
  list(beta_dt = init)
}
