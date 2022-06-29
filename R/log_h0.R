#' Baseline log hazard function
#' @param time Times to be predicted
#' @param lambda The lambda estimate
#' @param p The p estimate
#' @export

log_h0 <- function(time, lambda, p) {
  lambda_t <- lambda * time
  log_num <- log(lambda) + log(p) + (p - 1) * log(lambda_t)
  log_denom <- log1p(lambda_t^p)
  log_num - log_denom
}
