# Print method for SurvivalGamma works as expected

    Code
      x <- SurvivalGamma()
      print(x)
    Output
      
      Gamma Survival Model with parameters:
          sm_gamma_k ~ gamma(alpha = 2, beta = 0.5)
          sm_gamma_theta ~ gamma(alpha = 2, beta = 0.5)
          beta_os_cov ~ normal(mu = 0, sigma = 2)
      

---

    Code
      x <- SurvivalGamma(k = prior_gamma(3, 4), theta = prior_cauchy(0, 1))
      print(x)
    Output
      
      Gamma Survival Model with parameters:
          sm_gamma_k ~ gamma(alpha = 3, beta = 4)
          sm_gamma_theta ~ cauchy(mu = 0, sigma = 1)
          beta_os_cov ~ normal(mu = 0, sigma = 2)
      

