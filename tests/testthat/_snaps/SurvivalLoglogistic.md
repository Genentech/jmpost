# Print method for SurvivalLogLogistic works as expected

    Code
      x <- SurvivalLogLogistic()
      print(x)
    Output
      
      Log-Logistic Survival Model with parameters:
          sm_loglogis_a ~ lognormal(mu = -2.30259, sigma = 5)
          sm_loglogis_b ~ gamma(alpha = 2, beta = 5)
          beta_os_cov ~ normal(mu = 0, sigma = 5)
      

---

    Code
      x <- SurvivalLogLogistic(beta = prior_gamma(3, 4), b = prior_cauchy(0, 1))
      print(x)
    Output
      
      Log-Logistic Survival Model with parameters:
          sm_loglogis_a ~ lognormal(mu = -2.30259, sigma = 5)
          sm_loglogis_b ~ cauchy(mu = 0, sigma = 1)
          beta_os_cov ~ gamma(alpha = 3, beta = 4)
      

