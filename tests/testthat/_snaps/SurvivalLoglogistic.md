# Print method for SurvivalLogLogistic works as expected

    Code
      x <- SurvivalLogLogistic()
      print(x)
    Output
      
      Log-Logistic Survival Model with parameters:
          sm_logl_lambda ~ lognormal(mu = -2.30258509299405, sigma = 5)
          sm_logl_p ~ gamma(alpha = 2, beta = 5)
          beta_os_cov ~ normal(mu = 0, sigma = 5)
      

---

    Code
      x <- SurvivalLogLogistic(beta = prior_gamma(3, 4, init = 10), p = prior_cauchy(
        0, 1))
      print(x)
    Output
      
      Log-Logistic Survival Model with parameters:
          sm_logl_lambda ~ lognormal(mu = -2.30258509299405, sigma = 5)
          sm_logl_p ~ cauchy(mu = 0, sigma = 1)
          beta_os_cov ~ gamma(alpha = 3, beta = 4)
      

