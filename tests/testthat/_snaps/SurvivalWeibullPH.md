# Print method for SurvivalWeibullPH works as expected

    Code
      x <- SurvivalWeibullPH()
      print(x)
    Output
      
      Weibull-PH Survival Model with parameters:
          sm_weibull_ph_lambda ~ gamma(alpha = 2, beta = 0.5) T[0, ]
          sm_weibull_ph_gamma ~ gamma(alpha = 2, beta = 0.5) T[0, ]
          beta_os_cov ~ normal(mu = 0, sigma = 2)
      

---

    Code
      x <- SurvivalWeibullPH(beta = prior_gamma(3, 4), gamma = prior_cauchy(0, 1))
      print(x)
    Output
      
      Weibull-PH Survival Model with parameters:
          sm_weibull_ph_lambda ~ gamma(alpha = 2, beta = 0.5) T[0, ]
          sm_weibull_ph_gamma ~ cauchy(mu = 0, sigma = 1) T[0, ]
          beta_os_cov ~ gamma(alpha = 3, beta = 4)
      

