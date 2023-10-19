# Print method for SurvivalExponential works as expected

    Code
      x <- SurvivalExponential()
      print(x)
    Output
      
      Exponential Survival Model with parameters:
          sm_exp_lambda ~ gamma(alpha = 2, beta = 5)
          beta_os_cov ~ normal(mu = 0, sigma = 5)
      

---

    Code
      x <- SurvivalExponential(beta = prior_gamma(3, 4, init = 10))
      print(x)
    Output
      
      Exponential Survival Model with parameters:
          sm_exp_lambda ~ gamma(alpha = 2, beta = 5)
          beta_os_cov ~ gamma(alpha = 3, beta = 4)
      

