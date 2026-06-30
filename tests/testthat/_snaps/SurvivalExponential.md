# Print method for SurvivalExponential works as expected

    Code
      x <- SurvivalExponential()
      print(x)
    Output
      
      Exponential Survival Model with parameters:
          sm_exp_lambda ~ gamma(alpha = 2, beta = 5)
          beta_os_cov ~ normal(mu = 0, sigma = 2)
      

---

    Code
      x <- SurvivalExponential(beta = prior_gamma(3, 4))
      print(x)
    Output
      
      Exponential Survival Model with parameters:
          sm_exp_lambda ~ gamma(alpha = 2, beta = 5)
          beta_os_cov ~ gamma(alpha = 3, beta = 4)
      

# Different priors for the beta components are possible

    Code
      x <- SurvivalExponential(beta = prior_normal(0, 1))
      print(x)
    Output
      
      Exponential Survival Model with parameters:
          sm_exp_lambda ~ gamma(alpha = 2, beta = 5)
          beta_os_cov ~ normal(mu = 0, sigma = 1)
      

---

    Code
      x <- SurvivalExponential(beta = prior_normal_vector(c(0, 1, 2), c(1, 2, 3)))
      print(x)
    Output
      
      Exponential Survival Model with parameters:
          sm_exp_lambda ~ gamma(alpha = 2, beta = 5)
          beta_os_cov ~ normal(mus = [0, 1, 2], sigmas = [1, 2, 3])
      

