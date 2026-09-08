# Print method for SurvivalExponential works as expected

    Code
      print(x)
    Output
      
      Exponential Survival Model with parameters:
          sm_exp_lambda ~ gamma(alpha = 2, beta = 5) T[0, 8000]
          beta_os_cov ~ normal(mu = 0, sigma = 2)
      

---

    Code
      print(x)
    Output
      
      Exponential Survival Model with parameters:
          sm_exp_lambda ~ gamma(alpha = 2, beta = 5) T[0, 8000]
          beta_os_cov ~ gamma(alpha = 3, beta = 4)
      

---

    Code
      print(x)
    Output
      
      Exponential Survival Model with parameters:
          sm_exp_lambda = const(value = 1)
          beta_os_cov ~ normal(mu = 0, sigma = 2)
      

# Different priors for the beta components are possible

    Code
      print(x)
    Output
      
      Exponential Survival Model with parameters:
          sm_exp_lambda ~ gamma(alpha = 2, beta = 5) T[0, 8000]
          beta_os_cov ~ normal(mu = 0, sigma = 1)
      

---

    Code
      print(x)
    Output
      
      Exponential Survival Model with parameters:
          sm_exp_lambda ~ gamma(alpha = 2, beta = 5) T[0, 8000]
          beta_os_cov ~ normal(mus = [0, 1, 2], sigmas = [1, 2, 3])
      

