# Print method for LinkGSF works as expected

    Code
      x <- LinkGSF()
      print(x)
    Output
      
      GSF (dSLD + TTG) Link with parameters:
          lm_gsf_beta ~ normal(mu = 0, sigma = 5)
          lm_gsf_gamma ~ normal(mu = 0, sigma = 5)
      

---

    Code
      x <- LinkGSF(link_gsf_identity(tau = prior_cauchy(1, 2)), link_gsf_dsld(beta = prior_gamma(
        2, 2)))
      print(x)
    Output
      
      GSF (Identity + dSLD) Link with parameters:
          lm_gsf_tau ~ cauchy(mu = 1, sigma = 2)
          lm_gsf_beta ~ gamma(alpha = 2, beta = 2)
      

