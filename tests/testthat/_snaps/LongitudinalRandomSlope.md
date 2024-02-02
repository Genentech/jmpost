# Print method for LongitudinalRandomSlope works as expected

    Code
      x <- LongitudinalRandomSlope()
      print(x)
    Output
      
      Random Slope Longitudinal Model with parameters:
          lm_rs_intercept ~ normal(mu = 30, sigma = 10)
          lm_rs_slope_mu ~ normal(mu = 0, sigma = 15)
          lm_rs_slope_sigma ~ lognormal(mu = 0, sigma = 1.5)
          lm_rs_sigma ~ lognormal(mu = 0, sigma = 1.5)
          lm_rs_ind_rnd_slope ~ <None>
      

---

    Code
      x <- LongitudinalRandomSlope(intercept = prior_normal(0, 1), sigma = prior_gamma(
        2, 1))
      print(x)
    Output
      
      Random Slope Longitudinal Model with parameters:
          lm_rs_intercept ~ normal(mu = 0, sigma = 1)
          lm_rs_slope_mu ~ normal(mu = 0, sigma = 15)
          lm_rs_slope_sigma ~ lognormal(mu = 0, sigma = 1.5)
          lm_rs_sigma ~ gamma(alpha = 2, beta = 1)
          lm_rs_ind_rnd_slope ~ <None>
      

