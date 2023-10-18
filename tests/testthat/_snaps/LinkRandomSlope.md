# Print method for LinkRandomSlope works as expected

    Code
      x <- LinkRandomSlope()
      print(x)
    Output
      
      Random Slope Link with parameters:
          link_lm_phi ~ normal(mu = 0.2, sigma = 0.5)
      

---

    Code
      x <- LinkRandomSlope(link_lm_phi = prior_normal(0, 1))
      print(x)
    Output
      
      Random Slope Link with parameters:
          link_lm_phi ~ normal(mu = 0, sigma = 1)
      

