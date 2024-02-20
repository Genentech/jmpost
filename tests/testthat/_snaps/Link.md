# Link prints as expected

    Code
      print(Link())
    Output
      
      No Link

---

    Code
      print(Link(link_dsld()))
    Output
      
      Link with the following components/parameters:
          link_dsld ~ normal(mu = 0, sigma = 2)

---

    Code
      print(Link(link_dsld(), link_identity()))
    Output
      
      Link with the following components/parameters:
          link_dsld ~ normal(mu = 0, sigma = 2)
          link_identity ~ normal(mu = 0, sigma = 2)

