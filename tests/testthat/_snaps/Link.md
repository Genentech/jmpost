# Link prints as expected

    Code
      print(Link())
    Output
      
      No Link

---

    Code
      print(Link(linkDSLD()))
    Output
      
      Link with the following components/parameters:
          link_dsld ~ normal(mu = 0, sigma = 2)

---

    Code
      print(Link(linkDSLD(), linkIdentity()))
    Output
      
      Link with the following components/parameters:
          link_dsld ~ normal(mu = 0, sigma = 2)
          link_identity ~ normal(mu = 0, sigma = 2)

---

    Code
      link <- resolvePromise(Link(linkDSLD(), linkIdentity()), LongitudinalGSF())
      print(link)
    Output
      
      Link with the following components/parameters:
          link_dsld ~ normal(mu = 0, sigma = 2)
          link_identity ~ normal(mu = 0, sigma = 2)

---

    Code
      link <- resolvePromise(Link(linkDSLD(), linkIdentity()), LongitudinalGSF())
      print(link)
    Output
      
      Link with the following components/parameters:
          link_dsld ~ normal(mu = 0, sigma = 2)
          link_identity ~ normal(mu = 0, sigma = 2)

