# print works as expected

    Code
      print(linkDSLD())
    Output
      
      LinkComponent with parameter:
          link_dsld ~ normal(mu = 0, sigma = 2)
      

---

    Code
      print(linkTTG(prior_beta(4, 1)))
    Output
      
      LinkComponent with parameter:
          link_ttg ~ beta(a = 4, b = 1)
      

---

    Code
      print(LinkComponent(stan = StanModule(), prior = prior_normal(0, 5), key = "bob"))
    Output
      
      LinkComponent with parameter:
          bob ~ normal(mu = 0, sigma = 5)
      

