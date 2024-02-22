# print works as expected

    Code
      print(link_dsld())
    Output
      
      LinkComponent with parameter:
          link_dsld ~ normal(mu = 0, sigma = 2)
      

---

    Code
      print(link_ttg(prior_beta(4, 1)))
    Output
      
      LinkComponent with parameter:
          link_ttg ~ beta(a = 4, b = 1)
      

---

    Code
      print(LinkComponent(stan = StanModule(), parameters = ParameterList(Parameter(
        prior = prior_normal(0, 5), name = "bob", size = 1)), key = "link_bob"))
    Output
      
      LinkComponent with parameter:
          bob ~ normal(mu = 0, sigma = 5)
      

