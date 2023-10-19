# show() works for Prior objects

    Code
      print(prior_cauchy(0, 0.8, init = 4))
    Output
      
      Prior Object:
         cauchy(mu = 0, sigma = 0.8)
      

---

    Code
      print(prior_normal(0, 0.8))
    Output
      
      Prior Object:
         normal(mu = 0, sigma = 0.8)
      

---

    Code
      print(prior_std_normal())
    Output
      
      Prior Object:
         std_normal()
      

---

    Code
      print(prior_beta(5, 1))
    Output
      
      Prior Object:
         beta(a = 5, b = 1)
      

---

    Code
      print(prior_gamma(2.56, 12))
    Output
      
      Prior Object:
         gamma(alpha = 2.56, beta = 12)
      

---

    Code
      print(prior_none())
    Output
      
      Prior Object:
         <None>
      

