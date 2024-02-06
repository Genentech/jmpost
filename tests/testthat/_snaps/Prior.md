# show() works for Prior objects

    Code
      print(prior_cauchy(0, 0.8))
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
      print(prior_init_only(prior_normal(1, 4)))
    Output
      
      Prior Object:
         <None>
      

---

    Code
      print(prior_uniform(8, 10))
    Output
      
      Prior Object:
         uniform(alpha = 8, beta = 10)
      

---

    Code
      print(prior_student_t(3, 10, 4))
    Output
      
      Prior Object:
         student_t(nu = 3, mu = 10, sigma = 4)
      

---

    Code
      print(prior_logistic(sigma = 2, 10))
    Output
      
      Prior Object:
         logistic(mu = 10, sigma = 2)
      

---

    Code
      print(prior_loglogistic(1, 2))
    Output
      
      Prior Object:
         loglogistic(alpha = 1, beta = 2)
      

---

    Code
      print(prior_invgamma(alpha = 1, beta = 2))
    Output
      
      Prior Object:
         inv_gamma(alpha = 1, beta = 2)
      

