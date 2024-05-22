# SurvivalQuantities print method works as expected

    Code
      set.seed(3219)
      subjectgroups <- list(gtsubject1 = sample(test_data_1$dat_os$subject, 20),
      gtsubject2 = sample(test_data_1$dat_os$subject, 20), gtsubject3 = sample(
        test_data_1$dat_os$subject, 20))
      times <- seq(0, 100, by = 10)
      samps_p1 <- SurvivalQuantities(test_data_1$jsamples, grid = GridGrouped(groups = subjectgroups,
        times = times), type = "surv")
      print(samps_p1)
    Output
      
       SurvivalQuantities Object:
          # of samples    = 100
          # of quantities = 33
          Type            = surv 
      

---

    Code
      times <- seq(0, 100, by = 10)
      samps_p2 <- SurvivalQuantities(test_data_1$jsamples, grid = GridFixed(times = times),
      type = "loghaz")
      print(samps_p2)
    Output
      
       SurvivalQuantities Object:
          # of samples    = 100
          # of quantities = 4400
          Type            = loghaz 
      

