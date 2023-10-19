# smoke test for summary(SurvivalQuantities) and autoplot(SurvivalQuantities)

    Code
      ptgroups <- list(gtpt1 = sample(dat_os$pt, 20), gtpt2 = sample(dat_os$pt, 20),
      gtpt3 = sample(dat_os$pt, 20))
      times <- seq(0, 100, by = 10)
      samps_p1 <- SurvivalQuantities(mp, ptgroups, times, type = "surv")
      print(samps_p1)
    Output
      
         SurvivalQuantities Object:
            Type              = surv
            # of Groups       = 3
            # of Time Points  = 11 
      

---

    Code
      times <- seq(0, 100, by = 10)
      samps_p2 <- SurvivalQuantities(mp, time_grid = times, type = "loghaz")
      print(samps_p2)
    Output
      
         SurvivalQuantities Object:
            Type              = loghaz
            # of Groups       = 400
            # of Time Points  = 11 
      

