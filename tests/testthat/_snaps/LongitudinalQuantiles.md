# LongitudinalQuantities print method works as expected

    Code
      ptgroups <- c("pt_0011", "pt_0061", "pt_0001", "pt_0002")
      times <- seq(0, 100, by = 10)
      samps_p1 <- LongitudinalQuantities(test_data_1$jsamples, grid = GridFixed(
        subjects = ptgroups, times = times))
      print(samps_p1)
    Output
      
       LongitudinalQuantities Object:
          # of samples    = 100
          # of quantities = 44 
      

---

    Code
      ptgroups <- c("pt_0011", "pt_0061")
      samps_p2 <- LongitudinalQuantities(test_data_1$jsamples, grid = GridFixed(
        subjects = ptgroups))
      print(samps_p2)
    Output
      
       LongitudinalQuantities Object:
          # of samples    = 100
          # of quantities = 402 
      

