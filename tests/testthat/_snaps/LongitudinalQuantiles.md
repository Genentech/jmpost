# LongitudinalQuantities print method works as expected

    Code
      ptgroups <- c("pt_011", "pt_061", "pt_001", "pt_002")
      times <- seq(0, 100, by = 10)
      samps_p1 <- LongitudinalQuantities(test_data_1$jsamples, ptgroups, times)
      print(samps_p1)
    Output
      
         LongitudinalQuantities Object:
            # of Subjects     = 4
            # of Time Points  = 11 
      

---

    Code
      ptgroups <- c("pt_011", "pt_061")
      samps_p2 <- LongitudinalQuantities(test_data_1$jsamples, ptgroups)
      print(samps_p2)
    Output
      
         LongitudinalQuantities Object:
            # of Subjects     = 2
            # of Time Points  = 201 
      

