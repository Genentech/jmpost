# LongitudinalQuantities print method works as expected

    Code
      subjectgroups <- c("subject_0011", "subject_0061", "subject_0001",
        "subject_0002")
      times <- seq(0, 100, by = 10)
      samps_p1 <- LongitudinalQuantities(test_data_1$jsamples, grid = GridFixed(
        subjects = subjectgroups, times = times))
      print(samps_p1)
    Output
      
       LongitudinalQuantities Object:
          # of samples    = 200
          # of quantities = 44 
      

---

    Code
      subjectgroups <- c("subject_0011", "subject_0061")
      samps_p2 <- LongitudinalQuantities(test_data_1$jsamples, grid = GridFixed(
        subjects = subjectgroups))
      print(samps_p2)
    Output
      
       LongitudinalQuantities Object:
          # of samples    = 200
          # of quantities = 402 
      

