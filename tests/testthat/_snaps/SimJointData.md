# print methods work as expected

    Code
      sim_data <- SimJointData(longitudinal = SimLongitudinalGSF(), survival = SimSurvivalExponential(
        time_max = 4, lambda = 365 / 100, time_step = 1 / 365), .silent = TRUE)
      print(sim_data)
    Output
      
      A SimJointData Object
      

