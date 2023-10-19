# DataSurvival print method works as expected

    Code
      x <- data.frame(vpt = c("b", "a", "c", "d", "e"), vtime = c(10, 20, 30, 25, 15),
      vevent = c(1, 1, 0, 1, 0), vcov1 = c("A", "A", "B", "B", "A"), vcov2 = rnorm(5),
      vcov3 = rnorm(5))
      df <- DataSurvival(data = x, formula = Surv(vtime, vevent) ~ vcov1 * vcov2 +
        vcov1:vcov2 + vcov1^2 + vcov2^2)
      print(df)
    Output
      
         Survival-Data Object:
            # of Rows     = 5
            # of Columns  = 6
            # of Events   = 3
            Formula       = Surv(vtime, vevent) ~ vcov1 * vcov2 + vcov1:vcov2 + vcov1^2 +      vcov2^2 
      

