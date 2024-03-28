# Quantities print method works as expected

    Code
      raw_x <- matrix(1:10, ncol = 2)
      x <- Quantities(quantities = raw_x, groups = c("a", "b"), times = c(10, 20))
      print(x)
    Output
      
         Quantities Object:
            # of samples    = 5
            # of quantities = 2 
      

