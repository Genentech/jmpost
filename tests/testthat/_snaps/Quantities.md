# Quantities print method works as expected

    Code
      raw_x1 <- matrix(1:12, ncol = 3)
      raw_x2 <- matrix(21:32, ncol = 3)
      pobj <- Quantities(list(raw_x1, raw_x2))
      print(pobj)
    Output
      
         Quantities Object:
            # of Elements  = 2
            # of Rows      = 4
            # of Columns   = 3 
      

