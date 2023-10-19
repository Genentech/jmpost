# DataSurvival print method works as expected

    Code
      x <- data.frame(vpt = factor(c("b", "a", "a", "b", "c", "a"), levels = c("b",
        "c", "a")), vtime = c(10, 20, 15, 5, 25, 35), voutcome = c(2, 1, 4, 5, 6, 2))
      dl <- DataLongitudinal(data = x, formula = voutcome ~ vtime, threshold = 3)
      print(dl)
    Output
      
         Longitudinal-Data Object:
            # of Rows     = 6
            # of Columns  = 3
            # of Cen-Obvs = 3
            Formula       = voutcome ~ vtime 
      

---

    Code
      x <- data.frame(vpt = factor(c("b", "a", "a", "b", "c", "a"), levels = c("b",
        "c", "a")), vtime = c(10, 20, 15, 5, 25, 35), voutcome = c(2, 1, 4, 5, 6, 2))
      dl <- DataLongitudinal(data = x, formula = voutcome ~ vtime)
      print(dl)
    Output
      
         Longitudinal-Data Object:
            # of Rows     = 6
            # of Columns  = 3
            # of Cen-Obvs = 0
            Formula       = voutcome ~ vtime 
      

