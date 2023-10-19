# DataSubject print method works as expected

    Code
      df_subj <- data.frame(vpt = factor(c("A", "B", "C", "D"), levels = c("C", "B",
        "A", "D")), varm = c("A2", "A3", "A4", "A4"), vstudy = c("S1", "S1", "S2",
        "S2"))
      obj <- DataSubject(data = df_subj, subject = "vpt", arm = "varm", study = "vstudy")
      print(obj)
    Output
      
         Subject-Data Object:
            # of Subjects = 4
            # of Studies  = 2
            # of Arms     = 3 
      

