# DataJoint print method works as expected

    Code
      df_subj <- data.frame(vpt = factor(c("A", "B", "C"), levels = c("C", "B", "A")),
      varm = c("A2", "A3", "A4"), vstudy = c("S1", "S1", "S2"))
      df_surv <- data.frame(vpt = c("A", "B", "C"), vtime = c(100, 200, 150), vevent = c(
        0, 1, 1), vct = c(5, 2, 4))
      df_long <- data.frame(vpt = c("A", "A", "B", "B", "C", "B"), vtime = c(10, 10,
        20, 30, 40, 50), vout = c(1, 2, 3, 4, 5, 6))
      d_joint <- DataJoint(subject = DataSubject(data = df_subj, subject = "vpt",
        arm = "varm", study = "vstudy"), survival = DataSurvival(data = df_surv,
        formula = Surv(vtime, vevent) ~ vct), longitudinal = DataLongitudinal(data = df_long,
        formula = vout ~ vtime))
      print(d_joint)
    Output
      
       Joint-Data Object Containing:
      
            Subject-Data Object:
                # of Subjects = 3
                # of Studies  = 2
                # of Arms     = 3
      
            Survival-Data Object:
                # of Rows     = 3
                # of Columns  = 4
                # of Events   = 2
                Formula       = Surv(vtime, vevent) ~ vct
      
            Longitudinal-Data Object:
                # of Rows     = 6
                # of Columns  = 3
                # of Cen-Obvs = 0
                Formula       = vout ~ vtime 
      

---

    Code
      df_subj <- data.frame(vpt = factor(c("A", "B", "C"), levels = c("C", "B", "A")),
      varm = c("A2", "A3", "A4"), vstudy = c("S1", "S1", "S2"))
      df_surv <- data.frame(vpt = c("A", "B", "C"), vtime = c(100, 200, 150), vevent = c(
        0, 1, 1), vct = c(5, 2, 4))
      d_joint <- DataJoint(subject = DataSubject(data = df_subj, subject = "vpt",
        arm = "varm", study = "vstudy"), survival = DataSurvival(data = df_surv,
        formula = Surv(vtime, vevent) ~ vct))
      print(d_joint)
    Output
      
       Joint-Data Object Containing:
      
            Subject-Data Object:
                # of Subjects = 3
                # of Studies  = 2
                # of Arms     = 3
      
            Survival-Data Object:
                # of Rows     = 3
                # of Columns  = 4
                # of Events   = 2
                Formula       = Surv(vtime, vevent) ~ vct
      
            Longitudinal-Data Object:
                Not Specified 
      

---

    Code
      df_subj <- data.frame(vpt = factor(c("A", "B", "C"), levels = c("C", "B", "A")),
      varm = c("A2", "A3", "A4"), vstudy = c("S1", "S1", "S2"))
      df_long <- data.frame(vpt = c("A", "A", "B", "B", "C", "B"), vtime = c(10, 10,
        20, 30, 40, 50), vout = c(1, 2, 3, 4, 5, 6))
      d_joint <- DataJoint(subject = DataSubject(data = df_subj, subject = "vpt",
        arm = "varm", study = "vstudy"), longitudinal = DataLongitudinal(data = df_long,
        formula = vout ~ vtime, threshold = 4))
      print(d_joint)
    Output
      
       Joint-Data Object Containing:
      
            Subject-Data Object:
                # of Subjects = 3
                # of Studies  = 2
                # of Arms     = 3
      
            Survival-Data Object:
                Not Specified
      
            Longitudinal-Data Object:
                # of Rows     = 6
                # of Columns  = 3
                # of Cen-Obvs = 3
                Formula       = vout ~ vtime 
      

