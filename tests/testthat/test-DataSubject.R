
test_that("DataSubject works as expected", {
    df_subj <- data.frame(
        vpt = factor(c("A", "B", "C", "D"), levels = c("C", "B", "A", "D")),
        varm = c("A2", "A3", "A4", "A4"),
        vstudy = c("S1", "S1", "S2", "S2")
    )

    obj <- DataSubject(
        data = df_subj,
        subject = "vpt",
        arm = "varm",
        study = "vstudy"
    )

    expect_equal(obj@subject, "vpt")
    expect_equal(obj@study, "vstudy")
    expect_equal(obj@arm, "varm")


    expected_variables <- c(
        "Nind", "n_studies", "n_arms", "pt_study_index",
        "pt_arm_index", "pt_to_ind"
    )

    li <- as_stan_list(obj)

    expect_equal(names(li), expected_variables)

    expect_equal(li$Nind, 4)
    expect_equal(li$n_studies, 2)
    expect_equal(li$n_arms, 3)
    expect_equal(li$pt_study_index, c(2, 1, 1, 2))
    expect_equal(li$pt_arm_index, c(3, 2, 1, 3))
    expect_equal(li$pt_to_ind, c("C" = 1, "B" = 2, "A" = 3, "D" = 4))
})

