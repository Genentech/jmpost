
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
        "n_subjects", "n_studies", "n_arms", "subject_study_index",
        "subject_arm_index", "subject_to_index", "arm_to_index",
        "study_to_index", "pop_arm_index", "pop_study_index"
    )

    li <- as_stan_list(obj)

    expect_equal(names(li), expected_variables)

    expect_equal(li$n_subjects, 4)
    expect_equal(li$n_studies, 2)
    expect_equal(li$n_arms, 3)
    expect_equal(li$subject_study_index, c(2, 1, 1, 2))
    expect_equal(li$subject_arm_index, c(3, 2, 1, 3))
    expect_equal(li$subject_to_index, c("C" = 1, "B" = 2, "A" = 3, "D" = 4))
    expect_equal(li$arm_to_index, c("A2" = 1, "A3" = 2, "A4" = 3))
    expect_equal(li$study_to_index, c("S1" = 1, "S2" = 2))
    expect_equal(li$pop_arm_index, c(3, 2, 1))
    expect_equal(li$pop_study_index, c(2, 1, 1))

})

test_that("DataSubject print method works as expected", {

    expect_snapshot({
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
        print(obj)
    })
})
