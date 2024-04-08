test_that("DataJoint basic usage", {
    df_subj <- data.frame(
        vpt = factor(c("A", "B", "C"), levels = c("C", "B", "A")),
        varm = c("A2", "A3", "A4"),
        vstudy = c("S1", "S1", "S2")
    )

    df_surv <- data.frame(
        vpt = c("A", "B", "C"),
        vtime = c(100, 200, 150),
        vevent = c(0, 1, 1),
        vct = c(5, 2, 4)
    )

    df_long <- data.frame(
        vpt = c("A", "A", "B", "B", "C", "B"),
        vtime = c(10, 10, 20, 30, 40, 50),
        vout = c(1, 2, 3, 4, 5, 6)
    )

    d_joint <- DataJoint(
        subject = DataSubject(
            data = df_subj,
            subject = "vpt",
            arm = "varm",
            study = "vstudy"
        ),
        survival = DataSurvival(
            data = df_surv,
            formula = Surv(vtime, vevent) ~ vct
        ),
        longitudinal = DataLongitudinal(
            data = df_long,
            formula = vout ~ vtime
        )
    )

    li <- as.list(d_joint)
    expect_equal(li$n_subjects, 3)
    expect_equal(li$n_tumour_all, 6)
    expect_equal(li$subject_to_index, c("C" = 1, "B" = 2, "A" = 3))
    expect_equal(li$n_arms, 3)
    expect_equal(li$subject_study_index, c(2, 1, 1))
    expect_equal(li$n_studies, 2)
    expect_equal(li$subject_arm_index, c(3, 2, 1))
    expect_equal(li$event_times, c(150, 200, 100))
    expect_equal(li$subject_tumour_index, c(1, 2, 2, 2, 3, 3))
    expect_equal(li$subject_tumour_index_obs, c(1:6))
    expect_equal(li$subject_tumour_index_cens, integer(0))
    expect_equal(li$tumour_value, c(5, 3, 4, 6, 1, 2))
    expect_equal(li$subject_event_index, c(1, 2))
    expect_equal(li$os_cov_design, matrix(c(4, 2, 5), ncol = 1), ignore_attr = TRUE)
    expect_equal(li$tumour_value_lloq, -999999)
})



test_that("DataJoint errors if inconsistent subject IDs", {

    df_subj <- data.frame(
        vpt = factor(c("A", "B", "C"), levels = c("C", "B", "A")),
        varm = c("A2", "A3", "A4"),
        vstudy = c("S1", "S1", "S2")
    )

    df_surv <- data.frame(
        vpt = c("A", NA_character_, "C"),
        vtime = c(100, 200, 150),
        vevent = c(0, 1, 1),
        vct = c(5, 2, 4)
    )

    df_long <- data.frame(
        vpt = c("A", "A", "B", "B", NA_character_, "B"),
        vtime = c(10, 10, 20, 30, 40, 50),
        vout = c(1, 2, 3, 4, 5, 6)
    )

    expect_error(
        DataJoint(
            subject = DataSubject(
                data = df_subj,
                subject = "vpt",
                arm = "varm",
                study = "vstudy"
            ),
            survival = DataSurvival(
                data = df_surv,
                formula = Surv(vtime, vevent) ~ vct
            )
        ),
        "`survival` that are not present"
    )

    expect_error(
        DataJoint(
            subject = DataSubject(
                data = df_subj,
                subject = "vpt",
                arm = "varm",
                study = "vstudy"
            ),
            longitudinal = DataLongitudinal(
                data = df_long,
                formula = vout ~ vtime
            )
        ),
        "`longitudinal` that are not present"
    )


    df_subj <- data.frame(
        vpt = factor(c("A", "B", "C", "D"), levels = c("C", "B", "A", "D")),
        varm = c("A2", "A3", "A4", "A4"),
        vstudy = c("S1", "S1", "S2", "S2")
    )

    df_surv <- data.frame(
        vpt = c("A", "B", "C"),
        vtime = c(100, 200, 150),
        vevent = c(0, 1, 1),
        vct = c(5, 2, 4)
    )

    df_long <- data.frame(
        vpt = c("A", "A", "B", "B", "B", "B"),
        vtime = c(10, 10, 20, 30, 40, 50),
        vout = c(1, 2, 3, 4, 5, 6)
    )

    expect_error(
        DataJoint(
            subject = DataSubject(
                data = df_subj,
                subject = "vpt",
                arm = "varm",
                study = "vstudy"
            ),
            survival = DataSurvival(
                data = df_surv,
                formula = Surv(vtime, vevent) ~ vct
            )
        ),
        "`subjects` that are not present in `survival`"
    )

    expect_error(
        DataJoint(
            subject = DataSubject(
                data = df_subj,
                subject = "vpt",
                arm = "varm",
                study = "vstudy"
            ),
            longitudinal = DataLongitudinal(
                data = df_long,
                formula = vout ~ vtime
            )
        ),
        "`subjects` that are not present in `longitudinal"
    )

    df_subj <- data.frame(
        vpt = factor(c("A", "B", "C", "D"), levels = c("C", "B", "A", "D")),
        varm = c("A2", "A3", "A4", "A4"),
        vstudy = c("S1", "S1", "S2", "S2")
    )

    df_surv <- data.frame(
        vpt2 = c("A", "B", "C"),
        vtime = c(100, 200, 150),
        vevent = c(0, 1, 1),
        vct = c(5, 2, 4)
    )

    df_long <- data.frame(
        vpt2 = c("A", "A", "B", "B", "B", "B"),
        vtime = c(10, 10, 20, 30, 40, 50),
        vout = c(1, 2, 3, 4, 5, 6)
    )

    expect_error(
        DataJoint(
            subject = DataSubject(
                data = df_subj,
                subject = "vpt",
                arm = "varm",
                study = "vstudy"
            ),
            survival = DataSurvival(
                data = df_surv,
                formula = Surv(vtime, vevent) ~ vct
            )
        ),
        "`vpt` not found in `survival`"
    )

    expect_error(
        DataJoint(
            subject = DataSubject(
                data = df_subj,
                subject = "vpt",
                arm = "varm",
                study = "vstudy"
            ),
            longitudinal = DataLongitudinal(
                data = df_long,
                formula = vout ~ vtime
            )
        ),
        "`vpt` not found in `longitudinal`"
    )

})






test_that("DataJoint sorts handles pre-factored levels correctly", {
    df_subj <- data.frame(
        vpt = factor(c("A", "B", "C"), levels = c("C", "B", "A")),
        varm = c("A2", "A3", "A4"),
        vstudy = c("S1", "S1", "S2")
    )

    df_surv <- data.frame(
        vpt = c("A", "B", "C"),
        vtime = c(100, 200, 150),
        vevent = c(0, 1, 1),
        vct = c(5, 2, 4)
    )

    df_long <- data.frame(
        vpt = c("A", "A", "B", "B", "C", "B"),
        vtime = c(10, 10, 20, 30, 40, 50),
        vout = c(1, 2, 3, 4, 5, 6)
    )

    d_joint <- DataJoint(
        subject = DataSubject(
            data = df_subj,
            subject = "vpt",
            arm = "varm",
            study = "vstudy"
        ),
        survival = DataSurvival(
            data = df_surv,
            formula = Surv(vtime, vevent) ~ vct
        ),
        longitudinal = DataLongitudinal(
            data = df_long,
            formula = vout ~ vtime
        )
    )

    df_subj2 <- data.frame(
        vpt = factor(c("C", "B", "A"), levels = c("C", "B", "A")),
        varm = factor(c("A4", "A3", "A2"), levels = c("A2", "A3", "A4")),
        vstudy = factor(c("S2", "S1", "S1"), levels = c("S1", "S2"))
    )
    expect_equal(
        as.data.frame(d_joint@subject),
        df_subj2
    )

    df_long2 <- df_long |>
        dplyr::mutate(vpt = factor(vpt, levels = levels(df_subj$vpt))) |>
        dplyr::arrange(vpt, vtime, vout)
    expect_equal(
        as.data.frame(d_joint@longitudinal),
        df_long2
    )

    df_surv2 <- data.frame(
        vpt = factor(c("C", "B", "A"), levels = levels(df_subj$vpt)),
        vtime = c(150, 200, 100),
        vevent = c(1, 1, 0),
        vct = c(4, 2, 5)
    )
    expect_equal(
        as.data.frame(d_joint@survival),
        df_surv2
    )
})


test_that("DataJoint sorts handles character-to-factor levels correctly", {
    df_subj <- data.frame(
        vpt = c("B", "A", "C"),
        varm = c("A2", "A3", "A4"),
        vstudy = c("S1", "S1", "S2")
    )

    df_surv <- data.frame(
        vpt = c("A", "B", "C"),
        vtime = c(100, 200, 150),
        vevent = c(0, 1, 1),
        vct = c(5, 2, 4)
    )

    df_long <- data.frame(
        vpt = c("A", "A", "B", "B", "C", "B"),
        vtime = c(10, 10, 20, 30, 40, 50),
        vout = c(1, 2, 3, 4, 5, 6)
    )

    d_joint <- DataJoint(
        subject = DataSubject(
            data = df_subj,
            subject = "vpt",
            arm = "varm",
            study = "vstudy"
        ),
        survival = DataSurvival(
            data = df_surv,
            formula = Surv(vtime, vevent) ~ vct
        ),
        longitudinal = DataLongitudinal(
            data = df_long,
            formula = vout ~ vtime
        )
    )

    df_subj2 <- data.frame(
        vpt = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
        varm = factor(c("A3", "A2", "A4"), levels = c("A2", "A3", "A4")),
        vstudy = factor(c("S1", "S1", "S2"), levels = c("S1", "S2"))
    )
    expect_equal(
        as.data.frame(d_joint@subject),
        df_subj2
    )

    df_long2 <- df_long |>
        dplyr::mutate(vpt = factor(vpt, levels = c("A", "B", "C"))) |>
        dplyr::arrange(vpt, vtime, vout)
    expect_equal(
        as.data.frame(d_joint@longitudinal),
        df_long2
    )

    df_surv2 <- data.frame(
        vpt = factor(c("A", "B", "C")),
        vtime = c(100, 200, 150),
        vevent = c(0, 1, 1),
        vct = c(5, 2, 4)
    )
    expect_equal(
        as.data.frame(d_joint@survival),
        df_surv2
    )
})



# nolint start
test_that("subset(DataJoint) works as expected", {

    dat <- dplyr::tribble(
        ~patient, ~time, ~event,
           "a",     1,      1,
           "b",     1,      0,
           "c",     2,      1,
           "d",     2,      0,
           "e",     3,      1,
           "f",     3,      0,
           "g",     4,      1
    )
    pts <- list(
        "g1" = c("a", "e", "f"),
        "g2" = c("b", "c"),
        "g3" = "d"
    )
    expected <- dplyr::tribble(
        ~patient, ~time, ~event, ~group,
           "a",     1,      1,    "g1",
           "e",     3,      1,    "g1",
           "f",     3,      0,    "g1",
           "b",     1,      0,    "g2",
           "c",     2,      1,    "g2",
           "d",     2,      0,    "g3"
    )
    expect_equal(
        subset_and_add_grouping(dat, pts),
        expected
    )

    pts <- c("b", "d", "a")
    expected <- dplyr::tribble(
        ~patient, ~time, ~event, ~group,
           "b",     1,      0,    "b",
           "d",     2,      0,    "d",
           "a",     1,      1,    "a"
    )
    expect_equal(
        subset_and_add_grouping(dat, pts),
        expected
    )

    # Duplicate values between groups should be ok
    pts <- list(
        "g1" = c("a", "b", "c"),
        "g2" = c("a", "b", "c")
    )
    expected <- dplyr::tribble(
        ~patient, ~time, ~event, ~group,
           "a",     1,      1,    "g1",
           "b",     1,      0,    "g1",
           "c",     2,      1,    "g1",
           "a",     1,      1,    "g2",
           "b",     1,      0,    "g2",
           "c",     2,      1,    "g2"
    )
    expect_equal(
        subset_and_add_grouping(dat, pts),
        expected
    )


    # Should error for patients that dosn't exist in vector mode
    pts <- c("b", "d", "a", "z")
    expect_error(
        subset_and_add_grouping(dat, pts),
        regexp = "`patients`"
    )
    # Should error for patients that don't exist in list mode
    pts <- list("g1" = c("a", "z", "b"))
    expect_error(
        subset_and_add_grouping(dat, pts),
        regexp = "`patients`"
    )
    # Should error if we ask for the same patient multiple times
    pts <- c("b", "d", "a", "a")
    expect_error(
        subset_and_add_grouping(dat, pts),
        regexp = "`patients`"
    )


    df_surv <- data.frame(
        vpt = c("C", "B", "A"),
        vtime = c(150, 200, 100),
        vevent = c(1, 1, 0),
        vct = c(4, 2, 5),
        varm = c("A2", "A1", "A1"),
        vstudy = c("S2", "S1", "S1")
    )

    df_long <- data.frame(
        vpt = c("A", "A", "B", "B", "C", "B"),
        vtime = c(10, 10, 20, 30, 40, 50),
        vout = c(1, 2, 3, 4, 5, 6)
    )

    d_joint <- DataJoint(
        subject = DataSubject(
            data = df_surv,
            subject = "vpt",
            arm = "varm",
            study = "vstudy"
        ),
        survival = DataSurvival(
            data = df_surv,
            formula = Surv(vtime, vevent) ~ vct
        ),
        longitudinal = DataLongitudinal(
            data = df_long,
            formula = vout ~ vtime
        )
    )

    expected <- data.frame(
        time = c(150, 100),
        event = c(1, 0),
        patient = c("C", "A"),
        group = c("C", "A"),
        row.names = NULL
    )
    expect_equal(
        subset(d_joint, c("C", "A")),
        expected
    )

})
# nolint end

test_that("Error Handling", {

    x2 <- data.frame(
        varm = c("A", "A", "A"),
        vstudy = c("A", "A", "A"),
        vpt = c("b", "b", "c"),
        vtime = c(10, 20, 30),
        vevent = c(1, 1, 1)
    )

    expect_error(
        {
            DataJoint(
                survival = DataSurvival(
                    data = x2,
                    formula = Surv(vtime, vevent) ~ 1
                ),
                subject = DataSubject(x2, "vpt", "varm", "vstudy")
            )
        },
        "Contains duplicated values"
    )
})

test_that("DataJoint print method works as expected", {

    expect_snapshot({
        df_subj <- data.frame(
            vpt = factor(c("A", "B", "C"), levels = c("C", "B", "A")),
            varm = c("A2", "A3", "A4"),
            vstudy = c("S1", "S1", "S2")
        )

        df_surv <- data.frame(
            vpt = c("A", "B", "C"),
            vtime = c(100, 200, 150),
            vevent = c(0, 1, 1),
            vct = c(5, 2, 4)
        )

        df_long <- data.frame(
            vpt = c("A", "A", "B", "B", "C", "B"),
            vtime = c(10, 10, 20, 30, 40, 50),
            vout = c(1, 2, 3, 4, 5, 6)
        )

        d_joint <- DataJoint(
            subject = DataSubject(
                data = df_subj,
                subject = "vpt",
                arm = "varm",
                study = "vstudy"
            ),
            survival = DataSurvival(
                data = df_surv,
                formula = Surv(vtime, vevent) ~ vct
            ),
            longitudinal = DataLongitudinal(
                data = df_long,
                formula = vout ~ vtime
            )
        )
        print(d_joint)
    })

    expect_snapshot({
        df_subj <- data.frame(
            vpt = factor(c("A", "B", "C"), levels = c("C", "B", "A")),
            varm = c("A2", "A3", "A4"),
            vstudy = c("S1", "S1", "S2")
        )

        df_surv <- data.frame(
            vpt = c("A", "B", "C"),
            vtime = c(100, 200, 150),
            vevent = c(0, 1, 1),
            vct = c(5, 2, 4)
        )

        d_joint <- DataJoint(
            subject = DataSubject(
                data = df_subj,
                subject = "vpt",
                arm = "varm",
                study = "vstudy"
            ),
            survival = DataSurvival(
                data = df_surv,
                formula = Surv(vtime, vevent) ~ vct
            )
        )
        print(d_joint)
    })

    expect_snapshot({
        df_subj <- data.frame(
            vpt = factor(c("A", "B", "C"), levels = c("C", "B", "A")),
            varm = c("A2", "A3", "A4"),
            vstudy = c("S1", "S1", "S2")
        )

        df_long <- data.frame(
            vpt = c("A", "A", "B", "B", "C", "B"),
            vtime = c(10, 10, 20, 30, 40, 50),
            vout = c(1, 2, 3, 4, 5, 6)
        )

        d_joint <- DataJoint(
            subject = DataSubject(
                data = df_subj,
                subject = "vpt",
                arm = "varm",
                study = "vstudy"
            ),
            longitudinal = DataLongitudinal(
                data = df_long,
                formula = vout ~ vtime,
                threshold = 4
            )
        )
        print(d_joint)
    })
})
