test_that("DataJoint errors if subjects don't allign after", {
    df_surv <- data.frame(
        vpt = c("A", "B", "C"),
        vtime = c(100, 200, 150),
        vevent = c(0, 1, 1),
        vct = c(5, 2, 4),
        varm = c("A1", "A1", "A2"),
        vstudy = c("S1", "S1", "S2")
    )

    df_long <- data.frame(
        vpt = c("A", "A", "B", "B", "C", "B"),
        vtime = c(10, 10, 20, 30, 40, 50),
        vout = c(1, 2, 3, 4, 5, 6)
    )

    do_surv <- DataSurvival(
        data = df_surv,
        formula = Surv(vtime, vevent) ~ vct,
        subject = "vpt",
        arm = "varm",
        study = "vstudy"
    )

    do_long <- DataLongitudinal(
        data = df_long,
        formula = vout ~ vtime,
        subject = "vpt"
    )

    d_joint <- DataJoint(
        survival = do_surv,
        longitudinal = do_long
    )

    expect_equal(as.list(d_joint)$Nind, 3)
    expect_equal(as.list(d_joint)$Nta_total, 6)

    df_long2 <- df_long
    df_long2$vpt[df_long2$vpt == "C"] <- NA_character_

    df_surv2 <- df_surv
    df_surv2$vpt[df_surv2$vpt == "C"] <- NA_character_

    suppressMessages({
        do_long2 <- DataLongitudinal(
            data = df_long2,
            formula = vout ~ vtime,
            subject = "vpt"
        )
    })

    suppressMessages({
        do_surv2 <- DataSurvival(
            data = df_surv2,
            formula = Surv(vtime, vevent) ~ vct,
            subject = "vpt",
            arm = "varm",
            study = "vstudy"
        )
    })

    expect_error(
        {
            d_joint <- DataJoint(
                survival = do_surv,
                longitudinal = do_long2
            )
        },
        regexp = "subjects in the survival"
    )

    expect_error(
        {
            d_joint <- DataJoint(
                survival = do_surv2,
                longitudinal = do_long
            )
        },
        regexp = "subjects in the longitudinal"
    )
})





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
        vpt = c("A", "B", "C"),
        vtime = c(100, 200, 150),
        vevent = c(0, 1, 1),
        vct = c(5, 2, 4),
        varm = c("A1", "A1", "A2"),
        vstudy = c("S1", "S1", "S2")
    )

    df_long <- data.frame(
        vpt = c("A", "A", "B", "B", "C", "B"),
        vtime = c(10, 10, 20, 30, 40, 50),
        vout = c(1, 2, 3, 4, 5, 6)
    )

    d_joint <- DataJoint(
        survival = DataSurvival(
            data = df_surv,
            formula = Surv(vtime, vevent) ~ vct,
            subject = "vpt",
            arm = "varm",
            study = "vstudy"
        ),
        longitudinal = DataLongitudinal(
            data = df_long,
            formula = vout ~ vtime,
            subject = "vpt"
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
