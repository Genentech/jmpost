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
