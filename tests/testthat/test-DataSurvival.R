

test_that("DataSurvival being rendered to list is as expected for simple inputs", {

    x <- data.frame(
        vpt = c("b", "a", "c", "d", "e"),
        vtime = c(10, 20, 30, 25, 15),
        vevent = c(1, 1, 0, 1, 0),
        vcov1 = c("A", "A", "B", "B", "A"),
        vcov2 = rnorm(5)
    )

    df <- DataSurvival(
        data = x,
        formula = Surv(vtime, vevent) ~ vcov1 * vcov2
    )

    res <- as_stan_list(df)
    covmat <- model.matrix(~ vcov1 * vcov2, data = x)
    covmat <- covmat[, -1]
    rownames(covmat) <- NULL

    expect_equal(
        c(
            "Nind_dead", "dead_ind_index", "Times", "p_os_cov_design",
            "os_cov_design", "n_nodes", "nodes", "weights"
        ),
        names(res)
    )
    expect_equal(res$Nind_dead, 3)
    expect_equal(res$p_os_cov_design, 3)
    expect_equal(res$os_cov_design, covmat)
    expect_equal(res$dead_ind_index, c(1, 2, 4))
    expect_equal(res$Times, c(10, 20, 30, 25, 15))


    ## Dropped rows works as expected

    x <- data.frame(
        vpt = c("b", "a", "c", "d", "e"),
        vtime = c(10, 20, 30, 25, 15),
        vevent = c(1, 1, 0, 1, 0),
        vcov1 = c("A", "A", "B", NA, "A"),
        vcov2 = rnorm(5)
    )
    expect_message(
        obj <- DataSurvival(
            data = x,
            formula = Surv(vtime, vevent) ~ vcov1 * vcov2
        ),
        "1 observation(s) were removed"
    )
    expect_equal(
        as.data.frame(obj),
        x |> dplyr::filter(!is.na(vcov1))
    )
    res <- as_stan_list(obj)
    covmat <- model.matrix(~ vcov1 * vcov2, data = x)
    covmat <- covmat[, -1]
    rownames(covmat) <- NULL

    expect_equal(
        c(
            "Nind_dead", "dead_ind_index", "Times", "p_os_cov_design",
            "os_cov_design", "n_nodes", "nodes", "weights"
        ),
        names(res)
    )
    expect_equal(res$Nind_dead, 2)
    expect_equal(res$p_os_cov_design, 3)
    expect_equal(res$os_cov_design, covmat)
    expect_equal(res$dead_ind_index, c(1, 2))
    expect_equal(res$Times, c(10, 20, 30, 15))
})


test_that("DataSurvival print method works as expected", {

    expect_snapshot({
        x <- data.frame(
            vpt = c("b", "a", "c", "d", "e"),
            vtime = c(10, 20, 30, 25, 15),
            vevent = c(1, 1, 0, 1, 0),
            vcov1 = c("A", "A", "B", "B", "A"),
            vcov2 = rnorm(5),
            vcov3 = rnorm(5)
        )

        df <- DataSurvival(
            data = x,
            formula = Surv(vtime, vevent) ~ vcov1 * vcov2 + vcov1:vcov2 + vcov1^2 + vcov2^2
        )
        print(df)
    })

})
