



test_that("Can alter Gaussian Quadrature arguements", {
    x <- data.frame(
        vpt = c("b", "a", "c", "d", "e"),
        vtime = c(10, 20, 30, 25, 15),
        vevent = c(1, 1, 0, 1, 0),
        vcov1 = c("A", "A", "B", "B", "A"),
        vcov2 = rnorm(5)
    )

    ## Test defaults 15 + "legendre"
    df <- DataSurvival(
        data = x,
        formula = Surv(vtime, vevent) ~ vcov1 * vcov2
    )
    li <- as.list(df)
    expect_equal(li$n_nodes, 15)
    expect_equal(
        li[c("nodes", "weights")],
        statmod::gauss.quad(15, "legendre")
    )


    ## Test modified values
    options(
        "jmpost.gauss_quad_n" = 20,
        "jmpost.gauss_quad_kind" = "chebyshev2"
    )
    df <- DataSurvival(
        data = x,
        formula = Surv(vtime, vevent) ~ vcov1 * vcov2
    )
    li <- as.list(df)
    expect_equal(li$n_nodes, 20)
    expect_equal(
        li[c("nodes", "weights")],
        statmod::gauss.quad(20, "chebyshev2")
    )


    ## Reset back to default to not impact additional tests
    options(
        "jmpost.gauss_quad_n" = 15,
        "jmpost.gauss_quad_kind" = "legendre"
    )
})
