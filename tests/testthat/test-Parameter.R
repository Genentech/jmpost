test_that("Parameters smoke tests", {
    p <- Parameter(name = "intercept", prior = prior_beta(5, 4))

    expected_mu <- 5 / (5 + 4)
    with_mocked_bindings(
        expect_equal(
            initialValues(p),
            expected_mu * 0.5
        ),
        local_rbeta = \(n, ...) rep(0, n)
    )
    expect_equal(names(p), "intercept")
})

test_that("Parameter declaration helpers render Stan code", {
    eps <- getOption("jmpost.double_eps")

    expect_equal(
        render_stan_parameter_limits(c(-Inf, Inf)),
        ""
    )
    expect_equal(
        render_stan_parameter_limits(c(eps, 1)),
        paste0("<lower=", eps, ", upper=1>")
    )

    expect_equal(
        render_stan_parameter_declaration("theta", 1, c(eps, Inf)),
        paste0("real<lower=", eps, "> theta;")
    )
    expect_equal(
        render_stan_parameter_declaration("theta", "n_subjects", c(-Inf, Inf)),
        "vector[n_subjects] theta;"
    )

    expect_equal(
        render_stan_const_declaration("theta", 1, c(eps, Inf)),
        paste0("real<lower=", eps, "> theta = prior_const_theta;")
    )
    expect_equal(
        render_stan_const_declaration("theta", "n_subjects", c(eps, Inf)),
        paste0(
            "vector<lower=", eps, ">[n_subjects] theta = ",
            "rep_vector(prior_const_theta, n_subjects);"
        )
    )
})

test_that("Parameter declaration blocks depend on prior_const", {
    eps <- getOption("jmpost.double_eps")

    sampled <- Parameter(
        name = "theta",
        prior = set_limits(prior_normal(0, 1), lower = eps),
        size = 1
    )
    sampled_module <- as.StanModule.ParameterDeclaration(sampled)
    expect_equal(
        sampled_module@parameters,
        paste0("    real<lower=", eps, "> theta;")
    )
    expect_equal(sampled_module@transformed_parameters, "")

    fixed <- Parameter(
        name = "theta",
        prior = set_limits(prior_const(1), lower = eps),
        size = 1
    )
    fixed_module <- as.StanModule.ParameterDeclaration(fixed)
    expect_equal(fixed_module@parameters, "")
    expect_equal(
        fixed_module@transformed_parameters,
        paste0("    real<lower=", eps, "> theta = prior_const_theta;")
    )
})

test_that("show() works for Paramneter objects", {
    x <- Parameter(prior_normal(1, 3), "bob", "size1")
    expect_snapshot(print(x))

    x <- Parameter(prior_beta(0.5, 0.2), "var1", "size1")
    expect_snapshot(print(x))

    x <- Parameter(prior_init_only(prior_normal(0, 1)), "x", "size1")
    expect_snapshot(print(x))
})
