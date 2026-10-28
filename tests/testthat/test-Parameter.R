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
            "vector<lower=",
            eps,
            ">[n_subjects] theta = ",
            "rep_vector(prior_const_theta, n_subjects);"
        )
    )
    expect_equal(
        render_stan_const_declaration(
            "theta",
            "n_subjects",
            c(eps, Inf),
            is_vector = TRUE
        ),
        paste0(
            "vector<lower=",
            eps,
            ">[n_subjects] theta = prior_const_theta;"
        )
    )
    expect_equal(
        render_stan_const_declaration(
            "theta",
            1,
            c(eps, Inf),
            is_vector = TRUE
        ),
        paste0("real<lower=", eps, "> theta = prior_const_theta[1];")
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

    fixed_vector <- Parameter(
        name = "theta",
        prior = prior_const_vector(c(1, 2)),
        size = 2
    )
    fixed_vector_module <- as.StanModule.ParameterDeclaration(fixed_vector)
    expect_equal(fixed_vector_module@parameters, "")
    expect_equal(
        fixed_vector_module@transformed_parameters,
        "    vector[2] theta = prior_const_theta;"
    )
})

test_that("horseshoe coefficients use affine non-centring", {
    prior <- prior_horseshoe()
    multiplier <- paste0(
        "scales_horseshoe(prior_local_beta, prior_global_beta, ",
        "square(prior_scale_slab_beta) * prior_slab_beta)"
    )

    for (size in list(0, 1, 3, "p")) {
        parameter <- Parameter(name = "beta", prior = prior, size = size)
        module <- as.StanModule(parameter)
        declaration <- if (identical(size, 1)) {
            paste0("    real<multiplier=(", multiplier, ")[1]> beta;")
        } else {
            paste0(
                "    vector<multiplier=", multiplier, ">[", size, "] beta;"
            )
        }
        expect_equal(tail(module@parameters, 1), declaration)

        # Stan must see the auxiliary parameters before the multiplier uses them.
        expect_equal(
            head(module@parameters, 3),
            as.StanModule(prior, name = "beta", size = size)@parameters
        )
        header <- StanModule("data {\n    int<lower=0> p;\n}")
        expect_stan_syntax(merge(
            merge(StanModule("base/functions.stan"), header),
            module
        ))
    }
})

test_that("bounded horseshoe coefficients retain their truncated prior", {
    for (limits in list(c(0, Inf), c(-Inf, 1), c(-1, 1))) {
        prior <- set_limits(prior_horseshoe(), limits[1], limits[2])
        module <- as.StanModule(Parameter(name = "beta", prior = prior, size = 3))
        expect_false(any(grepl("multiplier=", module@parameters, fixed = TRUE)))
        expect_match(
            tail(module@parameters, 1),
            paste0("vector", render_stan_parameter_limits(limits), "[3] beta;"),
            fixed = TRUE
        )
        expect_match(
            tail(module@model, 1),
            paste0("prior_scales_beta)", render_stan_limits.numeric(limits), ";"),
            fixed = TRUE
        )
        expect_stan_syntax(merge(StanModule("base/functions.stan"), module))
    }
})

test_that("partial horseshoe non-centring supports scalar and vector forms", {
    prior <- prior_horseshoe(scale_noncentred = 0.1)
    expect_equal(
        as_stan_list(prior, name = "beta"),
        as_stan_list(prior_horseshoe(), name = "beta")
    )
    for (size in list(0, 1, 3, "p")) {
        module <- as.StanModule(Parameter(
            name = "beta", prior = prior, size = size
        ))
        expect_match(tail(module@parameters, 1), "sqrt(", fixed = TRUE)
        header <- StanModule("data {\n    int<lower=0> p;\n}")
        expect_stan_syntax(merge(
            merge(StanModule("base/functions.stan"), header), module
        ))
    }
})

test_that("saved priors without affine multiplier metadata still render", {
    prior <- prior_normal(0, 1)
    attr(prior, "repr_multiplier") <- NULL
    module <- as.StanModule(Parameter(name = "beta", prior = prior))
    expect_equal(module@parameters, "    real beta;")
})

test_that("show() works for Paramneter objects", {
    x <- Parameter(prior_normal(1, 3), "bob", "size1")
    expect_snapshot(print(x))

    x <- Parameter(prior_beta(0.5, 0.2), "var1", "size1")
    expect_snapshot(print(x))

    x <- Parameter(prior_init_only(prior_normal(0, 1)), "x", "size1")
    expect_snapshot(print(x))
})
