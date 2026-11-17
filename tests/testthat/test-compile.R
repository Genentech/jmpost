model <- "
data {
    int n;
    array[n] real x;
}

parameters {
    real mu;
    real sigma;
}

model {
    target += normal_lpdf(x | mu, sigma);
}
"

test_that("compileStanModel doesn't error if the directory doesn't exist", {
    smod <- StanModule(model)
    fpath <- file.path(tempdir(), "abcd", "efg", "model")
    old_path <- options("jmpost.cache_dir")
    options("jmpost.cache_dir" = fpath)
    z <- compileStanModel(smod)
    options(old_path)
    expect_true(file.exists(fpath))
    expect_true(file.exists(z$exe_file()))
    expect_equal(normalizePath(dirname(z$exe_file())), normalizePath(fpath))
    expect_equal(
        basename(z$exe_file()),
        tools::file_path_sans_ext(
            basename(z$stan_file())
        ) |>
            paste0(if (is_windows()) ".exe" else "")
    )
})
