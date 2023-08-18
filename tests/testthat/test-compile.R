
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
    old_path <- options("jmpost.cache.dir")
    options("jmpost.cache.dir" = fpath)
    z <- compileStanModel(smod)
    options(old_path)
    expect_true(file.exists(fpath))
})
