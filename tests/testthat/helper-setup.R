mcmc_results <- readRDS(test_path("fixtures", "mcmc_results.rds"))

if (Sys.getenv("JMPOST_CACHE_DIR") == "") {
    CACHE_DIR <- tempdir()
} else {
    CACHE_DIR <- Sys.getenv("JMPOST_CACHE_DIR")
}
