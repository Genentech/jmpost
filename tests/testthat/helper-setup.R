
CACHE_DIR <- if (Sys.getenv("JMPOST_CACHE_DIR") == "") {
    tempdir()
} else {
    Sys.getenv("JMPOST_CACHE_DIR")
}


is_full_test <- function() {
    toupper(Sys.getenv("JMPOST_FULL_TEST")) == "TRUE"
}

is_graph_snapshot_enabled <- function() {
    toupper(Sys.getenv("JMPOST_GRAPH_SNAPSHOT")) == "TRUE"
}


# Runs stans syntax parser and will throw an error if stan detects an
# issue that would prevent compilation
expect_stan_syntax <- function(code) {
    model_obj <- cmdstanr::cmdstan_model(
        cmdstanr::write_stan_file(as.character(code)),
        compile = FALSE
    )
    expect_true(model_obj$check_syntax(quiet = TRUE))
}


# Load multiple `inst` stan files including the base functions file (as many of them
# depend on this)
load_with_base_stan <- function(...) {
    sm <- Reduce(
        merge,
        lapply(list(...), StanModule)
    )
    merge(
        sm,
        StanModule("base/functions.stan")
    )
}


run_quietly <- function(expr) {
    suppressMessages(capture.output((x <- expr)))
    return(x)
}
