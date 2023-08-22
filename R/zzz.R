
.onLoad <- function(libname, pkgname) {

    cache_dir <- Sys.getenv("JMPOST_CACHE_DIR")

    if (cache_dir == "" || is.null(cache_dir)) {
        cache_dir <- tempfile()
    }

    current_opts <- names(options())
    jmpost_opts <- list(
        jmpost.cache.dir = cache_dir
    )
    for (opt in names(jmpost_opts)) {
        if (!opt %in% current_opts) {
            options(jmpost_opts[opt])
        }
    }
}
