
.onLoad <- function(libname, pkgname) {
    current_ops <- names(options())
    jmpost_opts <- list(
        jmpost.cache.dir = tempfile()
    )
    for (opt in names(jmpost_opts)) {
        if (!opt %in% current_ops) {
            options(jmpost_opts[opt])
        }
    }
}
