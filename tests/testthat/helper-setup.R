
CACHE_DIR <- if (Sys.getenv("JMPOST_CACHE_DIR") == "") {
    tempdir()
} else {
    Sys.getenv("JMPOST_CACHE_DIR")
}
