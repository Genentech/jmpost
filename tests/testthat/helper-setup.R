
CACHE_DIR <- if (Sys.getenv("JMPOST_CACHE_DIR") == "") {
    tempdir()
} else {
    Sys.getenv("JMPOST_CACHE_DIR")
}


is_full_test <- function() {
    toupper(Sys.getenv("JMPOST_FULL_TEST")) == "TRUE"
}
