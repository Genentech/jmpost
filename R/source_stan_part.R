

source_stan_part <- function(filename) {
    absolute_filename <- system.file(
        "inst/stanparts",
        filename,
        package = "jmpost"
    )
    readChar(absolute_filename, file.info(absolute_filename)$size)
}

