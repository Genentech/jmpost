#' @export

source_stan_part <- function(filename) {
    absolute_filename <- system.file(
        "stanparts",
        filename,
        package = "jmpost"
    )
    readLines(absolute_filename)
}

