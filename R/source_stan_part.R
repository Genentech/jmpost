
#' source_stan_part exports a .stan file as a character
#' @param filename The name of the .stan file in the directory of the package.
#' @export

source_stan_part <- function(filename) {
    absolute_filename <- system.file(
        "stanparts",
        filename,
        package = "jmpost"
    )
    readLines(absolute_filename)
}

