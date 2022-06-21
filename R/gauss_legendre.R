#' @export
.gauss_legendre <- setClass(
    "gauss_legendre",
    representation(values = "list")
)


#' @export
gauss_legendre <- function(n = 15,
                           kind = "legendre") {
    .gauss_legendre(values = statmod::gauss.quad(n = n, kind = kind))
}
