


.Link <- setClass(
    Class = "Link",
    slots = list(
        "stan" = "StanModule"
    )
)


#' @export
Link <- function(stan = StanModule(), ...) {
    .Link(stan = stan, ...)
}
