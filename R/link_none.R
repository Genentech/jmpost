


.LinkNone <- setClass(
    Class = "LinkNone",
    contains = "Link"
)


#' @export 
LinkNone <- function() {
    stan = StanModule()
    .LinkNone(Link(stan = stan))
}

setMethod(
    f = "addLink",
    signature = c("LongitudinalModel", "LinkNone"),
    definition = function(x, y, ...) {
        x
    }
)

