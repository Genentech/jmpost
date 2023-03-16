


.LinkNone <- setClass(
    Class = "LinkNone",
    contains = "Link"
)


#' @export 
LinkNone <- function() {
    stan = StanModule()
    .LinkNone(Link(stan = stan))
}

# TODO - Document that you can override this if adding a no-link is not simple
setMethod(
    f = "addLink",
    signature = c("LongitudinalModel", "LinkNone"),
    definition = function(x, y, ...) {
        x
    }
)

