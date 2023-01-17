


.LinkNone <- setClass(
    Class = "LinkNone",
    contains = "Link"
)


#' @export 
LinkNone <- function() {
    stan = StanModule()
    .LinkNone(Link(stan = stan))
}



