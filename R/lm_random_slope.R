



.LongitudinalRandomSlope <- setClass(
    Class = "LongitudinalRandomSlope",
    contains = "LongitudinalModel"
)


LongitudinalRandomSlope <- function() {
    stan <- StanModule(
        x = "lm-random-slope/model.stan"
    )
    .LongitudinalRandomSlope(
        LongitudinalModel(stan = stan)
    )
}


.LinkRandomSlope <- setClass(
    Class = "LinkRandomSlope",
    contains = "Link"
)


#' @export 
LinkRandomSlope <- function() {
    stan = StanModule(
        x = "lm-random-slope/links.stan"
    )
    .LinkRandomSlope(
        Link(stan = stan)
    )
}


setMethod(
    f = "addLink",
    signature = c("LongitudinalRandomSlope", "LinkRandomSlope"),
    definition = function(x, y, ...) {
        x@stan <- merge(x@stan, y@stan)
        x
    }
)


