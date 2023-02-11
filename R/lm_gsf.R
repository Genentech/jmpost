



.LongitudinalGSF <- setClass(
    Class = "LongitudinalGSF",
    contains = "LongitudinalModel"
)

#' @export
LongitudinalGSF <- function() {

    stan <- StanModule(
        x = "lm-gsf/model.stan"
    )
    
    .LongitudinalGSF(
        LongitudinalModel(
            stan = stan
        )
    )
}

