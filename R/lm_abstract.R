

.LongitudinalModel <- setClass(
    Class = "LongitudinalModel",
    contains = "StanModel"
)


#' @export
LongitudinalModel <- function(stan = StanModule(), parameters = ParameterList(), ...) {
        
    base_long <- StanModule(
        x = "base/longitudinal.stan"
    )
    
    .LongitudinalModel(
        StanModel(
            stan = merge(base_long, stan),
            parameters = parameters,
            ...
        )
    )
}





