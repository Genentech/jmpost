


setMethod(
    "addLink",
    signature = c("LongitudinalModel", "NULL"),
    definition = function(x, y, ...) {
        x
    }
)

setMethod(
    "addLink",
    signature = c("NULL", "Link"),
    definition = function(x, y, ...) {
        stop("No Longitudinal model has been defined for the link function to be combined with")
    }
)

setMethod(
    "addLink",
    signature = c("NULL", "NULL"),
    definition = function(x, y, ...) {
        NULL
    }
)
