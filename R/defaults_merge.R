


setMethod(
    "merge",
    signature = c("LongitudinalModel", "NULL"),
    definition = function(x, y, ...) {
        x@stan
    }
)

setMethod(
    "merge",
    signature = c("NULL", "SurvivalModel"),
    definition = function(x, y, ...) {
        y@stan
    }
)


setMethod(
    "merge",
    signature = c("StanModule", "NULL"),
    definition = function(x, y, ...) {
        x
    }
)

setMethod(
    "merge",
    signature = c("NULL", "StanModule"),
    definition = function(x, y, ...) {
        y
    }
)

setMethod(
    "merge",
    signature = c("NULL", "NULL"),
    definition = function(x, y, ...) {
        NULL
    }
)
