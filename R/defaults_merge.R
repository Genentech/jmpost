


setMethod(
    "merge",
    signature = c("StanModel", "NULL"),
    definition = function(x, y, ...) {
        x@stan
    }
)
setMethod(
    "merge",
    signature = c("NULL", "StanModel"),
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
    signature = c("ParameterList", "NULL"),
    definition = function(x, y, ...) {
        x
    }
)
setMethod(
    "merge",
    signature = c("NULL", "ParameterList"),
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




