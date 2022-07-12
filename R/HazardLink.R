#' Hazard Link Class
#'
#' TODO - Description
#'
#' @slot stan TODO
#' @slot contribution TODO
#' @slot parameter TODO
#' @export
HazardLink <- setClass(
    "HazardLink",
    representation = list(
        "stan" = "StanModule",
        "contribution" = "character",
        "parameter" = "character"
    )
)


#' Title - TODO
#'
#' Description
#' TODO - Find way to merge roxygen page between constructor and initaliser
#'
#' @importFrom assertthat assert_that
#' @export
setMethod(
    f = "initialize",
    signature = "HazardLink",
    definition = function(.Object, ..., stan, contribution, parameter) {

        assert_that(
            is.character(parameter),
            length(parameter) == 1,
            is.character(contribution),
            length(contribution) == 1,
            msg = "`Contribution` and `parameter` must be length 1 character vectors"
        )

        assert_that(
            length(stan@priors) == 1,
            msg = "`stan@priors` must be defined in a HazardLink object"
        )

        if (length(stan@parameters) == 0) {
            stan@parameters <- sprintf("real %s ;\\n", parameter)
        }

        callNextMethod(
            .Object,
            ...,
            stan = stan,
            contribution = contribution,
            parameter = parameter
        )
    }
)


setGeneric(
    "merge",
    function(x, y) standardGeneric("merge")
)


setMethod(
    f = "merge",
    signature = "HazardLink",
    definition = function(x, y) {
        HazardLink(
            stan = merge(x@stan, y@stan),
            contribution = paste(x@contribution, y@contribution, collapse = " + "),
            parameter = c(x@parameter, y@parameter)
        )
    }
)

