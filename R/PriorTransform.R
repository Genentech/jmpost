#' Package-Defined Prior Transformation
#'
#' Describes a transformation from a model parameter to the scale on which a
#' prior density is evaluated. Only package-defined transformations are
#' supported; the object contains no user-supplied R or Stan functions.
#'
#' @slot kind (`character`)\cr Transformation identifier.
#' @slot scale (`numeric`)\cr Positive multiplier inside the exponential.
#' @seealso [prior_transform_exp()], [prior_transform_square()]
#' @aliases PriorTransform
#' @exportClass PriorTransform
.PriorTransform <- setClass(
    "PriorTransform",
    slots = c(kind = "character", scale = "numeric")
)

setValidity("PriorTransform", function(object) {
    if (
        length(object@kind) != 1 ||
            !object@kind %in% c("exp", "square")
    ) {
        return("Unknown prior transformation")
    }
    if (
        length(object@scale) != 1 ||
            !is.finite(object@scale) ||
            object@scale <= 0
    ) {
        return("`scale` must be a finite positive number")
    }
    if (object@kind == "square" && object@scale != 1) {
        return("The square transformation does not accept a scale")
    }
    TRUE
})


#' Transformations to the Prior Density Scale
#'
#' `prior_transform_exp()` describes `exp(scale * x)` for an unconstrained
#' model parameter `x`. `prior_transform_square()` describes `x^2` on the
#' non-negative branch only. Both map to a non-negative density scale, with
#' strictly positive values in the interior of the parameter domain.
#'
#' @details
#' Supply these objects as the `transform` argument to a scalar distribution
#' helper such as [prior_normal()] or to [prior_transform()].
#'
#' The intended density is `p(g(x)) * abs(g'(x))`, including the Jacobian.
#' The square transformation requires a non-negative model parameter so that
#' it is one-to-one. It must not silently fold an unconstrained parameter.
#'
#' @typed scale: number
#'   finite positive multiplier inside the exponential. Use `2` for a prior
#'   on the variance when `x` is a log standard deviation.
#' @returns A `PriorTransform` object.
#' @name prior-transformations
#' @export
#' @examples
#' prior_transform_exp()
#' prior_transform_exp(scale = 2)
#' prior_transform_square()
#' prior_normal(60, 60, transform = prior_transform_exp())
prior_transform_exp <- function(scale = 1) {
    .PriorTransform(kind = "exp", scale = scale)
}


#' @rdname prior-transformations
#' @export
prior_transform_square <- function() {
    .PriorTransform(kind = "square", scale = 1)
}


# Bounds include endpoints for mapping truncation limits; Stan samples in the
# interior of the domain.
.prior_transform_domain <- function(object) {
    switch(object@kind, exp = c(-Inf, Inf), square = c(0, Inf))
}

.prior_transform_forward <- function(object, x) {
    if (object@kind == "square" && any(x < 0)) {
        stop("The square prior transformation requires non-negative values")
    }
    switch(object@kind, exp = exp(object@scale * x), square = x^2)
}

.prior_transform_inverse <- function(object, y) {
    if (any(y < 0)) {
        stop("The inverse prior transformation requires non-negative values")
    }
    switch(object@kind, exp = log(y) / object@scale, square = sqrt(y))
}

.prior_transform_stan <- function(object, x) {
    # Scalar expressions; vector reduction of Jacobians belongs in the renderer.
    if (object@kind == "exp") {
        scaled_x <- if (object@scale == 1) {
            x
        } else {
            glue::glue("{object@scale} * ({x})")
        }
        return(list(
            value = as.character(glue::glue("exp({scaled_x})")),
            log_jacobian = if (object@scale == 1) {
                x
            } else {
                as.character(glue::glue("log({object@scale}) + {scaled_x}"))
            }
        ))
    }
    list(
        value = as.character(glue::glue("square({x})")),
        log_jacobian = as.character(glue::glue("log(2) + log({x})"))
    )
}
