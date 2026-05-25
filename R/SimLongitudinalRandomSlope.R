#' @include SimLongitudinal.R
#' @include generics.R
NULL

#' Simulate Longitudinal Data from a Random Slope Model
#'
#' @typed times: numeric
#'   the times to generate observations at.
#' @typed intercept: numeric
#'   the mean baseline value for each study.
#' @typed slope_mu: numeric
#'   the population slope for each treatment arm.
#' @typed slope_sigma: numeric
#'   the random slope standard deviation for each treatment arm.
#' @typed sigma: number
#'   the variance of the longitudinal values.
#' @typed link_dsld: number
#'   the link coefficient for the DSLD contribution.
#' @typed link_identity: number
#'   the link coefficient for the identity contribution.
#'
#' @slot intercept (`numeric`)\cr See arguments.
#' @slot slope_mu (`numeric`)\cr See arguments.
#' @slot slope_sigma (`numeric`)\cr See arguments.
#' @slot sigma (`numeric`)\cr See arguments.
#' @slot link_dsld (`numeric`)\cr See arguments.
#' @slot link_identity (`numeric`)\cr See arguments.
#'
#' @family SimLongitudinal
#' @name SimLongitudinalRandomSlope-class
#' @exportClass SimLongitudinalRandomSlope
.SimLongitudinalRandomSlope <- setClass(
    "SimLongitudinalRandomSlope",
    contains = "SimLongitudinal",
    slots = c(
        intercept = "numeric",
        slope_mu = "numeric",
        slope_sigma = "numeric",
        sigma = "numeric",
        link_dsld = "numeric",
        link_identity = "numeric"
    )
)

#' @rdname SimLongitudinalRandomSlope-class
#' @export
SimLongitudinalRandomSlope <- function(
    times = c(-100, -50, 0, 50, 100, 150, 250, 350, 450, 550),
    intercept = 50,
    slope_mu = c(0.01, 0.03),
    slope_sigma = c(0.5, 0.6),
    sigma = 2,
    link_dsld = 0,
    link_identity = 0
) {
    if (length(slope_sigma) == 1) {
        slope_sigma <- rep(slope_sigma, length(slope_mu))
    }

    .SimLongitudinalRandomSlope(
        times = times,
        intercept = intercept,
        slope_mu = slope_mu,
        slope_sigma = slope_sigma,
        sigma = sigma,
        link_dsld = link_dsld,
        link_identity = link_identity
    )
}

#' @rdname as_print_string
#' @exportS3Method NULL
as_print_string.SimLongitudinalRandomSlope <- function(object, ...) {
    return("SimLongitudinalRandomSlope")
}

#' @rdname sampleObservations
#' @export
sampleObservations.SimLongitudinalRandomSlope <- function(object, times_df) {
    times_df |>
        dplyr::mutate(
            err = stats::rnorm(dplyr::n(), 0, object@sigma),
            sld_mu = .data$intercept + .data$slope_ind * .data$time,
            sld = .data$sld_mu + .data$err,
            log_haz_link = object@link_dsld *
                .data$slope_ind +
                object@link_identity * .data$sld_mu
        )
}

#' @rdname sampleSubjects
#' @export
sampleSubjects.SimLongitudinalRandomSlope <- function(object, subjects_df) {
    assert_that(
        is.factor(subjects_df[["study"]]),
        is.factor(subjects_df[["arm"]])
    )

    assert_that(
        length(object@slope_mu) == nlevels(subjects_df[["arm"]]),
        msg = "`length(slope_mu)` should be equal to the number of arms"
    )

    assert_that(
        length(object@slope_sigma) == nlevels(subjects_df[["arm"]]),
        msg = "`length(slope_sigma)` should be equal to the number of arms"
    )

    assert_that(
        length(object@intercept) == nlevels(subjects_df[["study"]]),
        msg = "`length(intercept)` should be equal to the number of studies"
    )

    assert_that(
        nrow(subjects_df) == length(unique(subjects_df[["subject"]])),
        msg = "The number of rows in `subjects_df` should be equal to the number of unique subjects"
    )

    subjects_df |>
        dplyr::mutate(intercept = object@intercept[as.numeric(.data$study)]) |>
        dplyr::mutate(
            slope_ind = stats::rnorm(
                n = dplyr::n(),
                mean = object@slope_mu[as.numeric(.data$arm)],
                sd = object@slope_sigma[as.numeric(.data$arm)]
            )
        )
}
