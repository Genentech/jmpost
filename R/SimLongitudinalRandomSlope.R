
#' @include SimLongitudinal.R
#' @include generics.R
NULL

#' Simulate Longitudinal Data from a Random Slope Model
#'
#' @param times (`numeric`)\cr the times to generate observations at.
#' @param intercept (`number`)\cr the mean baseline value for each study.
#' @param slope_mu (`numeric`)\cr the population slope for each treatment arm.
#' @param slope_sigma (`number`)\cr the random slope standard deviation.
#' @param sigma (`number`)\cr the variance of the longitudinal values.
#' @param link_dsld (`number`)\cr the link coefficient for the DSLD contribution.
#' @param link_identity (`number`)\cr the link coefficient for the identity contribution.
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
    slope_sigma = 0.5,
    sigma = 2,
    link_dsld = 0,
    link_identity = 0
) {
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
as_print_string.SimLongitudinalRandomSlope <- function(object) {
    return("SimLongitudinalRandomSlope")
}

#' @rdname sampleObservations
#' @export
sampleObservations.SimLongitudinalRandomSlope <- function(object, times_df) {
    times_df |>
        dplyr::mutate(err = stats::rnorm(dplyr::n(), 0, object@sigma)) |>
        dplyr::mutate(sld_mu = .data$intercept + .data$slope_ind * .data$time) |>
        dplyr::mutate(sld = .data$sld_mu + .data$err) |>
        dplyr::mutate(
            log_haz_link =
                object@link_dsld * .data$slope_ind +
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
        length(object@slope_mu) == length(unique(subjects_df[["arm"]])),
        msg = "`length(slope_mu)` should be equal to the number of unique arms"
    )

    assert_that(
        length(object@intercept) == length(unique(subjects_df[["study"]])),
        msg = "`length(intercept)` should be equal to the number of unique studies"
    )

    assert_that(
        nrow(subjects_df) == length(unique(subjects_df[["subject"]])),
        msg = "The number of rows in `subjects_df` should be equal to the number of unique subjects"
    )

    subjects_df |>
        dplyr::mutate(intercept = object@intercept[as.numeric(.data$study)]) |>
        dplyr::mutate(slope_ind = stats::rnorm(
            n = dplyr::n(),
            mean = object@slope_mu[as.numeric(.data$arm)],
            sd = object@slope_sigma
        ))
}
