#' Simulating Joint Longitudinal and Time-to-Event Data
#'
#' @param design (`list`)\cr a list of [`SimGroup`] objects. See details.
#' @param longitudinal ([`SimLongitudinal`])\cr object specifying how to simulate the longitudinal data
#' @param survival ([`SimSurvival`])\cr object specifying how to simulate the survival data
#' @param .silent (`flag`)\cr whether to suppress info messages
#'
#' @slot longitudinal (`data.frame`)\cr the simulated longitudinal data.
#' @slot survival (`data.frame`)\cr the simulated survival data.
#'
#' @details
#'
#' The `design` argument is used to specify how many distinct groups should be simulated
#' including key information such as the number of subjects within the group as well as
#' which treatment arm and study the group belongs to. The `design` argument should be a
#' list of [`SimGroup`] objects e.g.
#' ```
#' design = list(
#'     SimGroup(n = 50, study = "Study-1", arm = "Arm-A"),
#'     SimGroup(n = 50, study = "Study-1", arm = "Arm-B")
#' )
#' ```
#'
#' @name SimJointData-class
#' @exportClass SimJointData
.SimJointData <- setClass(
    "SimJointData",
    slots = list(
        longitudinal = "data.frame",
        survival = "data.frame"
    )
)




#' @rdname SimJointData-class
#' @export
SimJointData <- function(
    design = list(
        SimGroup(n = 50, study = "Study-1", arm = "Arm-A"),
        SimGroup(n = 50, study = "Study-1", arm = "Arm-B")
    ),
    longitudinal,
    survival,
    .silent = FALSE
) {

    assert(
        all(vapply(design, \(x) is(x, "SimGroup"), logical(1))),
        msg = "All elements of `design` must be of class `SimGroup`"
    )

    hazard_evaluation_info <- hazardWindows(survival)

    n_group <- vapply(design, function(x) x@n, numeric(1))
    arms <- vapply(design, function(x) x@arm, character(1))
    studies <- vapply(design, function(x) x@study, character(1))
    n_subjects <- sum(n_group)
    n_times <- length(hazard_evaluation_info$midpoint)

    sprintf_string <- paste0("pt_%0", ceiling(log(n_subjects, 10)), "i")

    baseline <- dplyr::tibble(pt = sprintf(sprintf_string, seq_len(n_subjects))) |>
        dplyr::mutate(arm = factor(rep(arms, times = n_group), levels = unique(arms))) |>
        dplyr::mutate(study = factor(rep(studies, times = n_group), levels = unique(studies)))

    os_baseline <- sampleSubjects(survival, subjects_df = baseline)
    lm_baseline <- sampleSubjects(longitudinal, subjects_df = baseline)

    lm_dat_no_obvs <- lapply(
        longitudinal@times,
        \(time) {
            baseline[["time"]] <- time
            baseline
        }
    ) |>
        dplyr::bind_rows() |>
        dplyr::left_join(lm_baseline, by = c("pt", "study", "arm"))

    lm_dat <- sampleObservations(longitudinal, lm_dat_no_obvs)


    hazard_eval_df <- dplyr::tibble(
        pt = rep(lm_baseline$pt, each = n_times),
        arm = rep(lm_baseline$arm, each = n_times),
        study = rep(lm_baseline$study, each = n_times),
        midpoint = rep(as.double(hazard_evaluation_info$midpoint), times = n_subjects),
        time = rep(as.double(hazard_evaluation_info$upper), times = n_subjects),
        width = rep(as.double(hazard_evaluation_info$width), times = n_subjects)
    )

    lm_link_dat <- sampleObservations(
        longitudinal,
        dplyr::left_join(hazard_eval_df, lm_baseline, by = c("pt", "study", "arm"))
    )[, c("pt", "study", "arm", "log_haz_link", "time", "width", "midpoint")]

    os_eval_df <- lm_link_dat |>
        dplyr::left_join(os_baseline, by = c("pt", "study", "arm"))

    withCallingHandlers(
        os_dat <- sampleObservations(survival, os_eval_df),
        message = function(e) {
            if (!.silent) message(e)
            invokeRestart("muffleMessage")
        }
    )

    lm_dat2 <- lm_dat |>
        dplyr::left_join(dplyr::select(os_dat, pt, os_time = time), by = "pt") |>
        dplyr::mutate(observed = (.data$time <= .data$os_time)) |>
        dplyr::arrange(dplyr::pick(c("pt", "time")))

    assert_that(
        length(unique(os_dat$pt)) == length(os_dat$pt),
        length(os_dat$pt) == n_subjects,
        all(os_dat$time >= 0),
        all(os_dat$event %in% c(0, 1)),
        msg = "Assumptions for the Survival data are not met (please report this issue)"
    )

    assert_that(
        nrow(lm_dat2) == n_subjects * length(longitudinal@times),
        length(unique(lm_dat2$pt)) == n_subjects,
        msg = "Assumptions for the Longitudinal data are not met (please report this issue)"
    )

    return(
        .SimJointData(
            survival = os_dat,
            longitudinal = lm_dat2[, c("pt", "arm", "study", "time", "sld", "observed")]
        )
    )
}
