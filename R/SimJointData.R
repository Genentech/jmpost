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

    sprintf_string <- paste0("subject_%0", ceiling(log(n_subjects, 10)) + 1, "i")

    baseline <- dplyr::tibble(subject = sprintf(sprintf_string, seq_len(n_subjects))) |>
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
        dplyr::left_join(lm_baseline, by = c("subject", "study", "arm"))

    lm_dat <- sampleObservations(longitudinal, lm_dat_no_obvs)


    hazard_eval_df <- dplyr::tibble(
        subject = rep(lm_baseline$subject, each = n_times),
        arm = rep(lm_baseline$arm, each = n_times),
        study = rep(lm_baseline$study, each = n_times),
        midpoint = rep(as.double(hazard_evaluation_info$midpoint), times = n_subjects),
        time = rep(as.double(hazard_evaluation_info$upper), times = n_subjects),
        width = rep(as.double(hazard_evaluation_info$width), times = n_subjects)
    )

    lm_link_dat <- sampleObservations(
        longitudinal,
        dplyr::left_join(hazard_eval_df, lm_baseline, by = c("subject", "study", "arm"))
    )[, c("subject", "study", "arm", "log_haz_link", "time", "width", "midpoint")]

    os_eval_df <- lm_link_dat |>
        dplyr::left_join(os_baseline, by = c("subject", "study", "arm"))

    withCallingHandlers(
        os_dat <- sampleObservations(survival, os_eval_df),
        message = function(e) {
            if (!.silent) message(e)
            invokeRestart("muffleMessage")
        }
    )

    lm_dat2 <- lm_dat |>
        dplyr::left_join(dplyr::select(os_dat, "subject", os_time = "time"), by = "subject") |>
        dplyr::mutate(observed = (.data$time <= .data$os_time)) |>
        dplyr::arrange(dplyr::pick(c("subject", "time")))

    assert_that(
        length(unique(os_dat$subject)) == length(os_dat$subject),
        length(os_dat$subject) == n_subjects,
        all(os_dat$time >= 0),
        all(os_dat$event %in% c(0, 1)),
        msg = "Assumptions for the Survival data are not met (please report this issue)"
    )

    assert_that(
        nrow(lm_dat2) == n_subjects * length(longitudinal@times),
        length(unique(lm_dat2$subject)) == n_subjects,
        msg = "Assumptions for the Longitudinal data are not met (please report this issue)"
    )

    return(
        .SimJointData(
            survival = os_dat,
            longitudinal = lm_dat2[, c("subject", "arm", "study", "time", "sld", "observed")]
        )
    )
}

#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "SimJointData",
    definition = function(object) {
        x <- sprintf("\nA SimJointData Object\n\n")
        cat(x)
        return(object)
    }
)


#' Add PFS events at Tumour Progression to Data
#'
#' Adds new columns `pfs_time` and `pfs_event` based on observed changes to SLD.
#'
#' @param object A [SimJointData] object
#' @param relative_threshold (`number`)\cr a multiplicative threshold for the change in SLD compared to the `min(SLD)`.
#'  Default is 1.2 meaning a 20% increase.
#' @param absolute_threshold (`number`)\cr an absolute threshold for the change in SLD compared to the minimum.
#'   Default is 5.
#' @param from_time (`number`)\cr Ignore observations before this time for determining SLD minimum.
#' @param observed_after (`logical`)\cr If `FALSE` set longitudinal observations after the progression time to
#'   `observed = FALSE`
#' @details
#' Both thresholds must be met for a progression to be declared.
#'
#' @export
#' @examples
#' data <- SimJointData(
#'   survival = SimSurvivalExponential(lambda = 1/10),
#'   longitudinal = SimLongitudinalSteinFojo()
#' )
#' data <- add_pfs(data)
#' data@survival # now has pfs_time and pfs_event columns
add_pfs <- function(object, relative_threshold = 1.2, absolute_threshold = 5, from_time = 0, observed_after = FALSE) {
    assert_class(object, "SimJointData")

    pd_times <- object@longitudinal |>
        dplyr::filter(.data$time >= from_time) |>
        dplyr::mutate(
            min_sld = cummin(.data$sld),
            is_pd = .data$sld >= pmax(
                .data$min_sld * relative_threshold,
                .data$min_sld + absolute_threshold
            ) & .data$observed,
            pd_time = min(.data$time[.data$is_pd], Inf),
            .by = "subject"
        ) |>
        dplyr::select("subject", "pd_time") |>
        dplyr::slice_head(by = "subject")

    if (isFALSE(observed_after)) {
        object@longitudinal <- object@longitudinal |>
            dplyr::left_join(pd_times, by = "subject") |>
            dplyr::mutate(
                observed = dplyr::if_else(.data$time > .data$pd_time, FALSE, .data$observed),
                pd_time = NULL
            )
    }

    object@survival <-
        object@survival |>
        dplyr::left_join(pd_times, by = "subject") |>
        dplyr::mutate(
            pfs_time = pmin(.data$time, .data$pd_time, na.rm = TRUE),
            pfs_event = dplyr::if_else(.data$pfs_time < .data$time, 1, .data$event),
            pd_time = NULL
        )
    object
}


#' Cut Study Data
#' @param object A [SimJointData] object
#' @param cut_time (`numeric`)\cr A vector of cut off times, either length 1 for all patients or
#'   `nrow(object@survival)` for a time per patient.
#' @details
#'   All observations after this time are remove. Survival is censored at this time and any longitudinal
#'   values are removed.
#' @export
#' @examples
#' data <- SimJointData(
#'   survival = SimSurvivalExponential(lambda = 1/10),
#'   longitudinal = SimLongitudinalSteinFojo()
#' )
#' data <- cut_data(data, 5)
#' data@survival
#' # Now max time is 5
#' max(data@survival$time)
cut_data <- function(object, cut_time) {
    assert_class(object, "SimJointData")
    check_len <- if (length(cut_time) > 1) nrow(object@survival) else 1
    assert_numeric(cut_time, lower = 0, len = check_len)

    object@survival <- object@survival |>
        dplyr::mutate(
            cut_time = cut_time,
            event = dplyr::if_else(.data$time < .data$cut_time, .data$event, 0),
            time = pmin(.data$cut_time, .data$time)
        )
    if (all(c("pfs_event", "pfs_time") %in% colnames(object@survival))) {
        object@survival <- object@survival |>
            dplyr::mutate(
                pfs_event = dplyr::if_else(.data$pfs_time < .data$cut_time, .data$pfs_event, 0),
                pfs_time = pmin(.data$cut_time, .data$pfs_time)
            )
    }

    object@longitudinal <- object@longitudinal |>
        dplyr::left_join(
            dplyr::select(object@survival, "subject", "cut_time"),
            by = "subject"
        ) |>
        dplyr::filter(.data$time <= .data$cut_time) |>
        dplyr::mutate(cut_time = NULL)

    object@survival$cut_time <- NULL

    object
}
