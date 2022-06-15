

gauss_legendre <- function(n = 15,
                           kind = "legendre") {
  .gauss_legendre(values = statmod::gauss.quad(n = n, kind = kind))
}


setGeneric("data_prep", def = function(object, formula) {
  standardGeneric("data_prep")
})

setMethod("data_prep",
  signature(object = "JMdata"),
  value = "JMdata",
  function(object) {
    cens_threshold <- 2.5

    # Derive sparse matrices.
    obs_y_dat <- object@data_sld[object@data_sld[, object@vars$longitudinal] >= cens_threshold, ]
    mat_inds_obs_y <- t(model.matrix(as.formula(paste("~ -1 +", object@vars$long_user_id)),
      data = obs_y_dat
    ))
    sparse_mat_inds_obs_y <- extract_sparse_parts(mat_inds_obs_y)

    cens_y_dat <- object@data_sld[object@data_sld[, object@vars$longitudinal] < cens_threshold, ]
    mat_inds_cens_y <- t(model.matrix(as.formula(paste("~ -1 +", object@vars$long_user_id)),
      data = cens_y_dat
    ))
    sparse_mat_inds_cens_y <- extract_sparse_parts(mat_inds_cens_y)


    # Timepoints for OS hazard and survival function estimation to generate predictions.
    os_pred_times <- seq(from = 0.001, to = 2, length = 100)

    os_cov_design <- model.matrix(~ -1 + BECOG + CLOGNLR + CLOGCRP + CALBU + CLIVER, data = object@data_os)



    jm_data(
      data_sld = object@data_sld,
      data_os = object@data_os,
      data = list(
        Nind = nrow(object@data_os),
        Nta_total = nrow(object@data_sld),
        Nta_obs_y = sum(object@data_sld[, object@vars$longitudinal] >= cens_threshold),
        Nta_cens_y = sum(object@data_sld[, object@vars$longitudinal] < cens_threshold),
        Nind_dead = sum(object@data_os[, object@vars$overall_survival_death] == 1),

        # Index vectors.
        ind_index = object@data_sld$USUBJID_INDEX,
        obs_y_index = which(object@data_sld[, object@vars$longitudinal] >= cens_threshold),
        cens_y_index = which(object@data_sld[, object@vars$longitudinal] < cens_threshold),
        dead_ind_index = which(object@data_os[, object@vars$overall_survival_death] == 1),
        n_studies = 3L,
        study_index = as.numeric(factor(object@data_os$STUDYID, levels = c("GO29293", "GO29294", "WO39613"))),
        n_arms = 6L,
        arm_index = as.numeric(factor(object@data_os$ARM, levels = c("IMV210_A", "IMV211_A", "MOR_A", "MOR_AT", "MOR_ASG", "MOR_AEV"))),
        n_save_individual = sum(object@data_os$STUDYID == "WO39613"),
        index_save_individual = which(object@data_os$STUDYID == "WO39613"),
        n_sld_par_shared = 3L,
        sld_par_shared = 1:3,
        n_sld_par_separate = 3L,
        sld_par_separate = 4:6,
        arm_to_study_index = c(1L, 2L, 3L, 3L, 3L, 3L),

        # The patients in each of the four different treatment arms.
        n_index_per_arm = c(
          sum(object@data_os$ARM == "IMV210_A"),
          sum(object@data_os$ARM == "IMV211_A"),
          sum(object@data_os$ARM == "MOR_A"),
          sum(object@data_os$ARM == "MOR_AT"),
          sum(object@data_os$ARM == "MOR_ASG"),
          sum(object@data_os$ARM == "MOR_AEV")
        ),
        index_per_arm = c(
          which(object@data_os$ARM == "IMV210_A"),
          which(object@data_os$ARM == "IMV211_A"),
          which(object@data_os$ARM == "MOR_A"),
          which(object@data_os$ARM == "MOR_AT"),
          which(object@data_os$ARM == "MOR_ASG"),
          which(object@data_os$ARM == "MOR_AEV")
        ),

        # Tumor assessment values and time points.
        Yobs = object@data_sld[, object@vars$longitudinal, drop = TRUE],
        Tobs = object@data_sld$AYR,
        Ythreshold = cens_threshold,

        # Matrix of individuals x observed tumor assessments.
        n_w_mat_inds_obs_y = length(sparse_mat_inds_obs_y$w),
        w_mat_inds_obs_y = sparse_mat_inds_obs_y$w,
        n_v_mat_inds_obs_y = length(sparse_mat_inds_obs_y$v),
        v_mat_inds_obs_y = sparse_mat_inds_obs_y$v,
        n_u_mat_inds_obs_y = length(sparse_mat_inds_obs_y$u),
        u_mat_inds_obs_y = sparse_mat_inds_obs_y$u,

        # Matrix of individuals x censored tumor assessments.
        n_w_mat_inds_cens_y = length(sparse_mat_inds_cens_y$w),
        w_mat_inds_cens_y = sparse_mat_inds_cens_y$w,
        n_v_mat_inds_cens_y = length(sparse_mat_inds_cens_y$v),
        v_mat_inds_cens_y = sparse_mat_inds_cens_y$v,
        n_u_mat_inds_cens_y = length(sparse_mat_inds_cens_y$u),
        u_mat_inds_cens_y = sparse_mat_inds_cens_y$u,

        # Survival data.
        Times = object@data_os$AYR,
        Death = object@data_os[, object@vars$overall_survival_death, drop = TRUE],
        os_cov_design = os_cov_design,
        p_os_cov_design = ncol(os_cov_design),

        # For OS predictions.
        os_pred_times = os_pred_times,
        n_os_pred_times = length(os_pred_times),

        # Integration paramaters.
        n_nodes = length(gauss_legendre()@values$nodes),
        nodes = gauss_legendre()@values$nodes,
        weights = gauss_legendre()@values$weights
      )
    )
  }
)
