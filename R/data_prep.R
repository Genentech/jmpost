#' @export



setGeneric("data_prep", def = function(object,
                                       os_formula,
                                       options,
                                       index_save_ind,
                                       predictions) {
  standardGeneric("data_prep")
})

setMethod("data_prep",
  signature(
    object = "JMdata",
    options = "mcmc_options"
  ),
  value = "JMdata",
  function(object,
           os_formula,
           options,
           index_save_ind,
           predictions) {
    cens_threshold <- object@censoring_threshold

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
    os_pred_times <- predictions

    os_cov_design <- model.matrix(os_formula, data = object@data_os)

    # each of the different treatment arms.
    treat_arms <- unique(object@data_os[, object@vars$os_arm, drop = T])

    studies_name <- unique(object@data_os[, object@vars$os_study_id, drop = T])

    # The patients in each of the different treatment arms.
    n_index_per_arm <- c()
    for (i in 1:length(treat_arms)) {
      n_index_per_arm[i] <- sum(object@data_os[, object@vars$os_arm, drop = TRUE] == treat_arms[i])
    }


    index_per_arm <- c()
    for (i in 1:length(treat_arms)) {
      index_per_arm <- c(
        index_per_arm,
        which(object@data_os[, object@vars$os_arm, drop = TRUE] == treat_arms[i])
      )
    }

    arm_study_trt <- unique(object@data_os[, c(object@vars$os_arm, object@vars$os_study_id, object@vars$treatment)])
    arm_to_study_values <- as.integer(factor(arm_study_trt[, object@vars$os_study_id, drop = TRUE]))

    sld_par_shared <- which(arm_study_trt[, object@vars$treatment] == object@shared_treatement)
    sld_par_separate <- which(arm_study_trt[, object@vars$treatment] != object@shared_treatement)


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
        ind_index = object@data_sld[, object@vars$ID_INDEX, drop = TRUE],
        obs_y_index = which(object@data_sld[, object@vars$longitudinal] >= cens_threshold),
        cens_y_index = which(object@data_sld[, object@vars$longitudinal] < cens_threshold),
        dead_ind_index = which(object@data_os[, object@vars$overall_survival_death, drop = TRUE] == 1),
        n_studies = length(studies_name),
        study_index = as.numeric(factor(object@data_os[, object@vars$os_study_id, drop = TRUE],
          levels = studies_name
        )),
        n_arms = length(treat_arms),
        arm_index = as.numeric(factor(object@data_os[, object@vars$os_arm, drop = TRUE],
          levels = treat_arms
        )),
        n_save_individual = length(index_save_ind),
        index_save_individual = index_save_ind,
        n_sld_par_shared = length(sld_par_shared),
        sld_par_shared = sld_par_shared,
        n_sld_par_separate = length(sld_par_separate),
        sld_par_separate = sld_par_separate,
        arm_to_study_index = arm_to_study_values,

        # The patients in each of the four different treatment arms.
        n_index_per_arm = n_index_per_arm,
        index_per_arm = index_per_arm,

        # Tumor assessment values and time points.
        Yobs = object@data_sld[, object@vars$longitudinal, drop = TRUE],
        Tobs = object@data_sld[, object@vars$AYR, drop = TRUE],
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
        Times = object@data_os[, object@vars$AYR, drop = TRUE],
        Death = object@data_os[, object@vars$overall_survival_death, drop = TRUE],
        os_cov_design = os_cov_design,
        p_os_cov_design = ncol(os_cov_design),

        # For OS predictions.
        os_pred_times = os_pred_times,
        n_os_pred_times = length(os_pred_times),

        # Integration paramaters.
        n_nodes = length(options@gauss_legendre@values$nodes),
        nodes = options@gauss_legendre@values$nodes,
        weights = options@gauss_legendre@values$weights
      )
    )
  }
)
