

# TODO - Remap required variables as string / object
# time, event
#' @export
as_stan_data <- function(os, lm, frm, cens_threshold = 5) {
    design_mat <- stats::model.matrix(frm, data = os)
    remove_index <- grep("(Intercept)", colnames(design_mat), fixed = TRUE)
    design_mat <- design_mat[,-remove_index, drop = FALSE]
    
    gh_parameters <- statmod::gauss.quad(n = 15, kind = "legendre")
    
    # Derive sparse matrices.
    mat_inds_obs_y <- t(
        model.matrix(
            ~ -1 + pt,
            data = lm |>filter(sld >= cens_threshold)
        )
    )
    sparse_mat_inds_obs_y <- rstan::extract_sparse_parts(mat_inds_obs_y)


    mat_inds_cens_y <- t(
        model.matrix(
            ~ -1 + pt,
            data = lm |>filter(sld <= cens_threshold)
        )
    )
    sparse_mat_inds_cens_y <- rstan::extract_sparse_parts(mat_inds_cens_y)

    model_data <- list(
        
        Nind = nrow(os),
        
        ##########################
        #
        # OS data
        #
        #
        
        Nind_dead = sum(os$event),
        dead_ind_index = which(os$event == 1),
        Times = os$time,
        p_os_cov_design = ncol(design_mat),
        os_cov_design = design_mat,
        n_nodes = length(gh_parameters$nodes),
        nodes = gh_parameters$nodes,
        weights = gh_parameters$weights,
        
        #########################
        #
        # Longmodel specific data
        #
        #
        
        Nta_total = nrow(lm),
        Yobs = lm$sld,
        Tobs = lm$time,
        ind_index = as.numeric(factor(lm$pt)),
        Ythreshold = cens_threshold,
        
        # Number of individuals and tumor assessments.
        Nta_obs_y = sum(lm$sld >= cens_threshold),
        Nta_cens_y = sum(lm$sld < cens_threshold),
        
        # Index vectors
        ind_index = as.numeric(factor(as.character(lm$pt))),
        obs_y_index = which(lm$sld >= cens_threshold),
        cens_y_index = which(lm$sld < cens_threshold),
        
        n_studies = length(unique(lm$study)),
        n_arms = length(unique(lm$arm)),
        study_index = as.numeric(factor(as.character(lm$study))),
        arm_index = as.numeric(factor(as.character(lm$arm))),
        
        
        # Sparse matrix parameters
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
        u_mat_inds_cens_y = sparse_mat_inds_cens_y$u
    )
    return(model_data)
}



### TODO - Left over parameter in daniels data object that we've not included... ?
# # Survival data.
# Death = osd_final$DEATH,

# dat <- list(


#     arm_to_study_index = c(1L, 2L, 3L, 3L, 3L, 3L),

#     # The patients in each of the four different treatment arms.
#     n_index_per_arm = c(
#         sum(osd_final$ARM == "IMV210_A"),
#         sum(osd_final$ARM == "IMV211_A"),
#         sum(osd_final$ARM == "MOR_A"),
#         sum(osd_final$ARM == "MOR_AT"),
#         sum(osd_final$ARM == "MOR_ASG"),
#         sum(osd_final$ARM == "MOR_AEV")
#     ),
#     index_per_arm = c(
#         which(osd_final$ARM == "IMV210_A"),
#         which(osd_final$ARM == "IMV211_A"),
#         which(osd_final$ARM == "MOR_A"),
#         which(osd_final$ARM == "MOR_AT"),
#         which(osd_final$ARM == "MOR_ASG"),
#         which(osd_final$ARM == "MOR_AEV")
#     ),

# )




# lm <- dat_lm 











# # Integration parameters.
# gh_parameters <- statmod::gauss.quad(n = 15, kind = "legendre")



# # Timepoints for OS hazard and survival function estimation to generate predictions.
# os_pred_times <- seq(from = 0.001, to = 2, length = 100)


# # OS covariate design matrix.


# # transform available numeric covariates.
# osd <- osd %>%
#     dplyr::mutate(
#         CLIVER = LIVER - 1,
#         CLOGNLR = log(NLR) - log(3.8),
#         CLOGCRP = log(CRP) - log(17.4),
#         CALBU = ALBU - 39
#     )


# # exclude intercept:
# os_cov_design <- model.matrix(~ -1 + BECOG + CLOGNLR + CLOGCRP + CALBU + CLIVER, data = osd)
# head(os_cov_design)


# # Censored or complete Morpheus data? ----
# data_setting <- "censored"
# stopifnot(data_setting %in% c("censored", "complete"))


# # For first goal:
# # Manipulate OS data to censor all Morpheus data at time 0.
# osd_final <- if (data_setting == "censored") {
#     osd %>%
#         dplyr::mutate(
#             AYR = ifelse(STUDYID == "WO39613", 0, AYR),
#             DEATH = ifelse(STUDYID == "WO39613", 0, DEATH)
#         )
# } else {
#     osd
# }

# # Assemble everything in a list matching stan file data elements.
# dat <- list(
#     # Number of individuals and tumor assessments.
#     Nind = nrow(osd_final),
#     Nta_total = nrow(sld),
#     Nta_obs_y = sum(sld$AVAL >= cens_threshold),
#     Nta_cens_y = sum(sld$AVAL < cens_threshold),
#     Nind_dead = sum(osd_final$DEATH == 1),

#     # Index vectors.
#     ind_index = sld$USUBJID_INDEX,
#     obs_y_index = which(sld$AVAL >= cens_threshold),
#     cens_y_index = which(sld$AVAL < cens_threshold),
#     dead_ind_index = which(osd_final$DEATH == 1),

#     n_studies = 3L,
#     study_index = as.numeric(factor(osd_final$STUDYID, levels = c("GO29293", "GO29294", "WO39613"))),
#     n_arms = 6L,
#     arm_index = as.numeric(factor(osd_final$ARM, levels = c("IMV210_A", "IMV211_A", "MOR_A", "MOR_AT", "MOR_ASG", "MOR_AEV"))),

#     n_save_individual = sum(osd_final$STUDYID == "WO39613"),
#     index_save_individual = which(osd_final$STUDYID == "WO39613"),

#     n_sld_par_shared = 3L,
#     sld_par_shared = 1:3,
#     n_sld_par_separate = 3L,
#     sld_par_separate = 4:6,

#     arm_to_study_index = c(1L, 2L, 3L, 3L, 3L, 3L),

#     # The patients in each of the four different treatment arms.
#     n_index_per_arm = c(
#         sum(osd_final$ARM == "IMV210_A"),
#         sum(osd_final$ARM == "IMV211_A"),
#         sum(osd_final$ARM == "MOR_A"),
#         sum(osd_final$ARM == "MOR_AT"),
#         sum(osd_final$ARM == "MOR_ASG"),
#         sum(osd_final$ARM == "MOR_AEV")
#     ),
#     index_per_arm = c(
#         which(osd_final$ARM == "IMV210_A"),
#         which(osd_final$ARM == "IMV211_A"),
#         which(osd_final$ARM == "MOR_A"),
#         which(osd_final$ARM == "MOR_AT"),
#         which(osd_final$ARM == "MOR_ASG"),
#         which(osd_final$ARM == "MOR_AEV")
#     ),

#     # Tumor assessment values and time points.
#     Yobs = sld$AVAL,
#     Tobs = sld$AYR,
#     Ythreshold = cens_threshold,

#     # Matrix of individuals x observed tumor assessments.
#     n_w_mat_inds_obs_y = length(sparse_mat_inds_obs_y$w),
#     w_mat_inds_obs_y = sparse_mat_inds_obs_y$w,
#     n_v_mat_inds_obs_y = length(sparse_mat_inds_obs_y$v),
#     v_mat_inds_obs_y = sparse_mat_inds_obs_y$v,
#     n_u_mat_inds_obs_y = length(sparse_mat_inds_obs_y$u),
#     u_mat_inds_obs_y = sparse_mat_inds_obs_y$u,

#     # Matrix of individuals x censored tumor assessments.
#     n_w_mat_inds_cens_y = length(sparse_mat_inds_cens_y$w),
#     w_mat_inds_cens_y = sparse_mat_inds_cens_y$w,
#     n_v_mat_inds_cens_y = length(sparse_mat_inds_cens_y$v),
#     v_mat_inds_cens_y = sparse_mat_inds_cens_y$v,
#     n_u_mat_inds_cens_y = length(sparse_mat_inds_cens_y$u),
#     u_mat_inds_cens_y = sparse_mat_inds_cens_y$u,

#     # Survival data.
#     Times = osd_final$AYR,
#     Death = osd_final$DEATH,
#     os_cov_design = os_cov_design,
#     p_os_cov_design = ncol(os_cov_design),

#     # For OS predictions.
#     os_pred_times = os_pred_times,
#     n_os_pred_times = length(os_pred_times),

#     # Integration paramaters.
#     n_nodes = length(gh_parameters$nodes),
#     nodes = gh_parameters$nodes,
#     weights = gh_parameters$weights
# )