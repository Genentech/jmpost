

# TODO - Checkmate for input validation



# TODO - Remap required variables as string / object
# time, event
#' @export
as_stan_data <- function(os, lm, frm, cens_threshold = 5) {
    assert_that(
        all(lm$pt %in% os$pt),
        all(os$pt %in% lm$pt),
        nrow(os) == length(unique(os$pt))
    )

    os <- os |>
        dplyr::mutate(study = factor(study)) |>
        dplyr::mutate(arm = factor(arm)) |>
        dplyr::mutate(pt_f = factor(pt))

    lm <- lm |>
        dplyr::left_join(dplyr::select(os, pt, pt_f), by = "pt")
        
    
    design_mat <- stats::model.matrix(frm, data = os)
    remove_index <- grep("(Intercept)", colnames(design_mat), fixed = TRUE)
    design_mat <- design_mat[,-remove_index, drop = FALSE]
    
    # Integration parameters.
    gh_parameters <- statmod::gauss.quad(n = 15, kind = "legendre")
    
    
    mat_sld_index <- t(
        model.matrix(~ -1 + pt, data = lm)
    )
    
    index_obs <- which(lm$sld >= cens_threshold)
    index_cen <- which(lm$sld < cens_threshold)
    
    sparse_mat_inds_obs_y <- rstan::extract_sparse_parts(mat_sld_index[,index_obs])
    sparse_mat_inds_cens_y <- rstan::extract_sparse_parts(mat_sld_index[,index_cen])


    ###########
    #
    # OS data
    #
    model_data_os <- list(
        Nind = nrow(os),
        Nind_dead = sum(os$event),
        dead_ind_index = which(os$event == 1),
        Times = os$time,
        p_os_cov_design = ncol(design_mat),
        os_cov_design = design_mat,
        n_nodes = length(gh_parameters$nodes),
        nodes = gh_parameters$nodes,
        weights = gh_parameters$weights
    )
        
        
    ###########
    #
    # Longmodel specific data
    #
    model_data_lm <- list(
        Nta_total = nrow(lm),
        Yobs = lm$sld,
        Tobs = lm$time,
        Ythreshold = cens_threshold,

        # Number of individuals and tumor assessments.
        Nta_obs_y = sum(lm$sld >= cens_threshold),
        Nta_cens_y = sum(lm$sld < cens_threshold),

        # Index vectors
        ind_index = as.numeric(lm$pt_f),
        obs_y_index = which(lm$sld >= cens_threshold),
        cens_y_index = which(lm$sld < cens_threshold),

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
    
    model_data <- append(model_data_lm, model_data_os)
    
    u_arm_study <- os |>
        dplyr::distinct(study, arm) |>
        dplyr::arrange(arm, study)

    n_per_arm <- os |>
        dplyr::group_by(arm) |>
        dplyr::tally() |>
        dplyr::arrange(arm)


    # TODO - Assumption that arms are unique to studies
    assert_that(
        length(u_arm_study[["arm"]]) == length(unique(u_arm_study[["arm"]]))
    )

    model_data[["arm_to_study_index"]] <- as.numeric(u_arm_study[["study"]])
    model_data[["n_studies"]] <- length(unique(u_arm_study[["study"]]))
    model_data[["n_arms"]] <- nrow(u_arm_study)
    model_data[["study_index"]] <- as.numeric(os[["study"]])
    model_data[["arm_index"]] <- as.numeric(os[["arm"]])
    model_data[["index_per_arm"]] <- as.numeric(os[["pt_f"]])
    model_data[["n_index_per_arm"]] <- n_per_arm[["n"]]
    
    return(model_data)
}




### TODO - The following parameters were only used in the generated quantities which are
###        currently excluded

#     Death = osd_final$DEATH,   # Only used in generated quantities
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

