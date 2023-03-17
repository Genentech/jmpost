




# dat_os <- DataSurvival(
#     data = os_dataset,
#     frm = Surv(time, event) ~ var1 + var2 + var3,
#     subject = "PT",
#     arm = "ARM",
#     study = "STUDYID"
# )

# dat_lm <- DataLongitudinal(
#     data = lm_dataset,
#     frm = outcome ~ time,
#     subject = "PT"
# )

# DataJoint(
#     survival = dat_os,
#     longitudinal = dat_lm
# )



.DataJoint <- setClass(
    Class = "DataJoint",
    representation = list(
        survival = "DataSurvival",
        longitudinal = "DataLongitudinal"
    )
)

#' @export
DataJoint <- function(survival, longitudinal) {
    .DataJoint(
        survival = survival,
        longitudinal = longitudinal
    )
}




#' @importFrom survival Surv
as_stan_data <- function(object) {
    lm <- object@longitudinal@data
    os <- object@survival@data
    lm_pt <- object@longitudinal@subject
    os_pt <- object@survival@subject
    os_arm <- object@survival@arm
    os_study <- object@survival@study
    
    os_frm <- object@survival@formula
    os_time <- as.character(os_frm[[2]][[2]])
    os_event <- as.character(os_frm[[2]][[3]])
    
    lm_frm <- object@longitudinal@formula
    lm_time <- as.character(lm_frm[[3]])
    lm_outcome <- as.character(lm_frm[[2]])
    
    threshold <- object@longitudinal@threshold
    
    
    assert_that(
        all(as.character(lm[[lm_pt]]) %in% as.character(os[[os_pt]])),
        msg = "There are subjects in the longitudinal data that do not exist in the survival data"
    )
    assert_that(
        all(as.character(os[[os_pt]]) %in% as.character(lm[[lm_pt]])),
        msg = "There are subjects in the survival data that do not exist in the longitudinal data"
    )
    assert_that(
        length(os[[os_pt]]) == length(unique(os[[os_pt]])),
        msg = "The survival data contains multiple entries per subject"
    )
    
    os_fct <- os
    os_fct[[os_arm]] <- factor(os_fct[[os_arm]])
    os_fct[[os_study]] <- factor(os_fct[[os_study]])
    os_fct[[os_pt]] <- factor(os_fct[[os_pt]])
    
    lm_fct <- lm
    # TODO - test this doesn't break anything
    lm_fct[[lm_pt]] <- factor(lm_fct[[lm_pt]], levels = levels(os_fct[[os_pt]]))


    os_ordered <- os_fct[order(os_fct[[os_pt]]), ]
    lm_ordered <- lm_fct[order(lm_fct[[lm_pt]], lm_fct[[lm_time]]), ]

    # Formulas retain the environment from where they are created
    # we change its environment to that of the current function so that
    # it can access the Surv function loaded by the package
    environment(os_frm) <- environment()
    design_mat <- stats::model.matrix(os_frm, data = os_ordered)
    remove_index <- grep("(Intercept)", colnames(design_mat), fixed = TRUE)
    design_mat <- design_mat[,-remove_index, drop = FALSE]
    
    # Integration parameters.
    gh_parameters <- statmod::gauss.quad(n = 15, kind = "legendre")
    
    
    mat_sld_index <- t(
        model.matrix(as.formula(paste("~", lm_pt)), data = lm_ordered)
    )
    
    # TODO - Maybe reimplement this using a more robust approach
    # If threshold has not been set convert to magic low number
    adj_threshold <- switch(is.null(threshold),
        "TRUE" = -999999,
        "FALSE" = threshold
    )
    
    index_obs <- which(lm_ordered[[lm_outcome]] >= adj_threshold)
    index_cen <- which(lm_ordered[[lm_outcome]] < adj_threshold)
    
    sparse_mat_inds_all_y <- rstan::extract_sparse_parts(mat_sld_index)
    sparse_mat_inds_obs_y <- rstan::extract_sparse_parts(mat_sld_index[,index_obs])
    sparse_mat_inds_cens_y <- rstan::extract_sparse_parts(mat_sld_index[,index_cen])


    ###########
    #
    # OS data
    #
    model_data_os <- list(
        Nind = nrow(os_ordered),
        Nind_dead = sum(os_ordered[[os_event]]),
        dead_ind_index = which(os_ordered[[os_event]] == 1),
        Times = os_ordered[[os_time]],
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
        Nta_total = nrow(lm_ordered),
        Yobs = lm_ordered[[lm_outcome]],
        Tobs = lm_ordered[[lm_time]],
        Ythreshold = threshold,

        # Number of individuals and tumor assessments.
        Nta_obs_y = length(index_obs),
        Nta_cens_y = length(index_cen),

        # Index vectors
        ind_index = as.numeric(os_ordered[[lm_pt]]),
        obs_y_index = index_obs,
        cens_y_index = index_cen,

        # Sparse matrix parameters
        # Matrix of individuals x observed tumor assessments.
        n_mat_inds_obs_y = c(
            length(sparse_mat_inds_obs_y$w),
            length(sparse_mat_inds_obs_y$v),
            length(sparse_mat_inds_obs_y$u)
        ),
        w_mat_inds_obs_y = sparse_mat_inds_obs_y$w,
        v_mat_inds_obs_y = sparse_mat_inds_obs_y$v,
        u_mat_inds_obs_y = sparse_mat_inds_obs_y$u,

        # Matrix of individuals x censored tumor assessments.
        n_mat_inds_cens_y = c(
            length(sparse_mat_inds_cens_y$w),
            length(sparse_mat_inds_cens_y$v),
            length(sparse_mat_inds_cens_y$u)
        ),
        w_mat_inds_cens_y = sparse_mat_inds_cens_y$w,
        v_mat_inds_cens_y = sparse_mat_inds_cens_y$v,
        u_mat_inds_cens_y = sparse_mat_inds_cens_y$u,
        
        # Matrix of all individuals tumour assessments
        n_mat_inds_all_y = c(
            length(sparse_mat_inds_all_y$w),
            length(sparse_mat_inds_all_y$v),
            length(sparse_mat_inds_all_y$u)
        ),
        w_mat_inds_all_y = sparse_mat_inds_all_y$w,
        v_mat_inds_all_y = sparse_mat_inds_all_y$v,
        u_mat_inds_all_y = sparse_mat_inds_all_y$u
    )
    
    model_data <- append(model_data_lm, model_data_os)
    
    u_arm_study <- unique(os_ordered[, c(os_study, os_arm)])
    u_arm_study_ord <- u_arm_study[order(u_arm_study[[os_arm]], u_arm_study[[os_study]]), ]

    n_per_arm <- tapply(
        rep(1, nrow(os_ordered)),
        os_ordered[[os_arm]],
        sum,
        simplify = FALSE
    )
    model_data[["n_index_per_arm"]] <- unlist(n_per_arm[levels(os_ordered[[os_arm]])], use.names = FALSE)


    # TODO - Assumption that arms are unique to studies
    assert_that(
        length(u_arm_study[[os_arm]]) == length(unique(u_arm_study[[os_arm]])),
        msg = "Arms must be unique across Studies i.e. arm A can't be in Study-1 and Study-2"
    )

    model_data[["arm_to_study_index"]] <- as.numeric(u_arm_study[["study"]])
    model_data[["n_studies"]] <- length(unique(u_arm_study[["study"]]))
    model_data[["n_arms"]] <- nrow(u_arm_study)
    model_data[["study_index"]] <- as.numeric(os_ordered[[os_study]])
    model_data[["arm_index"]] <- as.numeric(os_ordered[[os_arm]])
    model_data[["index_per_arm"]] <- as.numeric(os_ordered[[os_pt]])
    
    return(model_data)
}

#' @export
setMethod(
    "as.StanData",
    signature = "DataJoint",
    definition = as_stan_data
)


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

