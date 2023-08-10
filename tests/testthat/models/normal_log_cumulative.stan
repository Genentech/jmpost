
data {
    int nrm_logden_n;
    vector[nrm_logden_n] nrm_logden_obs;
    vector[nrm_logden_n] nrm_logden_mu;
    vector[nrm_logden_n] nrm_logden_sigma;
        
    int nrm_logcum_n;
    real nrm_logcum_quant;
    vector[nrm_logcum_n] nrm_logcum_mu;
    vector[nrm_logcum_n] nrm_logcum_sigma;
}


generated quantities {    
    vector[nrm_logden_n] nrm_logden_results = vect_normal_log_dens(
        nrm_logden_obs,
        nrm_logden_mu,
        nrm_logden_sigma
    );
    
    vector[nrm_logcum_n] nrm_logcum_results = vect_normal_log_cum(
        nrm_logcum_quant,
        nrm_logcum_mu,
        nrm_logcum_sigma
    );
}
