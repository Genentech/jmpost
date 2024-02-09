


transformed parameters {
    //
    // Source - lm-gsf/link.stan
    //
    matrix[Nind, 4] link_function_inputs;
    pars_lm[,1] = lm_gsf_psi_bsld;
    pars_lm[,2] = lm_gsf_psi_ks;
    pars_lm[,3] = lm_gsf_psi_kg;
    pars_lm[,4] = lm_gsf_psi_phi;
}



