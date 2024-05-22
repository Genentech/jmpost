

functions {
    //
    // Source - lm-gsf/functions.stan
    //
    vector sld(
        vector time,
        vector psi_bsld,
        vector psi_ks,
        vector psi_kg,
        vector psi_phi
    ) {
        int nrow = rows(time);
        vector[nrow] psi_phi_mod = if_lt0_else(time, psi_phi, 0);

        vector[nrow] result = fmin(
            8000.0,
            psi_bsld .* 
            (psi_phi_mod .* exp(- psi_ks .* time) + (1 - psi_phi_mod) .* exp(psi_kg .* time))
        );
        return result;
    }

    vector lm_predict_value(vector time, matrix long_gq_parameters) {
        return sld(
            time,
            long_gq_parameters[,1],
            long_gq_parameters[,2],
            long_gq_parameters[,3],
            long_gq_parameters[,4]
        );
    }
}

