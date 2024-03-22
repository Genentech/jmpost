

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

    row_vector lm_predict_individual_patient(vector time, row_vector long_gq_parameters) {
        int nrow = rows(time);
        return sld(
            time,
            rep_vector(long_gq_parameters[1], nrow),
            rep_vector(long_gq_parameters[2], nrow),
            rep_vector(long_gq_parameters[3], nrow),
            rep_vector(long_gq_parameters[4], nrow)
        )';
    }
}

