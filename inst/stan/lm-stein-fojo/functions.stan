

functions {
    //
    // Source - lm-stein-fojo/functions.stan
    //
    vector sld(
        vector time,
        vector psi_bsld,
        vector psi_ks,
        vector psi_kg
    ) {
        int n = rows(time);
        vector[n] psi_ks_mod = if_lt0_else(time, psi_ks, 0);
        vector[n] result = fmin(
            8000.0,
            psi_bsld  .* (
                exp(- psi_ks_mod .* time)
                + exp(psi_kg .* time)
                - rep_vector(1, n)
            )
        );
        return result;
    }
    row_vector lm_predict_individual_patient(vector time, row_vector long_gq_parameters) {
        int nrow = rows(time);
        return sld(
            time,
            rep_vector(long_gq_parameters[1], nrow),
            rep_vector(long_gq_parameters[2], nrow),
            rep_vector(long_gq_parameters[3], nrow)
         )';
    }
}

