

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
        vector[n] psi_ks_mod = if_gte0_else(time, psi_ks, 0);
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
    vector lm_predict_value(vector time, matrix long_gq_parameters) {
        return sld(
            time,
            long_gq_parameters[,1],
            long_gq_parameters[,2],
            long_gq_parameters[,3]
        );
    }
}

