

functions {
    //
    // Source - lm-claret-bruno/functions.stan
    //
    vector sld(
        vector time,
        vector ind_b,
        vector ind_g,
        vector ind_c,
        vector ind_p
    ) {
        int nrow = rows(time);
        vector[nrow] ind_p_mod = if_lt0_else(time, ind_p, 0);

        vector[nrow] result = fmin(
            8000.0,
            ind_b .* exp(ind_g .* time - (ind_p_mod ./ ind_c) .* (1 - exp(-ind_c .* time)))
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

