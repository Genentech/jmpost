

functions {
    //
    // Source - lm-sf/functions.stan
    //
    vector sld(
        vector time,
        vector psi_bsld,
        vector psi_ks,
        vector psi_kg
    ) {
        int n = num_elements(time);
        vector[n] include_shrinkage = ifelse(
            is_negative(time),
            zeros_vector(n),
            rep_vector(1, n)
        );
        vector[n] result = fmin(
            8000.0,
            psi_bsld .* (include_shrinkage .* exp(- psi_ks .* time) + exp(psi_kg .* time) - 1)
        );
        return result;
    }
}

