

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
        vector[n] is_post_baseline = ifelse(
            is_negative(time),
            zeros_vector(n),
            rep_vector(1, n)
        );
        vector[n] result = fmin(
            8000.0,
            psi_bsld .* is_post_baseline .* (
                exp(- psi_ks .* time) +  exp(psi_kg .* time) - rep_vector(1, n)
            )
        );
        return result;
    }
}

