

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
        vector[nrow] psi_phi_mod = replace_lt_0(psi_phi, 0);

        vector[nrow] result = fmin(
            8000.0,
            psi_bsld .* 
            (psi_phi_mod .* exp(- psi_ks .* time) + (1 - psi_phi_mod) .* exp(psi_kg .* time))
        );
        return result;
    }
}

