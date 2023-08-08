
functions {
    //
    // Source - lm-gsf/link_identity.stan
    //

    // Identity of SLD
    matrix link_identity_contribution(
        matrix time,
        vector psi_bsld,
        vector psi_ks,
        vector psi_kg,
        vector psi_phi
    ) {
        int nrows = rows(psi_bsld);
        int ncols = cols(time);
        matrix[nrows, ncols] result;
        for (i in 1:ncols) {
            result[,i] = fmin(
                sld(time[,i], psi_bsld, psi_ks, psi_kg, psi_phi),
                8000.0
            );
        }
        return result;
    }
}


