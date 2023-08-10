data {
    int sld_n;
    int time_p;
    matrix[sld_n, time_p] sld_time;
    vector[sld_n] sld_bsld;
    vector[sld_n] sld_s;
    vector[sld_n] sld_g;
    vector[sld_n] sld_phi;
}

generated quantities {
    matrix[sld_n, time_p] results = link_identity_contribution(
        sld_time,
        sld_bsld,
        sld_s,
        sld_g,
        sld_phi
    );
}
