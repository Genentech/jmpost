
data {
    int sld_n;
    vector[sld_n] sld_time;
    vector[sld_n] sld_bsld;
    vector[sld_n] sld_s;
    vector[sld_n] sld_g;
    vector[sld_n] sld_phi;
}

generated quantities {
    vector[sld_n] sld_results = sld(sld_time, sld_bsld, sld_s, sld_g, sld_phi);
}

