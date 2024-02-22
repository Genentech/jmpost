data {
    int sld_n;
    int time_p;
    matrix[sld_n, time_p] sld_time;
    vector[sld_n] sld_bsld;
    vector[sld_n] sld_s;
    vector[sld_n] sld_g;
    vector[sld_n] sld_phi;
}


transformed data {
    //
    // Source - lm-gsf/link.stan
    //
    matrix[sld_n, 4] link_function_inputs;
    link_function_inputs[,1] = sld_bsld;
    link_function_inputs[,2] = sld_s;
    link_function_inputs[,3] = sld_g;
    link_function_inputs[,4] = sld_phi;
}


generated quantities {
    matrix[sld_n, time_p] results = link_identity_contrib(
        sld_time,
        link_function_inputs
    );
}
