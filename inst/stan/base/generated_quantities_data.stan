
data {
    //
    // Source - base/generated_quantities_data.stan
    //
    int <lower=0, upper=1> gq_long_flag;
    int <lower=0, upper=1> gq_surv_flag;

    int <lower=1> gq_n_quant;

    array[gq_n_quant] int<lower=1, upper=Nind> gq_pt_index;
    vector[gq_n_quant] gq_times;
}
