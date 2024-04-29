
data {
    //
    // Source - base/generated_quantities_data.stan
    //
    int <lower=0, upper=1> gq_long_flag;
    int <lower=0, upper=1> gq_surv_flag;
    int <lower=0, upper=1> gq_long_population_flag;

    int <lower=1> gq_n_quant;

    array[gq_n_quant] int<lower=1, upper=n_subjects> gq_pt_index;
    vector[gq_n_quant] gq_times;

    array [gq_n_quant] int <lower=1, upper=n_arms> gq_long_pop_arm_index;
    array [gq_n_quant] int <lower=1, upper=n_studies> gq_long_pop_study_index;
}
