functions {
    vector lm_predict_value(vector time, matrix long_gq_parameters) {
        return sld(
            time,
            long_gq_parameters[, 1],
            long_gq_parameters[, 2],
            long_gq_parameters[, 3]
        );
    }
}
