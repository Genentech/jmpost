functions {
    //
    // Source - base/functions.stan
    //

    // Constant used in below.
    real neg_log_sqrt_2_pi() {
        return -0.9189385332046727;
    }


    // Vectorized version of normal_lpdf, i.e. returns log normal density values.
    vector vect_normal_log_dens(vector y, vector mu, vector sigma) {
        vector[num_elements(y)] y_stand = (y - mu) ./ sigma;
        vector[num_elements(y)] main_result = - (y_stand .* y_stand) / 2;
        return main_result + neg_log_sqrt_2_pi() - log(sigma);
    }


    // Vectorized version of normal_lcdf, i.e. return log normal CDF values.
    vector vect_normal_log_cum(real quantile, vector mu, vector sigma) {
        vector[num_elements(mu)] quant_stand = (quantile - mu) ./ sigma;
        vector[num_elements(mu)] cdf_vals = Phi(quant_stand);
        return log(cdf_vals);
    }


    // Means across the rows of a matrix
    vector row_means(matrix x) {
        vector[rows(x)] result = x * rep_vector(1.0 / rows(x), cols(x)) ;
        return result;
    }
    
    // Means across the columns of a matrix
    row_vector col_means(matrix x) {
        row_vector[cols(x)] result = rep_row_vector(1.0 / cols(x), rows(x)) * x;
        return result;
    }


    // Vectorized ifelse() similar as in R.
    vector ifelse(array[] int condition, vector yes, vector no) {
        vector[num_elements(yes)] result;
        for (i in 1:num_elements(yes)) {
            result[i] = condition[i] ? yes[i] : no[i];
        }
        return result;
    }



    // Vectorized negative predicate. Basically returns ifelse(x < 0, 1, 0).
    array[] int is_negative(vector x) {
        array[num_elements(x)] int result;
        for (i in 1:num_elements(x)) {
            result[i] = x[i] < 0.0;
        }
        return result;
    }


    // Vectorized positive predicate. Basically returns ifelse(x > 0, 1, 0).
    array[] int is_positive(vector x) {
        return is_negative(- x);
    }


    // which function. Returns which of the 0/1 elements are not 0.
    array[] int which(array[] int x) {
        int len = sum(x);
        array[len] int ret;
        int pointer = 0;
        for (i in 1:num_elements(x)) {
            if (x[i] != 0) {
                if (x[i] != 1) {
                    reject("integer array passed to `which` function must only contain 0 or 1");
                }
                pointer = pointer + 1;
                ret[pointer] = i;
            }
        }
        return ret;
    }

    // Replaces the values of `y` with `replacement` if the corresponding value
    // of `x` is less than 0.
    matrix if_gte0_else(matrix x, matrix y, real replacement) {
        matrix[rows(x), cols(x)] result;
        for (i in 1:rows(x)) {
            for (j in 1:cols(x)) {
                result[i, j] = x[i, j] >= 0 ?  y[i, j] : replacement;
            }
        }
        return result;
    }
    vector if_gte0_else(vector x, vector y, real replacement) {
        vector[rows(x)] result;
        for (i in 1:rows(x)) {
            result[i] = x[i] >= 0 ? y[i] : replacement;
        }
        return result;
    }
}

