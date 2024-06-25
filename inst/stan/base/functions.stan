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

    // Element wise "if greater than 0 return y, else return y"
    //
    // Input:
    //     item: a matrix or vector
    //     x: an object of the same type and dimensionality of `item`
    //     y: an object of the same type and dimensionality of `item` or a `real`
    //
    // Description:
    //     The following overloaded functions take an object "item"
    //     and loop over each element returning the corresponding element
    //     of `x` if the element is >= 0 else returning the corresponding
    //     element of `y` (or just `y` if why is a single real)
    //
    // Returns:
    //     an object of the same type and dimensionality of `item`
    //     but with the values of either `x` or `y`. 
    vector if_gte0_else(vector item, vector x, real y) {
        vector[rows(item)] result;
        for (i in 1:rows(item)) {
            result[i] = item[i] >= 0 ? x[i] : y;
        }
        return result;
    }
    vector if_gte0_else(vector item, vector x, vector y) {
        vector[rows(item)] result;
        for (i in 1:rows(item)) {
            result[i] = item[i] >= 0 ? x[i] : y[i];
        }
        return result;
    }
    matrix if_gte0_else(matrix item, matrix x, real y) {
        matrix[rows(item), cols(item)] result;
        for (i in 1:rows(item)) {
            for (j in 1:cols(item)) {
                result[i, j] = item[i, j] >= 0 ?  x[i, j] : y;
            }
        }
        return result;
    }
    matrix if_gte0_else(matrix item, matrix x, matrix y) {
        matrix[rows(item), cols(item)] result;
        for (i in 1:rows(item)) {
            for (j in 1:cols(item)) {
                result[i, j] = item[i, j] >= 0 ? x[i, j] : y[i, j];
            }
        }
        return result;
    }
}

