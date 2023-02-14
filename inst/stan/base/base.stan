functions {

    // Constant used in below.
    real neg_log_sqrt_2_pi() {
        return -0.9189385332046727;
    }


    // Vectorized version of normal_lpdf, i.e. returns log normal density values.
    row_vector vect_normal_log_dens(row_vector y, row_vector mu, row_vector sigma) {
        row_vector[num_elements(y)] y_stand = (y - mu) ./ sigma;
        row_vector[num_elements(y)] main_result = - (y_stand .* y_stand) / 2;
        return main_result + neg_log_sqrt_2_pi() - log(sigma);
    }
    vector vect_normal_log_dens(vector y, vector mu, vector sigma) {
        return vect_normal_log_dens(y', mu', sigma')';
    }


    // Vectorized version of normal_lcdf, i.e. return log normal CDF values.
    row_vector vect_normal_log_cum(real quantile, row_vector mu, row_vector sigma) {
        row_vector[num_elements(mu)] quant_stand = (quantile - mu) ./ sigma;
        row_vector[num_elements(mu)] cdf_vals = Phi(quant_stand);
        return log(cdf_vals);
    }
    vector vect_normal_log_cum(real quantile, vector mu, vector sigma) {
        return vect_normal_log_cum(quantile, mu', sigma')';
    }


    // Means across the rows of a matrix
    row_vector row_means(matrix x) {
        row_vector[cols(x)] result = rep_row_vector(1.0 / rows(x), rows(x)) * x;
        return result;
    }


    // Vectorized ifelse() similar as in R.
    row_vector ifelse(array[] int condition, row_vector yes, row_vector no) {
        row_vector[num_elements(yes)] result;
        for (i in 1:num_elements(yes)) {
            result[i] = condition[i] ? yes[i] : no[i];
        }
        return result;
    }
    vector ifelse(array[] int condition, vector yes, vector no) {
        return ifelse(condition, yes', no')';
    }


    // Vectorized negative predicate. Basically returns ifelse(x < 0, 1, 0).
    array[] int is_negative(row_vector x) {
        array[num_elements(x)] int result;
        for (i in 1:num_elements(x)) {
            result[i] = x[i] < 0.0;
        }
        return result;
    }
    array[] int is_negative(vector x) {
        return is_negative(x');
    }

    // Vectorized positive predicate. Basically returns ifelse(x > 0, 1, 0).
    array[] int is_positive(row_vector x) {
        return is_negative(- x);
    }
    array[] int is_positive(vector x) {
        return is_positive(x');
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



{% if link_none %}
    // If user has requested link_none then provide a dummy link_contribution function
    // that does nothing    
    matrix link_contribution(matrix time, matrix pars_lm) {
        return  rep_matrix(0, rows(time), cols(time));
    }
{% endif %}

{{ longditudinal.functions }}
{{ survival.functions }}

}



data{
    int<lower=1> Nind;                 // Number of individuals.
    
{{ survival.data }}
{{ longditudinal.data }}

}


transformed data {

{{ longditudinal.transformed_data }}
{{ survival.transformed_data }}


{% if link_none %}
    // If user has requested link_none then provide a dummy pars_lm object
    // that contains nothing
    matrix[Nind, 0] pars_lm = rep_matrix(0, Nind, 0);
{% endif %}

}



parameters{
{{ longditudinal.parameters }}
{{ survival.parameters }}
}



transformed parameters{

    // Log-likelihood values for using the loo package.
    vector[Nind] log_lik = rep_vector(0.0, Nind);
    
{{ longditudinal.transformed_parameters }}
{{ survival.transformed_parameters }}

}


model{
{{ longditudinal.model }}
{{ survival.model }}
{{ priors.model }}
    target += sum(log_lik);
}

