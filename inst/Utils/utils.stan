row_vector ifelse(int[] condition, row_vector yes, row_vector no) {
    row_vector[num_elements(yes)] result;
    for (i in 1:num_elements(yes)) {
      result[i] = condition[i] ? yes[i] : no[i];
    }
    return result;
  }

  int[] is_negative(row_vector x) {
    int result[num_elements(x)];
    for (i in 1:num_elements(x)) {
      result[i] = x[i] < 0.0;
    }
    return result;
  }

  int[] is_positive(row_vector x) {
    return is_negative(- x);
  }

  int[] which(int[] x) {
    int len = sum(x);
    int ret[len];
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


  real neg_log_sqrt_2_pi() {
    return -0.9189385332046727;
  }


  row_vector row_means(matrix x) {
    row_vector[cols(x)] result = rep_row_vector(1.0 / rows(x), rows(x)) * x;
    return result;
  }
