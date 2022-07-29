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

  //- sld ----
  // SLD model (GSF)

  row_vector sld(row_vector time, row_vector psi_bsld, row_vector psi_ks,
  row_vector psi_kg, row_vector psi_phi) {
    row_vector[num_elements(time)] psi_phi_mod = ifelse(is_negative(time), zeros_row_vector(num_elements(time)), psi_phi);
    row_vector[num_elements(time)] result = fmin(
      8000.0,
      psi_bsld .* (psi_phi_mod .* exp(- psi_ks .* time) + (1 - psi_phi_mod) .* exp(psi_kg .* time))
    );
    return result;
  }

  //- ttg ----
  // Time-to-growth (TTG)

  row_vector ttg(row_vector psi_ks, row_vector psi_kg, row_vector psi_phi) {
    row_vector[num_elements(psi_ks)] num = logit(psi_phi) + log(psi_ks ./ psi_kg);
    row_vector[num_elements(psi_ks)] denom = psi_ks + psi_kg;
    row_vector[num_elements(psi_ks)] result = num ./ denom;
    return result;
  }

  //- ttg_for_hr ----
  // TTG for HR calculation

  real ttg_for_hr(real mu_ks, real mu_kg, real mu_phi) {
    real num = logit(mu_phi) + log(mu_ks / mu_kg);
    real denom = mu_ks + mu_kg;
    real result = num / denom;
    return result;
  }

  //- dtsld ----
  // Derivative of SLD

  matrix dtsld(matrix time, row_vector psi_bsld, row_vector psi_ks,
  row_vector psi_kg, row_vector psi_phi) {
    // Here we assume that psi's are replicated along the rows of the time matrix.
    matrix[rows(time), cols(psi_bsld)] psi_bsld_matrix = rep_matrix(psi_bsld, rows(time));
    matrix[rows(time), cols(psi_ks)] psi_ks_matrix = rep_matrix(psi_ks, rows(time));
    matrix[rows(time), cols(psi_kg)] psi_kg_matrix = rep_matrix(psi_kg, rows(time));
    // We also assume that all the time values are positive. Therefore no need to change phi.
    matrix[rows(time), cols(psi_phi)] psi_phi_matrix = rep_matrix(psi_phi, rows(time));
    matrix[rows(time), cols(time)] result = fmin(
      8000.0,
      psi_bsld_matrix .* (
        (1 - psi_phi_matrix) .* psi_kg_matrix .* exp(psi_kg_matrix .* time) -
        psi_phi_matrix .* psi_ks_matrix .* exp(- psi_ks_matrix .* time)
      )
    );
    return result;
  }

  //- dtsld_for_hr ----
  // Derivative of SLD for HR calculation

  row_vector dtsld_for_hr(row_vector time, real mu_bsld, real mu_ks,
  real mu_kg, real mu_phi) {
    row_vector[cols(time)] result = fmin(
      8000.0,
      mu_bsld * ((1 - mu_phi) * mu_kg * exp(mu_kg * time) - mu_phi * mu_ks * exp(- mu_ks * time))
    );
    return result;
  }
