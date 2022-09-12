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
