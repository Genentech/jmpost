  row_vector[Nind] Times;
  int Death[Nind];
  int<lower=1> p_os_cov_design;
  matrix[Nind, p_os_cov_design] os_cov_design;


  int<lower=1> n_os_pred_times;
  row_vector<lower=0>[n_os_pred_times] os_pred_times;

  int<lower=1> n_nodes;
  vector[n_nodes] nodes;
  row_vector<lower=0, upper=1>[n_nodes] weights;
