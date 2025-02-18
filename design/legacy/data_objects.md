

## Global Data Objects


```R
int<lower=1> Nind        #  Number of individuals
int<lower=1> n_studies   #  Number of different studies in the data
int<lower=1> n_arms      #  Number of unique treatment arms (No duplicates across studies)

#  Study index per patient (sorted by pt e.g. study_index[3] = study index for patient #3)
array[Nind] int<lower=1,upper=n_studies> pt_study_index

#  Arm index per patient (sorted by pt e.g. arm_index[3] = arm index for patient #3)
array[Nind] int<lower=1,upper=n_arms> pt_arm_index
```



## Survival Data Objects


```R
int<lower=1> Nind_dead                 # Number of patients who died
vector[Nind] Times                     # Event and Censor times per subject
array[Nind_dead] int dead_ind_index    # Patient Index numbers for which patients died
int<lower=1> p_os_cov_design           # Number of columns in design matrix
matrix[Nind, p_os_cov_design] os_cov_design  # Design matrix

# Parameters for gausian quadrature integration
int<lower=1> n_nodes
vector[n_nodes] nodes
vector<lower=0, upper=1>[n_nodes] weights
```


## Longitudinal Data Objects


```R
int<lower=1> Nta_total   #  Number of tumour observations
int<lower=1> Nta_obs_y   #  Number of observed tumour values
int<lower=0> Nta_cens_y  #  Number of censored tumour values
# Nta_total = Nta_obs_y + Nta_cens_y


#  Patient index numbers for ownership of tumour observations 
array[Nta_total] int ind_index

#  Tumour Index numbers for observed tumour values
array[Nta_obs_y] int obs_y_index

#  Tumour Index numbers for censored tumour values
array[Nta_cens_y] int cens_y_index   


vector[Nta_total] Yobs   #  Tumour size observations
vector[Nta_total] Tobs   #  Tumour observation time
real Ythreshold          #  Tumour censor threshold value


# Index matrix of tumour observations by patients in sparse matrix format (all obvs)
array [3] int<lower=0> n_mat_inds_all_y;
vector[n_mat_inds_all_y[1]] w_mat_inds_all_y;
array[n_mat_inds_all_y[2]] int v_mat_inds_all_y;
array[n_mat_inds_all_y[3]] int u_mat_inds_all_y;

# Index matrix of tumour observations by patients in sparse matrix format (observed obvs)
array [3] int<lower=1> n_mat_inds_obs_y;
vector[n_mat_inds_obs_y[1]] w_mat_inds_obs_y;
array[n_mat_inds_obs_y[2]] int v_mat_inds_obs_y;
array[n_mat_inds_obs_y[3]] int u_mat_inds_obs_y;


# Index matrix of tumour observations by patients in sparse matrix format (censored obvs)
array [3] int<lower=0> n_mat_inds_cens_y;
vector[n_mat_inds_cens_y[1]] w_mat_inds_cens_y;
array[n_mat_inds_cens_y[2]] int v_mat_inds_cens_y;
array[n_mat_inds_cens_y[3]] int u_mat_inds_cens_y;



```



