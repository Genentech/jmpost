

```R
Nind[1]        #  Number of individuals
Nta_total[1]   #  Number of tumour observations
Nta_obs_y[1]   #  Number of observed tumour values
Nta_cens_y[1]  #  Number of censored tumour values
Nind_dead[1]   #  Number of patients who died

# Nta_total = Nta_obs_y + Nta_cens_y
  

ind_index[Nta_total]       #  Patient index numbers for ownership of tumour observations 
obs_y_index[Nta_obs_y]     #  Tumour Index numbers for observed tumour values
cens_y_index[Nta_cens_y]   #  Tumour Index numbers for censored tumour values
dead_ind_index[Nind_dead]  #  Patient Index numbers for which patients died

Yobs[Nta_total]   #  Tumour size observations
Tobs[Nta_total]   #  Tumour observation time
Ythereshold[1]    #  Tumour censor threshold value
  

n_studies[1]       #  Number of different studies in the data
study_index[Nind]  #  Study index per patient
n_arms[1]          #  Number of unique treatment arms (No duplicates across studies)
arm_index[Nind]    #  Arm index per patient



n_save_individual[1]        #  number of individuals to export more involved data from
index_save_individual[n_save_individual]  #  Patient index number for the above



n_sld_par_shared[1] = 3L                   #  ?
sld_par_shared[n_sld_par_shared] = 1:3     #  ?
n_sld_par_separate[1] = 3L                 #  ?
sld_par_separate[n_sld_par_separate] = 4:6 #  ?

arm_to_study_index[n_arms]      # Study index per Arm (map arms to studies)


n_index_per_arm[n_arms]  #  Number of subjects per arm
index_per_arm[Nind]      #  Patient index per arm (identical to 1:Nind if sorted)


## Sparse matrix representation parameters for mat_inds_obs_y (Not included)
mat_inds_obs_y[Nind, Nta_obs_y]       
n_mat_inds_obs_y[3]
w_mat_inds_obs_y 
v_mat_inds_obs_y
u_mat_inds_obs_y


## Sparse matrix representation parameters for mat_inds_cens_y
n_w_mat_inds_cens_y[Nind, Nta_cens_y]  
n_mat_inds_cens_y[3]
w_mat_inds_cens_y
v_mat_inds_cens_y 
u_mat_inds_cens_y

## Sparse matrix representation parameters for mat_inds_all_y
mat_inds_all_y[Nind, Nta_obs_y]       
n_mat_inds_all_y[3]
w_mat_inds_all_y 
v_mat_inds_all_y
u_mat_inds_all_y

```


