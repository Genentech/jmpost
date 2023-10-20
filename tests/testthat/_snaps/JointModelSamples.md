# smoke test for JointModelSamples

    Code
      devnull <- capture.output({
        suppressMessages({
          mpp <- sampleStanModel(jm, data = jdat, iter_sampling = 100, iter_warmup = 150,
            chains = 1, refresh = 0, parallel_chains = 1)
        })
      })
      print(mpp)
    Output
      
         JointModelSamples Object with:
        
            # of samples per chain = 100
            # of chains            = 1
        
            Variables:
                Ypred[2400]
                beta_os_cov[3]
                lm_rs_ind_rnd_slope[400]
                lm_rs_intercept
                lm_rs_rslope_ind[2400]
                lm_rs_sigma
                lm_rs_slope_mu[2]
                lm_rs_slope_sigma
                log_lik[400]
                log_surv_fit_at_obs_times[400]
                lp__
                os_cov_contribution[400]
                pars_os
                sm_exp_lambda 
      

