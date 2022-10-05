# merge is working as expected

    Code
      LogLogisticModule()
    Output
      An object of class "StanModule"
      Slot "functions":
      [1] "os_functions.stan"
      
      Slot "data":
      [1] "os_data.stan"
      
      Slot "parameters":
      [1] "os_parameters.stan"
      
      Slot "transformed_parameters":
      [1] "os_transformed_parameters.stan"
      
      Slot "model":
      [1] "target += sum(log_lik);"
      
      Slot "priors":
      $p
      [1] "gamma(2, 0.5);"
      
      $`1/lambda`
      [1] "lognormal(0, 5);"
      
      $beta_os_cov
      [1] "normal(0, 5);"
      
      
      Slot "generated_quantities":
      [1] "os_generated_quantities.stan"
      
      Slot "inits":
      list()
      

