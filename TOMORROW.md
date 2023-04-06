

Fix breakage in random slope model
    Model is currently not converging to correct values with all parameters -> 0
    except intercept and sigma. 
        - Appears to be an issue with prior_cauchy() on the sigma parameter


Look into random effects terms
    Double check how / if we need to specify priors on them
    Double check how this is implemented in GSF
    Double check how we specify init values for them


