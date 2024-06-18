#' #############################################################################
#'
#' For a time to event model of ‘pb’ data from the {flexsurv} package,
#' assuming an exponential baseline hazard, the goal here is to
#' illustrate the way to assess:
#' (i) Convergence
#' (ii) GoF
#'
#' Solutions proposed by {survstan} will be used as benchmark
#'
#' Initiated on: 2024-02-14
#' Author: F. Mercier
#'
#' #############################################################################



#' ===========================================================
#'
#' INSTALL AND LOAD NECESSARY LIBRARIES
#'
#' ===========================================================

library(tidyverse)

library(flexsurv)
library(cmdstanr)

#' install.packages("tidybayes")
library(posterior)
library(bayesplot)
library(tidybayes)
#' install.packages("bayestestR")
library(distributional)
library(ggdist)
library(bayestestR)
#' install.packages("hexbin")
library(hexbin)
library(loo)

library(jmpost)


#' install.packages("simsurv")
#' install.packages("ggsurvfit")
library(simsurv)
library(ggsurvfit)

#' ===========================================================
#'
#' Data (from the flexsurv package)
#'
#' ===========================================================

head(bc, 2)
glimpse(bc)
rbind(head(bc), tail(bc))

#' Kaplan-Meier plot and Risk table
p001 <- ggsurvfit::survfit2(Surv(recyrs, censrec) ~ group, data = bc) |>
    ggsurvfit(linewidth = 1) +
    add_confidence_interval() +
    add_risktable() +
    add_quantile(y_value = 0.5, color = "gray50", linewidth = 0.75) +
    scale_ggsurvfit()
p001

#' Checking independent censoring
p002 <- ggsurvfit::survfit2(Surv(recyrs, 1-censrec) ~ group, data = bc) |>
    ggsurvfit(linewidth = 1) +
    add_confidence_interval() +
    scale_ggsurvfit()
p002

#' Kernel density estimate for the hazard
futime=bc$recyrs; fustat=bc$censrec
fit1 <- muhaz::muhaz(futime, fustat, bw.method="g")
plot(fit1)
summary(fit1)


#' ===========================================================
#'
#' jmpost exponential PH implementation
#'
#' ===========================================================


jmpost.survonly.exp<-JointModel(survival=SurvivalExponential(lambda=prior_lognormal(log(0.06), 1)))

bc1<-bc %>% mutate(ID=as.character(1:n()), study=1)

jdat<-DataJoint(
  subject=DataSubject(data=bc1, subject="ID", arm="group", study="study"),
  survival=DataSurvival(data=bc1, formula=Surv(recyrs, censrec)~group)
)

mp<-sampleStanModel(jmpost.survonly.exp, data=jdat, iter_warmup=4000,
                    iter_sampling=1000, chains=4, refresh=0)

mp@results
##
vars<-c("sm_exp_lambda", "beta_os_cov")
# mp@results$summary(vars)


#' ===========================================================
#'
#' I. POSTERIOR DISTRIBUTION SUMMARY
#'
#' ===========================================================


#' Convert the samples in a df
#' ------------------------------------------------------------\
my_fitall_df <- posterior::as_draws_df(mp@results)
my_pars<-c("sm_exp_lambda", "beta_os_cov[1]", "beta_os_cov[2]")
my_fitpop_df<-posterior::subset_draws(my_fitall_df, variable=my_pars)

#' Summarize the posterior samples
#' ------------------------------------------------------------\
#' Using {posterior}
posterior::summarise_draws(my_fitpop_df)


#' Quantiles or Highest density interval
#' ------------------------------------------------------------\
#' For each parameter, it is possible to extract various quantities:
#' mean_hdci(), median_hdci(), etc ... from [median|mean|mode]_[qi|hdi]
#' see: ggdist:: ?mean_qi
my_fitpop_df %>%
  tidybayes::spread_rvars(sm_exp_lambda) %>%
    ggdist::mean_hdci()
my_fitpop_df %>%
  tidybayes::spread_rvars(sm_exp_lambda) %>%
    ggdist::mean_qi()

#' Median (and associated uncertainty expressed as lower and upper bounds for
#' pmf containing 50%, 89%, 90% of the distribution density
#' = e.g. 89% credibility interval for the median)
#' for sm_exp_lambda across all chains
my_fitpop_df %>%
  tidybayes::spread_rvars(sm_exp_lambda) %>%
  tidybayes::median_qi(.width = c(.50, .89, .95))

#' Q1, median, Q3 of sm_exp_lambda across all chains
lambda_samples<-my_fitpop_df %>%
  tidybayes::spread_draws(sm_exp_lambda)
lambda_samples$sm_exp_lambda %>% quantile(., probs=c(0.025, 0.5, 0.975))


#' Posterior density
#' using tidybayes see https://mjskay.github.io/tidybayes/articles/tidy-rstanarm.html
#' ------------------------------------------------------------\

my_fitpop_df %>%
  tidybayes::spread_draws(sm_exp_lambda) %>%
  tidybayes::median_qi(., .width = c(.95, .66)) %>%
  ggplot(aes(y = NA, x = sm_exp_lambda, xmin = .lower, xmax = .upper)) +
  scale_x_continuous("Lambda_0")+
  scale_y_discrete("")+
  tidybayes::geom_pointinterval()+
  labs(title="sm_exp_lambda posterior distributions",
       caption="OS analysis / mercief3 / 2024-02-20")+
  theme_minimal()

#' (Post-warmup) Posterior distributions
#' ------------------------------------------------------------\
bayesplot::mcmc_areas(my_fitpop_df, pars = my_pars, prob = 0.8)+
  theme_minimal()
bayesplot::mcmc_dens_overlay(my_fitpop_df, pars=my_pars)+
  theme_minimal()



#' ===========================================================
#'
#' II. CONVERGENCE
#'
#' ===========================================================


#' Checking that Rhat<1.05
#' ------------------------------------------------------------\
#' see https://cran.r-project.org/web/packages/bayesplot/vignettes/visual-mcmc-diagnostics.html

#' Using {bayesplot}
rhats <- bayesplot::rhat(mp@results)
print(rhats)
color_scheme_set("brightblue") # see help("color_scheme_set")
bayesplot::mcmc_rhat(rhats)+ yaxis_text(hjust = 1)


#' Checking ESS
#' ------------------------------------------------------------\
#' Using {bayesplot}
#' see https://cran.r-project.org/web/packages/bayesplot/vignettes/visual-mcmc-diagnostics.html
neff_ratios <- bayesplot::neff_ratio(mp@results)
print(neff_ratios)
color_scheme_set("brightblue") # see help("color_scheme_set")
bayesplot::mcmc_neff(neff_ratios, size = 2)+ yaxis_text(hjust = 1)


#' Checking autocorrelation
#' ------------------------------------------------------------\
#' Using {bayesplot}
#' see https://cran.r-project.org/web/packages/bayesplot/vignettes/visual-mcmc-diagnostics.html
bayesplot::mcmc_acf_bar(my_fit_df, pars="sm_weibull_ph_lambda", lags=10)+
  theme_minimal()


#' Caterpillar plot for each param
#' ------------------------------------------------------------\
bayesplot::mcmc_trace(my_fit_df,  pars = my_pars)+
  theme_minimal()



#' ===========================================================
#'
#' III. GOODNESS OF FIT
#'
#' ===========================================================

# Log Likelihood
#' ===========================================================
log_lik <- mp@results$draws("log_lik", format = "draws_matrix") |>
  apply(1, sum) |>
  mean()
log_lik

# AIC
k <- 2
-2 * log_lik + k * 4

# BIC
(4 * log(nrow(bc1))) + (-2 * log_lik)

# Leave one out CV
mp@results$loo()





#' ===========================================================
#'
#' IV. PREDICTIONS
#'
#' Overlaying KM and posterior survival curves
#' See example:
#' https://stablemarkets.netlify.app/post/post2/specifying-accelerated-failure-time-models-in-stan/
#'
#'
#' ===========================================================

# Calculate the survival distribution for each subject at each desired timepoint
# To get different quantities change the `pweibullPH` to the desired distribution
# function e.g. hweibullPH / HweibullPH

zparms<-my_fitpop_df |>
    pivot_longer(cols = 1:3, names_to = "parms", values_to = "value") |>
    group_by(parms) |>
        tidybayes::median_qi() |>
    ungroup()

jdat<-DataJoint(
    subject=DataSubject(data=bc1, subject="ID", arm="group", study="study"),
    survival=DataSurvival(data=bc1, formula=Surv(recyrs, censrec)~group)
)

set.seed(13579)
covs <- data.frame(ID = bc1$ID, trt=bc1$group)
s1 <- simsurv(dist="exponential",
              x=covs, betas=c(trt=0),
              lambdas = zsurv$lambda[1], maxt = max(bc1$recyrs))

survfit2(Surv(eventtime, status) ~ 1, data = s1) |>
    ggsurvfit(linewidth = 1) +
    add_confidence_interval() +
    add_risktable() +
    add_quantile(y_value = 0.6, color = "gray50", linewidth = 0.75) +
    scale_ggsurvfit()
