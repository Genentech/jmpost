#' #############################################################################
#'
#' Fit Weibull PH model to ‘pb’ data from the {flexsurv} package,
#' using various tools/packages:
#' I. flexsurv
#' II. survstan
#' III. jmpost
#'
#' Initiated on: 2024-03-28
#' Author: F. Mercier
#'
#' #############################################################################



#' ===========================================================
#'
#' INSTALL AND LOAD NECESSARY LIBRARIES
#'
#' ===========================================================

#' General
#' ==========================
library(Rcpp)
library(cli)
library(here)
library(tidyverse)

#' Survival
#' ==========================
#' install.packages("devtools")
#' install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
#' devtools::install("~/jmpost")
library(cmdstanr)
library(flexsurv)
library(survstan)
library(jmpost)


#' ===========================================================
#'
#' DATA PREP
#'
#' ===========================================================

#' From the flexsurv package
head(bc, 2)

#' recyrs represents the time (in years) of death or cancer recurrence when
#' censrec is 1, or (right-)censoring when censrec is 0.
#' The covariate group is a factor representing a prognostic score, with 3 levels:
#' "Good" (the reference), "Medium" and "Poor".
#' In flexsur, the baseline Weibull model is implemented as:
#' S(t) = exp(-(t/mu)^alpha), with alpha=shape param, mu=scale param.
#' group is a linear effect on log(mu)


#' ===========================================================
#'
#' Weibull PH model fits
#'
#' ===========================================================

#' Flexsurv
#' ==========================
# flexsurv.exp<-flexsurvreg(Surv(recyrs, censrec)~group, data=bc, dist="exp")
# flexsurv.exp
# plot(flexsurv.exp)
# flexsurv.exp$AIC
# flexsurv.exp$res

flexsurv.weiph<-flexsurvreg(Surv(recyrs, censrec)~group, data=bc, dist="weibullph")
flexsurv.weiph

#' Survstan
#' ==========================
# survstan.exp<-survstan::phreg(Surv(recyrs, censrec)~group, data=bc, dist="exponential")
# sumsurvstan<-summary(survstan.exp)
# sumsurvstan$AIC
# sumsurvstan$coefficients
# sumsurvstan$tbl

survstan.weiph<-survstan::phreg(Surv(recyrs, censrec)~group, data=bc, dist="weibull")
summary(survstan.weiph)


#' jmpost
#' ==========================
# jmpost.survonly.exp<-JointModel(survival=SurvivalExponential(lambda=prior_lognormal(log(0.06), 1)))

# bc1<-bc %>% mutate(ID=as.character(1:n()), study=1)

# jdat<-DataJoint(
#   subject=DataSubject(data=bc1, subject="ID", arm="group", study="study"),
#   survival=DataSurvival(data=bc1, formula=Surv(recyrs, censrec)~group)
#   )

# mp<-sampleStanModel(jmpost.survonly.exp, data=jdat, iter_warmup=4000,
#                     iter_sampling=1000, chains=4, refresh=0)

# vars<-c("sm_exp_lambda", "beta_os_cov")
# mp@results$summary(vars)

jmpost.weiph<-JointModel(survival=SurvivalWeibullPH())

bc1<-bc %>% mutate(ID=as.character(1:n()), study=1)

jdat<-DataJoint(
    subject=DataSubject(data=bc1, subject="ID", arm="group", study="study"),
    survival=DataSurvival(data=bc1, formula=Surv(recyrs, censrec)~group)
)

mp<-sampleStanModel(jmpost.weiph, data=jdat, iter_warmup=4000,
                    iter_sampling=1000, chains=4, refresh=0)
mp@results

# vars<-c("sm_exp_lambda", "beta_os_cov")
# mp@results$summary(vars)



#' ===========================================================
#'
#' Pooling results together
#'
#' ===========================================================

flexsurv.prep<-as_tibble(signif(flexsurv.exp$res, 3)) |>
  rename(Estimate=est, SE=se, P025=`L95%`, P975=`U95%`) |>
  relocate(SE, .after=Estimate) |>
  mutate(meth="flexsurv", rowN=1:3, libel=c("lambda", "beta_group_Medium", "beta_group_Poor"), .before=1)

survstan.prep1<-as_tibble(signif(sumsurvstan$tbl, 3)) |>
  rename(Estimate=estimate, P025=`2.5%`, P975=`97.5%`, SE=se)
survstan.prep2<-as_tibble(signif(sumsurvstan$coefficients[,1:2], 3)) |>
  rename(SE=StdErr) |>
  mutate(P025=999, P975=999, .after=1)
survstan.prep<-rbind(survstan.prep1, survstan.prep2) |>
  mutate(meth="survstan", rowN=1:3, libel=c("lambda", "beta_group_Medium", "beta_group_Poor"), .before=1)

jmpost.prep<-as_tibble(mp@results$summary(vars)) |>
  rename(Estimate=mean, SE=sd, P025=q5, P975=q95) |>
  select(Estimate, SE, P025, P975) |>
  mutate(meth="jmpost", rowN=1:3, libel=c("lambda", "beta_group_Medium", "beta_group_Poor"), .before=1)


altogether<-rbind(flexsurv.prep, survstan.prep, jmpost.prep)









