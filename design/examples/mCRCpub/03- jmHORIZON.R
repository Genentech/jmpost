#' ####################################################
#'   
#'   Joint models
#'   HORIZONIII study (from PDS)
#'   
#'   Q3-2024
#'   Francois Mercier 
#'   
#' ####################################################


#' sessionInfo()

os1<-readRDS("data/HORIZONIII/HorizOSads.rds")
tk1<-readRDS("data/HORIZONIII/HorizTGIads.rds")

#' ===============================================
#' Model SLD
#' ===============================================

ads<-biom.df |> filter(SUBJID %in% retainIDsub)
adsuni1<-ads[!duplicated(ads$SUBJID), ]

tgi.dat<-DataJoint(
  subject = DataSubject(data=adsuni1, subject="SUBJID", arm="ATRT", study="STUDY"),
  longi   = DataLongitudinal(data=ads, threshold=5, formula= VALUE~VISITYR)
  )

tgi.in<-JointModel(longitudinal=LongitudinalSteinFojo(
  mu_bsld=prior_lognormal(log(70), .2),
  mu_ks=prior_lognormal(log(0.01), .3),
  mu_kg=prior_lognormal(log(0.01), .3),
  omega_bsld=prior_lognormal(log(0.5), 0.3),
  omega_ks=prior_lognormal(log(0.7), 0.3),
  omega_kg=prior_lognormal(log(0.7), 0.3),
  sigma=prior_lognormal(log(0.2), 0.1)))

tgi.samples<-sampleStanModel(tgi.in, data=tgi.dat, iter_warmup=2000,
  iter_sampling=1000, chains=3, parallel=3, refresh=500)
tgi.out<-as.CmdStanMCMC(tgi.samples)
print(tgi.out, max_rows=500, digits=5)

selected_subjects<-sample(adsuni1$SUBJID, 10)
longquant_obs<-LongitudinalQuantities(tgi.samples, grid=GridObserved(subjects=selected_subjects))
autoplot(longquant_obs)
summary(longquant_obs)

#' mu_BSLD: hist(rlnorm(1000, log(70), .2))
#' mu_ks: hist(rlnorm(1000, log(0.01), .3))
#' om_BSLD: hist(rlnorm(1000, log(0.5), 0.3))
#' om_ks: hist(rlnorm(1000, log(0.7), 0.3))
#' om_sigma: hist(rlnorm(1000, log(0.2), 0.1))

#' ===============================================
#' Model OS
#' ===============================================

surv.dat<-DataJoint(
  subject  = DataSubject(data=os1, subject="SUBJID", arm="ATRT", study="STUDY"),
  survival = DataSurvival(data=os1, formula=Surv(EVENTYR, EVENTFL)~1)
)

surv.in<-JointModel(survival=SurvivalWeibullPH())

surv.samples<-sampleStanModel(surv.in, data=surv.dat, iter_warmup=2000,
    iter_sampling=1000, chains=3, parallel=3, refresh=500)
surv.out<-as.CmdStanMCMC(surv.samples)
print(surv.out, max_rows=500, digits=5)

expected.surv<-SurvivalQuantities(surv.samples, type="surv",
  grid=GridGrouped(times=seq(from=0, to=3, by=0.1), 
                   groups=split(os1$SUBJID, os1$ATRT)))
autoplot(expected.surv, add_km=T, add_wrap=F)+theme_minimal()


#' ===============================================
#' Model JM
#' ===============================================

jm.dat<-DataJoint(
  subject  = DataSubject(data=os1, subject="SUBJID", arm="ATRT", study="STUDY"),
  longitudinal = DataLongitudinal(data=tk1, threshold=2, formula= VALUE~VISITYR),
  survival = DataSurvival(data=os1, formula=Surv(EVENTYR, EVENTFL)~1)
)

jm.in<-JointModel(
  longitudinal=LongitudinalSteinFojo(
    mu_bsld=prior_lognormal(log(70), .2),
    mu_ks=prior_lognormal(log(0.01), .3),
    mu_kg=prior_lognormal(log(0.01), .3),
    omega_bsld=prior_lognormal(log(0.5), 0.3),
    omega_ks=prior_lognormal(log(0.7), 0.3),
    omega_kg=prior_lognormal(log(0.7), 0.3),
    sigma=prior_lognormal(log(0.2), 0.1)),
  survival=SurvivalWeibullPH(),
  link=Link(linkTTG(prior_normal(0.01, 3)))
)

jm.samples<-sampleStanModel(jm.in, data=jm.dat,
    iter_sampling=1000, iter_warmup=2000, chains=3, parallel_chains=3)

jm.out<-as.CmdStanMCMC(jm.samples)
#' print(jm.out, max_rows=500, digits=5)

expected.surv<-SurvivalQuantities(jm.samples, type="surv",
                                  grid=GridGrouped(times=seq(from=0, to=3, by=0.1),
                                                   groups=split(os1$SUBJID, os1$ATRT)))
autoplot(expected.surv, add_km=T, add_wrap=F)+theme_minimal()

