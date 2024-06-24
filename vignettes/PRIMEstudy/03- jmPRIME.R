#' ####################################################
#'
#'   Joint models
#'   PRIME study (from PDS)
#'
#'   Q3-2024
#'   Francois Mercier
#'
#' ####################################################


#' sessionInfo()

ads<-readRDS("./vignettes/PRIMEstudy/data/PRIMEads.rds")

#' ===============================================
#' Model SLD
#' ===============================================

adsuni<-ads[!duplicated(ads$SUBJID), ]

tgi.dat<-DataJoint(
  subject = DataSubject(data=adsuni, subject="SUBJID", arm="ATRT", study="STUDY"),
  longi   = DataLongitudinal(data=ads, threshold=5, formula= LSSLD~VISITYR)
  )

tgi.in<-JointModel(longitudinal=LongitudinalSteinFojo())

tgi.samples<-sampleStanModel(tgi.in, data=tgi.dat, iter_warmup=2000,
  iter_sampling=1000, chains=2, refresh=500)
tgi.out<-as.CmdStanMCMC(tgi.samples)
print(tgi.out)

#' saveRDS(tgi.out, file="../PRIME/PRIMEtgiout.rds")

#' Problems here:
#' iPred vs. Obs
#' see https://github.com/Genentech/jmpost/blob/fe04d4aa049f5244f7e6e76f97901ef3c7536146/design/examples/quantity_plots.R#L139
longquant_obs<-LongitudinalQuantities(tgi.samples, grid = GridObserved())


#' ===============================================
#' Model OS
#' ===============================================

adsuni<-ads[!duplicated(ads$SUBJID), ]

surv.dat<-DataJoint(
  subject  = DataSubject(data=adsuni, subject="SUBJID", arm="ATRT", study="STUDY"),
  survival = DataSurvival(data=adsuni, formula=Surv(DTHYR, DTH)~ATRT)
)

surv.in<-JointModel(survival=SurvivalWeibullPH())

surv.samples<-sampleStanModel(surv.in, data=surv.dat, iter_warmup=2000,
                             iter_sampling=1000, chains=2, refresh=500)
surv.out<-as.CmdStanMCMC(surv.samples)
print(surv.out)


expected.surv<-SurvivalQuantities(surv.samples, type="surv",
  grid=GridGrouped(times=seq(from=0, to=5, by=0.1),
                   groups=split(adsuni$SUBJID, adsuni$ATRT)))
autoplot(expected.surv, add_km=T, add_wrap=F)+theme_minimal()


#' ===============================================
#' Model JM
#' ===============================================

jm.dat<-DataJoint(
  subject  = DataSubject(data=adsuni, subject="SUBJID", arm="ATRT", study="STUDY"),
  longitudinal = DataLongitudinal(data=ads, threshold=5, formula= LSSLD~VISITYR),
  survival = DataSurvival(data=adsuni, formula=Surv(DTHYR, DTH)~ATRT)
)

jm.in<-JointModel(
  longitudinal=LongitudinalGSF(
    mu_bsld = prior_lognormal(log(60), 0.5),
    mu_ks = prior_lognormal(log(0.55), 0.5),
    mu_kg = prior_lognormal(log(0.15), 0.5),
    a_phi = prior_beta(5, 20),

    omega_bsld = prior_lognormal(log(0.1), 0.5),
    omega_ks = prior_lognormal(log(0.1), 0.5),
    omega_kg = prior_lognormal(log(0.1), 0.5),
    b_phi = prior_lognormal(log(0.1), 0.5),

    sigma = prior_lognormal(log(0.18), 0.5)
  ),
  survival = SurvivalWeibullPH(),
  link = Link(linkIdentity(prior_normal(0.002, 0.2)))
)

jm.samples<-sampleStanModel(jm.in,
  data = jm.dat,
  iter_sampling = 500,
  iter_warmup = 1000,
  chains = 1,
  parallel_chains = 1
)

jm.out<-as.CmdStanMCMC(jm.samples)
#' print(jm.out)

#' saveRDS(jm.out, file="../PRIME/PRIMEjm.out.rds")


expected.surv<-SurvivalQuantities(jm.samples, type="surv",
                                  grid=GridGrouped(times=seq(from=0, to=5, by=0.1),
                                                   groups=split(adsuni$SUBJID, adsuni$ATRT)))
#' Compiling Stan program...
#' Warning: Chain 1 finished unexpectedly!
#'
#'  Error: Generating quantities for all MCMC chains failed. Unable to retrieve the generated quantities.

autoplot(expected.surv, add_km=T, add_wrap=F)+theme_minimal()

