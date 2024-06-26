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

ads<-readRDS("./design/examples/PRIMEstudy/data/PRIMEads.rds")

#' ===============================================
#' Model SLD
#' ===============================================

adsuni<-ads[!duplicated(ads$SUBJID), ]

tgi.dat<-DataJoint(
  subject = DataSubject(data=adsuni, subject="SUBJID", arm="ATRT", study="STUDY"),
  longi   = DataLongitudinal(data=ads, threshold=5, formula= LSSLD~VISITYR)
  )

tgi.in<-JointModel(longitudinal=LongitudinalSteinFojo(
    mu_bsld = prior_lognormal(log(70), .1),
    mu_ks = prior_lognormal(log(1.8), .1),
    mu_kg = prior_lognormal(log(0.15), .1),
    omega_bsld = prior_lognormal(log(0.1), .1),
    omega_ks = prior_lognormal(log(0.1), .5),
    omega_kg = prior_lognormal(log(0.1), .5),
    sigma = prior_lognormal(log(0.18), .5),
))

tgi.samples<-sampleStanModel(tgi.in, data=tgi.dat,
                             iter_sampling = 1000,
                             iter_warmup = 2000,
                             chains = 3, parallel_chains = 3)
tgi.out<-as.CmdStanMCMC(tgi.samples)
print(tgi.out)

#' saveRDS(tgi.out, file="./design/examples/PRIMEstudy/PRIMEtgiout.rds")

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

surv.samples<-sampleStanModel(surv.in, data=surv.dat,
                              iter_sampling = 1000,
                              iter_warmup = 2000,
                              chains = 3, parallel_chains = 3)
surv.out<-as.CmdStanMCMC(surv.samples)
print(surv.out)


expected.surv<-SurvivalQuantities(surv.samples, type="surv",
  grid=GridGrouped(times=seq(from=0, to=5, by=0.1),
                   groups=split(adsuni$SUBJID, adsuni$ATRT)))

mycols<-c(rev(ghibli::ghibli_palettes$YesterdayMedium)[c(2,4)])
g3<-autoplot(expected.surv, add_km=T, add_wrap=F)+
    scale_fill_manual(values=mycols)+
    scale_colour_manual(values=mycols)+
    theme_minimal()+
    theme(panel.grid.minor=element_blank())
g3


#' ===============================================
#' Model JM
#' ===============================================

jm.dat<-DataJoint(
  subject  = DataSubject(data=adsuni, subject="SUBJID", arm="ATRT", study="STUDY"),
  longitudinal = DataLongitudinal(data=ads, threshold=5, formula= LSSLD~VISITYR),
  survival = DataSurvival(data=adsuni, formula=Surv(DTHYR, DTH)~ATRT)
)

jm.in<-JointModel(
  longitudinal=LongitudinalSteinFojo(
    mu_bsld = prior_lognormal(log(70), .1),
    mu_ks = prior_lognormal(log(1.8), .1),
    mu_kg = prior_lognormal(log(0.15), .1),
    omega_bsld = prior_lognormal(log(0.1), .1),
    omega_ks = prior_lognormal(log(0.1), .5),
    omega_kg = prior_lognormal(log(0.1), .5),
    sigma = prior_lognormal(log(0.18), .5),
  ),
  survival = SurvivalWeibullPH(),
  link = Link(linkTTG(prior_normal(0.01, 3)))
)

jm.samples<-sampleStanModel(jm.in,
  data = jm.dat,
  iter_sampling = 1000,
  iter_warmup = 2000,
  chains = 3, parallel_chains = 3)

jm.out<-as.CmdStanMCMC(jm.samples)
#' print(jm.out)

#' saveRDS(jm.out, file="./design/examples/PRIMEjm.out.rds")


expected.surv<-SurvivalQuantities(jm.samples, type="surv",
                                  grid=GridGrouped(times=seq(from=0, to=5, by=0.1),
                                                   groups=split(adsuni$SUBJID, adsuni$ATRT)))
#' Compiling Stan program...
#' Warning: Chain 1 finished unexpectedly!
#'
#'  Error: Generating quantities for all MCMC chains failed. Unable to retrieve the generated quantities.

autoplot(expected.surv, add_km=T, add_wrap=F)+theme_minimal()

