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


#' biom.df<-readRDS("./design/examples/mCRCpub/data/PRIME/PRIMETGIads.rds")
#' summary(biom.df)

#' event.df<-readRDS("./design/examples/mCRCpub/data/PRIME/PRIMEOSads.rds")
#' summary(event.df)


#' ===============================================
#' Model SLD
#' ===============================================

tgi.dat<-DataJoint(
  subject = DataSubject(data=event.df, subject="SUBJID", arm="ATRT", study="STUDY"),
  longi   = DataLongitudinal(data=biom.df, threshold=3, formula= BIOMVAL~BIOMYR)
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
print(tgi.out, max_rows=500, digits=5)

#' Display profiles OBS vs IPRED for 10 random individuals
selected_subjects<-head(event.df$SUBJID, 10)
longquant_obs<-LongitudinalQuantities(tgi.samples, grid=GridObserved(subjects=selected_subjects))
autoplot(longquant_obs)


#' ===============================================
#' Model OS
#' ===============================================

surv.dat<-DataJoint(
  subject  = DataSubject(data=event.df, subject="SUBJID", arm="ATRT", study="STUDY"),
  survival = DataSurvival(data=event.df, formula=Surv(EVENTYR, EVENTFL)~ATRT)
)

surv.in<-JointModel(survival=SurvivalWeibullPH())

surv.samples<-sampleStanModel(surv.in, data=surv.dat,
                              iter_sampling = 1000,
                              iter_warmup = 2000,
                              chains = 3, parallel_chains = 3)
surv.out<-as.CmdStanMCMC(surv.samples)
print(surv.out, max_rows=500, digits=5)

#' Display PRED vs OBS surv curves
expected.surv<-SurvivalQuantities(surv.samples, type="surv",
  grid=GridGrouped(times=seq(from=0, to=4, by=0.1),
  groups=split(event.df$SUBJID, event.df$ATRT)))

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
  subject  = DataSubject(data=event.df, subject="SUBJID", arm="ATRT", study="STUDY"),
  longitudinal = DataLongitudinal(data=biom.df, threshold=3, formula= BIOMVAL~BIOMYR),
  survival = DataSurvival(data=event.df, formula=Surv(EVENTYR, EVENTFL)~ATRT)
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
print(jm.out, max_rows=500, digits=5)

#' Display PRED vs OBS surv curves
expected.surv<-SurvivalQuantities(jm.samples, type="surv",
       grid=GridGrouped(times=seq(from=0, to=4, by=0.1),
       groups=split(event.df$SUBJID, event.df$ATRT)))
autoplot(expected.surv, add_km=T, add_wrap=F)+
    scale_fill_manual(values=mycols)+
    scale_colour_manual(values=mycols)+
    theme_minimal()+
    theme(panel.grid.minor=element_blank())

