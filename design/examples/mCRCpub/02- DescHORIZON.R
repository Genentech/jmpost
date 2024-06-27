#' ####################################################
#'
#'   Descriptive plots
#'   HORIZONIII study (from PDS)
#'
#'   Q3-2024
#'   Francois Mercier
#'
#' ####################################################


#' sessionInfo()

#' ===============================================
#' Visualize SLD
#' ===============================================

#' biom.df<-readRDS("./design/examples/mCRCpub/data/HORIZONIII/HorizTGIads.rds")
#' summary(biom.df)

ybreaks<-c(3, 30, 100, 300)
g0<-ggplot(biom.df, aes(x=BIOMYR, y=BIOMVAL))+
  geom_line(aes(group=SUBJID), colour="grey", alpha=0.2)+
  geom_point(colour="orange4", alpha=0.6, size=0.9)+
  scale_x_continuous("Year", breaks=0.5*(0:5))+
  scale_y_continuous("SLD (mm)", breaks=ybreaks)+
  theme_minimal()+
  theme(panel.grid.minor=element_blank())
g0

set.seed(130)
retainIDsub<-sample(retainID, size=60)

g0sub<-biom.df |> filter(SUBJID %in% retainIDsub) |>
  ggplot(aes(x=BIOMYR, y=BIOMVAL))+
  geom_line(aes(group=SUBJID), colour="grey", alpha=0.2)+
  geom_point(colour="orange4", alpha=0.6, size=0.9)+
  facet_wrap(~as.factor(SUBJID))+
  scale_x_continuous("Year", breaks=0.5*(0:5))+
  scale_y_continuous("SLD (mm)", breaks=ybreaks)+
  theme_minimal()+
  theme(panel.grid.minor=element_blank())
g0sub


#' ===============================================
#' Visualize OS
#' ===============================================

#' event.df<-readRDS("./design/examples/mCRCpub/data/HORIZONIII/HorizOSads.rds")
#' summary(event.df)

cox<-coxph(Surv(EVENTYR, EVENTFL)~1, data=event.df)
summary(cox)

os.kmest<-survfit(Surv(EVENTYR, EVENTFL)~1, data=event.df)
mycols<-c(rev(ghibli::ghibli_palettes$YesterdayMedium)[c(2,4)])
g1<-survminer::ggsurvplot(os.kmest, data=event.df,
   risk.table=T, break.x.by=0.5, legend.title="",
   xlab="Time (year)", ylab="Overall survival",
   risk.table.fontsize=4, legend=c(0.8, 0.8), palette = mycols)
g1



