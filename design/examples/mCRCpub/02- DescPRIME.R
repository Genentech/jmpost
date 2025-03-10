#' ####################################################
#'
#'   Descriptive plots
#'   PRIME study (from PDS)
#'
#'   Q3-2024
#'   Francois Mercier
#'
#' ####################################################


#' sessionInfo()

#' ===============================================
#' Visualize SLD
#' ===============================================

#' biom.df<-readRDS("./design/examples/mCRCpub/data/PRIME/PRIMETGIads.rds")
#' summary(biom.df)

#' Display SLD spaghetti
ybreaks<-c(3, 30, 100, 300)
mycols<-c(rev(ghibli::ghibli_palettes$YesterdayMedium)[c(2,4)])
g0<-ggplot(data=biom.df, aes(x=BIOMYR, y=BIOMVAL))+
    geom_point(colour="grey33", alpha=0.3, size=0.9)+
    geom_line(aes(group=SUBJID, colour=as.factor(ATRT)), alpha=0.6)+
    facet_wrap(~ATRT)+
    scale_x_continuous("Year", breaks=0.5*(0:5))+
    scale_y_continuous("SLD (mm)", breaks=ybreaks)+
    scale_colour_manual(values=mycols, guide="none")+
    theme_minimal()+
    theme(panel.grid.minor=element_blank())
g0

#' ===============================================
#' Visualize OS
#' ===============================================

#' event.df<-readRDS("./design/examples/mCRCpub/data/PRIME/PRIMEOSads.rds")
#' summary(event.df)

cox<-coxph(Surv(EVENTYR, EVENTFL)~ATRT, data=event.df)
summary(cox)

os.kmest<-survfit(Surv(EVENTYR, EVENTFL)~ATRT, data=event.df)

#' Display OS KM
g1<-survminer::ggsurvplot(os.kmest,
  data=event.df, risk.table=T, break.x.by=.5, legend.title="",
  xlab="Year", ylab="Overall survival", palette = mycols,
  risk.table.fontsize=4, legend=c(0.8, 0.8))
g1




