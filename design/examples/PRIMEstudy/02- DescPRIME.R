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

ads<-readRDS("./vignettes/PRIMEstudy/data/PRIMEads.rds")

#' ===============================================
#' Visualize SLD
#' ===============================================

#' Display SLD spaghetti
ybreaks<-c(3, 30, 100, 300)
g0<-ads |>
  filter(!is.na(ATRT)) |>
  ggplot(aes(x=VISITYR, y=LSSLD))+
  geom_line(aes(group=SUBJID), colour="wheat4", alpha=0.2)+
  geom_point(colour="grey33", alpha=0.3, size=0.9)+
  facet_wrap(~ATRT)+
  scale_x_continuous("Year", breaks=0.5*(0:5))+
  scale_y_continuous("SLD (mm)", breaks=ybreaks)+
  theme_minimal()+
  theme(panel.grid.minor=element_blank())
g0

#' ===============================================
#' Visualize OS
#' ===============================================

osdf<-ads |>
  group_by(SUBJID) |> slice(1) |> ungroup()
cox<-coxph(Surv(DTHYR, DTH)~ATRT, data=osdf)
summary(cox)

os.kmest<-survfit(Surv(DTHYR, DTH)~ATRT, data=adsl0)
#' Display OS KM
g1<-survminer::ggsurvplot(os.kmest,
                          data=adsl0,
                          risk.table=T,
                          break.x.by=.5, legend.title="",
                          xlab="Year", ylab="Overall survival",
                          risk.table.fontsize=4, legend=c(0.8, 0.8))
g1




