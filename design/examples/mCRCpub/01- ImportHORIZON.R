#' ####################################################
#'   
#'   Data preparation
#'   HORIZONIII study (from PDS)
#'   
#'   Q3-2024
#'   Francois Mercier 
#'   
#' ####################################################


#' sessionInfo()

#' ===============================================
#' Import and Select
#' ===============================================================

#' UID values
#' ---------------------------------
subj<-haven::read_sas("data/HORIZONIII/rdpsubj.sas7bdat")
rcist<-haven::read_sas("data/HORIZONIII/rdprcist.sas7bdat") 

#' Only keep patients in PerProtocol set (PP_SET==1)
#' and exposed to BEV for at least 3 weeks (i.e. at least 1st TA visit)
subj0<-subj |> 
  filter(PP_SET==1, !is.na(BEV_SDY), !is.na(BEV_EDY), BEV_EDY>3*7) |>
  mutate(SUBJID=as.character(RANDCODE)) |>
  select(SUBJID, LDH1_5)

#' OS values
#' ---------------------------------
event.df0<-rcist |> 
  mutate(SUBJID=as.character(RANDCODE)) |>
  filter(SUBJID %in% subj0$SUBJID) |>
  group_by(SUBJID) %>% slice(1) %>% ungroup() |>
  mutate(EVENTYR=OSTIM/365.25, EVENTFL=ifelse(DEATFLAG==1, 0, 1),
         ATRT="FOLFOX alone", STUDY="2") |>
  select(SUBJID, ATRT, STUDY, EVENTYR, EVENTFL)
#' length(event.df0$SUBJID)
#' 645

#' SLD values
#' ---------------------------------
#' - remove rows where SLD is NA
#' - remove rows where patients have PBLCNT=NA i.e. remove patient with at least one post-baseline TA
biom.df0<-rcist |> 
  mutate(SUBJID=as.character(RANDCODE)) |>
  filter(SUBJID %in% subj0$SUBJID) |>
  drop_na(STLDI) |>
  filter(!is.na(PBLCNT)) |>
  mutate(VALUE=ifelse(STLDI==0, 2.5, STLDI*10), STUDY="2",
         VISITYR=ORDYTRT/365.25, ATRT="FOLFOX alone") |>
  select(SUBJID, STUDY, ATRT, VALUE, VISITYR)
#' length(unique(biom.df0$SUBJID))
#' 640


retainID<-intersect(unique(biom.df0$SUBJID), event.df0$SUBJID)
#' 640

#' ===============================================
#' Build analysis dataset
#' ===============================================================

subj.df<-subj0 |> filter(SUBJID %in% retainID)

biom.df<-biom.df0 |> filter(SUBJID %in% retainID)
#' length(unique(biom.df$SUBJID))

event.df<-event.df0 |> filter(SUBJID %in% retainID)

#' saveRDS(event.df, file="data/HORIZONIII/HorizOSads.rds")
#' saveRDS(biom.df, file="data/HORIZONIII/HorizTGIads.rds")



