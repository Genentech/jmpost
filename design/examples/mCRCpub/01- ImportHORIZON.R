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

#' ----------------------------------
#' Structure analysis data set:
#' ----------------------------------
#' For time to event sub-model (event.df):
#' ***************************
#' SUBJID (chr)
#' STUDY (chr)
#' ATRT (chr)
#'
#' EVENTYR (num) - Time to event (in years)
#' EVENTFL (0/1) - Flag =1 if the ind died, 0 if censored
#'
#' For longi (TGI) sub-model (biom.df):
#' ***************************
#' SUBJID (chr)
#' STUDY (chr)
#' ATRT (chr)
#'
#' BIOMVAL (num) - Biomarker (here, SLD in mm) value
#' BIOMYR (num) - Biomarker measurement time (in years)
#' ----------------------------------


#' ===============================================
#' Import and Select
#' ===============================================================

#' Event data frame
#' -----------------
subj<-haven::read_sas("./design/examples/mCRCpub/data/HORIZONIII/rdpsubj.sas7bdat")
rcist<-haven::read_sas("./design/examples/mCRCpub/data/HORIZONIII/rdprcist.sas7bdat")

#' Only keep patients in PerProtocol set (PP_SET==1)
#' and exposed to BEV for at least 3 weeks (i.e. at least 1st TA visit)
subj0<-subj |>
  filter(PP_SET==1, !is.na(BEV_SDY), !is.na(BEV_EDY), BEV_EDY>3*7) |>
  mutate(SUBJID=as.character(RANDCODE)) |>
  select(SUBJID, LDH1_5)
#' length(unique(subj0$SUBJID))
#' 645

event.df0<-rcist |>
  mutate(SUBJID=as.character(RANDCODE)) |>
  filter(SUBJID %in% subj0$SUBJID) |>
  group_by(SUBJID) %>% slice(1) %>% ungroup() |>
  mutate(EVENTYR=OSTIM/365.25, EVENTFL=ifelse(DEATFLAG==1, 0, 1),
         ATRT="FOLFOX alone", STUDY="2") |>
  select(SUBJID, STUDY, ATRT, EVENTYR, EVENTFL)

UID.event0<-unique(event.df0$SUBJID)
#' length(UID.event0)
#' 645

#' Biomarker (SLD) data frame
#' -----------------
#' - remove rows where SLD is NA
#' - remove rows where patients have PBLCNT=NA i.e. remove patient with at least one post-baseline TA
biom.df0<-rcist |>
  filter(!is.na(PBLCNT), !is.na(STLDI)) |>
  mutate(SUBJID=as.character(RANDCODE), STUDY="2", ATRT="FOLFOX alone",
         BIOMVAL=ifelse(STLDI==0, 2.5, STLDI*10),
         BIOMYR=ORDYTRT/365.25) |>
  select(SUBJID, STUDY, ATRT, BIOMVAL, BIOMYR)

UID.biom0<-unique(biom.df0$SUBJID)
#' length(UID.biom0)
#' 660

#' Retain matching patients
#' -----------------

retainID<-intersect(UID.event0, UID.biom0)
#' 640

event.df<-event.df0 |> filter(SUBJID %in% retainID)
#' length(unique(event.df$SUBJID))
#' 640
#' saveRDS(event.df, file="./design/examples/mCRCpub/data/HORIZONIII/HorizOSads.rds")


biom.df<-biom.df0 |> filter(SUBJID %in% retainID)
#' length(unique(biom.df$SUBJID))
#' 640
#' saveRDS(biom.df, file="./design/examples/mCRCpub/data/HORIZONIII/HorizTGIads.rds")




