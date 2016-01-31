# Load packages
library(fishWiDNR)   # for read.FMDB()
library(FSA)         # for headtail(), filterD(), Summarize(), hist(), lencat(), psdVal(), psdAdd()
library(dplyr)       # for %>%, select(), mutate(), group_by(), summarize()
library(magrittr)    # for %<>%

# Load and examine the Sawyer County FMDB data
# User must set working directory appropriate to where the CSV file
#   is saved on their computer ... below if for Derek's computer.
setwd("C:/aaaWork/Web/GitHub/RcourseWiDNR2016")
d <- read.FMDB("SAWYER_fish_raw_data_012915.csv",expandCounts=TRUE)
str(d)
headtail(d,n=2)

d <- select(d,Species1,Waterbody.Name,Gear,Survey.Year,Mon,Len) %>%
  mutate(LCat=psdAdd(Len,Species1,units="in",verbose=FALSE))

xtabs(~Survey.Year+Mon,data=d)
fall <- d %>% filterD(Survey.Year==2010,Mon %in% c("Aug","Sep","Oct"))

xtabs(~Waterbody.Name,data=fall)
xtabs(~Species1,data=fall)

## Three subsets
brt <- filterD(d,Species1=="Brown Trout")
nam <- filterD(d,Waterbody.Name=="NAMEKAGON RIVER")
nambrt <- filterD(d,Species1=="Brown Trout",Waterbody.Name=="NAMEKAGON RIVER")

## Summaries of Browth Trout in Namakagon River
Summarize(~Len,data=nambrt,digits=2)
hist(~Len,data=nambrt,breaks=1:23)

tmp <- psdVal("Brown Trout (lotic)",units="in",addLens=10)
nambrt %<>% mutate(LCat2=lencat(Len,breaks=tmp,use.names=TRUE,drop.levels=TRUE))
( rcum <- rcumsum(xtabs(~LCat2,data=nambrt))[-1] )
rcum/rcum["stock"]*100

## Brown Trout length summaries by waterbody (for fall samples)
brt %<>% mutate(LCat2=lencat(Len,breaks=tmp,use.names=TRUE,drop.levels=TRUE))
brt %>% group_by(Waterbody.Name) %>%
  summarize(n=n(),valid_n=sum(!is.na(Len)),
            meanLen=mean(Len,na.rm=TRUE),sdLen=sd(Len,na.rm=TRUE),
            minLen=min(Len,na.rm=TRUE),Q1Len=quantile(Len,0.25,na.rm=TRUE),
            mdnLen=quantile(Len,0.5,na.rm=TRUE),Q3Len=quantile(Len,0.75,na.rm=TRUE),
            maxLen=max(Len,na.rm=TRUE)) %>%
  as.data.frame()

( rcum <- t(apply(xtabs(~Waterbody.Name+LCat2,data=brt),FUN=rcumsum,MARGIN=1))[,-1] )
round(rcum/rcum[,"stock"]*100,1)

## All species in Namekagon (for fall samples)
nam %<>% filterD(!is.na(Len))
nam %>% group_by(Species1) %>%
  summarize(n=n(),valid_n=sum(!is.na(Len)),
            meanLen=mean(Len,na.rm=TRUE),sdLen=sd(Len,na.rm=TRUE),
            minLen=min(Len,na.rm=TRUE),Q1Len=quantile(Len,0.25,na.rm=TRUE),
            mdnLen=quantile(Len,0.5,na.rm=TRUE),Q3Len=quantile(Len,0.75,na.rm=TRUE),
            maxLen=max(Len,na.rm=TRUE)) %>%
  as.data.frame()

( rcum <- t(apply(xtabs(~Species1+LCat,data=nam),FUN=rcumsum,MARGIN=1))[,-1] )
round(rcum/rcum[,"stock"]*100,1)

## Note the problem with Brown Trout ... psdAdd expect Brown Trout (lotic)
