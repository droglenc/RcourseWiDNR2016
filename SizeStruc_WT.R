# User must set working directory appropriately.
# ============================================================
# BEGIN -- Ignore this code (only for producing workshop handout)
library(knitr)
source("knitr_setup.R")
options(str=strOptions(strict.width="cut"))
rqrd <- c("knitr","fishWiDNR","FSA","dplyr","magrittr")
# End -- Ignore this code
# ============================================================

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

# ============================================================
# BEGIN DEMO code only -- not strictly needed for this analysis
# One version of select that does not use piping
d <- select(d,Species1,Waterbody.Name,Gear,Survey.Year,Mon,Len)

# A version that uses piping, but assignment is separate
d <- d %>% select(Species1,Waterbody.Name,Gear,Survey.Year,Mon,Len)
# END DEMO code only
# ============================================================

# Reduce number of variables ... for simplicity only, it is not
#   necessary for this analysis
# Combines piping and assignment with %<>%
d %<>% select(Species1,Waterbody.Name,Gear,Survey.Year,Mon,Len)
headtail(d,n=2)

# ============================================================
# BEGIN DEMO code only -- not strictly needed for this analysis
# Add natural log of length
d %<>% mutate(loglen=log(Len))
headtail(d)

# Add 1-in length categories variable to d
d %<>% mutate(Len1=lencat(Len,w=1))
headtail(d)

# examine Gabelhouse length categories for LMB & WAE
psdVal("Largemouth Bass",units="in")
psdVal("Walleye",units="in")
# END DEMO code only
# ============================================================

# Add Gabelhouse length categories for ALL species
d %<>% mutate(Lcat=psdAdd(Len,Species1,units="in",verbose=FALSE))
headtail(d)

# Create subsets for use below
Spr <- filterD(d,Survey.Year==2013,Mon %in% c("Apr","May","Jun"))
str(Spr)

BGSpr <- filterD(Spr,Species1=="Bluegill")
BGSprLC <- filterD(BGSpr,Waterbody.Name=="LAKE CHETAC",Gear=="BOOM SHOCKER")
SprLC <- filterD(Spr,Waterbody.Name=="LAKE CHETAC")

# ============================================================
# BEGIN DEMO code only -- not strictly needed for the analysis
# Frequency of each species in spring Lake Chetac summaries
xtabs(~Species1,data=SprLC)
# Frequency of Bluegill by month and lake in Spring samples
xtabs(~Mon+Waterbody.Name,data=BGSpr)
# END DEMO code only
# ============================================================

# Numericanl summaries of lengths of Lake Chetac Bluegill
Summarize(~Len,data=BGSprLC,digits=2)

# Length frequency histogram
hist(~Len,data=BGSprLC,breaks=seq(3,9,0.2),
     xlab="Total Length (In.)",ylab="Number of Bluegill",
     xlim=c(3,9),ylim=c(0,40))

# Bluegill length summaries by waterbody (for spring samples)
BGSpr %>%
  group_by(Waterbody.Name) %>%
  summarize(n=n(),valid_n=sum(!is.na(Len)),
            meanLen=mean(Len,na.rm=TRUE),sdLen=sd(Len,na.rm=TRUE),
            minLen=min(Len,na.rm=TRUE),maxLen=max(Len,na.rm=TRUE) ) %>%
  as.data.frame()

# Length summaries by species within waterbodies
#   Write results out to a file that can be opened in Excel
Spr %>%
  group_by(Waterbody.Name,Species1) %>%
  summarize(n=n(),valid_n=sum(!is.na(Len)),
            meanLen=round(mean(Len,na.rm=TRUE),2),sdLen=round(sd(Len,na.rm=TRUE),2),
            minLen=min(Len,na.rm=TRUE),maxLen=max(Len,na.rm=TRUE) ) %>%
  as.data.frame() %>%
  write.csv("LenSum_Sawyer_Spr13.csv",row.names=FALSE)

# frequency of individuals between each length category
( freq <- xtabs(~Lcat,data=BGSprLC) )
# reverse cumulative sum ... freq of each category
( rcum <- rcumsum(freq) )
# extract the "stock" frequency ... DEMO only
rcum["stock"]
# compute PSD values
rcum/rcum["stock"]*100

# Vector of length categories, with one for 7 in
( brks <- psdVal("Bluegill",units="in",addLens=7) )
# New variable with new length categories
BGSprLC %<>% mutate(Lcat2=lencat(Len,breaks=brks,use.names=TRUE,drop.levels=TRUE))
headtail(BGSprLC)

( freq <- xtabs(~Lcat2,data=BGSprLC) )
( rcum <- rcumsum(freq) )
round(rcum/rcum["stock"]*100,1)

BGSpr %<>% mutate(Lcat2=lencat(Len,breaks=brks,use.names=TRUE,drop.levels=TRUE))
( freq <- xtabs(~Waterbody.Name+Lcat2,data=BGSpr) )

# ============================================================
# BEGIN DEMO code only -- not strictly needed for the analysis
# apply() result has wrong orientation, only partial results shown
apply(freq,FUN=rcumsum,MARGIN=1)

apply(freq,FUN=rcumsum,MARGIN=1)[1:5,1:6]
# BEGIN DEMO code only -- not strictly needed for the analysis
# ============================================================

# properly oriented table of reverse cumulative sums by lake
( rcum <- t(apply(freq,FUN=rcumsum,MARGIN=1)) )

# remove "substock" column
rcum <- rcum[,-1]

# Compute PSD values
round(rcum/rcum[,"stock"]*100,1)

# Compute PSDs for multiple species in a single waterbody
( freq <- xtabs(~Species1+Lcat,data=SprLC) )
( rcum <- t(apply(freq,FUN=rcumsum,MARGIN=1)) )
rcum <- rcum[,-1] 
round(rcum/rcum[,"stock"]*100,1)


# Script created at 2016-01-31 14:23:52
