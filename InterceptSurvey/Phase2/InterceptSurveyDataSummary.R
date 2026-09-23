#InterceptSurveyDataSummary.R



date<-"2026Sep21"
setwd("/home/mves/Dropbox/Professional/UCD_CCCS/MPA_Watch/InterceptSurvey")
dat<-read.csv("RELAUNCH MPA Watch Intercept Survey October 2025_September 21, 2026_21.38.csv", skip=1)
dat.split<-split(dat,dat$M3...What.program.are.you.collecting.intercept.surveys.for.)
for (i in names(dat.split))
  write.csv(dat.split[i],paste(i,"_",date,"_",".csv",sep=""))  

## manual corrections
## remove the one that says "REPLACEMENT SURVEY" in the column "Q17...Q17...Is.there.anything.else.you.would.like.to.tell.us.about.yourself...your.experience.with.oceans.and.coasts.in.California..and.or.your..thoughts.on.local.marine.resource.management.
"


num.surv<-sum(dat$Q1.2...Did.the.person.you.approached.participate.in.the.survey.=="They participated in the survey")
num.surv
num.declines<-sum(dat$Q1.2...Did.the.person.you.approached.participate.in.the.survey.=="They declined to participate in the survey")
num.declines
num.surv/(num.surv+num.declines)




dat$surveyor<-dat$M5...Surveyor.name.s...First.and.Last...You.can.also.select.multiple.names.by.holding.down.the.cmd.ctrl.button

summary(factor(dat$surveyor))
length(levels(factor(dat$surveyor)))


all.people<-strsplit(dat$surveyor, ",", perl=TRUE)
one.list<-unlist(all.people)
unique.people<-unique(one.list)
write.csv(unique.people,"SurveyorListFromData.csv")

#There are 47 people in the drop down, and 33 have shown up in the surveys

#phase 1 surveys:
dat1<-read.csv("[Pilot 1] MPA Watch Intercept Survey_August 2, 2026_13.38.csv", skip=1)

dat1$surveyor<-dat1$Q7...Surveyor.name.s...First.and.Last.

all.people<-strsplit(dat1$surveyor, ",", perl=TRUE)
one.list<-unlist(all.people)
unique.people<-unique(one.list)
write.csv(unique.people,"SurveyorListFromDataPhase1.csv")

#need to check on the numerical codes - who are they?


