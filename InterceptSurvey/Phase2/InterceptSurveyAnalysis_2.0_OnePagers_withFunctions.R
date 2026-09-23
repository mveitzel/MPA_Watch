#InterceptSurveyAnalysis.R

library(ggplot2)
library(viridis)
library(forcats)
#library(ggmosaic)

#####################################
# analysis functions 			    #
#####################################


#response rate and other surveying stats
calculate.surveying.stats<-function(d,subset.name){
	resp<-sum(d$Q1.2...Did.the.person.you.approached.participate.in.the.survey.==
		"They participated in the survey")
	nresp<-sum(d$Q1.2...Did.the.person.you.approached.participate.in.the.survey.==
		"They declined to participate in the survey")
	nsurveyors<-length(levels(factor(d$M5...Surveyor.name.s...First.and.Last...You.can.also.select.multiple.names.by.holding.down.the.cmd.ctrl.button)))
	ntransects<-length(levels(factor(d$M4...Site.transect.name)))
	return(list(subset=subset.name,n.finished=nrow(d),
		n.response=resp,n.nonresponse=nresp,
		resp.rate=(resp/(resp+nresp)),
		n.surveyors=nsurveyors,n.transects=ntransects))
}


summarize.origin.location<-function(d,subset.name,local.zips){
	d<-d[d$Q1.2...Did.the.person.you.approached.participate.in.the.survey.==
		"They participated in the survey",]

	d$origin<-factor(d$Q1...Did.you.start.your.day.from.home.or.another.location....Selected.Choice)
		plt<-ggplot(d[!(is.na(d$origin)),],aes(y="",fill=origin))+
		geom_bar(position="fill") +
		labs(y=NULL,x ="Proportion")+
 		ggtitle(paste("Visit origin? (N=",sum(!is.na(d$origin)),")",sep=""))+
		theme(legend.position="bottom",legend.box.spacing=unit(0,"pt"),
			  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
 		labs(x="",fill="")+
# #		geom_text(aes(label = Count), y = 0, vjust = 0, hjust=1.5)+
# 		geom_text(aes(label = loc), angle = 90, y = 50, vjust = 1, hjust=1)+
 		scale_fill_viridis_d(option="mako")+
 		guides(fill = guide_legend(reverse = TRUE))

	ggsave(paste("Origin_Bar_",subset.name,"_",todaydate,".png",sep=""), units="in", width=4,height=1.5,dpi=300)

	plt<-ggplot(d[!(is.na(d$origin)),],aes(x="",fill=origin))+
	  geom_bar() +
	  coord_polar(theta = "y")+
 	  ggtitle(paste("Visit origin? (N=",sum(!is.na(d$origin)),")",sep=""))+
	  scale_fill_viridis_d(option="mako")+
	  theme(legend.position="bottom",legend.box.spacing=unit(0,"pt"),
	  	 	  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
	  labs(x=NULL,y=NULL,fill="")+
	  guides(fill = guide_legend(reverse = TRUE))

	ggsave(paste("Origin_Pie_",subset.name,"_",todaydate,".png",sep=""), units="in", width=4,height=4,dpi=300)


	d$otherloc<-d$Q2...Q2..Where.is.home.for.you..Please.provide.your.zip.code.or.country.if.not.the.U.S....Selected.Choice=="Or non-US country"
	d$ZIP<-d$Q2_1_TEXT...Q2..Where.is.home.for.you..Please.provide.your.zip.code.or.country.if.not.the.U.S....Zip.code...Text

	d$loc<-NA
	if(!(is.null(local.zips))) {
		d$loc[d$ZIP %in% local.zips]<-"Local County"
	}

	d$loc[(d$ZIP>=as.numeric(as.character(90001)))&(d$ZIP<=as.numeric(as.character(96162)))
		&!(d$ZIP %in% sd.zips)]<- "Other CA Cnty"
	d$loc[(d$ZIP!="")&( (d$ZIP<as.numeric(as.character(90001)))|(d$ZIP>as.numeric(as.character(96162))) )]<-"Non-CA US"
	d$loc[d$otherloc]<-"Int'l"
	d$loc<-factor(d$loc,levels=c("Int'l","Non-CA US","Other CA Cnty","Local County"))

	print(summary(d$loc))

	plt<-ggplot(d[!(is.na(d$loc)),],aes(y="",fill=loc))+
		geom_bar(position="fill") +
		labs(y=NULL,x ="Proportion")+
 		ggtitle(paste("Where is home? (N=",sum(!is.na(d$loc)),")",sep=""))+
		theme(legend.position="bottom",legend.box.spacing=unit(0,'pt'),
		      panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
 		labs(x="",fill="")+
# #		geom_text(aes(label = Count), y = 0, vjust = 0, hjust=1.5)+
# 		geom_text(aes(label = loc), angle = 90, y = 50, vjust = 1, hjust=1)+
 		scale_fill_viridis_d(option="mako")+
	    guides(fill = guide_legend(reverse = TRUE))
	ggsave(paste("Home_Bar_",subset.name,"_",todaydate,".png",sep=""), units="in", width=4,height=1.5,dpi=300)

	plt<-ggplot(d[!(is.na(d$loc)),],aes(x="",fill=loc))+
	  geom_bar() +
	  coord_polar(theta = "y")+
 	  ggtitle(paste("Where is home? (N=",sum(!is.na(d$loc)),")",sep=""))+
	  scale_fill_viridis_d(option="mako")+
		theme(legend.position="bottom",legend.box.spacing=unit(0,'pt'),
			  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
	  labs(x=NULL,y=NULL,fill="")+
	    guides(fill = guide_legend(reverse = TRUE))
	ggsave(paste("Home_Pie_",subset.name,"_",todaydate,".png",sep=""), units="in", width=4,height=4,dpi=300)

	return(plt)

}


summarize.language<-function(d,subset.name){

	d$lang<-factor(d$Q15...Q15..What.language.s..do.you.speak.at.home....Selected.Choice)	
	d.nonEng<-d[-grep("English",d$lang),]
	d.nonEng<-d.nonEng[d.nonEng$lang!="" & d.nonEng$lang!="Declined to respond",]
	print(summary(factor(d.nonEng$lang)))

	plt<-ggplot(d.nonEng,
		aes(x=fct_rev(fct_infreq(lang)),fill=lang))+
		geom_bar(fill=viridis(3,option="mako")[2])+
	    ggtitle(paste("Non-English-speaking\nhousehold languages(N=",sum(d.nonEng$lang!=""),")",sep=""))+
		theme(legend.position="none",
			  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
		labs(y="",x="")+
		 geom_text(
		    stat = "count", 
		    aes(label = after_stat(count), y = after_stat(max(count)), x = fct_rev(fct_infreq(lang)),
		    hjust= 1.2))+
		 geom_text(aes(y = 0, label = lang),vjust=0, hjust = -0.5)+
 #		geom_text(aes(label = Count), y = max(langs$Count), vjust = 0, hjust=1)+
#		geom_text(aes(label = Language), y = 0, vjust = 0, hjust=0)+
#		geom_text(aes(label = Count), y = 0, vjust = 0, hjust=1.5)+
		theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks=element_blank())+
		coord_flip()
	ggsave(paste("Language_NonEnglish_",subset.name,"_",todaydate,".png",sep=""), units="in", width=3,height=5,dpi=300)


	all.langs<-strsplit(as.character(d$lang), ",", perl=TRUE)
	one.list.langs<-unlist(all.langs)
	unique.langs<-unique(one.list.langs)

	result <- lapply(all.langs, function(element) unique.langs %in% element)
	for(i in 1:length(unique.langs)){
			 d[,unique.langs[i]] <- unlist(lapply(result, function(element) element[i]))
		}

	langs<-data.frame(Language=names(colSums(d[,unique.langs],na.rm=TRUE)),Count=colSums(d[,unique.langs],na.rm=TRUE))
	rownames(langs)<-NULL

	plt<-ggplot(langs,
		aes(x=reorder(Language,Count),y=Count))+
		geom_bar(stat="identity",fill=viridis(3,option="mako")[2])+
	    ggtitle(paste("What language do you\nspeak at home?(N=",sum(d$lang!=""),")",sep=""))+
		theme(legend.position="none",
			  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
		labs(y="",x="")+
		scale_fill_viridis_d(option="mako")+
		geom_text(aes(label = Count), y = max(langs$Count), vjust = 0, hjust=1)+
		geom_text(aes(label = Language), y = 0, vjust = 0, hjust=0)+
		theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks=element_blank())+
		coord_flip()
	ggsave(paste("Language_",subset.name,"_",todaydate,".png",sep=""), units="in", width=2.5,height=11,dpi=300)
	return(plt)

}



summarize.visit.purpose<-function(d,subset.name){

	d$purpose<-d$Q3...Q3..What.is.the.primary.purpose.of.your.trip.to.the.coast.today...select.all.that.apply....Selected.Choice
	d$other<-d$Q3_.1_TEXT...Q3..What.is.the.primary.purpose.of.your.trip.to.the.coast.today...select.all.that.apply....Other..please.specify...Text
	print(d$other[d$purpose=="Other, please specify"])

	all.activities<-strsplit(d$purpose, ",(?=[A-Z])", perl=TRUE)
	one.list<-unlist(all.activities)
	unique.activities<-unique(one.list)
	#print(unique.activities)

	result <- lapply(all.activities, function(element) unique.activities %in% element)
	for(i in 1:length(unique.activities)){
		 d[,unique.activities[i]] <- unlist(lapply(result, function(element) element[i]))
		}

	purpose<-data.frame(Activity=names(colSums(d[,unique.activities],na.rm=TRUE)),Count=colSums(d[,unique.activities],na.rm=TRUE))
	rownames(purpose)<-NULL

	purpose$Activity[purpose$Activity=="Walking or running"]<- "Walking/running"
	purpose$Activity[purpose$Activity=="Driving or sitting in your car to enjoy the views/sunsets"]<- "Driving/enjoying view"
	purpose$Activity[purpose$Activity=="Relaxing, reading, sun-bathing, meditation"]<- "Relaxing"
	purpose$Activity[purpose$Activity=="Swimming or bodysurfing"]<- "Swimming/bodysurfing"
	purpose$Activity[purpose$Activity=="Group or family gatherings or activities (e.g., family outing, bbq)"]<- "Group/family outing"
	purpose$Activity[purpose$Activity=="Bicycling, roller skating, skateboarding, etc"]<- "Bicycling/skating"
	purpose$Activity[purpose$Activity=="Other, please specify"]<- "Other"
	purpose$Activity[purpose$Activity=="Observing or photographing nature or wildlife, outdoor education"]<- "Wildlife/outoor ed"
	purpose$Activity[purpose$Activity=="Stand up paddleboarding/kite or sailboarding/kayaking"]<- "Boating (non-powered)"
	purpose$Activity[purpose$Activity=="Beach games or sports (e.g., frisbee, volleyball, yoga)"]<- "Beach games/sports"
	purpose$Activity[purpose$Activity=="Fishing or collecting food    "]<- "Fishing/collecting food"
	purpose$Activity[purpose$Activity=="Cultural or religious practices or ceremonies"]<-"Cultural/religious"
	purpose$Activity[purpose$Activity=="Volunteering (e.g., beach clean-ups)"]<-"Volunteering"
	purpose$Activity[purpose$Activity=="Sailing/Boating (engine powered)"]<-"Sailing/Boating (powered)"
	purpose$Activity<-factor(purpose$Activity)
	#print(purpose)

	plt<-ggplot(purpose,
		aes(x=reorder(Activity,Count),y=Count))+
		geom_bar(stat="identity",fill=viridis(3,option="mako")[2])+
		ggtitle(paste("Primary purpose of visit (N=",sum(d$purpose!=""),")",sep=""))+
		theme(legend.position="none",
			  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
		labs(y="",x="")+
		geom_text(aes(label = Count), y = max(purpose$Count), vjust = 0, hjust=1)+
		geom_text(aes(label = Activity), y = 0, vjust = 0, hjust=0)+
		theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks=element_blank())+
		#scale_fill_viridis_d(option="mako")+
		coord_flip()
	ggsave(paste("VisitPrimaryPurpose_",subset.name,"_",todaydate,".png",sep=""), units="in", width=3,height=6,dpi=300)
	return(plt)

}


summarize.barriers<-function(d,subset.name){

	d$barriers<-d$Q6...Q6..Is.there.anything.that.makes.being.at.the.beach.difficult.for.....you...Check.all.that.apply....Selected.Choice
	d$other<-d$Q6_9_TEXT...Q6..Is.there.anything.that.makes.being.at.the.beach.difficult.for.....you...Check.all.that.apply....Other...Text
	print(d$other[d$barriers=="Other"])

	all.barriers<-strsplit(d$barriers, ",(?=[A-Z])", perl=TRUE)
	one.list<-unlist(all.barriers)
	unique.barriers<-unique(one.list)

	result <- lapply(all.barriers, function(element) unique.barriers %in% element)
	for(i in 1:length(unique.barriers)){
			 d[,unique.barriers[i]] <- unlist(lapply(result, function(element) element[i]))
		}

	barriers<-data.frame(Barrier=names(colSums(d[,unique.barriers],na.rm=TRUE)),Count=colSums(d[,unique.barriers],na.rm=TRUE))
	rownames(barriers)<-NULL

	barriers$Barrier[barriers$Barrier=="Transportation (parking, accessing transportation, etc.)"]<- "Transportation"
	barriers$Barrier[barriers$Barrier=="Lack of clean and accessible amenities/infrastructure (e.g., bathrooms, picnic tables, trash/recycling cans)"]<- "Low amenities"
	barriers$Barrier[barriers$Barrier=="Physically accessing the coast (e.g. no wheelchair access, steep/uneven terrain, etc.)"]<- "Physical access"
	barriers$Barrier[barriers$Barrier=="Travel/geographical distance"]<- "Travel/Distance"
	barriers$Barrier[barriers$Barrier=="Feeling unwelcome/uncomfortable"]<- "Unwelcome/uncomfortable"
	barriers$Barrier<-factor(barriers$Barrier)

	plt<-ggplot(barriers,
		aes(x=reorder(Barrier,Count),y=Count))+
		geom_bar(stat="identity",fill=viridis(3,option="mako")[2])+
	    ggtitle(paste("Primary barriers to visit (N=",sum(d$barriers!=""),")",sep=""))+
		theme(legend.position="none",
			  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
		labs(y="",x="")+
		geom_text(aes(label = Count), y = max(barriers$Count), vjust = 0, hjust=1)+
		geom_text(aes(label = Barrier), y = 0, vjust = 0, hjust=0)+
		theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks=element_blank())+
		coord_flip()
	ggsave(paste("PrimaryBarriers_",subset.name,"_",todaydate,".png",sep=""), units="in", width=3,height=4,dpi=300)
	return(plt)

}



summarize.how.known<-function(d,subset.name){

	d$howknown<-d$Q9...Q9..If.you.responded..Yes..or..No..to.the.question.above..please......share.how.you.came.to.know.this.information..check.all.that.apply......NOTE..If.the.participant.s.response.above.was..I.am.not.sure..skip.this.question....Selected.Choice
	#TODO*** should we subset only for those who said yes? should we subset for those who were correct?
	d$other<-d$Q9_.1_TEXT...Q9..If.you.responded..Yes..or..No..to.the.question.above..please......share.how.you.came.to.know.this.information..check.all.that.apply......NOTE..If.the.participant.s.response.above.was..I.am.not.sure..skip.this.question....Other...Text
	print(d$other[d$howknown=="Other"])

	all.howknown<-strsplit(d$howknown, ",(?=[A-Z])", perl=TRUE)
	one.list<-unlist(all.howknown)
	unique.howknown<-unique(one.list)

	result <- lapply(all.howknown, function(element) unique.howknown %in% element)
	for(i in 1:length(unique.howknown)){
			 d[,unique.howknown[i]] <- unlist(lapply(result, function(element) element[i]))
		}

	howknown<-data.frame(HowKnown=names(colSums(d[,unique.howknown],na.rm=TRUE)),Count=colSums(d[,unique.howknown],na.rm=TRUE))
	rownames(howknown)<-NULL

	plt<-ggplot(howknown,
		aes(x=reorder(HowKnown,Count),y=Count))+
		geom_bar(stat="identity",fill=viridis(3,option="mako")[2])+
	    ggtitle(paste("How do you know you are\nin an MPA?(N=",sum(d$howknown!=""),")",sep=""))+
		theme(legend.position="none",
			  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
		labs(y="",x="")+
		geom_text(aes(label = Count), y = max(howknown$Count), vjust = 0, hjust=1)+
		geom_text(aes(label = HowKnown), y = 0, vjust = 0, hjust=0)+
		theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks=element_blank())+
		coord_flip()
	ggsave(paste("MPAHowKnown_",subset.name,"_",todaydate,".png",sep=""), units="in", width=2.5,height=4,dpi=300)
	return(plt)

}


summarize.race.ethnicity<-function(d,subset.name){

	d$raceEthn<-d$Q14...Q14..How.do.you.identify.your.race.ethnicity..Check.all.that.apply....Selected.Choice

	all.raceEthn<-strsplit(d$raceEthn, ",(?=[A-Z])", perl=TRUE)
	one.list<-unlist(all.raceEthn)
	unique.raceEthn<-unique(one.list)

	result <- lapply(all.raceEthn, function(element) unique.raceEthn %in% element)
	for(i in 1:length(unique.raceEthn)){
			 d[,unique.raceEthn[i]] <- unlist(lapply(result, function(element) element[i]))
		}

	raceEthn<-data.frame(RaceEthnicity=names(colSums(d[,unique.raceEthn],na.rm=TRUE)),Count=colSums(d[,unique.raceEthn],na.rm=TRUE))
	rownames(raceEthn)<-NULL

	raceEthn$RaceEthnicity[raceEthn$RaceEthnicity=="Native Hawaiian or other Pacific Islander"]<- "Native Hawaiian/Pacific Islander"
	raceEthn$RaceEthnicity[raceEthn$RaceEthnicity=="Hispanic or Latinx"]<- "Hispanic/Latinx"
	raceEthn$RaceEthnicity[raceEthn$RaceEthnicity=="Native American or Alaskan Native"]<- "Native American/Alaskan Native"
	raceEthn$RaceEthnicity[raceEthn$RaceEthnicity=="Black or African American"]<- "Black/African American"
	raceEthn$RaceEthnicity<-factor(raceEthn$RaceEthnicity)

	plt<-ggplot(raceEthn,
		aes(x=reorder(RaceEthnicity,Count),y=Count))+
		geom_bar(stat="identity",fill=viridis(3,option="mako")[2])+
	    ggtitle(paste("Race/Ethnicity (N=",sum(d$raceEthn!=""),")",sep=""))+
		theme(legend.position="none",
			  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
		labs(y="",x="")+
		geom_text(aes(label = Count), y = max(raceEthn$Count), vjust = 0, hjust=1)+
		geom_text(aes(label = RaceEthnicity), y = 0, vjust = 0, hjust=0)+
		theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks=element_blank())+
		coord_flip()
	ggsave(paste("RaceEthnicity_",subset.name,"_",todaydate,".png",sep=""), units="in", width=3,height=4,dpi=300)
	return(plt)

}


summarize.age<-function(d,subset.name){
	plt<-ggplot(d[!is.na(dat.f$Age),],
		aes(x=Age))+
		geom_histogram(fill=viridis(3,option="mako")[2])+
	    ggtitle(paste("Age (N=",sum(!is.na(d$Age)),")",sep=""))+
		labs(y="",x="")+
		theme_classic()
		# theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks=element_blank())+
	ggsave(paste("Age_",subset.name,"_",todaydate,".png",sep=""), units="in", width=3,height=1.5,dpi=300)
	return(plt)
}

summarize.gender<-function(d,subset.name){
	d$gender<-d$Q13...Q13..How.do.you.identify.your.gender....Selected.Choice
	d<-d[d$gender!="",]
	d$gender<-factor(d$gender)
	d$gender<-fct_rev(d$gender)

	plt<-ggplot(d[!(is.na(d$gender)),],aes(y="",fill=gender))+
		geom_bar(position="fill") +
		labs(y=NULL,x ="Proportion")+
 		ggtitle(paste("Gender (N=",sum(!is.na(d$gender)),")",sep=""))+
		theme(legend.position="bottom",
			  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
 		labs(x="",fill="")+
# #		geom_text(aes(label = Count), y = 0, vjust = 0, hjust=1.5)+
# 		geom_text(aes(label = loc), angle = 90, y = 50, vjust = 1, hjust=1)+
 		scale_fill_viridis_d(option="mako")+
	    guides(fill = guide_legend(reverse = TRUE))
	ggsave(paste("Gender_Bar_",subset.name,"_",todaydate,".png",sep=""), units="in", width=5.5,height=2,dpi=300)

	plt<-ggplot(d[!(is.na(d$gender)),],aes(x="",fill=gender))+
	  geom_bar() +
	  coord_polar(theta = "y")+
 	  ggtitle(paste("Gender (N=",sum(!is.na(d$gender)),")",sep=""))+
	  scale_fill_viridis_d(option="mako")+
	  theme(legend.position="bottom",
	  		  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
	  labs(x=NULL,y=NULL,fill="")
	ggsave(paste("Gender_Pie_",subset.name,"_",todaydate,".png",sep=""), units="in", width=5.5,height=4,dpi=300)

	}


summarize.importance<-function(d,subset.name){
	d$important<-(d$Q10...Q10..How.important.is.it.to.have.ocean.and.or.coastal.zones.be.managed.to.support.conservation.for.ocean.health.)
	d<-d[d$important!="",]
	d$important<-factor(d$important)
#	d$important<-factor(d$important,levels=c("Very Important","Important ","Neutral","Unimportant","Very unimportant"))
	d$important<-factor(d$important,levels=c("Very unimportant","Unimportant","Neutral","Important ","Very Important"))
	print(summary(d$important))

	plt<-ggplot(d[!(is.na(d$important)),],aes(y="",fill=important))+
		geom_bar(position="fill") +
		labs(y=NULL,x ="Proportion")+
 		ggtitle(paste("How Important are MPAs? (N=",sum(!is.na(d$important)),")",sep=""))+
		theme(legend.position="bottom",legend.box.spacing=unit(0, "pt"),
			  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
 		labs(x="",fill="")+
# #		geom_text(aes(label = Count), y = 0, vjust = 0, hjust=1.5)+
# 		geom_text(aes(label = loc), angle = 90, y = 50, vjust = 1, hjust=1)+
 		scale_fill_viridis_d(option="mako",direction=1)+
 		guides(fill = guide_legend(reverse = TRUE))
	ggsave(paste("Importance_",subset.name,"_",todaydate,".png",sep=""), units="in", width=5.5,height=2,dpi=300)
	return(plt)
}

#the colors don't always match all the levels, when one is missing
summarize.frequency<-function(d,subset.name){

	d$often<-(d$Q5...Q5...How.often.do.you.spend.time.in.ocean...coastal.areas.in.California.)
	d<-d[d$often!="",]

	d$often[d$often=="Several times per week or more"]<-"> Several times/week"
	d$often[d$often=="Several times per month"]<-"Several times/month"
	d$often[d$often=="Once every couple months"]<-"Several times/year"
	d$often[d$often=="Once a year"]<-"Once/year"
	d$often[d$often=="Less than once a year (i.e., rarely or never)"]<-"< Once/year"
	d$often[d$often=="Decline to respond"]<-NA

	d$often<-factor(d$often)
#	d$often<-factor(d$often,levels=c("> Several times/week","Several times/month","Several times/year","Once/year","< Once/year", "Declined"))
	d$often<-factor(d$often,levels=c("< Once/year","Once/year","Several times/year","Several times/month","> Several times/week"))
	print(summary(d$often))

 	plt<-ggplot(d[!(is.na(d$often)),],aes(y="",fill=often))+
 		geom_bar(position="fill") +
 		labs(y=NULL,x ="Proportion")+
  		ggtitle(paste("How frequently do you visit the coast? (N=",sum(!is.na(d$often)),")",sep=""))+
 		theme(legend.position="bottom",legend.box.spacing=unit(0,"pt"),
 			  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
  		labs(x="",fill="")+
 # #		geom_text(aes(label = Count), y = 0, vjust = 0, hjust=1.5)+
 # 		geom_text(aes(label = loc), angle = 90, y = 50, vjust = 1, hjust=1)+
  		scale_fill_viridis_d(option="mako")+
    	guides(fill = guide_legend(nrow = 2, byrow = TRUE,reverse=TRUE))

 	ggsave(paste("Frequency_",subset.name,"_",todaydate,".png",sep=""), units="in", width=6,height=2,dpi=300)
 	return(plt)

}

#color scheme needs work for this few options
summarize.awareness.CA.MPA<-function(d,subset.name){
	d$awareness.CA<-d$Q7...Q7..Prior.to.this.survey..were.you.aware.that.California.has.....Marine.Protected.Areas.
	d<-d[d$awareness.CA!="",]
	d$awareness.CA<-factor(d$awareness.CA)

	plt<-ggplot(d[!(is.na(d$awareness.CA)),],aes(y="",fill=awareness.CA))+
		geom_bar(position="fill") +
		labs(y=NULL,x ="Proportion")+
 		ggtitle(paste("Were you aware of CA MPAs? (N=",sum(!is.na(d$awareness.CA)),")",sep=""))+
		theme(legend.position="bottom",legend.box.spacing=unit(0,"pt"),
			  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
 		labs(x="",fill="")+
# #		geom_text(aes(label = Count), y = 0, vjust = 0, hjust=1.5)+
# 		geom_text(aes(label = loc), angle = 90, y = 50, vjust = 1, hjust=1)+
 		scale_fill_viridis_d(option="mako")
	ggsave(paste("CA_MPA_Bar_",subset.name,"_",todaydate,".png",sep=""), units="in", width=5.5,height=2,dpi=300)

	plt<-ggplot(d[!(is.na(d$awareness.CA)),],aes(x="",fill=awareness.CA))+
	  geom_bar() +
	  coord_polar(theta = "y")+
 	  ggtitle(paste("Were you aware of CA MPAs? (N=",sum(!is.na(d$awareness.CA)),")",sep=""))+
	  scale_fill_viridis_d(option="mako")+
	  theme(legend.position="bottom",legend.box.spacing=unit(0,"pt"),
	  		  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
 	  guides(fill = guide_legend(reverse = TRUE))+
	  labs(x=NULL,y=NULL,fill="")
	ggsave(paste("CA_MPA_Pie_",subset.name,"_",todaydate,".png",sep=""), units="in", width=5.5,height=4,dpi=300)
	return(plt)

	}


#note that it would be good, too, to say whether they were correct or not
#color scheme needs work for this few options
summarize.awareness.this.MPA<-function(d,subset.name){
	d$awareness.MPA<-d$Q8...Q8..Based.on.your.knowledge..do.you.know.whether.or.not.you.are.at.an.MPA.
	d<-d[d$awareness.MPA!="",]
	d$awareness.MPA<-factor(d$awareness.MPA,levels=c("No, I am not at an MPA","I am not sure","Yes, I am at an MPA"))

	plt<-ggplot(d[!(is.na(d$awareness.MPA)),],aes(x="",fill=awareness.MPA))+
	  geom_bar() +
	  coord_polar(theta = "y")+
 	  ggtitle(paste("Are you in an MPA? (N=",sum(!is.na(d$awareness.MPA)),")",sep=""))+
	  scale_fill_viridis_d(option="mako")+
	  theme(legend.position="bottom",legend.box.spacing=unit(0,"pt"),
			  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
	  labs(x=NULL,y=NULL,fill="")+
	  guides(fill = guide_legend(reverse = TRUE))

	ggsave(paste("This_MPA_Pie_",subset.name,"_",todaydate,".png",sep=""), units="in", width=5.5,height=4,dpi=300)

	plt<-ggplot(d[!(is.na(d$awareness.MPA)),],aes(y="",fill=awareness.MPA))+
		geom_bar(position="fill") +
		labs(y=NULL,x ="Proportion")+
 		ggtitle(paste("Are you in an MPA? (N=",sum(!is.na(d$awareness.MPA)),")",sep=""))+
		theme(legend.position="bottom",legend.box.spacing=unit(0,"pt"),
			  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
 		labs(x="",fill="")+
# #		geom_text(aes(label = Count), y = 0, vjust = 0, hjust=1.5)+
# 		geom_text(aes(label = loc), angle = 90, y = 50, vjust = 1, hjust=1)+
 		scale_fill_viridis_d(option="mako")+
	    guides(fill = guide_legend(reverse = TRUE))
	ggsave(paste("This_MPA_Bar_",subset.name,"_",todaydate,".png",sep=""), units="in", width=5.5,height=2,dpi=300)

	return(plt)
}


mpa.awareness.crosstab<-function(d,subset.name){
	d$MPA.aware<-d$Q7...Q7..Prior.to.this.survey..were.you.aware.that.California.has.....Marine.Protected.Areas.
	d$CA.MPA<-d$Q8...Q8..Based.on.your.knowledge..do.you.know.whether.or.not.you.are.at.an.MPA.
	d<-d[d$MPA.aware!="",]
	mpaaware<-as.data.frame(table(d$MPA.aware,d$CA.MPA))

	plt<-ggplot(mpaaware,
			aes(x=Var1,y=Freq, fill=Var2))+
			geom_bar(stat="identity",position="dodge")+
			ggtitle(paste("Awareness about CA MPAs (N=",nrow(d),")",sep=""))+
			theme(legend.position="bottom",
							  panel.background = element_rect(fill = "white", colour = "white"),
    		  plot.background = element_rect(fill = "white", colour = "white"),
    		  panel.grid = element_blank())+
			labs(x="",y="Count",fill="")+
			scale_fill_viridis_d(option="mako")+
#			scale_fill_manual(
#			name="",
#	#		labels=c("Aware of CA MPAs","Unaware of CA MPAs"),
#			values=viridis(6)[5:2])+
#		scale_fill_viridis_d(option="mako")+
#		scale_color_manual(
#			name="",
#	#		labels=c("Aware of CA MPAs","Unaware of CA MPAs"),
#			values=viridis(6)[5:2])+
#			geom_text(aes(label = Freq),position = position_dodge(.9), vjust = 1,fontface="bold")
			#geom_text(aes(label = Freq),y = max(mpaaware$Freq), vjust = 0, hjust=1)+
			#geom_text(aes(label = Var1), y = 0, vjust = 0, hjust=0)+
			theme(axis.text.x = element_blank(),axis.ticks=element_blank())+
			coord_flip()
	ggsave(paste("MPAaware_",subset.name,"_",todaydate,".png",sep=""), units="in", width=5,height=5,dpi=300)
	return(plt)
}


mpa.awareness.purpose.crosstab<-function(d,subset.name){
	d$MPA.aware<-d$Q7...Q7..Prior.to.this.survey..were.you.aware.that.California.has.....Marine.Protected.Areas.
	d<-d[d$MPA.aware!="",]

	d$purpose<-d$Q3...Q3..What.is.the.primary.purpose.of.your.trip.to.the.coast.today...select.all.that.apply....Selected.Choice

	all.activities<-strsplit(d$purpose, ",(?=[A-Z])", perl=TRUE)
	one.list<-unlist(all.activities)
	unique.activities<-unique(one.list)
	#print(unique.activities)

	result <- lapply(all.activities, function(element) unique.activities %in% element)
	for(i in 1:length(unique.activities)){
		 d[,unique.activities[i]] <- unlist(lapply(result, function(element) element[i]))
		}

	d.mpa.yes<-d[d$MPA.aware=="Yes",]
	d.mpa.no<-d[d$MPA.aware=="No",]

	purpose.y<-data.frame(Activity=names(colSums(d.mpa.yes[,unique.activities],na.rm=TRUE)),Count=colSums(d.mpa.yes[,unique.activities],na.rm=TRUE))
	rownames(purpose.y)<-NULL

	purpose.y$Activity[purpose.y$Activity=="Walking or running"]<- "Walking/running"
	purpose.y$Activity[purpose.y$Activity=="Driving or sitting in your car to enjoy the views/sunsets"]<- "Driving/enjoying view"
	purpose.y$Activity[purpose.y$Activity=="Relaxing, reading, sun-bathing, meditation"]<- "Relaxing"
	purpose.y$Activity[purpose.y$Activity=="Swimming or bodysurfing"]<- "Swimming/bodysurfing"
	purpose.y$Activity[purpose.y$Activity=="Group or family gatherings or activities (e.g., family outing, bbq)"]<- "Group/family outing"
	purpose.y$Activity[purpose.y$Activity=="Bicycling, roller skating, skateboarding, etc"]<- "Bicycling/skating"
	purpose.y$Activity[purpose.y$Activity=="Other, please specify"]<- "Other"
	purpose.y$Activity[purpose.y$Activity=="Observing or photographing nature or wildlife, outdoor education"]<- "Wildlife/outoor ed"
	purpose.y$Activity[purpose.y$Activity=="Stand up paddleboarding/kite or sailboarding/kayaking"]<- "Boating (non-powered)"
	purpose.y$Activity[purpose.y$Activity=="Beach games or sports (e.g., frisbee, volleyball, yoga)"]<- "Beach games/sports"
	purpose.y$Activity[purpose.y$Activity=="Fishing or collecting food    "]<- "Fishing/collecting food"
	purpose.y$Activity[purpose.y$Activity=="Cultural or religious practices or ceremonies"]<-"Cultural/religious"
	purpose.y$Activity[purpose.y$Activity=="Volunteering (e.g., beach clean-ups)"]<-"Volunteering"
	purpose.y$Activity[purpose.y$Activity=="Sailing/Boating (engine powered)"]<-"Sailing/Boating (powered)"
	purpose.y$Activity<-factor(purpose.y$Activity)

	purpose.n<-data.frame(Activity=names(colSums(d.mpa.no[,unique.activities],na.rm=TRUE)),Count=colSums(d.mpa.no[,unique.activities],na.rm=TRUE))
	rownames(purpose.n)<-NULL

	purpose.n$Activity[purpose.n$Activity=="Walking or running"]<- "Walking/running"
	purpose.n$Activity[purpose.n$Activity=="Driving or sitting in your car to enjoy the views/sunsets"]<- "Driving/enjoying view"
	purpose.n$Activity[purpose.n$Activity=="Relaxing, reading, sun-bathing, meditation"]<- "Relaxing"
	purpose.n$Activity[purpose.n$Activity=="Swimming or bodysurfing"]<- "Swimming/bodysurfing"
	purpose.n$Activity[purpose.n$Activity=="Group or family gatherings or activities (e.g., family outing, bbq)"]<- "Group/family outing"
	purpose.n$Activity[purpose.n$Activity=="Bicycling, roller skating, skateboarding, etc"]<- "Bicycling/skating"
	purpose.n$Activity[purpose.n$Activity=="Other, please specify"]<- "Other"
	purpose.n$Activity[purpose.n$Activity=="Observing or photographing nature or wildlife, outdoor education"]<- "Wildlife/outoor ed"
	purpose.n$Activity[purpose.n$Activity=="Stand up paddleboarding/kite or sailboarding/kayaking"]<- "Boating (non-powered)"
	purpose.n$Activity[purpose.n$Activity=="Beach games or sports (e.g., frisbee, volleyball, yoga)"]<- "Beach games/sports"
	purpose.n$Activity[purpose.n$Activity=="Fishing or collecting food    "]<- "Fishing/collecting food"
	purpose.n$Activity[purpose.n$Activity=="Cultural or religious practices or ceremonies"]<-"Cultural/religious"
	purpose.n$Activity[purpose.n$Activity=="Volunteering (e.g., beach clean-ups)"]<-"Volunteering"
	purpose.n$Activity[purpose.n$Activity=="Sailing/Boating (engine powered)"]<-"Sailing/Boating (powered)"
	purpose.n$Activity<-factor(purpose.n$Activity)

	purpose<-rbind(cbind(Awareness=rep("Unaware",nrow(purpose.n)),purpose.n),cbind(Awareness=rep("Aware",nrow(purpose.y)),purpose.y))

	plt<-ggplot(purpose,
			aes(x=Activity,y=Count, fill=Awareness))+
			geom_bar(stat="identity",position="dodge")+
			ggtitle(paste("Purpose of visit (N=",nrow(d),")",sep=""))+
			theme(legend.position="bottom")+
			labs(x="",y="Count",fill="")+
			scale_fill_viridis_d(option="mako")+
		labs(y="",x="")+
#		geom_text(aes(label = Count), y = max(purpose$Count), position = position_dodge(.9), vjust = 0, hjust=1)+
		geom_text(aes(label = Activity), y = 0, vjust = 0, hjust=0)+
#		theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks=element_blank())+
	#		scale_fill_manual(
	#		name="",
	#		labels=c("Aware of CA MPAs","Unaware of CA MPAs"),
	#		values=viridis(6)[5:2])+
	#	scale_color_manual(
	#		name="",
	#		labels=c("Aware of CA MPAs","Unaware of CA MPAs"),
	#		values=viridis(6)[5:2])+
	#		geom_text(aes(label = Count),position = position_dodge(.9), vjust = 1,fontface="bold")+
			coord_flip()
	ggsave(paste("MPAawareVisitPurpose_",subset.name,"_",todaydate,".png",sep=""), units="in", width=2.5,height=6,dpi=300)
	return(plt)
}

#####################################
# Data preprocessing and subsets    #
#####################################


todaydate<-"2026Aug15"

setwd("D:\\DropboxFiles\\Dropbox\\Professional\\UCD_CCCS\\MPA_Watch\\InterceptSurvey")
#setwd("/home/mves/Dropbox/Professional/UCD_CCCS/MPA_Watch/InterceptSurvey")

#dat<-read.csv("CleanedMPAWatchInterceptSurveyData_09252024-FullDataset.csv",skip=1)
#dat<-read.csv("RELAUNCH+MPA+Watch+Intercept+Survey+October+2025_April+27,+2026_14.56 LABELS.csv",skip=1)
dat<-read.csv("RELAUNCH MPA Watch Intercept Survey October 2025_August 15, 2026_15.47.csv",skip=1)
#note to self, don't open it in libreoffice where it interpreted semicolons as separators, and then save it.
#just use the direct csv export into R, it's more reliable

#have to calculate birth year immediately in order to remove underage respondents
dat$BirthYear<-dat$Q12...Q12..What.year.were.you.born....If.the.participant..declined.to.respond...please.write.that.in.the.text.box.below.
dat$BirthYear[dat$BirthYear==20003]<-2003
dat$Age<-2026-as.numeric(as.character(dat$BirthYear))

#remove incomplete surveys and survey previews, as well as any underage surveys
dat.f<-dat[dat$Finished=="True" & dat$Response.Type!="Survey Preview",]
#might not want to remove the unfinished ones?
#dat.f<-dat[dat$Response.Type!="Survey Preview",]

#TODO*** need to remove underage surveys (there are 2) but for some reason "& dat$Age>=18" introduces NAs in other places

#subsetting for various summaries
#Matlahuayl for an example site
datm<-dat.f[dat.f$M4...Site.transect.name=="Matlahuyal 1 (WILDCOAST",]
#bioregion: WILDCOAST, Orange County CoastKeepers, Heal The Bay, and Santa Barbara Channel Keepers will all be south
#			Just EAC Marin and TDN will be Central/North.
dats<- dat.f[dat.f$M3...What.program.are.you.collecting.intercept.surveys.for. %in% c("WILDCOAST", "Heal the Bay", "Orange County Coastkeeper","Santa Barbara Channelkeepers"),]
datn<- dat.f[dat.f$M3...What.program.are.you.collecting.intercept.surveys.for. %in% c("EAC Marin", "TDN"),]


#####################################
# Summarizing and making plots      #
#####################################


all.surv.summaries<-calculate.surveying.stats(dat.f,"AllSurveys")
mat.surv.summaries<-calculate.surveying.stats(datm,"Matlahuayl")
nor.surv.summaries<-calculate.surveying.stats(datn,"North")
sou.surv.summaries<-calculate.surveying.stats(dats,"South")

programs<-split(dat.f,dat.f$M3...What.program.are.you.collecting.intercept.surveys.for.)
lapply(programs,calculate.surveying.stats,names(programs))



#location
 #from San diego county (Jenny's colleague made code to look this up online)
sd.zips<-c(91901,91902,91905,91906,91910,91911,92028,92029,92036,92058,92058,91913,91914,91915,91916,91934,91935,91941,91942,91945,91917,91931,91932,91948,91950,91962,92058,92058,92058,91963,91977,91978,91980,92003,92027,92004,92007,92008,92009,92010,92011,92014,92019,92020,92021,92024,92025,92026,92037,92037,92040,92054,92055,92056,92057,92058,92058,92058,92059,92060,92061,92064,92065,92083,92083,92084,92086,92091,92155,92066,92067,92069,92070,92071,92075,92078,92081,92082,92093,92161,92096,92101,92102,92103,92104,92123,92124,92105,92106,92107,92108,92109,92110,92111,92113,92114,92115,92116,92117,92118,92119,92120,92121,92136,92139,92122,92140,92145,92154,92126,92127,92128,92129,92130,92131,92134,92135,92173,92182,92259,92536,92672)
origin.plt<-summarize.origin.location(datm,"Matlahuayl",sd.zips)
origin.plt
origin.plt<-summarize.origin.location(datn,"North",NULL)
origin.plt
origin.plt<-summarize.origin.location(dats,"South",NULL)
origin.plt
origin.plt<-summarize.origin.location(dat.f,"AllSurveys",NULL)
origin.plt
#for the 'south' I have some NAs and not sure where those are coming from

# language
lang.plt<-summarize.language(dat.f,"AllSurveys")
lang.plt
lang.plt<-summarize.language(datm,"Matlahuayl")
lang.plt
lang.plt<-summarize.language(datn,"North")
lang.plt
lang.plt<-summarize.language(dats,"South")
lang.plt


#Purpose
purp.plt<-summarize.visit.purpose(dat.f,"AllSurveys")
purp.plt
purp.plt<-summarize.visit.purpose(datm,"Matlahuayl")
purp.plt
purp.plt<-summarize.visit.purpose(dats,"South")
purp.plt
purp.plt<-summarize.visit.purpose(datn,"North")
purp.plt

#how often do you come to the ocean

freq.plt<-summarize.frequency(dat.f,"AllSurveys")
freq.plt<-summarize.frequency(datm,"Matlahuayl")
freq.plt<-summarize.frequency(datn,"North")
freq.plt<-summarize.frequency(dats,"South")


# barriers
barr.plt<-summarize.barriers(dat.f,"AllSurveys")
barr.plt
barr.plt<-summarize.barriers(datm,"Matlahuayl")
barr.plt
barr.plt<-summarize.barriers(datn,"North")
barr.plt
barr.plt<-summarize.barriers(dats,"South")
barr.plt


# MPA Awareness 

CAMPA.plt<-summarize.awareness.CA.MPA(dat.f,"AllSurveys")
CAMPA.plt<-summarize.awareness.CA.MPA(datm,"Matlahuayl")
CAMPA.plt<-summarize.awareness.CA.MPA(datn,"North")
CAMPA.plt<-summarize.awareness.CA.MPA(dats,"South")


thisMPA.plt<-summarize.awareness.this.MPA(dat.f,"AllSurveys")
thisMPA.plt<-summarize.awareness.this.MPA(datm,"Matlahuayl")
thisMPA.plt<-summarize.awareness.this.MPA(datn,"North")
thisMPA.plt<-summarize.awareness.this.MPA(dats,"South")

# how known
known.plt<-summarize.how.known(dat.f,"AllSurveys")
known.plt
known.plt<-summarize.how.known(datm,"Matlahuayl")
known.plt
known.plt<-summarize.how.known(datn,"North")
known.plt
known.plt<-summarize.how.known(dats,"South")
known.plt


# how important are MPAs

imp.plt<-summarize.importance(dat.f,"AllSurveys")
imp.plt<-summarize.importance(datm,"Matlahuayl")
imp.plt<-summarize.importance(datn,"North")
imp.plt<-summarize.importance(dats,"South")


aware.plt<-mpa.awareness.crosstab(dat.f,"AllSurveys")

aware.purpose<-mpa.awareness.purpose.crosstab(dat.f,"AllSurveys")

#age

age.plt<-summarize.age(dat.f,"AllSurveys")
age.plt
age.plt<-summarize.age(datm,"Matlahuayl")
age.plt
age.plt<-summarize.age(datn,"North")
age.plt
age.plt<-summarize.age(dats,"South")
age.plt

#gender

all.gender.summary<-summarize.gender(dat.f,"AllSurveys")
mat.gender.summary<-summarize.gender(datm,"Matlahuayl")
nor.gender.summary<-summarize.gender(datn,"North")
sou.gender.summary<-summarize.gender(dats,"South")

#Race/Ethnicity
re.plt<-summarize.race.ethnicity(dat.f,"AllSurveys")
re.plt
re.plt<-summarize.race.ethnicity(datm,"Matlahuayl")
re.plt
re.plt<-summarize.race.ethnicity(datn,"North")
re.plt
re.plt<-summarize.race.ethnicity(dats,"South")
re.plt

##########################################

# # count data info:

# datm$Date <- sub("^(\\d)/", "0\\1/", datm$Date..MM.DD.YYYY.)
# datm$Date <- sub("/(\\d)/", "/0\\1/", datm$Date)


# cdat<-read.csv("CountSurveys_2026-05-01_CaliforniaPrograms.csv")

# cdatm<-cdat[cdat$Survey.Site=="Matlahuayl 1 (MAT 1)",]

# cdatmintsurv<-cdatm[
# as.Date(cdatm$Date,format="%m/%d/%Y")>=min(as.Date(datm$Date..MM.DD.YYYY.,format="%d/%d/%Y")) &
# as.Date(cdatm$Date,format="%m/%d/%Y")<=max(as.Date(datm$Date..MM.DD.YYYY.,format="%d/%d/%Y")),
# ]

# cdatm[is.na(cdatm)]<-""

# cdatm[cdatm=="no"]<-0
# cdatm[cdatm=="No"]<-0

# write.csv(cdatm,"SortOfCleanCountsMat1.csv")
# cdatm<-read.csv("SortOfCleanCountsMat1.csv")

# colSums(cdatm[,28:36],na.rm=TRUE)/(nrow(cdatm)*1.1) #MAT 1 is 1.1 miles long, so multiply by number of surveys for survey-miles

# colSums(cdatm[,c(41:94)],na.rm=TRUE)/(nrow(cdatm)*1.1) #MAT 1 is 1.1 miles long, so multiply by number of surveys for survey-miles


