#InterceptSurveyAnalysis.R

library(ggplot2)
library(viridis)
library(ggmosaic)

setwd("D:\\DropboxFiles\\Dropbox\\Professional\\UCD_CCCS\\MPA_Watch\\InterceptSurvey")

#dat<-read.csv("CleanedMPAWatchInterceptSurveyData_09252024-FullDataset.csv",skip=1)
dat<-read.csv("Cleaned MPA Watch Intercept Survey Data_09252024_edited.csv",skip=1,sep="\t")

dat$barriers<-dat$Q17...Q6..Is.there.anything.that.makes.being.at.the.beach.difficult.for...you...Check.all.that.apply....Selected.Choice

#datb is just for the 'barriers' questions, because one person declined to respond and we want to remove that person
datb<-dat
#datb<-dat[!(1:nrow(dat) %in%(grep("J.",dat$barriers))),]


datb$barrier.cost<-FALSE
datb$barrier.cost[grep("A.",datb$barriers)]<-TRUE

datb$barrier.travel<-FALSE
datb$barrier.travel[grep("B.",datb$barriers)]<-TRUE

datb$barrier.access<-FALSE
datb$barrier.access[grep("C.",datb$barriers)]<-TRUE

datb$barrier.parking<-FALSE
datb$barrier.parking[grep("D.",datb$barriers)]<-TRUE

#no selections for E
datb$barrier.safety<-FALSE
datb$barrier.safety[grep("E.",datb$barriers)]<-TRUE

datb$barrier.comfort<-FALSE
datb$barrier.comfort[grep("F.",datb$barriers)]<-TRUE

datb$barrier.bathroom<-FALSE
datb$barrier.bathroom[grep("G.",datb$barriers)]<-TRUE

#H is 'Other' so that will be qualitatively analyzed 
#(and we can come back and search the "other text" column if we want to)
datb$barrier.other<-FALSE
datb$barrier.other[grep("H.",datb$barriers)]<-TRUE

#I is 'nothing to report'  we might look at these later, if we re-code the
#variable to be "any barriers = TRUE" versus "no barriers = FALSE"
datb$barrier.NTR<-FALSE
datb$barrier.NTR[grep("I.",datb$barriers)]<-TRUE

#J is "declined to respond" -- we don't know if this person had barriers or not, 
# so for now let's just plan to remove them from this analysis.
datb$barrier.DTR<-FALSE
datb$barrier.DTR[grep("J.",datb$barriers)]<-TRUE

#***TODO: Jadda has a bar in her summary with 'more than one barrier' - we could do this if we
# searched for a reponse with more than one '.' in it.

#***TODO "comfort" isn't the best one-word descriptor for emotional safety

#***TODO: make text perpendicular and write out 'nothing to report' and 'declined to respond'

#***TODO: check numbers against Jadda's numbers because they don't match.

barriers<-data.frame(Barrier=c("Cost","Travel","Access","Comfort","Safety","Parking","Bathroom",
		"Other","NTR","DTR"),
					   Count=c(sum(datb$barrier.cost),
						 	  sum(datb$barrier.travel),
							  sum(datb$barrier.access),
							  sum(datb$barrier.comfort),
							  sum(datb$barrier.safety),
							  sum(datb$barrier.parking),
							  sum(datb$barrier.bathroom),
							  sum(datb$barrier.other),
							  sum(datb$barrier.NTR),
							  sum(datb$barrier.DTR)
							  ) )

plt.b<-ggplot(barriers,
		aes(x=Barrier,y=Count,fill=Barrier))+
		geom_bar(stat="identity",position=position_dodge())+
		ggtitle(paste("Barriers to Coastal Access (N=",nrow(datb),")",sep=""))+
		theme(legend.position="none")+
		labs(x="")+
		scale_fill_viridis_d()+
		geom_text(aes(label = Count),position = position_dodge(.9), vjust = -0.2,fontface="bold")

plt.b
ggsave("BarriersToBeachAccess.png", units="in", width=8,height=4,dpi=300)

#***TODO: MV will figure out how to code age, race/ethnicity, gender ID, and language
# that will raise questions for Ryan and Jadda about how to code those 
#(e.g. English?  Spanish? or any combo of languages as a 'level' in the factor)

#other response variables


datb$CA.MPA.aware<-datb$Q19...Q7..Prior.to.this.survey..were.you.aware.that.California.has...Marine.Protected.Areas.
datb$temp<-NA
datb$temp[grep("A.",datb$CA.MPA.aware)]<-FALSE
datb$temp[grep("B.",datb$CA.MPA.aware)]<-TRUE
datb$CA.MPA.aware<-datb$temp

datb$MPA.aware<-factor(datb$Q20...Q8..Do.you.know.whether.or.not.you.are.at.an.MPA.)

datb$CAMPAaware<-NA
datb$CAMPAaware[datb$CA.MPA.aware==TRUE]<-"Aware of MPAs"
datb$CAMPAaware[datb$CA.MPA.aware==FALSE]<-"Not aware of MPAs"

datb.narm<-subset(datb,!is.na(datb$CA.MPA.aware)&datb$MPA.aware!="")
datb.narm$MPA.aware<-factor(datb.narm$MPA.aware)

mpaaware<-as.data.frame(table(datb.narm$MPA.aware,datb.narm$CAMPAaware))

mpaa<-ggplot(mpaaware,
		aes(x=Var1,y=Freq, fill=Var2))+
		geom_bar(stat="identity",position="dodge")+
		ggtitle(paste("Awareness of being in an MPA (N=",nrow(datb.narm),")",sep=""))+
		theme(legend.position="bottom")+
		labs(x="",y="Count",fill="")+
		scale_fill_manual(
		name="",
		labels=c("Aware of CA MPAs","Unaware of CA MPAs"),
		values=viridis(4)[3:2])+
	scale_color_manual(
		name="",
		labels=c("Aware of CA MPAs","Unaware of CA MPAs"),
		values=viridis(4)[3:2])+
		geom_text(aes(label = Freq),position = position_dodge(.9), vjust = -0.2,fontface="bold")

mpaa
ggsave("AwareOfBeingInMPA.png", units="in", width=8,height=4,dpi=300)



#do we want to parse this out like we did with barriers? - yes
#multiple response, need to discuss how to parse it
datb$how.know<-datb$Q21...Q9..If.you.responded..Yes..or..No..to.the.question.above..please....share.how.you.came.to.know.this.information..check.all.that.apply.........NOTE..If.the.participant.s.response.above.was..B..I.am.not.sure..skip.this.question....Selected.Choice


datb$how.know.sign<-FALSE
datb$how.know.sign[grep("A.",datb$how.know)]<-TRUE

datb$how.know.lknow<-FALSE
datb$how.know.lknow[grep("B.",datb$how.know)]<-TRUE

datb$how.know.outreach<-FALSE
datb$how.know.outreach[grep("C.",datb$how.know)]<-TRUE

datb$how.know.socialm<-FALSE
datb$how.know.socialm[grep("D.",datb$how.know)]<-TRUE

datb$how.know.famfr<-FALSE
datb$how.know.famfr[grep("E.",datb$how.know)]<-TRUE

datb$how.know.notsure<-FALSE
datb$how.know.notsure[grep("F.",datb$how.know)]<-TRUE

datb$how.know.other<-FALSE
datb$how.know.other[grep("G.",datb$how.know)]<-TRUE

how.known<-data.frame(KnownBy=c("Signage","Local Knowledge","Outreach","Social Media","Family/Friends","Not Sure","Other"),
					   Count=c(sum(datb$how.know.sign),
						 	  sum(datb$how.know.lknow),
							  sum(datb$how.know.outreach),
							  sum(datb$how.know.socialm),
							  sum(datb$how.know.famfr),
							  sum(datb$how.know.notsure),
							  sum(datb$how.know.other)
							  ) )

plt.b<-ggplot(how.known,
		aes(x=KnownBy,y=Count,fill=KnownBy))+
		geom_bar(stat="identity",position=position_dodge())+
		ggtitle(paste("How did you know you were in an MPA? (N=",sum(datb$how.know!=""),")",sep=""),)+
		theme(legend.position="none")+
		labs(x="")+
		scale_fill_viridis_d()+
		geom_text(aes(label = Count), vjust = -0.2,fontface="bold")
plt.b
ggsave("HowKnownMPA.png", units="in", width=8,height=4,dpi=300)



#this only has one selection, so easier to parse 
datb$purpose1<-factor(datb$Q13...Q3..What.is.the.primary.purpose.of.your.trip.to.the.coast.today....Selected.Choice)
#secondary purpose
datb$purpose2<-factor(datb$Q14...Q4..What.is.the.secondary.purpose.of.your.trip.to.the.coast.today...Or.select.N.A.if.there.is.no.secondary.reason.....Selected.Choice)

datb$purpose[datb$purpose1=="A. Visiting family and friends"]<-"Visit family & friends"
datb$purpose[datb$purpose1=="B. Sightseeing of wildlife viewing"]<-"Sightseeing/Wildlife viewing"
datb$purpose[datb$purpose1=="C. Visiting a state park"]<-"State park visit"
datb$purpose[datb$purpose1=="F. Water sport (e.g., surfing, kiteboarding, kayaking, etc.)"]<-"Water Sport"
datb$purpose[datb$purpose1=="G. Walking"]<-"Walking"
datb$purpose[datb$purpose1=="H. General beach use (e.g., tidepooling, picnicking, etc.)"]<-"General Beach Use"
datb$purpose[datb$purpose1=="J. Other"]<-"Other"

datb$CAMPAaware<-NA
datb$CAMPAaware[datb$CA.MPA.aware==TRUE]<-"Aware of MPAs"
datb$CAMPAaware[datb$CA.MPA.aware==FALSE]<-"Not aware of MPAs"

datb.narm<-subset(datb,!is.na(datb$CA.MPA.aware))



purpose.mpaaware<-as.data.frame(table(datb$purpose,datb$CAMPAaware))

purp<-ggplot(datb.narm,
		aes(x=purpose,fill=purpose))+
		geom_bar()+
		ggtitle(paste("Primary purpose of visit (N=",nrow(datb.narm),")",sep=""))+
		theme(legend.position="none")+
		labs(x="")+
		theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
		scale_fill_viridis_d()
purp
ggsave("VisitPrimaryPurpose.png", units="in", width=8,height=4,dpi=300)

purp.mpa<-ggplot(purpose.mpaaware,
		aes(x=Var1,y=Freq, fill=Var2))+
		geom_bar(stat="identity",position="dodge")+
		ggtitle(paste("Primary purpose of visit (N=",nrow(datb.narm),")",sep=""))+
		theme(legend.position="bottom")+
		labs(x="",y="Count",fill="")+
		theme(axis.text.x = element_text(angle = 67.5, vjust = 1, hjust=1))+
		scale_fill_manual(
		name="",
		labels=c("Aware of CA MPAs","Unaware of CA MPAs"),
		values=viridis(4)[3:2])+
	scale_color_manual(
		name="",
		labels=c("Aware of CA MPAs","Unaware of CA MPAs"),
		values=viridis(4)[3:2])+
		geom_text(aes(label = Freq),position = position_dodge(.9), vjust = -0.2,fontface="bold")
purp.mpa
ggsave("VisitPrimaryPurposeByMPAaware.png", units="in", width=8,height=6,dpi=300)



purp.mpa.mos<-ggplot(purpose.mpaaware)+
		aes(x=product(Var1,Freq), fill=Var2)+
		geom_mosaic()+
		ggtitle(paste("Primary purpose of visit (N=",nrow(datb.narm),")",sep=""))+
		theme(legend.position="bottom")+
		labs(x="",y="Count",fill="")+
		theme(axis.text.x = element_text(angle = 67.5, vjust = 1, hjust=1))+
		scale_fill_manual(
		name="",
		labels=c("Aware of CA MPAs","Unaware of CA MPAs"),
		values=viridis(4)[3:2])+
	scale_color_manual(
		name="",
		labels=c("Aware of CA MPAs","Unaware of CA MPAs"),
		values=viridis(4)[3:2])+
		geom_text(aes(label = Freq),position = position_dodge(.9), vjust = -0.2,fontface="bold")
purp.mpa.mos
ggsave("VisitPrimaryPurposeByMPAaware_Mosiac.png", units="in", width=8,height=6,dpi=300)


datb$home<-factor(datb$Q11...Q1..Did.you.start.your.trip.from.home.today.or.another.location....Selected.Choice)

#how to parse answers that aren't numeric
#  e.g. take out 'first timers', recode 3-4 to be 3.5, clean up '4 days' or 'about 30'
#I have just decided to manually correct this.  
datb$visitdays<-as.numeric(as.character(datb$VisitDaysManuallyCleaned))
datb.narm<-subset(datb,!is.na(datb$visitdays)&!is.na(datb$CA.MPA.aware))
 
visitdays<-ggplot(datb.narm,aes(x=visitdays,color=CA.MPA.aware,fill=CA.MPA.aware))+
	geom_histogram(position="dodge")+
	ggtitle(paste("Number of Coastal Visit Days (N=",nrow(datb.narm),")",sep=""))+
	scale_fill_manual(
		name="",
		labels=c("Unaware of CA MPAs","Aware of CA MPAs"),
		values=viridis(4)[2:3])+
	scale_color_manual(
		name="",
		labels=c("Unaware of CA MPAs","Aware of CA MPAs"),
		values=viridis(4)[2:3])+
		labs(x="")+
	theme(legend.position="bottom")
visitdays
ggsave("VisitDays.png", units="in", width=8,height=4,dpi=300)




# predictors


datb$year<-datb$Q25...Q12..What.year.were.you.born.......If.the.participant..declined.to.respond...please.write.that.in.the.text.box.below.
datb$year.num<-as.numeric(datb$year)
datb$age<-2024-datb$year.num
datb.narm<-subset(datb,!(is.na(datb$CA.MPA.aware)))

age<-ggplot(datb.narm,aes(x=age,color=CA.MPA.aware,fill=CA.MPA.aware))+
	geom_histogram(position="dodge")+
	ggtitle(paste("Visitor Age (N=",nrow(datb.narm),")",sep=""))+
	scale_fill_manual(
		name="",
		labels=c("Unaware of CA MPAs","Aware of CA MPAs"),
		values=viridis(4)[2:3])+
	scale_color_manual(
		name="",
		labels=c("Unaware of CA MPAs","Aware of CA MPAs"),
		values=viridis(4)[2:3])+
		labs(x="")+
	theme(legend.position="bottom")
age
ggsave("VisitorAge.png", units="in", width=8,height=4,dpi=300)


datb$site<-factor(datb$Q6...Site.transect.name)
#a variable for which program it was
datb$program<-NA
datb$program[grep("(Orange County)",datb$site)]<-"Coastkeeper"
datb$program[grep("(San Diego)",datb$site)]<-"Wildcoast"


datb$gender<-factor(datb$Q26...Q13..How.do.you.identify.your.gender....Selected.Choice)

datb.narm<-subset(datb,!is.na(datb$CA.MPA.aware)&datb$gender!="")
datb.narm$gender<-factor(datb.narm$gender)

gender<-as.data.frame(table(datb.narm$gender,datb.narm$CAMPAaware))

gend<-ggplot(gender,
		aes(x=Var1,y=Freq, fill=Var2))+
		geom_bar(stat="identity",position="dodge")+
		ggtitle(paste("Gender Identity (N=",nrow(datb.narm),")",sep=""))+
		theme(legend.position="bottom")+
		labs(x="",y="Count",fill="")+
	scale_fill_manual(
		name="",
		labels=c("Aware of CA MPAs","Unaware of CA MPAs"),
		values=viridis(4)[3:2])+
	scale_color_manual(
		name="",
		labels=c("Aware of CA MPAs","Unaware of CA MPAs"),
		values=viridis(4)[3:2])+
	geom_text(aes(label = Freq),position = position_dodge(.9), vjust = -0.2,fontface="bold")

gend
ggsave("GenderMPAawareness.png", units="in", width=8,height=4,dpi=300)



datb$race.ethn<-factor(datb$Q27...Q14..How.do.you.identify.your.race.ethnicity..Check.all.that.apply....Selected.Choice)
datb.MPAyes<-subset(datb,datb$CA.MPA.aware==TRUE)
datb.MPAno<-subset(datb,datb$CA.MPA.aware==FALSE)


datb.MPAyes$AmIndAlNat<-FALSE
datb.MPAyes$AmIndAlNat[grep("A.",datb.MPAyes$race.ethn)]<-TRUE

datb.MPAyes$Asian<-FALSE
datb.MPAyes$Asian[grep("B.",datb.MPAyes$race.ethn)]<-TRUE

datb.MPAyes$Black<-FALSE
datb.MPAyes$Black[grep("C.",datb.MPAyes$race.ethn)]<-TRUE

datb.MPAyes$HispLat<-FALSE
datb.MPAyes$HispLat[grep("D.",datb.MPAyes$race.ethn)]<-TRUE

datb.MPAyes$NatHawPI<-FALSE
datb.MPAyes$NatHawPI[grep("E.",datb.MPAyes$race.ethn)]<-TRUE

datb.MPAyes$Wht<-FALSE
datb.MPAyes$Wht[grep("F.",datb.MPAyes$race.ethn)]<-TRUE

datb.MPAyes$Multi<-FALSE
datb.MPAyes$Multi[grep("G.",datb.MPAyes$race.ethn)]<-TRUE

datb.MPAyes$Other<-FALSE
datb.MPAyes$Other[grep("H.",datb.MPAyes$race.ethn)]<-TRUE

datb.MPAyes$DTR<-FALSE
datb.MPAyes$DTR[grep("I.",datb.MPAyes$race.ethn)]<-TRUE

race.eth.MPAyes<-data.frame(RaEth=c("American Indian or Alaska Native",
									"Asian",
									"Black or African American",
									"Hispanic or Latinx",
									"Native Hawaiian or other Pacific Islander",
									"White",
									"Multi-Racial",
									"Other",
									"Decline to Respond"),
					   Count=c(sum(datb.MPAyes$AmIndAlNat),
						 	  sum(datb.MPAyes$Asian),
							  sum(datb.MPAyes$Black),
							  sum(datb.MPAyes$HispLat),							  
							  sum(datb.MPAyes$NatHawPI),
							  sum(datb.MPAyes$Wht),
							  sum(datb.MPAyes$Multi),
							  sum(datb.MPAyes$Other),
							  sum(datb.MPAyes$DTR)
							  ) )

plt.b<-ggplot(race.eth.MPAyes,
		aes(x=RaEth,y=Count,fill=RaEth))+
		geom_bar(stat="identity",position=position_dodge())+
		ggtitle(paste("Race/Ethnicity for those aware of CA MPAs (N=",nrow(datb.MPAyes),")",sep=""))+
		theme(legend.position="none")+
		scale_fill_viridis_d()+
		labs(x="")+
		theme(axis.text.x = element_text(angle = 67.5, vjust = 1, hjust=1))+
		geom_text(aes(label = Count),position = position_dodge(.9), vjust = -0.2,fontface="bold")

plt.b
ggsave("RaceEthnicity_MPAyes.png", units="in", width=8,height=6,dpi=300)



datb.MPAno$AmIndAlNat<-FALSE
datb.MPAno$AmIndAlNat[grep("A.",datb.MPAno$race.ethn)]<-TRUE

datb.MPAno$Asian<-FALSE
datb.MPAno$Asian[grep("B.",datb.MPAno$race.ethn)]<-TRUE

datb.MPAno$Black<-FALSE
datb.MPAno$Black[grep("C.",datb.MPAno$race.ethn)]<-TRUE

datb.MPAno$HispLat<-FALSE
datb.MPAno$HispLat[grep("D.",datb.MPAno$race.ethn)]<-TRUE

datb.MPAno$NatHawPI<-FALSE
datb.MPAno$NatHawPI[grep("E.",datb.MPAno$race.ethn)]<-TRUE

datb.MPAno$Wht<-FALSE
datb.MPAno$Wht[grep("F.",datb.MPAno$race.ethn)]<-TRUE

datb.MPAno$Multi<-FALSE
datb.MPAno$Multi[grep("G.",datb.MPAno$race.ethn)]<-TRUE

datb.MPAno$Other<-FALSE
datb.MPAno$Other[grep("H.",datb.MPAno$race.ethn)]<-TRUE

datb.MPAno$DTR<-FALSE
datb.MPAno$DTR[grep("I.",datb.MPAno$race.ethn)]<-TRUE

race.eth.MPAno<-data.frame(RaEth=c("American Indian or Alaska Native",
									"Asian",
									"Black or African American",
									"Hispanic or Latinx",
									"Native Hawaiian or other Pacific Islander",
									"White",
									"Multi-Racial",
									"Other",
									"Decline to Respond"),
					   Count=c(sum(datb.MPAno$AmIndAlNat),
						 	  sum(datb.MPAno$Asian),
							  sum(datb.MPAno$Black),
							  sum(datb.MPAno$HispLat),
							  sum(datb.MPAno$NatHawPI),
							  sum(datb.MPAno$Wht),
							  sum(datb.MPAno$Multi),
							  sum(datb.MPAno$Other),
							  sum(datb.MPAno$DTR)
							  ) )

plt.b<-ggplot(race.eth.MPAno,
		aes(x=RaEth,y=Count,fill=RaEth))+
		geom_bar(stat="identity",position=position_dodge())+
		ggtitle(paste("Race/Ethnicity for those not aware of CA MPAs (N=",nrow(datb.MPAno),")",sep=""))+
		theme(legend.position="none")+
		scale_fill_viridis_d()+
		labs(x="")+
		theme(axis.text.x = element_text(angle = 67.5, vjust = 1, hjust=1))+
		geom_text(aes(label = Count),position = position_dodge(.9), vjust = -0.2,fontface="bold")

plt.b
ggsave("RaceEthnicity_MPAno.png", units="in", width=8,height=6,dpi=300)


#for trying to count how many indicated multiple
gsub("[^,]", "", "D. Hispanic or Latinx,F. White")
gsub("[^,]", "", "D. Hispanic or Latinx,F. White,G. Plaid")
#how do we get string length though?


datb$lang<-factor(datb$Q28...Q15..What.language.s..do.you.speak.at.home....Selected.Choice)


#datb$Q12_1_TEXT...Q2..Where.is.home.for.you....Zip.code...Text
#(23 should be NAs because they're declined or otehr countries)

datb$ZIP<-datb$HomeZipManuallyEdited 
#someone said, 'santee, CA' so I put in 92071, which is in SD county. the other santee zip code is 92072, which is 'mostly in sd county' - so this will sort into SD county
#someone put 'nashville' so I'm just going to put a random nashville ZIP, because it will sort into 'other US State'
#someone put 6840, and 96840 doesn't exist in CA, so this is probably a typo for someone out of state.  I will manually just call it 68400
datb$otherloc<-datb$Q12_2_TEXT...Q2..Where.is.home.for.you....Or.non.US.country...Text

#california zip codes from gis.data.ca.gov
#manually sorted them, they range from 90001 to 96162


sd.zips<-c(91901,91902,91905,91906,91910,91911,92028,92029,92036,92058,92058,91913,91914,91915,91916,91934,91935,91941,91942,91945,91917,91931,91932,91948,91950,91962,92058,92058,92058,91963,91977,91978,91980,92003,92027,92004,92007,92008,92009,92010,92011,92014,92019,92020,92021,92024,92025,92026,92037,92037,92040,92054,92055,92056,92057,92058,92058,92058,92059,92060,92061,92064,92065,92083,92083,92084,92086,92091,92155,92066,92067,92069,92070,92071,92075,92078,92081,92082,92093,92161,92096,92101,92102,92103,92104,92123,92124,92105,92106,92107,92108,92109,92110,92111,92113,92114,92115,92116,92117,92118,92119,92120,92121,92136,92139,92122,92140,92145,92154,92126,92127,92128,92129,92130,92131,92134,92135,92173,92182,92259,92536,92672) #from San diego county

oc.zips<-c(92683,92804,92704,92805,90631,92801,92703,92677,92630,92627,92647,92530,92780,92708,92620,92707,92646,92618,92840,92870,92833,92656,92626,92705,92701,90630,92843,92886,90620,92691,92692,92821,92648,92688,92867,92802,92806,92807,92675,92679,92831,92649,92672,92841,92869,90621,92660,92706,92883,92612,92602,92673,92604,92653,92835,92614,92868,90680,92629,92782,92694,92606,90740,92832,92651,92844,90720,92663,92887,92865,92808,92603,92637,92617,92845,92866 ,90623,92625,92610,92657,92861,92624,92655,92823,92676,92661,92662,92697,92698,92710,92725,92863,92628,92616,92619,92678,92684,92799,92822,92857,92871,90633,92709,92623,92650,92652,92654,92659,92658,92674,92685,92690,92693,92702,92711,92712,92735,92728,92781,92803,92811,92809,92814,92812,92816,92815,92817,92825,92834,92837,92836,92838,92842,92846,92856,92850,92859,92862,92864,92885,92899,90622,90624,90632,90721,90743,90742,92605,92609,92607,92615) #from Zillow

datb$loc<-NA
datb$loc[datb$ZIP %in% sd.zips]<- "San Diego County"
datb$loc[datb$ZIP %in% oc.zips]<- "Orange County"
datb$loc[(datb$ZIP>=90001)&(datb$ZIP<=96162)&!(datb$ZIP %in% sd.zips)&!(datb$ZIP %in% oc.zips)]<- "Other California County"
datb$loc[datb$otherloc!=""]<-"International"
datb$loc[!is.na(datb$ZIP)&( (datb$ZIP<90001)|(datb$ZIP>96162) )]<-"Non-CA US State"
datb$loc[datb$Q12...Q2..Where.is.home.for.you....Selected.Choice=="Declined to respond"]<-"Declined to respond"

#datb$Q12_1_TEXT...Q2..Where.is.home.for.you....Zip.code...Text[is.na(datb$loc)]
#datb$datb$Q12_1_TEXT...Q2..Where.is.home.for.you....Zip.code...Text[is.na(datb$loc)&datb$Q12...Q2..Where.is.home.for.you....Selected.Choice=="Zip code"]

datb.narm<-subset(datb,!is.na(datb$loc))

locations<-as.data.frame(table(datb.narm$loc))

loc.plt<-ggplot(locations,
		aes(x=factor(Var1),y=Freq,fill=factor(Var1)))+
		geom_bar(stat="identity")+
		ggtitle(paste("Home locations of visitors (N=",nrow(datb.narm),")",sep=""))+
		theme(legend.position="none")+
		labs(x="")+
		scale_fill_viridis_d()+
		geom_text(aes(label=Freq),position = position_dodge(.9), vjust = -0.2,fontface="bold")

loc.plt
ggsave("VisitorLocations.png", units="in", width=8,height=4,dpi=300)


