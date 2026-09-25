start.time<- Sys.time()


require(glmmTMB)
require(DHARMa)
require(ggplot2)
library(svglite)

require(multcomp)
library(xtable)
library(lubridate)
library(scales)

##################################################################
###########  FUNCTIONS ###########################################
##################################################################


remove.nas<-function(dataset, vars, resp){
  #currently this checks for NA values in response variable and fixed effect
  #predictors, but not random effects predictors
  if(length(vars)==1){
    dataset<-dataset[!is.na(dataset[vars]),]
    dataset<-dataset[!is.na(dataset[resp]),]
    return(dataset)
  }

  for (i in 1:length(vars))
    dataset<-dataset[!is.na(dataset[vars[i]]),]
    dataset<-dataset[!is.na(dataset[resp]),]
    return(dataset)
}


runDHARMaComparison<-function(mdname,act,cvars,zvars,dvars,dset,dstr,cts, an,dp1scp){

  allvars<-unique(c(cvars,zvars,dvars))
  #remove random effects (search for |) and interactions (search for *)
  #also powers (search for ^) and "NONE" if there is no zero-inflation
  remove<-c(allvars[grep("\\||\\*|\\^",allvars)],"NONE")
  allvars<-allvars[!allvars%in%remove]

  datclean<-remove.nas(dset,allvars,act)
  datclean.df<-as.data.frame(datclean)

#in case you need to force year to be a factor
  datclean.df$year<-factor(datclean.df$year)

  ctform<-as.formula(paste(act," ~ ",paste(cvars, collapse=" + "),collapse="") )

  if(!any(zvars=="NONE")){ 
    #for all models with zero-inflation
    if(dstr=="nbinom1" | dstr=="nbinom2"){ # these distributions have dispersion
      if(!is.null(zvars)) { # put in the variables or just have a basic intercept
        ziform<-as.formula(paste(" ~ ",paste(zvars, collapse=" + "),collapse="") )
        } else {
          ziform <-as.formula(paste(" ~ 1"))  
        }
      if(!is.null(dvars)) { # put in the variables or just have a basic intercept
        disform<-as.formula(paste(" ~ ",paste(dvars, collapse=" + "),collapse="") )
        } else {
          disform <-as.formula(paste(" ~ 1"))
        }
      time.mod<-system.time(mod<-tryCatch(glmmTMB(ctform, family=dstr,ziformula=ziform,
        dispformula=disform,dat=datclean.df,contrasts=cts) , 
        error=function(e) return(e$message)  ) )

    } else { #if no dispersion, then no dsvars or disform
      if(!is.null(zvars)) { #but still specify the variables or a basic intercept for zero inflation
        ziform<-as.formula(paste(" ~ ",paste(zvars, collapse=" + "),collapse="") )
        } else {
          ziform <-as.formula(paste(" ~ 1"))  
        }
      time.mod<-system.time(mod<-tryCatch(glmmTMB(ctform, family=dstr,ziformula=ziform,dat=datclean.df,contrasts=cts) , 
      error=function(e) return(e$message)  ) )
    }

  } else { # for models with no zero-inflation, but do include dispersion
      if(!is.null(dvars)) {
        disform<-as.formula(paste(" ~ ",paste(dvars, collapse=" + "),collapse="") )
        } else {
          disform <-as.formula(paste(" ~ 1"))
        }
        time.mod<-system.time(mod<-tryCatch(glmmTMB(ctform, family=dstr,
        dispformula=disform,dat=datclean.df,contrasts=cts) , 
        error=function(e) return(e$message)  ) )

  }

  if(class(mod)=="character") {
    modSts<-c(Name=mdname,ModTime=time.mod[[3]]/60,SimTime=NA,
                      Uniformity=NA,Outliers=NA,Dispersion=NA,
                      Quantiles=NA,Zeroinflation=NA, Convergence=mod)
    sim<-NA
    return(list(modSts,mod,sim))
    }

  time.mod/60
  time.sim<-system.time(sim<-simulateResiduals(mod, plot=F))
  time.sim/60

  par(mfrow=c(5,6))

  Un<-testUniformity(sim)
  Ou<-testOutliers(sim, type="bootstrap",plot=F)
  Dis<-testDispersion(sim)
  Zi<-testZeroInflation(sim)
  Qu<-testQuantiles(sim)

  modSts<-c(Name=paste(act,mdname,sep="-"),ModTime=time.mod[[3]]/60,SimTime=time.sim[[3]]/60,
                      Uniformity=Un$p.value,Outliers=Ou$p.value,Dispersion=Dis$p.value,
                      Quantiles=Qu$p.value,Zeroinflation=Zi$p.value,Convergence=mod$fit$message)


  plotResiduals(sim)
  mtext("AllResiduals",side=3,line=2.5)
  for (i in 1:length(allvars)){
    vr<-datclean.df[,allvars[i]]
    plotResiduals(sim,form=vr)
    mtext(allvars[i],side=3,line=2.5)
  }

  plot(x=0,y=0,main=paste(act,mdname,sep="-"))

  if(!(is.null(an))){
    print("including anova-based 'drop1'")
    stats<-drop1(mod,scope=dp1scp,test="Chisq",trace=TRUE)
    return(list(Stats=modSts,fitTMB=mod,DHARMa=sim,drop1=stats))
  } else {
    print("no anova-based 'drop1'")
    return(list(Stats=modSts,fitTMB=mod,DHARMa=sim))    
  }

}

readInModelResultsAndPlot<-function(ds,act,res){

  modSt<-data.frame(Name=character(),ModTime=numeric(),SimTime=numeric(),
                      Uniformity=numeric(),Outliers=numeric(),Dispersion=numeric(),
                      Quantiles=numeric(),Zeroinflation=numeric(),Convergence=character(),stringsAsFactors=FALSE)

  for(i in 1:length(res)){
    m<-names(res)[i]
    modSt[i,]<-res[[m]][[1]]
  }

  pdf(paste(act,"_resVars.pdf",sep=""),width=27,height=16)
  for (i in 1:length(res)){
    print(names(res)[i])
    par(mfrow=c(5,6))
    U<-testUniformity(res[[i]]$DHARMa)
    O<-testOutliers(res[[i]]$DHARMa, type="bootstrap",plot=F)
    modSt$Outliers[i]<-O$p.value
    D<-testDispersion(res[[i]]$DHARMa)
    Z<-testZeroInflation(res[[i]]$DHARMa)
    Q<-testQuantiles(res[[i]]$DHARMa)
    print("Get names of variables included in model")
    cv<-as.character(attr(res[[i]]$fitTMB$modelInfo$terms$cond$fixed,"variables"))[-1]
    zv<-as.character(attr(res[[i]]$fitTMB$modelInfo$terms$zi$fixed,"variables"))[-1]
    dv<-as.character(attr(res[[i]]$fitTMB$modelInfo$terms$disp$fixed,"variables"))[-1]

    allv<-unique(c(cv,zv,dv))
    #remove random effects (search for |) and interactions (search for *) and exponential terms (search for ^)
    rem<-c(act,allv[grep("\\||\\*|\\^",allv)])
    allv<-allv[!allv%in%rem]

    datcl<-remove.nas(ds,allv,act)
    datcl.df<-as.data.frame(datcl)

    print("Plot residuals")
    plotResiduals(res[[i]]$DHARMa)
    mtext("AllResiduals",side=3,line=2.5)
    for (j in 1:length(allv)){
      v<-datcl.df[,allv[j]]
      plotResiduals(res[[i]]$DHARMa,form=v)
      mtext(allv[j],side=3,line=2.5)
    }
  }
  plot(x=0,y=0,main=modSt$Name[i])
  dev.off()

  print(act)
  print(modSt)
  write.csv(modSt,paste(act,"_modStats.csv",collapse=""))

}

#------------ END modeling functions  --------------------------------------

#------- p-value assembly function ----

compile_p_values<-function(res){
  act<- formula(res$fitTMB)[2]
  print(act)
  #pull p-values from summary (all three sets, as long as they're not NULL)
  print("Getting conditional model p-values")
  pvals_summary_cond<-(summary(res$fitTMB))$coefficients$cond[,"Pr(>|z|)"]
  pvals_summary_cond_vars<-(summary(res$fitTMB))$coefficients$cond[,"Estimate"]
  pvals_summary_cond_SE<-(summary(res$fitTMB))$coefficients$cond[,"Std. Error"]  
  pvals<-data.frame(Vars=names(pvals_summary_cond),Vals=pvals_summary_cond_vars,SE=pvals_summary_cond_SE,p=as.numeric(pvals_summary_cond))
  if(!is.null((summary(res$fitTMB))$coefficients$zi[,"Pr(>|z|)"])){
    print("Getting zero-inflation p-values")
    pvals_summary_zi<-(summary(res$fitTMB))$coefficients$zi[,"Pr(>|z|)"]
    pvals_summary_zi_vars<-(summary(res$fitTMB))$coefficients$zi[,"Estimate"]
    pvals_summary_zi_SE<-(summary(res$fitTMB))$coefficients$zi[,"Std. Error"]  
    if(is.null(names(pvals_summary_zi))){
      names(pvals_summary_zi)<-"zi-(Intercept)"
    } else {
      names(pvals_summary_zi)<-sub("^","zi-",names(pvals_summary_zi)) 
    }
    print(pvals_summary_zi)
    pvals<-rbind(pvals,cbind(Vars=names(pvals_summary_zi),Vals=pvals_summary_zi_vars,SE=pvals_summary_zi_SE,p=as.numeric(pvals_summary_zi)))
  }

  if(!is.null((summary(res$fitTMB))$coefficients$disp[,"Pr(>|z|)"])){
    print("Getting dispersion p-values")
    pvals_summary_disp<-(summary(res$fitTMB))$coefficients$disp[,"Pr(>|z|)"]
    pvals_summary_disp_vars<-(summary(res$fitTMB))$coefficients$disp[,"Estimate"]
    pvals_summary_disp_SE<-(summary(res$fitTMB))$coefficients$disp[,"Std. Error"]  
    names(pvals_summary_disp)<-sub("^","disp-",names(pvals_summary_disp))
    print(pvals_summary_disp)
    pvals<-rbind(pvals,cbind(Vars=names(pvals_summary_disp),Vals=pvals_summary_disp_vars,SE=pvals_summary_disp_SE,p=as.numeric(pvals_summary_disp)))
  }

  pvals$Source<-"Wald"

  print("Collecting variable names for multiple comparisons")
  modvars<-names(res$fitTMB$frame)



  #pull p-vals from multiple comparisons
  if("MPA_Take"%in%modvars) {
    print("Removing MPA_Take from p-value list")
    pvals<-pvals[!(pvals$Vars %in% grep("^MPA_Take",pvals$Vars,value=TRUE)) ,]
    print("Getting MPA_Take multiple comparison p-values")

    pvals_var<-glht(res$fitTMB,linfct = mcp(MPA_Take = "Tukey"))
    ps<-cbind( Vars=rownames(summary(pvals_var)$linfct), Vals= summary(pvals_var)$test$coefficients, SE= summary(pvals_var)$test$sigma, p=summary(pvals_var)$test$pvalues,
        Source=rep("glht Tukey",length(summary(pvals_var)$test$pvalues)))
    rownames(ps)<-NULL
    pvals<-rbind(pvals,ps)

    png(paste(act,"MPATake_",datename,".png",sep=""),res=300,width=1500,height=1200)
    par(mai=c(1.02,2.2,0.82,0.42))
    plot(pvals_var)
    mtext(as.character(act),side=3,line=0.5)
    dev.off()

  }
  if("beach_type"%in%modvars) {
    print("Removing beach_type from p-value list")
    pvals<-pvals[!(pvals$Vars %in% grep("^beach_type",pvals$Vars,value=TRUE)) ,]
    print("Getting beach_type multiple comparison p-values")
    pvals_var<-glht(res$fitTMB,linfct = mcp(beach_type = "Tukey"))
    ps<-cbind( Vars=rownames(summary(pvals_var)$linfct), Vals= summary(pvals_var)$test$coefficients, SE= summary(pvals_var)$test$sigma, p=summary(pvals_var)$test$pvalues,
      Source=rep("glht Tukey",length(summary(pvals_var)$test$pvalues)))
    rownames(ps)<-NULL
    pvals<-rbind(pvals,ps)

    png(paste(act,"beachType_",datename,".png",sep=""),res=300,width=1500,height=1200)
    par(mai=c(1.02,2.2,0.82,0.42))
    plot(pvals_var)
    mtext(as.character(act),side=3,line=0.5)
    dev.off()
  }
  if("clouds"%in%modvars) {
    print("Removing clouds from p-value list")
    pvals<-pvals[!(pvals$Vars %in% grep("^clouds",pvals$Vars,value=TRUE)) ,]
    print("Getting clouds multiple comparison p-values")
    pvals_var<-glht(res$fitTMB,linfct = mcp(clouds = "Tukey"))
    ps<-cbind( Vars=rownames(summary(pvals_var)$linfct), Vals= summary(pvals_var)$test$coefficients, SE= summary(pvals_var)$test$sigma, p=summary(pvals_var)$test$pvalues,
      Source=rep("glht Tukey",length(summary(pvals_var)$test$pvalues)))
    rownames(ps)<-NULL
    pvals<-rbind(pvals,ps)

    png(paste(act,"clouds_",datename,".png",sep=""),res=300,width=1500,height=1200)
    par(mai=c(1.02,2,0.82,0.42))
    plot(pvals_var)
    mtext(as.character(act),side=3,line=0.5)
    dev.off()
  }
  if("visibility"%in%modvars) {
    print("Removing visibility from p-value list")
    pvals<-pvals[!(pvals$Vars %in% grep("^visibility",pvals$Vars,value=TRUE)) ,]
    print("Getting visibility multiple comparison p-values")
    pvals_var<-glht(res$fitTMB,linfct = mcp(visibility = "Tukey"))
    ps<-cbind( Vars=rownames(summary(pvals_var)$linfct), Vals= summary(pvals_var)$test$coefficients, SE= summary(pvals_var)$test$sigma, p=summary(pvals_var)$test$pvalues,
      Source=rep("glht Tukey",length(summary(pvals_var)$test$pvalues)))
    rownames(ps)<-NULL
    pvals<-rbind(pvals,ps)

    png(paste(act,"visibility_",datename,".png",sep=""),res=300,width=1500,height=1200)
    par(mai=c(1.02,2,0.82,0.42))
    plot(pvals_var)
    mtext(as.character(act),side=3,line=0.5)
    dev.off()
  }
  if("day_of_week"%in%modvars) {
    print("Removing day_of_week from p-value list")
    pvals<-pvals[!(pvals$Vars %in% grep("^day_of_week",pvals$Vars,value=TRUE)) ,]
    print("Getting day_of_week multiple comparison p-values")
    pvals_var<-glht(res$fitTMB,linfct = mcp(day_of_week = "Tukey"))
    ps<-cbind( Vars=rownames(summary(pvals_var)$linfct), Vals= summary(pvals_var)$test$coefficients, SE= summary(pvals_var)$test$sigma, p=summary(pvals_var)$test$pvalues,
      Source=rep("glht Tukey",length(summary(pvals_var)$test$pvalues)))
    rownames(ps)<-NULL
    pvals<-rbind(pvals,ps)

    png(paste(act,"dayOfWeek_",datename,".png",sep=""),res=300,width=1500,height=2400)
    par(mai=c(1.02,1.2,0.82,0.42))
    plot(pvals_var)
    mtext(as.character(act),side=3,line=0.5)
    dev.off()
  }

if("year"%in%modvars) {
    print("Removing year from p-value list")
    pvals<-pvals[!(pvals$Vars %in% grep("^year",pvals$Vars,value=TRUE)) ,]
    print("Getting year multiple comparison p-values")
    pvals_var<-glht(res$fitTMB,linfct = mcp(year = "Tukey"))
    ps<-cbind( Vars=rownames(summary(pvals_var)$linfct), Vals= summary(pvals_var)$test$coefficients, SE= summary(pvals_var)$test$sigma, p=summary(pvals_var)$test$pvalues,
      Source=rep("glht Tukey",length(summary(pvals_var)$test$pvalues)))
    rownames(ps)<-NULL
    pvals<-rbind(pvals,ps)

    png(paste(act,"year_",datename,".png",sep=""),res=300,width=1500,height=2400)
    par(mai=c(1.02,1.2,0.82,0.42))
    plot(pvals_var)
    mtext(as.character(act),side=3,line=0.5)
    dev.off()
  }

  #make this a column in a data frame
  pvals$padj<-p.adjust(pvals$p,"fdr")

  return(pvals)
}

#------- End p-value assembly function ----

#------------ Begin plotting functions  --------------------------------------


generatePredictions<-function(actv,resu,ndat,tp,PopLevel){
  #tp is "response" or "link" or "conditional"
  rane<-ifelse(PopLevel=="Pop",NA,NULL) #Otherwise do individual estimates from random effects
  print(rane)
  pred<-predict(resu,newdata=ndat,re.form=rane,type=tp,se.fit=TRUE)#,fast=TRUE) # apparently can't do fast with Pop pred
  print(paste("Prediction complete: ",actv,collapse=""))
  out<-cbind(activity=rep(actv,nrow(ndat)),
    predCount=as.numeric(pred$fit),
    PredLow=as.numeric(pred$fit)-2*as.numeric(pred$se.fit),
    PredHigh=as.numeric(pred$fit)+2*as.numeric(pred$se.fit),
    ndat)
  return(out)
}

#adjusting this one to include the predictive intervals, for the manuscript
generatePlots_SinglePage_TimeMPA<-function(predDat){

  ppp<-ggplot(data=predDat,aes(x=year,y=predCount,
                           ymin=PredLow,ymax=PredHigh,
                           col=MPA_Take))+
    geom_line(linewidth=2)+
    ylab("Estimated Counts")+
    xlab("")+
    theme(legend.position="bottom")+
    labs(color=NULL)+
    geom_ribbon(alpha=0.25)+
    scale_color_viridis_d(end=0.8)+
    scale_fill_viridis_d(end=0.8)+
    facet_wrap(~predDat$activity,scales="free")
  return(ppp)
}


generatePlots_Seasonality_WithFacets<-function(d,v){
ggp <- ggplot(d, aes(day_of_year_POSIXct, predCount)) +
  geom_line(linewidth=2) +
  ylab("Predicted Counts")+
  xlab("")+
#  geom_ribbon(alpha=0.25)+
  scale_x_datetime(labels = date_format("%b"))+#,date_breaks="1 month")+
      #,breaks = as.POSIXct(c("2013-01-01","2013-04-01","2013-07-01","2013-10-01","2013-12-01")))+
      #I wanted Dec at the end, but printing all months is too crowded, and if I don't use default it looks uneven
  facet_wrap(~ activity,scales="free")

ggp<-ggp +  geom_vline(data = v,
             aes(xintercept = MaxPOSIXct),linewidth=1.5,linetype="dashed")
return(ggp)
}


generatePlots_Diurnal_WithFacets<-function(d,v){
ggp <- ggplot(d, aes(todPOSIXct, predCount, ymin=PredLow)) +
  geom_line(linewidth=2) +
  ylab("Predicted Counts")+
  xlab("")+
  scale_x_datetime(labels = date_format("%l %p"),date_breaks="6 hours")+
  facet_wrap(~ activity,scales="free")

ggp<-ggp +  geom_vline(data = v,
             aes(xintercept = MaxHMSPOSIXct),linewidth=1.5,linetype="dashed")
return(ggp)
}

generateSpatialPredictions<-function(res,alldat,typ){
    modvrs<-names(res$fitTMB$frame)
    #remove random effects (search for |) and interactions (search for *)
    #also powers (search for ^) and "NONE" if there is no zero-inflation
    remove<-c(modvrs[grep("\\||\\*|\\^",modvrs)],"NONE")
    modvrs<-modvrs[!modvrs%in%remove]
    datclean<-remove.nas(alldat,modvrs,modvrs[1])
    datclean.df<-as.data.frame(datclean)

    Spatial<-unique(datclean.df[,c("s_access","s_popden","s_lat","s_length_miles","adjacent_to_park",
      "beach_type","has_tidepooling","MPA_Take","program","site_name","transect_name","survey_site_type","site_lat_centroid")])

    #removing from the display any of the transects with fewer than 10 surveys
    survey_counts<-as.data.frame(table(surv$transect_name))
    too_few<-survey_counts$Var1[survey_counts$Freq<10]
    Spatial<-Spatial[!(Spatial$transect_name%in%too_few),]

    Spatial$beach_type<-as.character(Spatial$beach_type)
    Spatial$has_tidepooling<-as.character(Spatial$has_tidepooling)
    Spatial$MPA_Take<-as.character(Spatial$MPA_Take)
    Spatial$program<-as.character(Spatial$program)
    Spatial$site_name<-as.character(Spatial$site_name)
    Spatial$transect_name<-as.character(Spatial$transect_name)
    Spatial$survey_site_type<-as.character(Spatial$survey_site_type)
    Spatial$adjacent_to_park<-as.character(Spatial$adjacent_to_park)
    Spatial$clouds<-rep("clear",nrow(Spatial))
    Spatial$precipitation<-rep("no",nrow(Spatial))
    Spatial$visibility<-rep("perfect",nrow(Spatial))
    Spatial$tide_standard<-rep(0,nrow(Spatial))
    Spatial$day_of_week<-rep("Sat",nrow(Spatial))
    Spatial$winterness<-rep(-1,nrow(Spatial)) #summer solstice
    Spatial$springness<-rep(0,nrow(Spatial)) #summer solstice
    #Spatial$days_since_first_survey<-rep(-1643, nrow(Spatial)) #halfway through, corresponding to halfway through 2016
    Spatial$s_tod<-rep(
      (as.numeric(hms("12:00:00"))-mean(as.numeric(surv$time_start)) )
        /sd(as.numeric(surv$time_start))
        ,nrow(Spatial))
    Spatial$s_duration<-rep(
      as.numeric((60-mean(surv$duration))/sd(surv$duration))
        ,nrow(Spatial)) # should be the one-hour duration
    Spatial$year<-rep("2016",nrow(Spatial))

    Spatial$predCount<-predict(res$fitTMB,newdata=Spatial,re.form=NULL,type=typ)#always do random effects estimation

    return(Spatial)
}


#------------ END plotting functions  --------------------------------------

######################################################################
############ END FUNCTIONS ###########################################
######################################################################


######################################################################
############ BEGIN READ DATA #########################################
######################################################################

surv<-read.csv("FinalMPAwatchData03142022_2026Feb.csv")

#center and scale
surv$s_access<-as.numeric(scale(surv$access_amenities_index))
surv$s_length_miles<-as.numeric(scale(surv$length_miles))
surv$s_popden<-as.numeric(scale(surv$popden))
surv$s_duration<-as.numeric(scale(surv$duration))
surv$s_lat<-as.numeric(scale(surv$transect_lat))
surv$s_tod<-scale(surv$time_of_day_lin)
#recode as needed
surv$beach_type<-as.factor(surv$beach_type)
surv$adjacent_to_park<-factor(surv$adjacent_to_park)
surv$survey_site_type<-as.factor(surv$survey_site_type)
surv$precipitation<-as.factor(surv$precipitation)
surv$MPA_Take<-as.factor(surv$MPA_Take)
surv$year<-as.factor(surv$year)
surv$clouds[surv$clouds==""]<-NA
surv$clouds<-factor(surv$clouds)
surv$visibility[surv$visibility==""]<-NA
surv$visibility<-factor(surv$visibility)
surv$has_tidepooling[surv$has_tidepooling=="TRUE"]<-"Yes"
surv$has_tidepooling[surv$has_tidepooling=="FALSE"]<-"No"
surv$day_of_week<-factor(surv$day_of_week, ordered=FALSE)
surv$time_start<-hms(surv$time_start)

# removing boat based surveys from all but fishing/recboating ('survL = Surveys-Land')
survL<-surv[surv$survey_site_type=="Land",]


#-------------- END reading in and prepping data ----#


######################################################################
############ END READ DATA ###########################################
######################################################################



######################################################################
############ BEGIN MODELS ############################################
######################################################################


#------------ BEGIN DHARMa exploration ---------------------------


datename<-"2026Feb15"

fname<-paste("AllActivities_FinalMods_",datename,sep="")

pdf(paste(fname,".pdf",sep=""),width=27,height=16)

results<-list()

#------------  TIDEPOOLING --------------------
dataset<-survL
distr<-"nbinom1"
activity<-"tidepool"
modname<-"nbinom1-Z-HasT-yearCat"
dataset$MPA_Take<-relevel(dataset$MPA_Take,"NonMPA")
dataset$clouds<-relevel(dataset$clouds,"clear")
dataset$precipitation<-relevel(dataset$precipitation,"no")
dataset$visibility<-relevel(dataset$visibility,"perfect")
dataset$day_of_week<-ordered(dataset$day_of_week,c("Sat","Sun","Mon","Tue","Wed","Thu","Fri"))
dataset$beach_type<-relevel(dataset$beach_type,"Sandy")
dataset$has_tidepooling<-factor(dataset$has_tidepooling,levels=c("Yes","No"))
cats<-list(day_of_week="contr.treatment")
ctvars<-c("s_access","s_popden","clouds","precipitation","visibility",
  "tide_standard","s_lat","I(s_lat^2)","adjacent_to_park",
  "day_of_week","winterness","springness",
  "s_tod","I(s_tod^2)",
  "s_duration", "s_length_miles","beach_type","(1 | program)",
  "has_tidepooling","MPA_Take", "(1 | site_name)", "(1 | transect_name)", "year", 
  "year*MPA_Take")
zivars<-c("has_tidepooling")
dsvars<-NULL
drop1_scope<- as.formula("~clouds+visibility+day_of_week+beach_type")
results[[paste(activity,modname,sep="_")]]<-runDHARMaComparison(modname,activity,ctvars,zivars,dsvars,dataset,distr,cats,an=NULL,dp1scp=drop1_scope)
print(warnings())
saveRDS(results,paste(fname,".RDS",sep=""))


#------ ONSHORE FISHING ---------------

dataset<-survL
distr<-"nbinom1"
activity<-"onshorefishing"
modname<-"nbinom1-SimpZifDisp-yearCat"
dataset$MPA_Take<-relevel(dataset$MPA_Take,"NonMPA")
dataset$clouds<-relevel(dataset$clouds,"clear")
dataset$precipitation<-relevel(dataset$precipitation,"no")
dataset$visibility<-relevel(dataset$visibility,"perfect")
dataset$day_of_week<-ordered(dataset$day_of_week,c("Sat","Sun","Mon","Tue","Wed","Thu","Fri"))
dataset$beach_type<-relevel(dataset$beach_type,"Sandy")
dataset$survey_site_type<-relevel(dataset$survey_site_type,"Land")
cats<-list(day_of_week="contr.treatment")
ctvars<-c("s_access","s_popden","clouds","precipitation","visibility",
  "tide_standard","s_lat","I(s_lat^2)","adjacent_to_park",
  "day_of_week","winterness","springness",
   "(1 | program)","year","beach_type",
  "s_tod","I(s_tod^2)","s_duration", "s_length_miles",
  "MPA_Take", "(1 | site_name)", "(1 | transect_name)", "year*MPA_Take")
zivars<-NULL
dsvars<-NULL
drop1_scope<- as.formula("~clouds+visibility+day_of_week+beach_type")
results[[paste(activity,modname,sep="_")]]<-runDHARMaComparison(modname,activity,ctvars,zivars,dsvars,dataset,distr,cats,an=NULL,dp1scp=drop1_scope)
print(warnings())
saveRDS(results,paste(fname,".RDS",sep=""))



#--------- OFFSHORE REC -----------------

dataset<-survL
distr<-"nbinom1"
activity<-"offshorerec"
modname<-"nbinom1-SimpZifDisp-yearCat"
dataset$MPA_Take<-relevel(dataset$MPA_Take,"NonMPA")
dataset$clouds<-relevel(dataset$clouds,"clear")
dataset$precipitation<-relevel(dataset$precipitation,"no")
dataset$visibility<-relevel(dataset$visibility,"perfect")
dataset$day_of_week<-ordered(dataset$day_of_week,c("Sat","Sun","Mon","Tue","Wed","Thu","Fri"))
dataset$beach_type<-relevel(dataset$beach_type,"Sandy")
cats<-list(day_of_week="contr.treatment")
ctvars<-c("s_access","s_popden","clouds","precipitation","visibility",
  "tide_standard","s_lat","I(s_lat^2)","adjacent_to_park",
  "day_of_week","winterness","springness",
  "s_tod","I(s_tod^2)","s_duration", "s_length_miles",
  "beach_type","(1 | program)","year",
  "MPA_Take", "(1 | site_name)", "(1 | transect_name)", "year*MPA_Take")
zivars<-NULL
dsvars<-NULL
drop1_scope<- as.formula("~clouds+visibility+day_of_week+beach_type")
results[[paste(activity,modname,sep="_")]]<-runDHARMaComparison(modname,activity,ctvars,zivars,dsvars,dataset,distr,cats,an=NULL,dp1scp=drop1_scope)
print(warnings())
saveRDS(results,paste(fname,".RDS",sep=""))


#--------- ANIMALS -----------------

dataset<-survL
distr<-"nbinom2"
activity<-"animals"
modname<-"nbinom2-lat8-yearCat"
dataset$MPA_Take<-relevel(dataset$MPA_Take,"NonMPA")
dataset$clouds<-relevel(dataset$clouds,"clear")
dataset$precipitation<-relevel(dataset$precipitation,"no")
dataset$visibility<-relevel(dataset$visibility,"perfect")
dataset$day_of_week<-ordered(dataset$day_of_week,c("Sat","Sun","Mon","Tue","Wed","Thu","Fri"))
dataset$beach_type<-relevel(dataset$beach_type,"Sandy")
cats<-list(day_of_week="contr.treatment")
ctvars<-c("s_access","s_popden","clouds","precipitation","visibility",
  "tide_standard","year",
  "s_lat","I(s_lat^2)","I(s_lat^3)","I(s_lat^4)","I(s_lat^5)","I(s_lat^6)","I(s_lat^7)","I(s_lat^8)",
  "day_of_week","winterness","springness","s_tod","I(s_tod^2)",
  "s_duration", "I(s_duration^2)", "s_length_miles","I(s_length_miles^2)",
  "beach_type","adjacent_to_park",
  "MPA_Take", "(1 | site_name)", "(1 | transect_name)", "year*MPA_Take")
zivars<-NULL
dsvars<-NULL
drop1_scope<- as.formula("~clouds+visibility+day_of_week+beach_type")
results[[paste(activity,modname,sep="_")]]<-runDHARMaComparison(modname,activity,ctvars,zivars,dsvars,dataset,distr,cats,an=NULL,dp1scp=drop1_scope)
print(warnings())
saveRDS(results,paste(fname,".RDS",sep=""))


#------ REC BOATING ---------------

dataset<-surv
distr<-"nbinom1"
activity<-"recboating"
modname<-"nbinom1-Lat8-yearCat"
dataset$MPA_Take<-relevel(dataset$MPA_Take,"NonMPA")
dataset$clouds<-relevel(dataset$clouds,"clear")
dataset$precipitation<-relevel(dataset$precipitation,"no")
dataset$visibility<-relevel(dataset$visibility,"perfect")
dataset$day_of_week<-ordered(dataset$day_of_week,c("Sat","Sun","Mon","Tue","Wed","Thu","Fri"))
cats<-list(day_of_week="contr.treatment")
ctvars<-c("s_access","s_popden","clouds","precipitation","visibility",
  "tide_standard","year",
  "s_lat","I(s_lat^2)","I(s_lat^3)","I(s_lat^4)","I(s_lat^5)","I(s_lat^6)","I(s_lat^7)","I(s_lat^8)",
  "day_of_week","winterness","springness","(1 | transect_name)","(1|site_name)","adjacent_to_park",
  "s_tod","I(s_tod^2)","s_duration", "s_length_miles","survey_site_type",
  "MPA_Take", "year*MPA_Take")
zivars<-NULL
dsvars<-NULL
drop1_scope<- as.formula("~clouds+visibility+day_of_week")
results[[paste(activity,modname,sep="_")]]<-runDHARMaComparison(modname,activity,ctvars,zivars,dsvars,dataset,distr,cats,an=NULL,dp1scp=drop1_scope)
print(warnings())
saveRDS(results,paste(fname,".RDS",sep=""))


#------ OFFSHORE FISHING ---------------

dataset<-surv
distr<-"nbinom2"
activity<-"offshorefishing"
modname<-"nbinom2-yearCat"
dataset$MPA_Take<-relevel(dataset$MPA_Take,"NonMPA")
dataset$clouds<-relevel(dataset$clouds,"clear")
dataset$precipitation<-relevel(dataset$precipitation,"no")
dataset$visibility<-relevel(dataset$visibility,"perfect")
dataset$day_of_week<-ordered(dataset$day_of_week,c("Sat","Sun","Mon","Tue","Wed","Thu","Fri"))
dataset$survey_site_type<-relevel(dataset$survey_site_type,"Land")
cats<-list(day_of_week="contr.treatment")
ctvars<-c("s_access","s_popden","clouds","precipitation","visibility",
  "s_lat","I(s_lat^2)","adjacent_to_park",
  "(1 | transect_name)","(1|site_name)","year",
  "day_of_week","winterness","springness",
  "s_tod","I(s_tod^2)","s_duration", "s_length_miles","survey_site_type",
  "MPA_Take", "year*MPA_Take")
zivars<-NULL
dsvars<-NULL
drop1_scope<- as.formula("~clouds+visibility+day_of_week")
results[[paste(activity,modname,sep="_")]]<-runDHARMaComparison(modname,activity,ctvars,zivars,dsvars,dataset,distr,cats,an=NULL,dp1scp=drop1_scope)
print(warnings())
saveRDS(results,paste(fname,".RDS",sep=""))



#------  ONSHOREREC ----------------------------

dataset<-survL
activity<-"onshorerec"
distr<-"nbinom1"
modname<-"nbinom1-noZif-prog-yearCat"
dataset$MPA_Take<-relevel(dataset$MPA_Take,"NonMPA")
dataset$clouds<-relevel(dataset$clouds,"clear")
dataset$precipitation<-relevel(dataset$precipitation,"no")
dataset$visibility<-relevel(dataset$visibility,"perfect")
dataset$day_of_week<-ordered(dataset$day_of_week,c("Sat","Sun","Mon","Tue","Wed","Thu","Fri"))
dataset$beach_type<-relevel(dataset$beach_type,"Sandy")
cats<-list(day_of_week="contr.treatment")
ctvars<-c("s_access","s_popden","clouds","precipitation","visibility",
  "tide_standard","s_lat","I(s_lat^2)",
  "day_of_week","winterness","springness",
  "s_tod","I(s_tod^2)","s_duration", "s_length_miles",
  "beach_type","adjacent_to_park",
  "has_tidepooling", "(1 | program)","year",
  "MPA_Take", "(1 | site_name)", "(1 | transect_name)", "year*MPA_Take")
zivars<-"NONE"
dsvars<-NULL
drop1_scope<- as.formula("~clouds+visibility+day_of_week+beach_type")
results[[paste(activity,modname,sep="_")]]<-runDHARMaComparison(modname,activity,ctvars,zivars,dsvars,dataset,distr,cats,an=NULL,dp1scp=drop1_scope)
saveRDS(results,paste(fname,".RDS",sep=""))


#------  HANDCOLLECTION ----------------------------


dataset<-survL
distr<-"nbinom1"
modname<-"nbinom1-D-beach-HasT-yearCat"
activity<-"hand_collect"
dataset$MPA_Take<-relevel(dataset$MPA_Take,"NonMPA")
dataset$clouds<-relevel(dataset$clouds,"clear")
dataset$precipitation<-relevel(dataset$precipitation,"no")
dataset$visibility<-relevel(dataset$visibility,"perfect")
dataset$day_of_week<-ordered(dataset$day_of_week,c("Sat","Sun","Mon","Tue","Wed","Thu","Fri"))
dataset$beach_type<-relevel(dataset$beach_type,"Sandy")
dataset$survey_site_type<-relevel(dataset$survey_site_type,"Land")
cats<-list(day_of_week="contr.treatment")
ctvars<-c("s_access","s_popden","clouds","precipitation","visibility",
  "tide_standard","s_lat","I(s_lat^2)","adjacent_to_park",
  "day_of_week","winterness","springness",
   "(1 | program)","year","beach_type","has_tidepooling",
  "s_tod","I(s_tod^2)","s_duration", "s_length_miles",
  "MPA_Take", "(1 | site_name)", "(1 | transect_name)", "year*MPA_Take")
zivars<-NULL
dsvars<-c("has_tidepooling","beach_type")
drop1_scope<- as.formula("~clouds+visibility+day_of_week+beach_type")
results[[paste(activity,modname,sep="_")]]<-runDHARMaComparison(modname,activity,ctvars,zivars,dsvars,dataset,distr,cats,an=NULL,dp1scp=drop1_scope)
print(warnings())
saveRDS(results,paste(fname,".RDS",sep=""))


#------  ALLACTIVITIES ----------------------------

dataset<-surv
distr<-"nbinom2"
activity<-"total_activities" 
modname<-"nbinom2-SimpDisp-NoZif-yearCat"
dataset$MPA_Take<-relevel(dataset$MPA_Take,"NonMPA")
dataset$clouds<-relevel(dataset$clouds,"clear")
dataset$precipitation<-relevel(dataset$precipitation,"no")
dataset$visibility<-relevel(dataset$visibility,"perfect")
dataset$day_of_week<-ordered(dataset$day_of_week,c("Sat","Sun","Mon","Tue","Wed","Thu","Fri"))
dataset$beach_type<-relevel(dataset$beach_type,"Sandy")
dataset$survey_site_type<-relevel(dataset$survey_site_type,"Land")
cats<-list(day_of_week="contr.treatment")
ctvars<-c("s_access","s_popden","clouds","precipitation","visibility",
  "tide_standard","s_lat","I(s_lat^2)","adjacent_to_park",
  "day_of_week","winterness","springness",
   "(1 | program)","beach_type", "has_tidepooling",
  "s_tod","I(s_tod^2)","s_duration", "s_length_miles","survey_site_type",
  "MPA_Take", "(1 | site_name)", "year", "(1 | transect_name)", "year*MPA_Take")
zivars<-"NONE"
dsvars<-NULL
drop1_scope<- as.formula("~clouds+visibility+day_of_week+beach_type")
results[[paste(activity,modname,sep="_")]]<-runDHARMaComparison(modname,activity,ctvars,zivars,dsvars,dataset,distr,cats,an=NULL,dp1scp=drop1_scope)
print(warnings())
saveRDS(results,paste(fname,".RDS",sep=""))


dev.off()

modStats<-data.frame(Name=character(),ModTime=numeric(),SimTime=numeric(),
                    Uniformity=numeric(),Outliers=numeric(),Dispersion=numeric(),
                    Quantiles=numeric(),Zeroinflation=numeric(),Convergence=character(),stringsAsFactors=FALSE)

for(i in 1:length(results)){
  md<-names(results)[i]
  modStats[i,]<-results[[md]][[1]]
}
print(modStats)
write.csv(modStats,paste(fname,".csv",sep=""))

######################################################################
############ END MODELS  #############################################
######################################################################



######################################################################
############ BEGIN P-VALUE COLLECTION & CORRECTION ###################
######################################################################


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  compile and correct p-values

results<-readRDS(paste(fname,".RDS",sep=""))


all_pvals_list<-list()
all_pvals<-compile_p_values(results[[1]])
all_pvals_list[[as.character( formula(results[[1]]$fitTMB)[2])]]<-all_pvals
all_pvals$Model<-rep(names(results)[1],nrow(all_pvals))
for (i in 2:length(results)){
  temp<-compile_p_values(results[[i]])
  all_pvals_list[[as.character( formula(results[[i]]$fitTMB)[2])]]<-temp
  temp$Model<-rep(names(results)[i],nrow(temp))
  all_pvals<-rbind(all_pvals,temp)
}

#all_pvals$padj_all<-p.adjust(all_pvals$p,"fdr")
all_pvals$p_cat<-"Suggestive"
all_pvals$p_cat[as.numeric(all_pvals$p)<0.000001]<-"Significant"
all_pvals$p_cat[as.numeric(all_pvals$p)>0.05]<-"Not Significant"
write.csv(all_pvals,paste("All_Pvals_Corrected_",datename,".csv",sep=""))

for (i in 1:length(all_pvals_list)){
  temp<-all_pvals_list[[i]]
  temp2<-split(temp,temp$Source)
  for(j in 1:length(temp2)){
    temp3<-temp2[[j]][,c("Vars","Vals","SE","p","padj")]
    activity_testSource<-paste(names(all_pvals_list)[i],names(temp2)[j],"pvals",sep="_")
    activity_testSource<-sub(" ","-",activity_testSource)
    temp3$Vars<-gsub("_"," ",temp3$Vars)
    temp3$Vals<-formatC(as.numeric(as.character(temp3$Vals)),format="f",digits=4)
    temp3$SE<-formatC(as.numeric(as.character(temp3$SE)),format="f",digits=4)
    temp3$padj<-ifelse(temp3$padj>0.05,formatC(temp3$padj,format="f",digits=4),sub("^(.*)$","\\\\textbf{\\1}",formatC(temp3$padj,format="f",digits=4),perl=TRUE))
    temp3$p<-formatC(as.numeric(as.character(temp3$p)),format="f",digits=4)
    names(temp3)<-c("Variable(s)","Parameter Estimate","Standard Error","Raw p-value","Adjusted p-value")
    statstable<-xtable(temp3,label=activity_testSource,align="p{0in}|p{2in}|c|c|c|c|",
      caption=paste("P-values for",names(all_pvals_list)[i],"from",names(temp2)[j],"test. Bold typeface indicates p$<$0.05; adjusted p-values have had the Benjamini and Hochberg `false discovery rate' correction applied to them.") )
    print(x=statstable,file=paste("ResultsTables_",datename,".tex",sep=""),sanitize.text.function = function(x){x},include.rownames=FALSE,
      hline.after=c(-1:nrow(statstable)),caption.placement="top",append=TRUE)#,tabular.environment="longtable",file=paste(activity_testSource,".tex",sep=""))
  }
}

#remember that you're using 'append' to spit out the formatted tex, so if you need to change something, you have to delete the file first


##~~~~~~~~~~~~~~~~~~~~~~~~
### pulling out random effect magnitudes for each model
#
results<-readRDS(paste(fname,".RDS",sep=""))


ranef.dispzif<-data.frame(model=character(),Parameter=character(),Value=numeric(),stringsAsFactors=FALSE)
count<-1
for(i in 1:length(results)){
  for(j in 1:length(summary(results[[i]]$fitTMB)$varcor$cond)){
    print(c(names(results)[[i]],names(summary(results[[i]]$fitTMB)$varcor$cond[j]),sqrt(as.numeric(summary(results[[i]]$fitTMB)$varcor$cond[j]))))
    ranef.dispzif[count,]<-(c(names(results)[[i]],names(summary(results[[i]]$fitTMB)$varcor$cond[j]),sqrt(as.numeric(summary(results[[i]]$fitTMB)$varcor$cond[j]))))
    count<-count+1
  }
  print(c(names(results)[[i]],"DispersionParameter",summary(results[[i]]$fitTMB)$sigma))
  if(!is.na(summary(results[[i]]$fitTMB)$sigma)){
    ranef.dispzif[count,]<-(c(names(results)[[i]],"DispersionParameter",summary(results[[i]]$fitTMB)$sigma))
    count<-count+1
    } else {
      ranef.dispzif[count,]<-(c(names(results)[[i]],"DispersionParameter",summary(results[[i]]$fitTMB)$coefficients$disp["(Intercept)","Estimate"]))
      count<-count+1      
    }
  if(length( c(names(results)[[i]],"ZeroInflationInterceptEstimate",summary(results[[i]]$fitTMB)$coefficients$zi["(Intercept)","Estimate"]))==3){
    print(c(names(results)[[i]],"ZeroInflationInterceptEstimate",summary(results[[i]]$fitTMB)$coefficients$zi["(Intercept)","Estimate"]))
    ranef.dispzif[count,]<-(c(names(results)[[i]],"ZeroInflationInterceptEstimate",summary(results[[i]]$fitTMB)$coefficients$zi["(Intercept)","Estimate"]))
    count<-count+1
  } else {
    print(c(names(results)[[i]],"ZeroInflationInterceptEstimate",summary(results[[i]]$fitTMB)$coefficients$zi["(Intercept)","Estimate"]))
    ranef.dispzif[count,]<-(c(names(results)[[i]],"ZeroInflationInterceptEstimate","NA"))
    count<-count+1    
  }
}

write.csv(ranef.dispzif,paste("RandomEffectDispersionZeroInflationParameters_",datename,".csv",sep=""))



######################################################################
############ END P-VALUE COLLECTION & CORRECTION #####################
######################################################################


######################################################################
############ BEGIN PLOTTING ##########################################
######################################################################



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Make figures

results<-readRDS(paste(fname,".RDS",sep=""))


#------------ time trends & MPA types plots -------------------

timeTrends<-data.frame(
s_access=rep(0,24),
s_popden=rep(0,24),
clouds=rep("clear",24),
precipitation=rep("no",24),
visibility=rep("perfect",24),
tide_standard=rep(0,24),
s_lat=rep(0,24), 
day_of_week=rep("Sat",24),
winterness=rep(-1,24), #summer solstice
springness=rep(0,24), #summer solstice
s_tod=rep(
  (as.numeric(hms("12:00:00"))-mean(as.numeric(surv$time_start)) )
    /sd(as.numeric(surv$time_start))
    ,24),
s_duration=rep(
  as.numeric((60-mean(surv$duration))/sd(surv$duration))
    ,24), # should be the one-hour duration
s_length_miles=rep(
  ((1-mean(surv$length_miles,na.rm=TRUE))/sd(surv$length_miles,na.rm=TRUE))
    ,24), #one mile, because it's closer to their survey-miles reports
beach_type=rep("Sandy",24),
has_tidepooling=rep("Yes",24),
adjacent_to_park=rep(TRUE,24),
MPA_Take=rep(levels(surv$MPA_Take),each=8),
program=factor(rep("Marin MPA Watch",24)),
site_name=factor(rep("Estero de Limantour SMR",24)),
transect_name=factor(rep("DRK_BEACH_E 02: Overlook Lim Estero SMR",24)),
survey_site_type=rep("Land",24),
year=rep(2013:2020,3)
  )


#For the main paper, we just need onshorefishing and offshorefishing for this figure.
#  So let's make the loop only include those for this one

  pred_time<-list()
#  for(i in 1:length(results)){
  for(i in c(2,6)){
    act<-sub("(.*)(_.*)","\\1",names(results)[i])
    print(act)
    pred_time[[i]]<-generatePredictions(act,results[[i]]$fitTMB,timeTrends,"response","Pop")

    pred_time[[i]]$MPA_Take<-factor(pred_time[[i]]$MPA_Take, levels = c("NonMPA","SomeTake","NoTake"))
  }
  timePred<-pred_time[[1]]
  for (i in 2:length(pred_time))
    timePred<-rbind(timePred,pred_time[[i]])

timePred$activity[timePred$activity=="offshorefishing"]<- "Offshore Fishing"
timePred$activity[timePred$activity=="onshorefishing"]<- "Onshore Fishing"

time_plot<-generatePlots_SinglePage_TimeMPA(timePred)
time_plot
#have it be a full-page figure, and make sure its resolution is good 
#ggsave(paste("AllActivitiesTimeTrendMPA_Final_",datename,".png",sep=""),width=7.5,height=10, unit="in")
ggsave(paste("Manuscript_TimeTrendMPA_",datename,".tiff",sep=""),width=5,height=3.33, dpi=800, unit="in",compression="lzw")
ggsave(paste("Fig2.tif",sep=""),width=5,height=3.33, dpi=800, unit="in",compression="lzw")

ggsave(paste("Manuscript_TimeTrendMPA_",datename,".svg",sep=""),width=6,height=4, unit="in")


#for appendix


  pred_time<-list()
  for(i in 1:length(results)){
    act<-sub("(.*)(_.*)","\\1",names(results)[i])
    print(act)
    pred_time[[i]]<-generatePredictions(act,results[[i]]$fitTMB,timeTrends,"response","Pop")

    pred_time[[i]]$MPA_Take<-factor(pred_time[[i]]$MPA_Take, levels = c("NonMPA","SomeTake","NoTake"))
  }
  timePred<-pred_time[[1]]
  for (i in 2:length(pred_time))
    timePred<-rbind(timePred,pred_time[[i]])

time_plot<-generatePlots_SinglePage_TimeMPA(timePred)
time_plot
#have it be a full-page figure, and make sure its resolution is good 
ggsave(paste("AllActivitiesTimeTrendMPA_Final_",datename,".png",sep=""),width=7.5,height=10, unit="in")

#------------ seasonality plots -----------------

#for appendix

Seasonality<-data.frame(
s_access=rep(0,365),
s_popden=rep(0,365),
clouds=rep("clear",365),
precipitation=rep("no",365),
visibility=rep("perfect",365),
tide_standard=rep(0,365),
s_lat=rep(0,365), 
day_of_week=rep("Sat",365),
winterness=cos(2*pi*(1:365+11.24)/365),
springness=sin(2*pi*(1:365+11.24)/365),
s_tod=rep(
  (as.numeric(hms("12:00:00"))-mean(as.numeric(surv$time_start)) )
    /sd(as.numeric(surv$time_start))
    ,365),
s_duration=rep(
  as.numeric((60-mean(surv$duration))/sd(surv$duration))
    ,365), # should be the one-hour duration
s_length_miles=rep(
  ((1-mean(surv$length_miles,na.rm=TRUE))/sd(surv$length_miles,na.rm=TRUE))
    ,365), #one mile, because it's closer to their survey-miles
beach_type=rep("Sandy",365),
has_tidepooling=rep("Yes",365),
adjacent_to_park=rep(TRUE,365),
MPA_Take=rep(levels(surv$MPA_Take)[1],365),
year=rep("2016",365),
program=factor(rep("Marin MPA Watch",365)),
site_name=factor(rep("Estero de Limantour SMR",365)),
transect_name=factor(rep("DRK_BEACH_E 02: Overlook Lim Estero SMR",365)),
survey_site_type=rep("Land",365),
day_of_year=1:365,
day_of_year_POSIXct=as.POSIXct(as.Date(1:365,origin="2012-12-31" ))
  )


  pred_seas<-list()
  for(i in 1:length(results)){
    act<-sub("(.*)(_.*)","\\1",names(results)[i])
    print(act)
    pred_seas[[i]]<-generatePredictions(act,results[[i]]$fitTMB,Seasonality,"response","Pop")
  }

SeasPred<-pred_seas[[1]]

for (i in 2:length(pred_seas)){
    SeasPred<-rbind(SeasPred,pred_seas[[i]])

}


maxes_seas<-data.frame(activity=names(split(SeasPred,SeasPred$activity)),
Max=as.numeric(lapply(split(SeasPred,SeasPred$activity),FUN=function(x){x$day_of_year[x$predCount==max(x$predCount)]}) ),
MaxVal=as.numeric(lapply(split(SeasPred,SeasPred$activity),FUN=function(x){max(x$predCount)}) )
)
maxes_seas$MaxPOSIXct<-as.POSIXct(as.Date(maxes_seas$Max,origin="2012-12-31" ))
write.csv(maxes_seas,paste("maxes_seas_",datename,".csv",sep=""))

seas_plot<-generatePlots_Seasonality_WithFacets(SeasPred,maxes_seas)
seas_plot
ggsave(paste("AllActivitiesSeasonalitySinglePage_Final_",datename,".png",sep=""),width=7.5,height=10, unit="in")


#---------- diurnal plots -------------------

#for appendix

Daily<-data.frame(
s_access=rep(0,100),
s_popden=rep(0,100),
clouds=rep("clear",100),
precipitation=rep("no",100),
visibility=rep("perfect",100),
tide_standard=rep(0,100),
s_lat=rep(0,100), 
day_of_week=rep("Sat",100),
winterness=rep(-1,100), #summer solstice
springness=rep(0,100), #summer solstice
s_tod=seq(min(surv$s_tod),max(surv$s_tod),length.out=100),
s_duration=rep(
  as.numeric((60-mean(surv$duration))/sd(surv$duration))
    ,100), # should be the one-hour duration
s_length_miles=rep(
  ((1-mean(surv$length_miles,na.rm=TRUE))/sd(surv$length_miles,na.rm=TRUE))
    ,100), #one mile, because it's closer to their survey-miles
beach_type=rep("Sandy",100),
has_tidepooling=rep("Yes",100),
adjacent_to_park=rep(TRUE,100),
MPA_Take=rep(levels(surv$MPA_Take)[1],100),
year=rep("2016",100),
program=factor(rep("Marin MPA Watch",100)),
site_name=factor(rep("Estero de Limantour SMR",100)),
transect_name=factor(rep("DRK_BEACH_E 02: Overlook Lim Estero SMR",100)),
survey_site_type=rep("Land",100)
  )
Daily$tod_num<-as.numeric(Daily$s_tod*sd(surv$time_start)+mean(surv$time_start))
Daily$tod<-hms::new_hms(Daily$tod_num)
Daily$todPOSIXct<-as.POSIXct(Daily$tod)


  pred_day<-list()
  for(i in 1:length(results)){
    act<-sub("(.*)(_.*)","\\1",names(results)[i])
    print(act)
    pred_day[[i]]<-generatePredictions(act,results[[i]]$fitTMB,Daily,"response","Pop")
  }


DailyPred<-pred_day[[1]]

for (i in 2:length(pred_day)){
    DailyPred<-rbind(DailyPred,pred_day[[i]])

}


maxes_daily<-data.frame(activity=names(split(DailyPred,DailyPred$activity)),
Max=as.numeric(lapply(split(DailyPred,DailyPred$activity),FUN=function(x){x$s_tod[x$predCount==max(x$predCount)]}) ),
MaxVal=as.numeric(lapply(split(DailyPred,DailyPred$activity),FUN=function(x){max(x$predCount)}) )
)
maxes_daily$tod_num<-as.numeric(maxes_daily$Max*sd(surv$time_start)+mean(surv$time_start))
maxes_daily$MaxHMS<-hms::new_hms(maxes_daily$tod_num)
maxes_daily$MaxHMSPOSIXct<-as.POSIXct(maxes_daily$MaxHMS)
write.csv(maxes_daily,paste("maxes_daily_",datename,".csv",sep=""))

day_plot<-generatePlots_Diurnal_WithFacets(DailyPred,maxes_daily)
day_plot
ggsave(paste("AllActivitiesDiurnalSinglePage_Final_",datename,".png",sep=""),width=7.5,height=10, unit="in")


#----------------- geographical plots -----------------------

pred_spat<-list()
for(i in 1:length(results)){
    act<-sub("(.*)(_.*)","\\1",names(results)[i])
    print(act)
    if(act %in% c("offshorefishing","onshoreconsumption","recboating")){
      pred_spat[[act]]<-generateSpatialPredictions(results[[i]],surv,"response")
    } else {
      pred_spat[[act]]<-generateSpatialPredictions(results[[i]],survL,"response")      
    }
    pred_spat[[act]]$MPA_Take<-factor(pred_spat[[act]]$MPA_Take, levels = c("NonMPA","SomeTake","NoTake"))
    #getting the average site effects by averaging the predictions of the transects within the site
    site_effects<-aggregate(pred_spat[[act]]$predCount,pred_spat[[act]]["site_name"],mean)
    names(site_effects)<-c("site_name","site_avg_pred")
    pred_spat[[act]]<-merge(pred_spat[[act]],site_effects,by.x="site_name",by.y="site_name",all.x=TRUE)
  }
 


#---------------------------------------------------------
#Plotting site averages

#for manuscript, preparing to put all predictions together
site_eff_ms<-list()


# just site predictions, ordered by latitude  
for (i in 1:length(pred_spat)){

  sites<-unique(pred_spat[[i]][,c("site_name","site_avg_pred","MPA_Take","site_lat_centroid")])


  # Approximate site effect ------------------------------
  sites <- sites[order(-sites$site_lat_centroid), ]
  sites$site_name <- factor(sites$site_name, unique(sites$site_name))
  
#for later compiling into a data frame to make the manuscript figures
  sites$activity<-names(pred_spat)[i]
  site_eff_ms[[i]]<-sites

 }


site_eff_ms_df<-as.data.frame(do.call(rbind, site_eff_ms))

#only consmptive plus total
site_eff_ms_df<- site_eff_ms_df[site_eff_ms_df$activity%in%c("total_activities","hand_collect","onshorefishing","offshorefishing"),]

#relevel them to put them in a sensical (not alphabetical) order
site_eff_ms_df$activity[site_eff_ms_df$activity=="total_activities"]<-"A) All activity types"
site_eff_ms_df$activity[site_eff_ms_df$activity=="hand_collect"]<-"B) Hand collection"
site_eff_ms_df$activity[site_eff_ms_df$activity=="onshorefishing"]<-"C) Onshore fishing"
site_eff_ms_df$activity[site_eff_ms_df$activity=="offshorefishing"]<-"D) Offshore fishing"
levels(as.factor(site_eff_ms_df$activity))

  site_eff_ms_df<- site_eff_ms_df[order(-site_eff_ms_df$site_lat_centroid), ]

ann_text<- data.frame(label=c("EU","SF","SB", "LA", "SD"),
                      activity=rep("A) All activity types",5),
                      x=c(2,36,67,77,90),y=c(500,400,550,500,600))

site_eff_nolat_con <- ggplot(site_eff_ms_df,aes(x = site_name, y = site_avg_pred))  +
  geom_point(aes(shape = MPA_Take,color=MPA_Take), size = 2) +    
  facet_wrap(~ activity,scales="free",ncol=1)+
  scale_shape_manual(values = c(15,16,17)) +
  scale_color_viridis_d(end=0.8)+
  theme_minimal()+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank())+
  theme(legend.position="bottom")+
  labs(
    x = "Sites (North on the left to South on the right)"
    , y = "Predicted Counts"
    , shape = "Type of Site"
    , color = "Type of Site"
    , title = element_blank())+
  geom_text(data=ann_text,mapping=aes(x=x,y=y,label=label))
site_eff_nolat_con

ggsave(paste("Manuscript_Site_ranked_",datename,"_consumptive.tiff",sep=""),width=7,height=5, dpi=600, unit="in",compression="lzw")
ggsave(paste("Fig3.tif",sep=""),width=7,height=5, dpi=600, unit="in",compression="lzw")



site_eff_ms_df<-as.data.frame(do.call(rbind, site_eff_ms))

#only nonconsmptive plus total
site_eff_ms_df<- site_eff_ms_df[!(site_eff_ms_df$activity%in%c("hand_collect","onshorefishing","offshorefishing","onshore_consumption")),]

#relevel them to put them in a sensical (not alphabetical) order
site_eff_ms_df$activity[site_eff_ms_df$activity=="total_activities"]<-"A) All activity types"
site_eff_ms_df$activity[site_eff_ms_df$activity=="onshorerec"]<-"B) Onshore recreation"
site_eff_ms_df$activity[site_eff_ms_df$activity=="animals"]<-"C) Domestic Animals"
site_eff_ms_df$activity[site_eff_ms_df$activity=="tidepool"]<-"D) Tidepooling"
site_eff_ms_df$activity[site_eff_ms_df$activity=="recboating"]<-"E) Recreational boating"
site_eff_ms_df$activity[site_eff_ms_df$activity=="offshorerec"]<-"F) Offshore recreation"
levels(as.factor(site_eff_ms_df$activity))



  site_eff_ms_df<- site_eff_ms_df[order(-site_eff_ms_df$site_lat_centroid), ]

ann_text<- data.frame(label=c("EU","SF","SB", "LA", "SD"),
                      activity=rep("A) All activity types",5),
                      x=c(2,36,67,77,90),y=c(500,400,550,500,600))

site_eff_nolat_noncon <- ggplot(site_eff_ms_df,aes(x = site_name, y = site_avg_pred))  +
  geom_point(aes(shape = MPA_Take,color=MPA_Take), size = 2) +    
  facet_wrap(~ activity,scales="free",ncol=1)+
  scale_shape_manual(values = c(15,16,17)) +
  scale_color_viridis_d(end=0.8)+
  theme_minimal()+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank())+
  theme(legend.position="bottom")+
  labs(
    x = "Sites (North on the left to South on the right)"
    , y = "Predicted Counts"
    , shape = "Type of Site"
    , color = "Type of Site"
    , title = element_blank())+
  geom_text(data=ann_text,mapping=aes(x=x,y=y,label=label))
site_eff_nolat_noncon

ggsave(paste("Manuscript_Site_ranked_",datename,"_nonconsumptive.tiff",sep=""),width=7,height=7.5, dpi=600, unit="in",compression="lzw")
ggsave(paste("Fig4.tif",sep=""),width=7,height=7.5, dpi=600, unit="in",compression="lzw")





# Now making this figure just as a barplot for Asilomar for all activities
site_eff_ms_df<-as.data.frame(do.call(rbind, site_eff_ms))
site_eff_ms_df<- site_eff_ms_df[!(site_eff_ms_df$activity%in%c("recboating","offshorefishing","onshore_consumption")),]

just.asilomar<-site_eff_ms_df[site_eff_ms_df$site_name=="Asilomar SMR",]

just.asilomar$Type<-NULL
just.asilomar$Type<-"Non-consumptive"
just.asilomar$Type[just.asilomar$activity%in%c("offshorefishing","onshorefishing","hand_collect")]<-"Consumptive" 


just.asilomar$activity[just.asilomar$activity=="total_activities"]<- "All Activities"
just.asilomar$activity[just.asilomar$activity=="onshorerec"]<- "Onshore Rec."
just.asilomar$activity[just.asilomar$activity=="animals"]<- "Animals"
just.asilomar$activity[just.asilomar$activity=="tidepool"]<- "Tidepooling"
just.asilomar$activity[just.asilomar$activity=="hand_collect"]<- "Hand-collecting"
just.asilomar$activity[just.asilomar$activity=="onshorefishing"]<- "Onshore Fishing"
just.asilomar$activity[just.asilomar$activity=="offshorerec"]<- "Offshore Rec."

just.asilomar$activity<-factor(just.asilomar$activity,
  levels=c("All Activities","Onshore Rec.","Offshore Rec.","Animals","Tidepooling","Hand-collecting","Onshore Fishing"))

plt.a<-ggplot(just.asilomar,
    aes(x=activity,y=site_avg_pred,fill=Type))+
    geom_bar(stat="identity",position=position_dodge())+
    ggtitle("Comparing activities at Asilomar State Marine Reserve")+
    theme(legend.position="none")+
    theme_minimal()+
    labs(x="")+ylim(c(0,180))+
    theme(legend.position="top")+
    ylab("Predicted Counts")+
    scale_fill_viridis_d(option="magma",direction=-1)+
    theme(axis.text.x=element_text(angle = 45,size=12,vjust=1,hjust=1))+
    geom_text(aes(label = round(site_avg_pred,1)),position = position_dodge(.9), vjust = -1,fontface="bold")

plt.a

ggsave("ManuscriptAsilomarComparison.tiff", units="in", width=6,height=4,dpi=600,compression="lzw")
ggsave("Fig5.tif", units="in", width=5,height=4,dpi=600,compression="lzw")

#for asilomar and carp (for appendix)

just.asilomar.carp<-site_eff_ms_df[site_eff_ms_df$site_name%in%c("Asilomar SMR","Control CARPE"),]

just.asilomar.carp$site_name<-as.character(just.asilomar.carp$site_name)
just.asilomar.carp$site_name[just.asilomar.carp$site_name=="Asilomar SMR"]<- "Asilomar"
just.asilomar.carp$site_name[just.asilomar.carp$site_name=="Control CARPE"]<- "Carpinteria"


plt.a<-ggplot(just.asilomar.carp,
    aes(x=activity,y=site_avg_pred,fill=activity))+
    facet_wrap(~site_name,nrow=2,scales="free")+
    geom_bar(stat="identity",position=position_dodge())+
    ggtitle("Comparative activities at Asilomar State Marine Reserve\nAnd Carpinteria Beach")+
    theme(legend.position="none")+
    labs(x="")+
    #ylim(c(0,175))+
    ylab("Predicted Counts")+
    scale_fill_viridis_d(option="magma",direction=1)+
    theme(axis.text.x=element_text(angle = 90))+
    geom_text(aes(label = round(site_avg_pred,1)),y = max(just.asilomar.carp$site_avg_pred)*0.05, vjust = -1,fontface="bold")

 plt.a

 ggsave("ManuscriptAsilomarComparisonCarpenteria.png", units="in", width=6,height=8,dpi=300)

asilomar.ta<-just.asilomar.carp$site_avg_pred[just.asilomar.carp$activity=="total_activities" & just.asilomar.carp$site_name=="Asilomar"]
carp.ta<-just.asilomar.carp$site_avg_pred[just.asilomar.carp$activity=="total_activities" & just.asilomar.carp$site_name=="Carpinteria"]

 just.asilomar.carp$ActivityProportion<-just.asilomar.carp$site_avg_pred
 just.asilomar.carp$ActivityProportion[just.asilomar.carp$site_name=="Carpinteria"]<-just.asilomar.carp$ActivityProportion[just.asilomar.carp$site_name=="Carpinteria"]/carp.ta
 just.asilomar.carp$ActivityProportion[just.asilomar.carp$site_name=="Asilomar"]<-just.asilomar.carp$ActivityProportion[just.asilomar.carp$site_name=="Asilomar"]/asilomar.ta

asilomar.carp.ratio<-data.frame(activity=levels(factor(just.asilomar.carp$activity)),ActivityProportion=NA,site_name="Ratio")
for(i in 1:nrow(asilomar.carp.ratio)){
  asilomar.carp.ratio$ActivityProportion[i]<-just.asilomar.carp$ActivityProportion[just.asilomar.carp$activity==levels(factor(just.asilomar.carp$activity))[i] & just.asilomar.carp$site_name=="Asilomar"]/
  just.asilomar.carp$ActivityProportion[just.asilomar.carp$activity==levels(factor(just.asilomar.carp$activity))[i] & just.asilomar.carp$site_name=="Carpinteria"]
}

a.c.compare<-just.asilomar.carp[,c("site_name","activity","ActivityProportion")]
norm.ratio.compare<-rbind(a.c.compare,asilomar.carp.ratio)

 plt.a<-ggplot(norm.ratio.compare,
     aes(x=activity,y=ActivityProportion,fill=activity))+
     geom_bar(stat="identity",position=position_dodge())+
     facet_wrap(~site_name,nrow=3,scales="free")+
     ggtitle("Comparative activities at Asilomar State Marine Reserve\nAnd Carpinteria Beach (Normalized) and Ratio Asilomar/Carpinteria")+
     theme(legend.position="none")+
     labs(x="")+
     #ylim(c(0,175))+
     ylab("Proportion/Ratio")+
     scale_fill_viridis_d(option="magma",direction=1)+
     theme(axis.text.x=element_text(angle = 90))
 plt.a

 ggsave("ManuscriptAsilomarComparisonCarpenteriaNormalized.png", units="in", width=6,height=8,dpi=300)


######################################################################
############ END PLOTTING ############################################
######################################################################



end.time<- Sys.time()

total.time<-end.time-start.time

print(total.time)
