setwd("C:\\Jig14387")
options(scipen = 999)
#Lodaing required library
library(Hmisc)
library(dplyr)
#reading Dataset provide in working directory dataframe
chrun<-read.csv("telecomfinal.csv",header = TRUE,stringsAsFactors = TRUE)
setwd("C:\\Jig14387\\Capstone")

names(chrun)
Var<-colnames(chrun)
class(var)
varname<-as.data.frame(Var)
#storing datatype
varname$datatype<-sapply(chrun,class)

View(varname)

#find na in dependent variabl chrun 
sum(is.na(chrun$churn))
############
summary(chrun)
####################
#Dataprofiling
#####################
dataQualityReport<-as.data.frame(names(chrun))
dataQualityReport$Datatypes<-sapply(chrun,class)
dataQualityReport$countofvalues<-nrow(chrun)
#data available
#minmaxquantilevalue for numeric and integer
sum(!is.na(chrun[4]))
for(i in 1:ncol(chrun))
{
  dataQualityReport$unique[i]<-length(unique(chrun[,i]))
  dataQualityReport$dataavailble[i]<-sum(!is.na(chrun[i]))
  dataQualityReport$dataavailblepercentage[i]<-round(sum(!is.na(chrun[i]))/nrow(chrun),4)
  #data available in missing values
  dataQualityReport$datamissing[i]<-sum(is.na(chrun[i]))
  dataQualityReport$datamissingpercentage[i]<-round(sum(is.na(chrun[i]))/nrow(chrun),4)
  
  dataQualityReport$min[i]<-round(ifelse(class(chrun[,i])=="numeric"|class(chrun[,i])=="integer",min(chrun[,i],na.rm = TRUE),0),4)
  dataQualityReport$max[i]<-round(ifelse(class(chrun[,i])=="numeric"|class(chrun[,i])=="integer",max(chrun[,i],na.rm = TRUE),0),4)
  dataQualityReport$Mean[i]<-round(ifelse(class(chrun[,i])=="numeric"|class(chrun[,i])=="integer",mean(chrun[,i],na.rm = TRUE),0),4)
  dataQualityReport$fifthpercent[i]<-round(ifelse(class(chrun[,i])=="numeric"|class(chrun[,i])=="integer",quantile(chrun[,i],p=0.05,na.rm = TRUE),0),2)
  dataQualityReport$tenthpercent[i]<-round(ifelse(class(chrun[,i])=="numeric"|class(chrun[,i])=="integer",quantile(chrun[,i],p=0.10,na.rm = TRUE),0),4)
  dataQualityReport$twentyfifthpercent[i]<-round(ifelse(class(chrun[,i])=="numeric"|class(chrun[,i])=="integer",quantile(chrun[,i],p=0.25,na.rm = TRUE),0),4)
  dataQualityReport$fiftythpercent[i]<-round(ifelse(class(chrun[,i])=="numeric"|class(chrun[,i])=="integer",quantile(chrun[,i],p=0.50,na.rm = TRUE),0),4)
  dataQualityReport$seventyfifthpercent[i]<-round(ifelse(class(chrun[,i])=="numeric"|class(chrun[,i])=="integer",quantile(chrun[,i],p=0.75,na.rm = TRUE),0),4)
  dataQualityReport$nightythpercent[i]<-round(ifelse(class(chrun[,i])=="numeric"|class(chrun[,i])=="integer",quantile(chrun[,i],p=0.90,na.rm = TRUE),0),4)
  dataQualityReport$nightyfifththpercent[i]<-round(ifelse(class(chrun[,i])=="numeric"|class(chrun[,i])=="integer",quantile(chrun[,i],p=0.95,na.rm = TRUE),0),4)
  
  
}

write.csv(dataQualityReport,"dataQualityReport.csv",row.names = T)
#########################################################
sum(is.na(chrun$retdays))
#below are crticalvariable creatingdummyvariable for this so that this wont be ddeleted

chrun$retdays<-ifelse(is.na(chrun$retdays==TRUE),0,1)
chrun$mailordr<-ifelse(is.na(chrun$mailordr==TRUE),0,1)
chrun$mailresp<-ifelse(is.na(chrun$mailresp==TRUE),0,1)

##############
#Removing variables with >0.25 missing percentage 
####
chrun<-chrun[,colMeans(is.na(chrun))<0.25]
dim(chrun)
#################

Var<-colnames(chrun)
class(var)
varname<-as.data.frame(Var)
rm(Var)
#storing datatype
varname$datatype<-sapply(chrun,class)

View(varname)

###############
#Removing variables which already present in other variables/
############
#ATTEMPT_MEAN	PLCD_DAT_MEAN + PLCD_VCE_MEAN
#COMPLETE_MEAN	COMP_DAT_MEAN + COMP_VCE_MEAN
#DROP_BLK_MEAN	BLCK_DAT_MEAN + BLCK_VCE_MEAN + DROP_DAT_MEAN + DROP_VCE_MEAN
#OVRREV_MEAN	DATOVR_MEAN + VCEOVR_MEAN
#ATTEMPT_MEAN	PLCD_DAT_MEAN + PLCD_VCE_MEAN
chrun$ATTEMPT_MEAN<-chrun$plcd_dat_Mean+chrun$plcd_vce_Mean

#

#COMPLETE_MEAN	COMP_DAT_MEAN + COMP_VCE_MEAN
chrun$COMPLETE_MEAN<-chrun$comp_dat_Mean+chrun$comp_vce_Mean
########listing all remaining variables for data profiling##
Var<-colnames(chrun)
class(var)
varname<-as.data.frame(Var)
rm(Var)
#storing datatype
varname$datatype<-sapply(chrun,class)

View(varname)
####################
#
Finalchurn<-chrun

##################################

#mou_mean
datmou_mean<-Finalchurn%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
datmou_mean$N<-unclass(Finalchurn%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
datmou_mean$churn_perc<-datmou_mean$n/datmou_mean$N
datmou_mean$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
datmou_mean$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
datmou_mean$varname<-rep("mou_Mean",nrow(datmou_mean))

#totmrc_Mean
dattotmrc_Mean<-Finalchurn%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dattotmrc_Mean$N<-unclass(Finalchurn%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dattotmrc_Mean$churn_perc<-dattotmrc_Mean$n/dattotmrc_Mean$N
dattotmrc_Mean$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dattotmrc_Mean$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dattotmrc_Mean$varname<-rep("totmrc_Mean",nrow(dattotmrc_Mean))

#rev_Range
datrev_Range<-Finalchurn%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
datrev_Range$N<-unclass(Finalchurn%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
datrev_Range$churn_perc<-datrev_Range$n/datrev_Range$N
datrev_Range$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
datrev_Range$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
datrev_Range$varname<-rep("rev_Range",nrow(datrev_Range))
#mou_Range
datmou_Range<-Finalchurn%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
datmou_Range$N<-unclass(Finalchurn%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
datmou_Range$churn_perc<-datmou_Range$n/datmou_Range$N
datmou_Range$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
datmou_Range$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
datmou_Range$varname<-rep("mou_Range",nrow(datmou_Range))
#change_mou
datchange_mou<-Finalchurn%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
datchange_mou$N<-unclass(Finalchurn%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
datchange_mou$churn_perc<-datchange_mou$n/datchange_mou$N
datchange_mou$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
datchange_mou$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
datchange_mou$varname<-rep("change_mou",nrow(datchange_mou))
#drop_blk_Mean
datdrop_blk_Mean<-Finalchurn%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
datdrop_blk_Mean$N<-unclass(Finalchurn%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
datdrop_blk_Mean$churn_perc<-datdrop_blk_Mean$n/datdrop_blk_Mean$N
datdrop_blk_Mean$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
datdrop_blk_Mean$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
datdrop_blk_Mean$varname<-rep("drop_blk_Mean",nrow(datdrop_blk_Mean))
#drop_vce_Range
datdrop_vce_Range<-Finalchurn%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
datdrop_vce_Range$N<-unclass(Finalchurn%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
datdrop_vce_Range$churn_perc<-datdrop_vce_Range$n/datdrop_vce_Range$N
datdrop_vce_Range$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
datdrop_vce_Range$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
datdrop_vce_Range$varname<-rep("drop_vce_Range",nrow(datdrop_vce_Range))
#owylis_vce_Range
datowylis_vce_Range<-Finalchurn%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
datowylis_vce_Range$N<-unclass(Finalchurn%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
datowylis_vce_Range$churn_perc<-datowylis_vce_Range$n/datowylis_vce_Range$N
datowylis_vce_Range$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
datowylis_vce_Range$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
datowylis_vce_Range$varname<-rep("owylis_vce_Range",nrow(datowylis_vce_Range))
#mou_opkv_Range
datmou_opkv_Range<-Finalchurn%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
datmou_opkv_Range$N<-unclass(Finalchurn%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
datmou_opkv_Range$churn_perc<-datmou_opkv_Range$n/datmou_opkv_Range$N
datmou_opkv_Range$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
datmou_opkv_Range$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
datmou_opkv_Range$varname<-rep("mou_opkv_Range",nrow(datmou_opkv_Range))
#months
datmonths<-Finalchurn%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)
datmonths$N<-unclass(Finalchurn%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
datmonths$churn_perc<-datmonths$n/datmonths$N
datmonths$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
datmonths$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
datmonths$varname<-rep("months",nrow(datmonths))
#totcalls
dattotcalls<-Finalchurn%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)
dattotcalls$N<-unclass(Finalchurn%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
dattotcalls$churn_perc<-dattotcalls$n/dattotcalls$N
dattotcalls$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
dattotcalls$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
dattotcalls$varname<-rep("totcalls",nrow(dattotcalls))
#eqpdays
#not able to decile this as no value is coming 
#custcare_Mean
datcustcare_Mean<-Finalchurn%>%mutate(dec=ntile(custcare_Mean,n=5))%>%count(churn,dec)%>%filter(churn==1)
datcustcare_Mean$N<-unclass(Finalchurn%>%mutate(dec=ntile(custcare_Mean,n=4))%>%count(dec)%>%unname())[[2]]
datcustcare_Mean$churn_perc<-datcustcare_Mean$n/datcustcare_Mean$N
datcustcare_Mean$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(custcare_Mean,n=4))%>%group_by(dec)%>%summarise(min(custcare_Mean)))[[2]]
datcustcare_Mean$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(custcare_Mean,n=4))%>%group_by(dec)%>%summarise(max(custcare_Mean)))[[2]]
datcustcare_Mean$varname<-rep("custcare_Mean",nrow(datcustcare_Mean))
#Finalchurn<-Finalchurn[,-grep("custcare_Mean",colnames(Finalchurn))]

#callwait_Mean
datcallwait_Mean<-Finalchurn%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
datcallwait_Mean$N<-unclass(Finalchurn%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(dec)%>%unname())[[2]]
datcallwait_Mean$churn_perc<-datcallwait_Mean$n/datcallwait_Mean$N
datcallwait_Mean$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
datcallwait_Mean$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
datcallwait_Mean$varname<-rep("callwait_Mean",nrow(datcallwait_Mean))

#iwylis_vce_Mean
datiwylis_vce_Mean<-Finalchurn%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(churn,dec)%>%filter(churn==1)
datiwylis_vce_Mean$N<-unclass(Finalchurn%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(dec)%>%unname())[[2]]
datiwylis_vce_Mean$churn_perc<-datiwylis_vce_Mean$n/datiwylis_vce_Mean$N
datiwylis_vce_Mean$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
datiwylis_vce_Mean$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
datiwylis_vce_Mean$varname<-rep("iwylis_vce_Mean",nrow(datiwylis_vce_Mean))
#callwait_Range removing

datcallwait_Range<-Finalchurn%>%mutate(dec=ntile(callwait_Range,n=3))%>%count(churn,dec)%>%filter(churn==1)
datcallwait_Range$N<-unclass(Finalchurn%>%mutate(dec=ntile(callwait_Range,n=3))%>%count(dec)%>%unname())[[2]]
datcallwait_Range$churn_perc<-datcallwait_Range$n/datcallwait_Range$N
datcallwait_Range$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(callwait_Range,n=3))%>%group_by(dec)%>%summarise(min(callwait_Range)))[[2]]
datcallwait_Range$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(callwait_Range,n=3))%>%group_by(dec)%>%summarise(max(callwait_Range)))[[2]]
datcallwait_Range$varname<-rep("callwait_Range",nrow(datcallwait_Range))
#Finalchurn<-Finalchurn[,-grep("callwait_Range",colnames(Finalchurn))]


#ccrndmou_Range omit
datccrndmou_Range<-Finalchurn%>%mutate(dec=ntile(ccrndmou_Range,n=3))%>%count(churn,dec)%>%filter(churn==1)
datccrndmou_Range$N<-unclass(Finalchurn%>%mutate(dec=ntile(ccrndmou_Range,n=3))%>%count(dec)%>%unname())[[2]]
datccrndmou_Range$churn_perc<-datccrndmou_Range$n/datccrndmou_Range$N
datccrndmou_Range$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(ccrndmou_Range,n=3))%>%group_by(dec)%>%summarise(min(ccrndmou_Range)))[[2]]
datccrndmou_Range$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(ccrndmou_Range,n=3))%>%group_by(dec)%>%summarise(max(ccrndmou_Range)))[[2]]
datccrndmou_Range$varname<-rep("ccrndmou_Range",nrow(datccrndmou_Range))
#Finalchurn<-Finalchurn[,-grep("ccrndmou_Range",colnames(Finalchurn))]

#datadjqty 
datadjqty<-Finalchurn%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)
datadjqty$N<-unclass(Finalchurn%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
datadjqty$churn_perc<-datadjqty$n/datadjqty$N
datadjqty$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
datadjqty$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
datadjqty$varname<-rep("adjqty",nrow(datadjqty))

#ovrrev_Mean

datovrrev_Mean<-Finalchurn%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
datovrrev_Mean$N<-unclass(Finalchurn%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(dec)%>%unname())[[2]]
datovrrev_Mean$churn_perc<-datovrrev_Mean$n/datovrrev_Mean$N
datovrrev_Mean$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
datovrrev_Mean$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
datovrrev_Mean$varname<-rep("ovrrev_Mean",nrow(datovrrev_Mean))



#datrev_Mean
datrev_Mean<-Finalchurn%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
datrev_Mean$N<-unclass(Finalchurn%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
datrev_Mean$churn_perc<-datrev_Mean$n/datrev_Mean$N
datrev_Mean$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
datrev_Mean$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
datrev_Mean$varname<-rep("rev_Mean",nrow(datrev_Mean))
#ovrmou_Mean
datovrmou_Mean<-Finalchurn%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
datovrmou_Mean$N<-unclass(Finalchurn%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(dec)%>%unname())[[2]]
datovrmou_Mean$churn_perc<-datovrmou_Mean$n/datovrmou_Mean$N
datovrmou_Mean$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
datovrmou_Mean$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
datovrmou_Mean$varname<-rep("ovrmou_Mean",nrow(datovrmou_Mean))
#

#avg3mou
datavg3mou<-Finalchurn%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
datavg3mou$N<-unclass(Finalchurn%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
datavg3mou$churn_perc<-datavg3mou$n/datavg3mou$N
datavg3mou$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
datavg3mou$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
datavg3mou$varname<-rep("avg3mou",nrow(datavg3mou))
#avgmou
datavgmou<-Finalchurn%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)
datavgmou$N<-unclass(Finalchurn%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
datavgmou$churn_perc<-datavgmou$n/datavgmou$N
datavgmou$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
datavgmou$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
datavgmou$varname<-rep("avgmou",nrow(datavgmou))
#avg3qty
datavg3qty<-Finalchurn%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)
datavg3qty$N<-unclass(Finalchurn%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
datavg3qty$churn_perc<-datavg3qty$n/datavg3qty$N
datavg3qty$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
datavg3qty$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
datavg3qty$varname<-rep("avg3qty",nrow(datavg3qty))
#avgqty
datavgqty<-Finalchurn%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)
datavgqty$N<-unclass(Finalchurn%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
datavgqty$churn_perc<-datavgqty$n/datavgqty$N
datavgqty$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
datavgqty$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
datavgqty$varname<-rep("avgqty",nrow(datavgqty))
#avg6mou
datavg6mou<-Finalchurn%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
datavg6mou$N<-unclass(Finalchurn%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
datavg6mou$churn_perc<-datavg6mou$n/datavg6mou$N
datavg6mou$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
datavg6mou$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
datavg6mou$varname<-rep("avg6mou",nrow(datavg6mou))
#avg6qty
datavg6qty<-Finalchurn%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)
datavg6qty$N<-unclass(Finalchurn%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
datavg6qty$churn_perc<-datavg6qty$n/datavg6qty$N
datavg6qty$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
datavg6qty$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
datavg6qty$varname<-rep("avg6qty",nrow(datavg6qty))
#opk_dat_Mean 
datopk_dat_Mean<-Finalchurn%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)
datopk_dat_Mean$N<-unclass(Finalchurn%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]
datopk_dat_Mean$churn_perc<-datopk_dat_Mean$n/datopk_dat_Mean$N
datopk_dat_Mean$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(opk_dat_Mean)))[[2]]
datopk_dat_Mean$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(opk_dat_Mean)))[[2]]
datopk_dat_Mean$varname<-rep("opk_dat_Mean",nrow(datopk_dat_Mean))
#retdays
datretdays<-Finalchurn%>%mutate(dec=ntile(retdays,n=2))%>%count(churn,dec)%>%filter(churn==1)
datretdays$N<-unclass(Finalchurn%>%mutate(dec=ntile(retdays,n=2))%>%count(dec)%>%unname())[[2]]
datretdays$churn_perc<-datretdays$n/datretdays$N
datretdays$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(retdays,n=2))%>%group_by(dec)%>%summarise(min(retdays)))[[2]]
datretdays$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(retdays,n=2))%>%group_by(dec)%>%summarise(max(retdays)))[[2]]
datretdays$varname<-rep("retdays",nrow(datretdays))
#roam_Mean 
datroam_Mean<-Finalchurn%>%mutate(dec=ntile(roam_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)
datroam_Mean$N<-unclass(Finalchurn%>%mutate(dec=ntile(roam_Mean,n=2))%>%count(dec)%>%unname())[[2]]
datroam_Mean$churn_perc<-datroam_Mean$n/datroam_Mean$N
datroam_Mean$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(roam_Mean,n=2))%>%group_by(dec)%>%summarise(min(roam_Mean)))[[2]]
datroam_Mean$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(roam_Mean,n=2))%>%group_by(dec)%>%summarise(max(roam_Mean)))[[2]]
datroam_Mean$varname<-rep("roam_Mean",nrow(datroam_Mean))
#recv_sms_Mean 
datrecv_sms_Mean<-Finalchurn%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)
datrecv_sms_Mean$N<-unclass(Finalchurn%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%count(dec)%>%unname())[[2]]
datrecv_sms_Mean$churn_perc<-datrecv_sms_Mean$n/datrecv_sms_Mean$N
datrecv_sms_Mean$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%group_by(dec)%>%summarise(min(recv_sms_Mean)))[[2]]
datrecv_sms_Mean$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%group_by(dec)%>%summarise(max(recv_sms_Mean)))[[2]]
datrecv_sms_Mean$varname<-rep("recv_sms_Mean",nrow(datrecv_sms_Mean))
#mou_pead_Mean lesstahn3 omit it
datmou_pead_Mean<-Finalchurn%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)
datmou_pead_Mean$N<-unclass(Finalchurn%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%count(dec)%>%unname())[[2]]
datmou_pead_Mean$churn_perc<-datmou_pead_Mean$n/datmou_pead_Mean$N
datmou_pead_Mean$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%group_by(dec)%>%summarise(min(mou_pead_Mean)))[[2]]
datmou_pead_Mean$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%group_by(dec)%>%summarise(max(mou_pead_Mean)))[[2]]
datmou_pead_Mean$varname<-rep("mou_pead_Mean",nrow(datmou_pead_Mean))
#
#da_Mean
datda_Mean<-Finalchurn%>%mutate(dec=ntile(da_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
datda_Mean$N<-unclass(Finalchurn%>%mutate(dec=ntile(da_Mean,n=4))%>%count(dec)%>%unname())[[2]]
datda_Mean$churn_perc<-datda_Mean$n/datda_Mean$N
datda_Mean$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
datda_Mean$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
datda_Mean$varname<-rep("da_Mean",nrow(datda_Mean))
#da_Range

datda_Range<-Finalchurn%>%mutate(dec=ntile(da_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)
datda_Range$N<-unclass(Finalchurn%>%mutate(dec=ntile(da_Range,n=4))%>%count(dec)%>%unname())[[2]]
datda_Range$churn_perc<-datda_Range$n/datda_Range$N
datda_Range$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
datda_Range$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
datda_Range$varname<-rep("da_Range",nrow(datda_Range))
#datovr_Range  
datdatovr_Range<-Finalchurn%>%mutate(dec=ntile(datovr_Range,n=2))%>%count(churn,dec)%>%filter(churn==1)
datdatovr_Range$N<-unclass(Finalchurn%>%mutate(dec=ntile(datovr_Range,n=2))%>%count(dec)%>%unname())[[2]]
datdatovr_Range$churn_perc<-datdatovr_Range$n/datdatovr_Range$N
datdatovr_Range$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(datovr_Range,n=2))%>%group_by(dec)%>%summarise(min(datovr_Range)))[[2]]
datdatovr_Range$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(datovr_Range,n=2))%>%group_by(dec)%>%summarise(max(datovr_Range)))[[2]]
datdatovr_Range$varname<-rep("datovr_Range",nrow(datdatovr_Range))
#Finalchurn<-Finalchurn[,-grep("datovr_Range",colnames(Finalchurn))]

#datovr_Range 
datdatovr_Range<-Finalchurn%>%mutate(dec=ntile(datovr_Range,n=2))%>%count(churn,dec)%>%filter(churn==1)
datdatovr_Range$N<-unclass(Finalchurn%>%mutate(dec=ntile(datovr_Range,n=2))%>%count(dec)%>%unname())[[2]]
datdatovr_Range$churn_perc<-datdatovr_Range$n/datdatovr_Range$N
datdatovr_Range$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(datovr_Range,n=2))%>%group_by(dec)%>%summarise(min(datovr_Range)))[[2]]
datdatovr_Range$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(datovr_Range,n=2))%>%group_by(dec)%>%summarise(max(datovr_Range)))[[2]]
datdatovr_Range$varname<-rep("datovr_Range",nrow(datdatovr_Range))

#adjmou
datadjmou<-Finalchurn%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)
datadjmou$N<-unclass(Finalchurn%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
datadjmou$churn_perc<-datadjmou$n/datadjmou$N
datadjmou$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
datadjmou$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
datadjmou$varname<-rep("adjmou",nrow(datadjmou))
#totrev
dattotrev<-Finalchurn%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
dattotrev$N<-unclass(Finalchurn%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dattotrev$churn_perc<-dattotrev$n/dattotrev$N
dattotrev$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dattotrev$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dattotrev$varname<-rep("totrev",nrow(dattotrev))
#adjrev
datadjrev<-Finalchurn%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
datadjrev$N<-unclass(Finalchurn%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
datadjrev$churn_perc<-datadjrev$n/datadjrev$N
datadjrev$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
datadjrev$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
datadjrev$varname<-rep("adjrev",nrow(datadjrev))
#avgrev
datavgrev<-Finalchurn%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
datavgrev$N<-unclass(Finalchurn%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
datavgrev$churn_perc<-datavgrev$n/datavgrev$N
datavgrev$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
datavgrev$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
datavgrev$varname<-rep("avgrev",nrow(datavgrev))

#ATTEMPT_MEAN 
datATTEMPT_MEAN<-Finalchurn%>%mutate(dec=ntile(ATTEMPT_MEAN,n=10))%>%count(churn,dec)%>%filter(churn==1)
datATTEMPT_MEAN$N<-unclass(Finalchurn%>%mutate(dec=ntile(ATTEMPT_MEAN,n=10))%>%count(dec)%>%unname())[[2]]
datATTEMPT_MEAN$churn_perc<-datATTEMPT_MEAN$n/datATTEMPT_MEAN$N
datATTEMPT_MEAN$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(ATTEMPT_MEAN,n=10))%>%group_by(dec)%>%summarise(min(ATTEMPT_MEAN)))[[2]]
datATTEMPT_MEAN$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(ATTEMPT_MEAN,n=10))%>%group_by(dec)%>%summarise(max(ATTEMPT_MEAN)))[[2]]
datATTEMPT_MEAN$varname<-rep("ATTEMPT_MEAN",nrow(datATTEMPT_MEAN))
#COMPLETE_MEAN
datCOMPLETE_MEAN<-Finalchurn%>%mutate(dec=ntile(COMPLETE_MEAN,n=10))%>%count(churn,dec)%>%filter(churn==1)
datCOMPLETE_MEAN$N<-unclass(Finalchurn%>%mutate(dec=ntile(COMPLETE_MEAN,n=10))%>%count(dec)%>%unname())[[2]]
datCOMPLETE_MEAN$churn_perc<-datCOMPLETE_MEAN$n/datCOMPLETE_MEAN$N
datCOMPLETE_MEAN$GreaterThan<-unclass(Finalchurn%>%mutate(dec=ntile(COMPLETE_MEAN,n=10))%>%group_by(dec)%>%summarise(min(COMPLETE_MEAN)))[[2]]
datCOMPLETE_MEAN$LessThan<-unclass(Finalchurn%>%mutate(dec=ntile(COMPLETE_MEAN,n=10))%>%group_by(dec)%>%summarise(max(COMPLETE_MEAN)))[[2]]
datCOMPLETE_MEAN$varname<-rep("COMPLETE_MEAN",nrow(datCOMPLETE_MEAN))

################
#Storing into object
###################

dat<-rbind(datmou_mean,dattotmrc_Mean,datrev_Range,datmou_Range,datchange_mou,
           datdrop_blk_Mean,datdrop_vce_Range,datowylis_vce_Range,datmou_opkv_Range,
           datmonths,dattotcalls,datcustcare_Mean,datcallwait_Mean,datiwylis_vce_Mean,
           datcallwait_Range,datccrndmou_Range,datadjqty,datovrrev_Mean,datrev_Mean,
           datovrmou_Mean,datavg3mou,datavgqty,datavg6mou,datavg6qty,
           datda_Mean,datda_Range,dattotrev,datadjmou,datadjrev,datavgrev,datATTEMPT_MEAN,datCOMPLETE_MEAN,
           datavg3qty,datavgmou,datdatovr_Range,datmou_pead_Mean,datopk_dat_Mean,
           datrecv_sms_Mean,datretdays,datroam_Mean)
write.csv(dat,"continous.csv",row.names = F)
rm(datmou_mean,dattotmrc_Mean,datrev_Range,datmou_Range,datchange_mou,
   datdrop_blk_Mean,datdrop_vce_Range,datowylis_vce_Range,datmou_opkv_Range,
   datmonths,dattotcalls,datcustcare_Mean,datcallwait_Mean,datiwylis_vce_Mean,
   datcallwait_Range,datccrndmou_Range,datadjqty,datovrrev_Mean,datrev_Mean,
   datovrmou_Mean,datavg3mou,datavgqty,datavg6mou,datavg6qty,
   datda_Mean,datda_Range,dattotrev,datadjmou,datadjrev,datavgrev,datATTEMPT_MEAN,datCOMPLETE_MEAN,
   datavg3qty,datavgmou,datdatovr_Range,datmou_pead_Mean,datopk_dat_Mean,
   datrecv_sms_Mean,datretdays,datroam_Mean)
#################
Finalchurn$age1<-ifelse(Finalchurn$age1==0,"Default",ifelse(Finalchurn$age1<=30,"young",
                                                            ifelse(Finalchurn$age1>30&Finalchurn$age1<=55,"Mid","old")))
Finalchurn$age2<-ifelse(Finalchurn$age2==0,"Default",ifelse(Finalchurn$age2<=30,"young",ifelse(Finalchurn$age2>30&Finalchurn$age2<=55,"Mid","old")))

###########################
############
#converting factors as factor variables
Finalchurn$actvsubs<-as.factor(Finalchurn$actvsubs)
Finalchurn$age1<-as.factor(Finalchurn$age1)
Finalchurn$age2<-as.factor(Finalchurn$age2)
Finalchurn$churn<-as.factor(Finalchurn$churn)
Finalchurn$forgntvl<-as.factor(Finalchurn$forgntvl)
Finalchurn$hnd_price<-as.factor(Finalchurn$hnd_price)
Finalchurn$income<-as.factor(Finalchurn$income)
Finalchurn$models<-as.factor(Finalchurn$models)
Finalchurn$mtrcycle<-as.factor(Finalchurn$mtrcycle)
Finalchurn$truck<-as.factor(Finalchurn$truck)
Finalchurn$uniqsubs<-as.factor(Finalchurn$uniqsubs)
Finalchurn$mailresp<-as.factor(Finalchurn$mailresp)
Finalchurn$mailordr<-as.factor(Finalchurn$mailordr)

#############

######factor variables
#income
Finalchurn%>%count(churn,levels=income)%>%filter(churn==1)->datincome
datincome$N<-unclass(Finalchurn%>%filter(income%in%datincome$levels)%>%count(income))[[2]]
datincome$ChurnPerc<-datincome$n/datincome$N
datincome$Var.Name<-rep("income",nrow(datincome))

#crclscod
Finalchurn%>%count(churn,levels=crclscod)%>%filter(churn==1)->datcrclscod
datcrclscod$N<-unclass(Finalchurn%>%filter(crclscod%in%datcrclscod$levels)%>%count(crclscod))[[2]]
datcrclscod$ChurnPerc<-datcrclscod$n/datcrclscod$N
datcrclscod$Var.Name<-rep("crclscod",nrow(datcrclscod))
#asl_flag
Finalchurn%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datasl_flag
datasl_flag$N<-unclass(Finalchurn%>%filter(asl_flag%in%datasl_flag$levels)%>%count(asl_flag))[[2]]
datasl_flag$ChurnPerc<-datasl_flag$n/datasl_flag$N
datasl_flag$Var.Name<-rep("asl_flag",nrow(datasl_flag)) 
#prizm_social_one
Finalchurn%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datprizm_social_one
datprizm_social_one$N<-unclass(Finalchurn%>%filter(prizm_social_one%in%datprizm_social_one$levels)%>%count(prizm_social_one))[[2]]
datprizm_social_one$ChurnPerc<-datprizm_social_one$n/datprizm_social_one$N
datprizm_social_one$Var.Name<-rep("prizm_social_one",nrow(datprizm_social_one))
#area
Finalchurn%>%count(churn,levels=area)%>%filter(churn==1)->datarea
datarea$N<-unclass(Finalchurn%>%filter(area%in%datarea$levels)%>%count(area))[[2]]
datarea$ChurnPerc<-datarea$n/datarea$N
datarea$Var.Name<-rep("area",nrow(datarea))
#refurb_new
Finalchurn%>%count(churn,levels=refurb_new)%>%filter(churn==1)->datrefurb_new
datrefurb_new$N<-unclass(Finalchurn%>%filter(refurb_new%in%datrefurb_new$levels)%>%count(refurb_new))[[2]]
datrefurb_new$ChurnPerc<-datrefurb_new$n/datrefurb_new$N
datrefurb_new$Var.Name<-rep("refurb_new",nrow(datrefurb_new))
#dathnd_webcap
Finalchurn%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->dathnd_webcap
dathnd_webcap$N<-unclass(Finalchurn%>%filter(hnd_webcap%in%dathnd_webcap$levels)%>%count(hnd_webcap))[[2]]
dathnd_webcap$ChurnPerc<-dathnd_webcap$n/dathnd_webcap$N
dathnd_webcap$Var.Name<-rep("hnd_webcap",nrow(dathnd_webcap))
#marital
Finalchurn%>%count(churn,levels=marital)%>%filter(churn==1)->datmarital
datmarital$N<-unclass(Finalchurn%>%filter(marital%in%datmarital$levels)%>%count(marital))[[2]]
datmarital$ChurnPerc<-datmarital$n/datmarital$N
datmarital$Var.Name<-rep("marital",nrow(datmarital))
#ethnic
Finalchurn%>%count(churn,levels=ethnic)%>%filter(churn==1)->datethnic
datethnic$N<-unclass(Finalchurn%>%filter(ethnic%in%datethnic$levels)%>%count(ethnic))[[2]]
datethnic$ChurnPerc<-datethnic$n/datethnic$N
datethnic$Var.Name<-rep("ethnic",nrow(datethnic))
#age1
Finalchurn%>%count(churn,levels=age1)%>%filter(churn==1)->datage1
datage1$N<-unclass(Finalchurn%>%filter(age1%in%datage1$levels)%>%count(age1))[[2]]
datage1$ChurnPerc<-datage1$n/datage1$N
datage1$Var.Name<-rep("age1",nrow(datage1))
#age2
Finalchurn%>%count(churn,levels=age2)%>%filter(churn==1)->datage2
datage2$N<-unclass(Finalchurn%>%filter(age2%in%datage2$levels)%>%count(age2))[[2]]
datage2$ChurnPerc<-datage2$n/datage2$N
datage2$Var.Name<-rep("age2",nrow(datage2))
#models
Finalchurn%>%count(churn,levels=models)%>%filter(churn==1)->datmodels
datmodels$N<-unclass(Finalchurn%>%filter(models%in%datmodels$levels)%>%count(models))[[2]]
datmodels$ChurnPerc<-datmodels$n/datmodels$N
datmodels$Var.Name<-rep("models",nrow(datmodels))
#hnd_price
Finalchurn%>%count(churn,levels=hnd_price)%>%filter(churn==1)->dathnd_price
dathnd_price$N<-unclass(Finalchurn%>%filter(hnd_price%in%dathnd_price$levels)%>%count(hnd_price))[[2]]
dathnd_price$ChurnPerc<-dathnd_price$n/dathnd_price$N
dathnd_price$Var.Name<-rep("hnd_price",nrow(dathnd_price))
#actvsubs
Finalchurn%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datactvsubs
datactvsubs$N<-unclass(Finalchurn%>%filter(actvsubs%in%datactvsubs$levels)%>%count(actvsubs))[[2]]
datactvsubs$ChurnPerc<-datactvsubs$n/datactvsubs$N
datactvsubs$Var.Name<-rep("actvsubs",nrow(datactvsubs))
#uniqsubs
Finalchurn%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datuniqsubs
datuniqsubs$N<-unclass(Finalchurn%>%filter(uniqsubs%in%datuniqsubs$levels)%>%count(uniqsubs))[[2]]
datuniqsubs$ChurnPerc<-datuniqsubs$n/datuniqsubs$N
datuniqsubs$Var.Name<-rep("uniqsubs",nrow(datuniqsubs))
#forgntvl
Finalchurn%>%count(churn,levels=forgntvl)%>%filter(churn==1)->datforgntvl
datforgntvl$N<-unclass(Finalchurn%>%filter(forgntvl%in%datforgntvl$levels)%>%count(forgntvl))[[2]]
datforgntvl$ChurnPerc<-datforgntvl$n/datforgntvl$N
datforgntvl$Var.Name<-rep("forgntvl",nrow(datforgntvl))
#mailorder
Finalchurn%>%count(churn,levels=mailordr)%>%filter(churn==1)->datmailordr
datmailordr$N<-unclass(Finalchurn%>%filter(mailordr%in%datmailordr$levels)%>%count(mailordr))[[2]]
datmailordr$ChurnPerc<-datmailordr$n/datmailordr$N
datmailordr$Var.Name<-rep("mailordr",nrow(datmailordr))
#mtrcycle
Finalchurn%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->datmtrcycle
datmtrcycle$N<-unclass(Finalchurn%>%filter(mtrcycle%in%datmtrcycle$levels)%>%count(mtrcycle))[[2]]
datmtrcycle$ChurnPerc<-datmtrcycle$n/datmtrcycle$N
datmtrcycle$Var.Name<-rep("mtrcycle",nrow(datmtrcycle))
#retdays
Finalchurn%>%count(churn,levels=retdays)%>%filter(churn==1)->datretdays
datretdays$N<-unclass(Finalchurn%>%filter(retdays%in%datretdays$levels)%>%count(retdays))[[2]]
datretdays$ChurnPerc<-datretdays$n/datretdays$N
datretdays$Var.Name<-rep("retdays",nrow(datretdays))
#truck
Finalchurn%>%count(churn,levels=truck)%>%filter(churn==1)->dattruck
dattruck$N<-unclass(Finalchurn%>%filter(truck%in%dattruck$levels)%>%count(truck))[[2]]
dattruck$ChurnPerc<-dattruck$n/dattruck$N
dattruck$Var.Name<-rep("truck",nrow(dattruck))
#chrun
Finalchurn%>%count(churn,levels=churn)%>%filter(churn==1)->datchurn
datchurn$N<-unclass(Finalchurn%>%filter(churn%in%datchurn$levels)%>%count(churn))[[2]]
datchurn$ChurnPerc<-datchurn$n/datchurn$N
datchurn$Var.Name<-rep("churn",nrow(datchurn))
#maliresp
Finalchurn%>%count(churn,levels=mailresp)%>%filter(churn==1)->datmailresp
datmailresp$N<-unclass(Finalchurn%>%filter(mailresp%in%datmailresp$levels)%>%count(mailresp))[[2]]
datmailresp$ChurnPerc<-datmailresp$n/datmailresp$N
datmailresp$Var.Name<-rep("mailresp",nrow(datmailresp))
#
#car_buy
Finalchurn%>%count(churn,levels=car_buy)%>%filter(churn==1)->datcar_buy
datcar_buy$N<-unclass(Finalchurn%>%filter(car_buy%in%datcar_buy$levels)%>%count(car_buy))[[2]]
datcar_buy$ChurnPerc<-datcar_buy$n/datcar_buy$N
datcar_buy$Var.Name<-rep("car_buy",nrow(datcar_buy))
#csa
Finalchurn%>%count(churn,levels=csa)%>%filter(churn==1)->datcsa
datcsa$N<-unclass(Finalchurn%>%filter(csa%in%datcsa$levels)%>%count(csa))[[2]]
datcsa$ChurnPerc<-datcsa$n/datcsa$N
datcsa$Var.Name<-rep("csa",nrow(datcsa))

##
datcat<-rbind(
  datcrclscod,datasl_flag,datprizm_social_one,datarea,datretdays,
  datrefurb_new,dathnd_webcap,datmarital,datethnic,datage1,
  datage2,datmodels,dathnd_price,datactvsubs,datuniqsubs,datincome,datmailordr,
  datforgntvl,datmtrcycle,dattruck,datcar_buy,datcsa,datchurn,datmailresp)

write.csv(datcat,"category.csv",row.names = F)
rm(datcrclscod,datasl_flag,datprizm_social_one,datarea,datretdays,
   datrefurb_new,dathnd_webcap,datmarital,datethnic,datage1,
   datage2,datmodels,dathnd_price,datactvsubs,datuniqsubs,datincome,datmailordr,
   datforgntvl,datmtrcycle,dattruck,datcar_buy,datcsa,datchurn,datmailresp)
########################
#creating derived variables
Finalchurn$completionpercenatage<-Finalchurn$comp_vce_Mean/Finalchurn$plcd_vce_Mean
Finalchurn$optimum<-Finalchurn$ovrrev_Mean/Finalchurn$totrev

summary(chrun)
######
Finalchurn<-Finalchurn[,-grep("plcd_dat_Mean",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("plcd_vce_Mean",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("comp_dat_Mean",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("comp_vce_Mean",colnames(Finalchurn))]
##DROP_BLK_MEAN	BLCK_DAT_MEAN + BLCK_VCE_MEAN + DROP_DAT_MEAN + DROP_VCE_MEAN
Finalchurn<-Finalchurn[,-grep("blck_dat_Mean",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("drop_dat_Mean",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("drop_vce_Mean",colnames(Finalchurn))]
#OVRREV_MEAN	DATOVR_MEAN + VCEOVR_MEAN
Finalchurn<-Finalchurn[,-grep("datovr_Mean",colnames(Finalchurn))]
#
#dropping below as derived variable is formed 
Finalchurn<-Finalchurn[,-grep("ovrrev_Mean",colnames(Finalchurn))]
#Finalchurn<-Finalchurn[,-grep("totrev",colnames(Finalchurn))]:exclude this in model as optimum is there 
####dropping insignificant continous variables
Finalchurn<-Finalchurn[,-grep("custcare_Mean",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("callwait_Mean",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("callwait_Range",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("ccrndmou_Range",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("da_Mean",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("da_Range",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("datovr_Range",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("mou_pead_Mean",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("opk_dat_Mean",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("ovrmou_Mean",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("recv_sms_Mean",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("roam_Mean",colnames(Finalchurn))]
#Removing csa as it is not signigicant
Finalchurn<-Finalchurn[,-grep("csa",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("crclscod",colnames(Finalchurn))]
#dropping all remaining range varables as we have all mean for ranges 
Finalchurn<-Finalchurn[,-grep("drop_vce_Range",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("mou_opkv_Range",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("owylis_vce_Range",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("rev_Range",colnames(Finalchurn))]
Finalchurn<-Finalchurn[,-grep("mou_Range",colnames(Finalchurn))]







################
Var<-colnames(Finalchurn)
class(var)
varname<-as.data.frame(Var)
rm(Var)
#storing datatype
varname$datatype<-sapply(Finalchurn,class)

View(varname)

length(varname$datatype)

##################
#############
##missing count
#############
missing_count<-colSums(is.na(Finalchurn))
lists<-1
maxs<-1
mins<-1
average<-1
sd<-1
u<-1

missing_count1<-data.frame(variable=names(Finalchurn),data_type=lists,min=mins,maxs=maxs,average=average,
                          sd=sd,unique=u,missing=missing_count)


##
##REmoving index
index<-which(is.na(Finalchurn$mou_Mean))
Finalchurn<-Finalchurn[-index,]
index<-which(is.na(Finalchurn$age2))
Finalchurn<-Finalchurn[-index,]
index<-which(is.na(Finalchurn$eqpdays))
Finalchurn<-Finalchurn[-index,]
index<-which(is.na(Finalchurn$area))
Finalchurn<-Finalchurn[-index,]


############
## imputing continous variable missing values with regression tree
###################
library("randomForest")   
Finalchurn$change_mou<-na.roughfix(Finalchurn$change_mou) 
Finalchurn$avg6mou<-na.roughfix(Finalchurn$avg6mou) 
Finalchurn$avg6qty<-na.roughfix(Finalchurn$avg6qty) 
##doing this as dividing twp variable which has 0 given as NAN so replcaed nan with 0
Finalchurn$completionpercenatage[is.na(Finalchurn$completionpercenatage)]<-0
##
missing_count<-colSums(is.na(Finalchurn))
lists<-1
maxs<-1
mins<-1
average<-1
sd<-1
u<-1

missing_count1<-data.frame(variable=names(Finalchurn),data_type=lists,min=mins,maxs=maxs,average=average,
                           sd=sd,unique=u,missing=missing_count)


#######################

unique(chrun$hnd_webcap)
Finalchurn$hnd_webcap<-ifelse(is.na(Finalchurn$hnd_webcap),"missing",Finalchurn$hnd_webcap)
str(Finalchurn$hnd_webcap)
Finalchurn$hnd_webcap<-as.factor(Finalchurn$hnd_webcap)
summary(Finalchurn$hnd_webcap)
Finalchurn$hnd_webcap<-factor(Finalchurn$hnd_webcap,labels=c("UNKW","WC","WCMB","missing"))

###
summary(Finalchurn$prizm_social_one)
Finalchurn$prizm_social_one<-ifelse(is.na(Finalchurn$prizm_social_one),"missing",Finalchurn$prizm_social_one)
str(Finalchurn$prizm_social_one)
Finalchurn$prizm_social_one<-as.factor(Finalchurn$prizm_social_one)
summary(Finalchurn$prizm_social_one)
Finalchurn$prizm_social_one<-factor(Finalchurn$prizm_social_one,labels=c("C","R","S","T","U","Missing"))
####
summary(Finalchurn$income)
Finalchurn$prizm_social_one<-ifelse(is.na(Finalchurn$prizm_social_one),"missing",Finalchurn$prizm_social_one)
str(Finalchurn$prizm_social_one)
Finalchurn$prizm_social_one<-as.factor(Finalchurn$prizm_social_one)
summary(Finalchurn$prizm_social_one)
Finalchurn$prizm_social_one<-factor(Finalchurn$prizm_social_one,labels=c("C","R","S","T","U","Missing"))
###########
summary(Finalchurn$income)
Finalchurn$income<-ifelse(is.na(Finalchurn$income),"missing",Finalchurn$income)
str(Finalchurn$income)
Finalchurn$income<-as.factor(Finalchurn$income)
summary(Finalchurn$income)
################################
unique(Finalchurn$hnd_price)

Finalchurn$hnd_price[is.na(Finalchurn$hnd_price)]<-199.9899902

summary(Finalchurn)

colSums(is.na(Finalchurn))

##########################################################################

Var<-colnames(Finalchurn)
class(var)
varname<-as.data.frame(Var)
rm(Var)
#storing datatype
varname$datatype<-sapply(Finalchurn,class)

View(varname)

length(varname$datatype)
#################checking miising count#################

missing_count<-colSums(is.na(Finalchurn))
lists<-1
maxs<-1
mins<-1
average<-1
sd<-1
u<-1

missing_count1<-data.frame(variable=names(Finalchurn),data_type=lists,min=mins,maxs=maxs,average=average,
                           sd=sd,unique=u,missing=missing_count)
missing_count1<-data.frame(missing_count1)
rm(missing_count1)
##########################################################

########outlier treatment###################
par()
List<-colnames(Finalchurn)
List
List<-List[-c(7,18:33,35:38,43)]
#par(mfrow =c(3,11))
for(i in 1:length(List))
{
  boxplot(Finalchurn[,List[i]],main = List[i])
}

for(i in 1:length(List))
{
  plot(Finalchurn[,List[i]],main = List[i])
}

for(i in 1:length(List))
{
  x<-boxplot(Finalchurn[,List[i]],main = List[i])
  x$out
  index<-which(Finalchurn[,List[i]]%in%x$out)
  Finalchurn[index,List[i]]<-mean(Finalchurn[,List[i]],na.rm=T)
}



for(i in 1:length(List))
{
  boxplot(Finalchurn[,List[i]],main = List[i])
}

for(i in 1:length(List))
{
  plot(Finalchurn[,List[i]],main = List[i])
}
dev.off()

###############
#removing unnessary variables
rm(dat,dataQualityReport,datcat,missing_count1,varname1)
rm(average,i,index,List,lists,maxs,mins,missing_count,sd,u,x)
rm(varname)
############
Finalchurn1<-Finalchurn
##
colSums(is.na(Finalchurn1))
Var<-colnames(Finalchurn1)
class(var)
varname<-as.data.frame(Var)
rm(Var)
#storing datatype
varname$datatype<-sapply(Finalchurn1,class)
###########
set.seed(400)
index<-sample(nrow(Finalchurn1),0.70*nrow(Finalchurn1),replace=F)
train<-Finalchurn1[index,]
head(train$mou_Mean)
test<-Finalchurn1[-index,]
########################
dim(test)
dim(train)
model<-glm(churn~.,data=train[,-c(40,43)],family = "binomial")
summary(model)
#step(model,direction = "both")


####creating sginficance categorical variable 

train$asl_flagY<-ifelse(train$asl_flag=="Y",1,0)
test$asl_flagY<-ifelse(test$asl_flag=="Y",1,0)

#2
train$prizm_social_oneR<-ifelse(train$prizm_social_one=="R",1,0)
test$prizm_social_oneR<-ifelse(test$prizm_social_one=="R",1,0)
train$prizm_social_oneT<-ifelse(train$prizm_social_one=="T",1,0)
test$prizm_social_oneT<-ifelse(test$prizm_social_one=="T",1,0)
#3
train$CALIFORNIANORTHAREA<-ifelse(train$area=="CALIFORNIA NORTH AREA",1,0)
test$CALIFORNIANORTHAREA<-ifelse(test$area=="CALIFORNIA NORTH AREA",1,0)

train$CENTRALSOUTHTEXASAREA<-ifelse(train$area=="CENTRAL/SOUTH TEXAS AREA",1,0)
test$CENTRALSOUTHTEXASAREA<-ifelse(test$area=="CENTRAL/SOUTH TEXAS AREA",1,0)

train$NEWYORKCITYAREA<-ifelse(train$area=="NEW YORK CITY AREA",1,0)
test$NEWYORKCITYAREA<-ifelse(test$area=="NEW YORK CITY AREA",1,0)
test$NORTHFLORIDAAREA<-ifelse(test$area=="NORTH FLORIDA AREA",1,0)
train$NORTHFLORIDAAREA<-ifelse(train$area=="NORTH FLORIDA AREA",1,0)
train$NORTHWESTROCKYMOUNTAINAREA<-ifelse(train$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
test$NORTHWESTROCKYMOUNTAINAREA<-ifelse(test$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
test$SOUTHfloridaarea<-ifelse(test$area=="SOUTH FLORIDA AREA",1,0)
train$SOUTHfloridaarea<-ifelse(train$area=="SOUTH FLORIDA AREA",1,0)
train$TENNESSEEAREA<-ifelse(train$area=="TENNESSEE AREA",1,0)
test$TENNESSEEAREA<-ifelse(test$area=="TENNESSEE AREA",1,0)
#################


###
train$refurb_newR<-ifelse(train$refurb_new=="R",1,0)
test$refurb_newR<-ifelse(test$refurb_new=="R",1,0)
###
train$ethnicC<-ifelse(train$ethnic=="C",1,0)
test$ethnicC<-ifelse(test$ethnic=="C",1,0)

train$ethnicF<-ifelse(train$ethnic=="F",1,0)
test$ethnicF<-ifelse(test$ethnic=="F",1,0)

train$ethnicG<-ifelse(train$ethnic=="G",1,0)
test$ethnicG<-ifelse(test$ethnic=="G",1,0)
####
train$ethnicI<-ifelse(train$ethnic=="I",1,0)
test$ethnicI<-ifelse(test$ethnic=="I",1,0)

train$ethnicN<-ifelse(train$ethnic=="N",1,0)
test$ethnicN<-ifelse(test$ethnic=="N",1,0)

train$ethnicM<-ifelse(train$ethnic=="M",1,0)
test$ethnicM<-ifelse(test$ethnic=="M",1,0)
train$ethnicS<-ifelse(train$ethnic=="S",1,0)
test$ethnicS<-ifelse(test$ethnic=="S",1,0)
train$ethnicU<-ifelse(train$ethnic=="U",1,0)
test$ethnicU<-ifelse(test$ethnic=="U",1,0)
train$ethnicZ<-ifelse(train$ethnic=="Z",1,0)
test$ethnicZ<-ifelse(test$ethnic=="Z",1,0)

train$age1MId<-ifelse(train$age1=="Mid",1,0)
train$age1Old<-ifelse(train$age1=="old",1,0)
train$age1Young<-ifelse(train$age1=="young",1,0)
test$age1MId<-ifelse(test$age1=="Mid",1,0)
test$age1Old<-ifelse(test$age1=="old",1,0)
test$age1Young<-ifelse(test$age1=="young",1,0)

##
train$age2Young<-ifelse(train$age2=="young",1,0)
test$age2Young<-ifelse(test$age2=="young",1,0)
###
train$models10<-ifelse(train$models=="10",1,0)
test$models10<-ifelse(test$models=="10",1,0)

##
train$hnd_price129.9899902<-ifelse(train$hnd_price=="129.9899902",1,0)
test$hnd_price129.9899902<-ifelse(test$hnd_price=="129.9899902",1,0)
train$hnd_price199.9899902<-ifelse(train$hnd_price=="199.9899902",1,0)
test$hnd_price199.9899902<-ifelse(test$hnd_price=="199.9899902",1,0)
train$hnd_price249.9899902<-ifelse(train$hnd_price=="249.9899902",1,0)
test$hnd_price249.9899902<-ifelse(test$hnd_price=="249.9899902",1,0)
####
train$uniqsubs2<-ifelse(train$uniqsubs=="2",1,0)
test$uniqsubs2<-ifelse(test$uniqsubs=="2",1,0)
train$uniqsubs3<-ifelse(train$uniqsubs=="3",1,0)
test$uniqsubs3<-ifelse(test$uniqsubs=="3",1,0)
train$uniqsubs4<-ifelse(train$uniqsubs=="4",1,0)
test$uniqsubs4<-ifelse(test$uniqsubs=="4",1,0)
train$uniqsubs5<-ifelse(train$uniqsubs=="5",1,0)
test$uniqsubs5<-ifelse(test$uniqsubs=="5",1,0)
train$uniqsubs7<-ifelse(train$uniqsubs=="7",1,0)
test$uniqsubs7<-ifelse(test$uniqsubs=="7",1,0)
train$uniqsubs9<-ifelse(train$uniqsubs=="9",1,0)
test$uniqsubs9<-ifelse(test$uniqsubs=="9",1,0)
###########creating test datatypes#############
summary(model)
#################

model2<-glm(churn~+mou_Mean+totmrc_Mean+change_mou+drop_blk_Mean+months+eqpdays+iwylis_vce_Mean+
              adjqty+rev_Mean+avgmou+avg3qty+asl_flagY+prizm_social_oneR+prizm_social_oneT+
              CALIFORNIANORTHAREA+ CENTRALSOUTHTEXASAREA+NEWYORKCITYAREA+NORTHFLORIDAAREA+
              NORTHWESTROCKYMOUNTAINAREA+SOUTHfloridaarea+TENNESSEEAREA+ refurb_newR+
              ethnicC+ethnicF+ethnicG+ethnicI+ethnicM+ethnicN+ethnicS+ethnicU+ethnicZ+
              age1MId+age1Old+age1Young+age2Young+models10+hnd_price129.9899902+
              hnd_price199.9899902+hnd_price249.9899902+uniqsubs2+uniqsubs3+uniqsubs4+uniqsubs4+
              uniqsubs5+uniqsubs7+uniqsubs9+retdays+adjmou+adjrev+completionpercenatage+optimum
            ,data=train,family = "binomial")

summary(model2)
###
model3<-glm(churn~+mou_Mean+totmrc_Mean+change_mou+drop_blk_Mean+months+eqpdays+iwylis_vce_Mean+
              adjqty+rev_Mean+avgmou+avg3qty+asl_flagY+prizm_social_oneR+prizm_social_oneT+
              CALIFORNIANORTHAREA+ CENTRALSOUTHTEXASAREA+NEWYORKCITYAREA+NORTHFLORIDAAREA+
              NORTHWESTROCKYMOUNTAINAREA+SOUTHfloridaarea+TENNESSEEAREA+ refurb_newR+
              ethnicC+ethnicF+ethnicG+ethnicN+ethnicS+ethnicU+ethnicZ+
              age1MId+age1Old+age2Young+models10+hnd_price129.9899902+
              hnd_price199.9899902+hnd_price249.9899902+uniqsubs2+uniqsubs3+uniqsubs4+uniqsubs4+
              uniqsubs5+uniqsubs7+retdays+adjrev+completionpercenatage+optimum
            ,data=train,family = "binomial")

summary(model3)

##########
model4<-glm(churn~+mou_Mean+totmrc_Mean+change_mou+drop_blk_Mean+months+eqpdays+iwylis_vce_Mean+
              adjqty+rev_Mean+avgmou+avg3qty+asl_flagY+prizm_social_oneR+prizm_social_oneT+
              CALIFORNIANORTHAREA+ CENTRALSOUTHTEXASAREA+NEWYORKCITYAREA+NORTHFLORIDAAREA+
              NORTHWESTROCKYMOUNTAINAREA+SOUTHfloridaarea+TENNESSEEAREA+ refurb_newR+
              ethnicC+ethnicG+ethnicN+ethnicS+ethnicU+ethnicZ+
              age1MId+age1Old+age2Young+models10+hnd_price129.9899902+
              hnd_price199.9899902+hnd_price249.9899902+uniqsubs2+uniqsubs3+uniqsubs4+uniqsubs4+
              uniqsubs5+uniqsubs7+retdays+adjrev+completionpercenatage+optimum
            ,data=train,family = "binomial")

summary(model4)
######


########################################
library(car)

vif(model4)

###########no singel value is greater than 5 so keep as is 

######################
library(irr)
#modeltesting

pred<-predict(model4,type="response",newdata=test)

head(pred)
library(caret)
#assuming
##############
confint(model4)



predicted <- model4$fitted.values
head(predicted)

head(train$churn)
##chossing cut off levels
table(Finalchurn1$churn)/nrow(Finalchurn1)

predbkt<-ifelse(predicted>0.239,1,0)
head(predbkt)
table(predbkt,train$churn)
table(train$churn)
library(ROCR)

# The prediction function of the ROCR library basically creates a structure to validate 
#our predictions qirh actual values

pred<-prediction(predicted,train$churn)

# "tpr" and "fpr" are arguments of the "performance" function indicating that the plot is 
#between the true positive rate and the false positive rate.
perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")


#How to choose cutoff's?
#use @ to access the slots
cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
head(cutoffs)
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(cutoffs)


auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc

###################

####

head(sort(abs(model4$coefficients),decreasing = TRUE),10)

names(model4$coefficients)

##should check once 

##
#2)

####
#4
library(gains)
test$churn<-as.numeric(test$churn)
gains(test$churn,predict(model4,type="response",newdata=test),groups = 10)

test$prob<-predict(model4,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

targeted<-test[test$prob>0.2755007&test$prob<=0.8191556,"Customer_ID"]
targeted<-data.frame(targeted)
nrow(targeted)
write.csv(targeted,"targeted.csv",row.names = F)

#############5

predict1<-predict(model4,type="response",newdata=test)
test$prob<-predict(model4,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
predict2<-ifelse(predict1<0.20,"low_scores",ifelse(predict1>0.20&predict1<0.30,
                                                  "Medium_scores", "high_scores"))

table(predict2,test$churn)
str(test$totrev)
quantile(test$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
revenue_lables<-ifelse(test$totrev<670.660,"Low_revenue",ifelse(test$totrev
                                          >670.660&test$totrev<1034.281,"Medium_revenue"                      
                                          ,"High_revenue"))
table(revenue_lables)
table(predict2,revenue_lables)

test$problevels<-ifelse(predict1<0.20,"low_scores",ifelse(predict1>0.20&predict1<0.30,
                                                   "Medium_scores", "high_scores"))

test$revenue_lables<-ifelse(test$totrev<670.660,"Low_revenue",ifelse(test$totrev
                                                                >670.660&test$totrev<1034.281,"Medium_revenue"                      
                                                                ,"High_revenue"))



targeted1<-test[test$problevels=="high_scores"&test$revenue_lables<="High_revenue","Customer_ID"]
targeted1<-as.data.frame(targeted1)
nrow(targeted1)
write.csv(targeted1,"targeted1.csv",row.names = F)







##############################






