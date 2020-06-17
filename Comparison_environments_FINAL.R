## Comparing Scenarios
# This script loads all of the Rdata files into invidiaul environments so that results can be compared in one place. 
library(data.table)

# Load required R libraries 
library(rmarkdown)
library(data.table)
library(RColorBrewer)
library(plyr)
library(stringr)
library(Hmisc)
library(lubridate)

###### SETUP ######################### 
setwd('./Results/') #location of files

ccsm_rcp45=new.env() #Set one environment variable for each model type
ccsm_rcp85=new.env()
gfdl_rcp45=new.env()
gfdl_rcp85=new.env()
inm_rcp45=new.env()
inm_rcp85=new.env()
ccsm_hist=new.env()
gfdl_hist=new.env()
inm_hist=new.env()

###### LOAD DATA #####################
load('./future_climate_V8/ccsm_rcp45/IM3_ccsm_rcp45.RData',ccsm_rcp45)
load('./future_climate_V8/ccsm_rcp85/IM3_ccsm_rcp85.RData',ccsm_rcp85)
load('./future_climate_V8/gfdl_rcp45/IM3_gfdl_rcp45.RData',gfdl_rcp45)
load('./future_climate_V8/gfdl_rcp85/IM3_gfdl_rcp85.RData',gfdl_rcp85)
load('./future_climate_V8/inm_rcp45/IM3_inm_rcp45.RData',inm_rcp45)
load('./future_climate_V8/inm_rcp85/IM3_inm_rcp85.RData',inm_rcp85)
load('./historic_climate_V8/ccsm/IM3_ccsm_historical.RData',ccsm_hist)
load('./historic_climate_V8/gfdl/IM3_gfdl_historical.RData',gfdl_hist)
load('./historic_climate_V8/inm/IM3_inm_historical.RData',inm_hist)

list_of_names=c("ccsm_rcp85","gfdl_rcp85","inm_rcp85",
                "ccsm_rcp45","gfdl_rcp45","inm_rcp45",
                "ccsm_hist." ,"gfdl_hist." ,"inm_hist."  )
env_list=list(ccsm_rcp85,gfdl_rcp85,inm_rcp85,
              ccsm_rcp45,gfdl_rcp45,inm_rcp45,
              ccsm_hist ,gfdl_hist ,inm_hist)


##### REGION INFO ###################################
BA_to_RSG=ccsm_rcp45$BA_to_RSG

##### FUNCTION TO RBIND FROM ENVIRONMENTS ############
rbindruns = function(item,env_list) {

modellist<-list(data.table(0))
for (i in 1:length(env_list)) {
    modellist[[i]]=get(item,envir=env_list[[i]])                    #Get the specified item from each i environment
}
setattr(modellist,'names',list_of_names)
bound=rbindlist(modellist,use.names=TRUE,idcol="Model")            #Combine
return(bound)

}

###### FUNCTION TO ORGANIZE BOUND RESULTS by adding new columns and cleaning up ##################
add_cols=function(item){
item[, GCM:=str_trunc(item$Model,4,side=c("right"),ellipsis="")] #Add GCM column
item[, RCP:=str_trunc(item$Model,5,side=c("left"),ellipsis="")]  #Add RCP column
item[, Scenario := gsub('.*85_', '', item$Scenario)]             #Reduce to only year
item[, Scenario := gsub('.*45_', '', item$Scenario)]             #Reduce to only year
item[, Scenario := gsub('.*year_', '', item$Scenario)]           #Reduce to only year
item[, Scenario := gsub('.*inm_', '', item$Scenario)]           #Reduce to only year
item[, Scenario := gsub('.*ccsm_', '', item$Scenario)]           #Reduce to only year
item[, Scenario := gsub('.*gfdl_', '', item$Scenario)]           #Reduce to only year

item[, Scenario:=as.numeric(item$Scenario)]
item[(Scenario>1980&Scenario<2011),Period:='1995'] #Add labels for 30 year periods
item[(Scenario>2035&Scenario<2066),Period:='2050'] 
item[(Scenario>2065&Scenario<2096),Period:='2080']
return(item)
}

###### FUNCTION TO MELT HYDRO GENERATION ITEMS #####################################################
meltitem = function(item,env_list,new_item,id_list){
  for (i in 1:length(env_list)) {
    temp_item=get(item,envir=env_list[[i]])    
    temp_item<-melt(temp_item, id.vars=id_list, variable.name='Scenario')
    assign(new_item,temp_item,envir=env_list[[i]])
    rm(temp_item)
  }
}


#### FUNCTION to t-test a variable by 30-year period for each GCM & RCP ############################

testitem = function(myformula,source){
  # t-test in R uses default var.equal=FALSE so it does not assume equal variance. 
  test_table<-data.table(GCM=c("ccsm","ccsm","gfdl","gfdl","inm_","inm_"),
                             RCP=c("rcp45","rcp85"),`Period`=c("2050"),`p-val`=c(0))
  
  test_table[(GCM=="ccsm"&(RCP=="hist."|(RCP=="rcp45"&Period=="2050"))),c("p-val")]<-t.test(myformula,data=source
            [(GCM=="ccsm"&(RCP=="hist."|(RCP=="rcp45"&Period=="2050"))),])$p.value

  test_table[(GCM=="ccsm"&(RCP=="hist."|(RCP=="rcp85"&Period=="2050"))),c("p-val")]<-t.test(myformula,data=source
            [(GCM=="ccsm"&(RCP=="hist."|(RCP=="rcp85"&Period=="2050"))),])$p.value
  
  test_table[(GCM=="gfdl"&(RCP=="hist."|(RCP=="rcp45"&Period=="2050"))),c("p-val")]<-t.test(myformula,data=source
            [(GCM=="gfdl"&(RCP=="hist."|(RCP=="rcp45"&Period=="2050"))),])$p.value

  test_table[(GCM=="gfdl"&(RCP=="hist."|(RCP=="rcp85"&Period=="2050"))),c("p-val")]<-t.test(myformula,data=source
            [(GCM=="gfdl"&(RCP=="hist."|(RCP=="rcp85"&Period=="2050"))),])$p.value

  test_table[(GCM=="inm_"&(RCP=="hist."|(RCP=="rcp45"&Period=="2050"))),c("p-val")]<-t.test(myformula,data=source
            [(GCM=="inm_"&(RCP=="hist."|(RCP=="rcp45"&Period=="2050"))),])$p.value

  test_table[(GCM=="inm_"&(RCP=="hist."|(RCP=="rcp85"&Period=="2050"))),c("p-val")]<-t.test(myformula,data=source
            [(GCM=="inm_"&(RCP=="hist."|(RCP=="rcp85"&Period=="2050"))),])$p.value
      
      
  test_table[,`Sig. at 0.05`:=(`p-val`<=0.05)*1] #Reject the null hypothesis that means are equal if less than 0.05 p-value; i.e. strong evidence to reject
  test_table[,`Sig. at 0.1`:=(`p-val`<=0.1)*1]

  return(test_table)
}


###########################################################################



###### COMBINE DATA ##################################

annual.gen.stats<-rbindruns('annual.gen.stats',env_list)
annual.gen.stats<-add_cols(annual.gen.stats)
annual.gen.stats[,UnitCost:=`Cost ($)`/`Generation (GWh)`]

violations<-rbindruns('violations',env_list)
violations<-add_cols(violations)


## Create hydro regional from regional/type detail####
meltitem('gen.type.region',env_list,'hydro.regionals',c("Type","TEPPC.Region"))
hydro.regionals<-rbindruns('hydro.regionals',env_list)[Type=="Hydro"]
hydro.regionals<-add_cols(hydro.regionals)

## Add column with scenario hydro sum
hydro.regionals[,ScenarioSumHydro:=sum(value),by=c("Scenario","GCM","RCP")]
hydro.regionals[,Region.Percent:=value/ScenarioSumHydro*100]
hydro.regionals.cast<-dcast(hydro.regionals[,c("TEPPC.Region","Model","Scenario","value")],Model+Scenario~TEPPC.Region)

## Create hydro hourly from region/type/time detail ###
meltitem('gen.type.region.time',env_list,'hydro.regionals.time',c("Type","TEPPC.Region","time"))
hydro.intervals<-rbindruns('hydro.regionals.time',env_list)[Type=="Hydro"]
hydro.intervals<-add_cols(hydro.intervals)
hydro.intervals[,MonthDate:=(floor_date(time,unit="month"))]
hydro.intervals[,Month:=as.character(month(MonthDate))]
setnames(hydro.intervals,"value","MWh")
hydro.intervals$Month<-factor(hydro.intervals$Month,levels=c('1','2','3','4','5','6','7','8','9','10','11','12'))

## Regional costs ##
meltitem('region.cost',env_list,'region.cost.melt',c("TEPPC.Region"))
region.cost<-rbindruns('region.cost.melt',env_list)
region.cost<-add_cols(region.cost)
region.cost.avg<-region.cost[,mean(value),by=c("TEPPC.Region","GCM","RCP")]
region.cost.hist<-region.cost.avg[RCP=="hist.",]
region.cost.avg<-merge(region.cost.avg,region.cost.hist,
                   by=c("TEPPC.Region","GCM"),
                   all.x=TRUE,suffixes=c("",".hist"))
region.cost.avg[,`Percent Increase in Cost`:=(V1-V1.hist)/V1.hist*100]

## Regional generation ##
meltitem('gen.type.region',env_list,'gen.type.region.melt',c("TEPPC.Region","Type"))
region.gen<-rbindruns('gen.type.region.melt',env_list)
region.gen<-add_cols(region.gen)
region.gen<-region.gen[,sum(value),by=c("TEPPC.Region","Scenario","GCM","RCP","Model","Period")]
setnames(region.gen,"V1","GWh")

region.gen.cost<-merge(region.cost,region.gen,by=c("TEPPC.Region","Scenario","GCM","RCP","Model","Period"))
region.gen.cost[,UnitCost:=value/GWh/1000]
region.gen.cost.avg<-region.gen.cost[,mean(UnitCost),by=c("TEPPC.Region","GCM","RCP")]
region.gen.cost.hist<-region.gen.cost.avg[RCP=="hist.",]
region.gen.cost.avg<-merge(region.gen.cost.avg,region.gen.cost.hist,
                           by=c("TEPPC.Region","GCM"),
                           all.x=TRUE,suffixes=c(""," Hist"))
region.gen.cost.avg[,`Change in Average Unit Cost ($/MWh)`:=(`V1`-`V1 Hist`)]

## Create hydro total from regional#######
ccsm_rcp85$hydro.totals<-ccsm_rcp85$hydro.regionals[Type=="Hydro",list(`Hydro GWh`=sum(value)),by="Scenario"]
gfdl_rcp85$hydro.totals<-gfdl_rcp85$hydro.regionals[Type=="Hydro",list(`Hydro GWh`=sum(value)),by="Scenario"]
inm_rcp85$hydro.totals <- inm_rcp85$hydro.regionals[Type=="Hydro",list(`Hydro GWh`=sum(value)),by="Scenario"]
ccsm_rcp45$hydro.totals<-ccsm_rcp45$hydro.regionals[Type=="Hydro",list(`Hydro GWh`=sum(value)),by="Scenario"]
gfdl_rcp45$hydro.totals<-gfdl_rcp45$hydro.regionals[Type=="Hydro",list(`Hydro GWh`=sum(value)),by="Scenario"]
inm_rcp45$hydro.totals <- inm_rcp45$hydro.regionals[Type=="Hydro",list(`Hydro GWh`=sum(value)),by="Scenario"]
ccsm_hist$hydro.totals <- ccsm_hist$hydro.regionals[Type=="Hydro",list(`Hydro GWh`=sum(value)),by="Scenario"]
gfdl_hist$hydro.totals <- gfdl_hist$hydro.regionals[Type=="Hydro",list(`Hydro GWh`=sum(value)),by="Scenario"]
inm_hist$hydro.totals  <-  inm_hist$hydro.regionals[Type=="Hydro",list(`Hydro GWh`=sum(value)),by="Scenario"]

## Create hydro deltas
ccsm_rcp85$hydro.totals$DeltaGWh<-c(NA,diff(ccsm_rcp85$hydro.totals$`Hydro GWh`))
gfdl_rcp85$hydro.totals$DeltaGWh<-c(NA,diff(gfdl_rcp85$hydro.totals$`Hydro GWh`))
inm_rcp85$hydro.totals$DeltaGWh <-c(NA,diff(inm_rcp85$hydro.totals$`Hydro GWh`))
ccsm_rcp45$hydro.totals$DeltaGWh<-c(NA,diff(ccsm_rcp45$hydro.totals$`Hydro GWh`))
gfdl_rcp45$hydro.totals$DeltaGWh<-c(NA,diff(gfdl_rcp45$hydro.totals$`Hydro GWh`))
inm_rcp45$hydro.totals$DeltaGWh <-c(NA,diff(inm_rcp45$hydro.totals$`Hydro GWh`))
ccsm_hist$hydro.totals$DeltaGWh<-c(NA,diff(ccsm_hist$hydro.totals$`Hydro GWh`))
gfdl_hist$hydro.totals$DeltaGWh<-c(NA,diff(gfdl_hist$hydro.totals$`Hydro GWh`))
inm_hist $hydro.totals$DeltaGWh <-c(NA,diff(inm_hist$hydro.totals$`Hydro GWh`))

# Combine
hydro.totals<-rbindruns('hydro.totals',env_list)
hydro.totals<-add_cols(hydro.totals)

# Bin hydro data
nbins=16
brks<-seq(0,300000,length=nbins)
mids<-brks[1:nbins]+brks[2]-brks[1]   #seq(2500,297500,length=60)
hydro.totals[,bin:=findInterval(`Hydro GWh`,brks,all.inside=TRUE),by=c("GCM","RCP")] #Bin data by hydro
hydro.bins<-data.table(bin=rep(c(1:nbins),time=9),GWh=rep(brks,time=9),GCM=rep(c("ccsm","gfdl","inm_"),each=nbins*3),RCP=rep(c("hist.","rcp45","rcp85"),each=nbins))
hydro.bins=merge(hydro.bins,hydro.totals[,list(.N),by=c("bin","GCM","RCP")],by=c("bin","GCM","RCP"),all=TRUE) #count number of years per bin
hydro.bins[is.na(N)==TRUE,N:=0]
hydro.bins[,prob:=N/30] #Convert to prob.


# Add hydro to annual gen stats 
annual.gen.stats<- merge(annual.gen.stats,hydro.totals[,c("Hydro GWh","Scenario","Model")],by=c("Scenario","Model"))
annual.gen.stats<- merge(annual.gen.stats,hydro.regionals[TEPPC.Region=="Northwest",c("value","Scenario","Model")],by=c("Scenario","Model"))
setnames(annual.gen.stats,"value","NW Hydro GWh")
annual.gen.stats[,`% NW Hydro`:=`NW Hydro GWh`/`Hydro GWh`*100]
annual.gen.stats<-merge(annual.gen.stats,hydro.regionals.cast,by=c("Scenario","Model"))
annual.gen.stats[,"NW and Canada Hydro":=Northwest+Canada]
annual.gen.stats[,"NW and NCal Hydro":=Northwest+`Northern California`]


## Add line violations to annual gen stats
annual.gen.stats<-merge(annual.gen.stats,violations[,c("Scenario","Model","Line Flow Violation (MWh)","Hours with Line Flow Violation")],
                                                    by=c("Scenario","Model"))

## Average of annual gen stats
annual.gen.stats.avg<-annual.gen.stats[,.(`Average Cost ($)`=mean(`Cost ($)`),
                                          `Mean Generation (GWh)`=mean(`Generation (GWh)`),
                                          `Mean Unit Cost ($/MWh)`=mean(UnitCost)/1000,
                                          `Average Hydro (GWh)`=mean(`Hydro GWh`),
                                          `Average Line Flow Violation (MWh)`=mean(`Line Flow Violation (MWh)`),
                                          `Average Hours with Line Flow Violations`=mean(`Hours with Line Flow Violation`)),
                                          by=c("GCM","RCP","Period")]
annual.gen.stats.avg<-merge(annual.gen.stats.avg,annual.gen.stats.avg[RCP=="hist.",],
                            by=c("GCM"),suffixes=c(""," Hist"))
annual.gen.stats.avg[,`Increase in Generation Cost ($)`:=(`Average Cost ($)`-`Average Cost ($) Hist`)]
annual.gen.stats.avg[,`Percent Increase in Cost (%)`:=((`Average Cost ($)`-`Average Cost ($) Hist`)/`Average Cost ($) Hist`)*100,]
annual.gen.stats.avg[,`Increase in Generation Cost ($/MWh)`:=(`Mean Unit Cost ($/MWh)`-`Mean Unit Cost ($/MWh) Hist`),]
annual.gen.stats.avg[,`Increase in Hydro (GWh)`:=(`Average Hydro (GWh)`-`Average Hydro (GWh) Hist`),]
annual.gen.stats.avg[,`Percent Increase in Hydro (%)`:=((`Increase in Hydro (GWh)`)/`Average Hydro (GWh) Hist`)*100,]
annual.gen.stats.avg[GCM=='ccsm'&RCP=="rcp45",`Change in Precip. (%)`:=1]
annual.gen.stats.avg[GCM=='ccsm'&RCP=="rcp85",`Change in Precip. (%)`:=3]
annual.gen.stats.avg[GCM=='gfdl'&RCP=="rcp45",`Change in Precip. (%)`:=11]
annual.gen.stats.avg[GCM=='gfdl'&RCP=="rcp85",`Change in Precip. (%)`:=17]
annual.gen.stats.avg[GCM=='inm_'&RCP=="rcp45",`Change in Precip. (%)`:=3]
annual.gen.stats.avg[GCM=='inm_'&RCP=="rcp85",`Change in Precip. (%)`:=-3]
###### SUMMARY STATS OF COST AND DELTAS ##
cost.stats<-annual.gen.stats[,list(`Mean Cost`=mean(`Cost ($)`),
                                `Max Cost`=max(`Cost ($)`),
                                `Min Cost`=min(`Cost ($)`),
                                `Std Cost`=sd(`Cost ($)`),
                                `Coeff Var`=sd(`Cost ($)`)/mean(`Cost ($)`)  
                                ),by=c("GCM","RCP","Period")]


###### SUMMARY STATS OF HYDRO AND DELTAS ##
  hydro.stats<-hydro.totals[,list(`Mean Hydro GWh`=mean(`Hydro GWh`),
                                  `Max Hydro GWh`=max(`Hydro GWh`),
                                  `Min Hydro GWh`=min(`Hydro GWh`),
                                  `Std Hydro GWh`=sd(`Hydro GWh`),
                                  `Coeff Var`=sd(`Hydro GWh`)/mean(`Hydro GWh`),  
                                  `Mean Delta Hydro GWh`=mean(DeltaGWh,na.rm=TRUE),
                                  `Mean Abs Delta Hydro GWh`=mean(abs(DeltaGWh),na.rm=TRUE),
                                  `Max Delta Hydro GWh`=max(DeltaGWh,na.rm=TRUE),
                                  `Min Delta Hydro GWh`=min(DeltaGWh,na.rm=TRUE),
                                  `Std Delta Hydro GWh`=sd(DeltaGWh,na.rm=TRUE)
                                  ),by=c("GCM","RCP","Period")]
### SUMMARY STATS OF ANNUAL REGIONAL HYDRO ####
  hydro.stats.annual<-hydro.regionals[,list(`Mean Hydro GWh`=mean(value),
                                  `Max Hydro GWh`=max(value),
                                  `Min Hydro GWh`=min(value),
                                  `Std Hydro GWh`=sd(value),
                                  `Coeff Var`=sd(value)/mean(value)  
  ),by=c("GCM","RCP","Period","TEPPC.Region")]
  
  
####### SUMMARY STATS OF HYDRO REGIONAL MONTHLY ####
  hydro.month<-hydro.intervals[,sum(MWh),by=c("GCM","RCP","Month","TEPPC.Region","Scenario","MonthDate")]
  setnames(hydro.month,"V1","MWh")
  hydro.month.stats<-hydro.month[,list(`Mean Hydro GWh`=mean(MWh)/1000,
                                  `Max Hydro GWh`=max(MWh)/1000,
                                  `Min Hydro GWh`=min(MWh)/1000,
                                  `Std Hydro GWh`=sd(MWh)/1000,
                                  `Coeff Var`=sd(MWh)/mean(MWh)  
                                  ),by=c("GCM","RCP","Month","TEPPC.Region")]
  
  
  ### SUMMARY STATS OF HOURLY REGIONAL HYDRO ####
  hydro.hour.stats<-hydro.intervals[,list(`Mean Hydro GWh`=mean(MWh)/1000,
                                         `Max Hydro GWh`=max(MWh)/1000,
                                         `Min Hydro GWh`=min(MWh)/1000,
                                         `Std Hydro GWh`=sd(MWh)/1000,
                                         `Coeff Var`=sd(MWh)/mean(MWh)  
  ),by=c("GCM","RCP","Month","TEPPC.Region")]
  

## Annual net gen by region ############################
# Generation and Load by Region GWh 

net.gen.RSG=rbindruns('net.gen.RSG',env_list)
net.gen.RSG<-add_cols(net.gen.RSG)


##### CORRELATIONS BETWEEN REGIONAL NET GEN ####

net.gen.temp<-dcast(net.gen.RSG[,.(TEPPC.Region,Scenario,GCM,RCP,netgen)],Scenario+GCM+RCP~TEPPC.Region,value.var='netgen')

net.gen.cor.hist<-rcorr(as.matrix(subset(net.gen.temp[RCP=='hist.',],,select=c("Basin","Northern California","Northwest","Rockies","Southern California","Southwest"))),type=c("pearson"))
net.gen.cor.hist.plot<-melt(as.data.table(upper.tri(net.gen.cor.hist$r,diag=TRUE)*round(net.gen.cor.hist$r,2),keep.rownames="Region Pair"),id.vars="Region Pair",variable.name="Region")
temp<-melt(as.data.table(upper.tri(net.gen.cor.hist$P,diag=TRUE)*net.gen.cor.hist$P,keep.rownames="Region Pair"),id.vars="Region Pair",variable.name="Region")
setnames(net.gen.cor.hist.plot,'value','Correlation')
setnames(temp,'value','P-value')
net.gen.cor.hist.plot<-merge(net.gen.cor.hist.plot,temp,by=c("Region","Region Pair"))
net.gen.cor.hist.plot[Correlation==0.0,Correlation:=NA]
rm(temp)


net.gen.cor.fut<-rcorr(as.matrix(subset(net.gen.temp[RCP!='hist.',],,select=c("Basin","Northern California","Northwest","Rockies","Southern California","Southwest"))),type=c("pearson"))
net.gen.cor.fut.plot<-melt(as.data.table(upper.tri(net.gen.cor.fut$r,diag=TRUE)*round(net.gen.cor.fut$r,2),keep.rownames="Region Pair"),id.vars="Region Pair",variable.name="Region")
temp<-melt(as.data.table(upper.tri(net.gen.cor.fut$P,diag=TRUE)*net.gen.cor.fut$P,keep.rownames="Region Pair"),id.vars="Region Pair",variable.name="Region")
setnames(net.gen.cor.fut.plot,'value','Correlation')
setnames(temp,'value','P-value')
net.gen.cor.fut.plot<-merge(temp,net.gen.cor.fut.plot,by=c("Region","Region Pair"))
net.gen.cor.fut.plot[Correlation==0.0,Correlation:=NA]


##### CORRELATIONS BETWEEN REGIONAL HYDRO AND REGIONAL NET GEN ####
net.gen.hydro<-merge(net.gen.RSG[,.(TEPPC.Region,Scenario,GCM,RCP,netgen)],hydro.regionals[,.(TEPPC.Region,Scenario,value,GCM,RCP,ScenarioSumHydro)],by=c("Scenario","GCM","RCP"),all=TRUE,allow.cartesian=TRUE) #Merge with hydro for corrrelation to hydro
setnames(net.gen.hydro,c("value","TEPPC.Region.x","TEPPC.Region.y"),c("Region Hydro","TEPPC.Region.gen","TEPPC.Region.Hydro"))

reg.hydro.temp<-dcast(hydro.regionals[,.(TEPPC.Region,Scenario,GCM,RCP,value)],Scenario+GCM+RCP~TEPPC.Region,value.var='value')
net.gen.hydro.format=merge(net.gen.temp[,.(Scenario,GCM,RCP,`Northern California`,`Southern California`,Northwest,Southwest,Basin,Rockies)],
                           reg.hydro.temp[,.(Scenario,GCM,RCP,`Northern California`,`Southern California`,Northwest,Southwest,Basin,Rockies)],
                           by=c("Scenario","GCM","RCP"),
                           suffixes=c(".netgen",".hydro"))
net.gen.hydro.cor<-rcorr(as.matrix(subset(net.gen.hydro.format,,select=c("Basin.netgen","Northern California.hydro","Northern California.netgen","Northwest.hydro","Northwest.netgen","Rockies.netgen","Southern California.netgen","Southwest.hydro","Southwest.netgen"))),type=c("pearson"))
net.gen.hydro.cor.plot<-melt(as.data.table(upper.tri(net.gen.hydro.cor$r,diag=TRUE)*round(net.gen.hydro.cor$r,2),keep.rownames="Region Pair"),id.vars="Region Pair",variable.name="Region")
temp                  <-melt(as.data.table(upper.tri(net.gen.hydro.cor$P,diag=TRUE)*net.gen.hydro.cor$P,keep.rownames="Region Pair")         ,id.vars="Region Pair",variable.name="Region")
setnames(net.gen.hydro.cor.plot,'value','Correlation')
setnames(temp,'value','P-value')
net.gen.hydro.cor.plot<-merge(net.gen.hydro.cor.plot,temp,by=c("Region","Region Pair"))
net.gen.hydro.cor.plot[Correlation==0.0,Correlation:=NA]
rm(temp)


##### FIND WET, DRY, MEDIAN OF ALL CLIMATE RUNS HISTORICAL/FUTURE####

#First do rankings:
annual.gen.stats$CostRankHist=0
annual.gen.stats$CostRankFut=0
annual.gen.stats[RCP=="hist."]$CostRankHist<-rank(annual.gen.stats[RCP=="hist."]$`Cost ($)`)
annual.gen.stats[RCP!="hist."]$CostRankFut <-rank(annual.gen.stats[RCP!="hist."]$`Cost ($)`)

hydro.totals$HydroRankHist=0
hydro.totals$HydroRankFut=0
hydro.totals[RCP=="hist."]$HydroRankHist<-rank(hydro.totals[RCP=="hist."]$`Hydro GWh`)
hydro.totals[RCP!="hist."]$HydroRankFut <-rank(hydro.totals[RCP!="hist."]$`Hydro GWh`)

#Then make tables of 10/50/90 percentiles:
yearsrankedhist<-length(annual.gen.stats[RCP=="hist."]$`Cost ($)`)
yearsrankedfut <-length(annual.gen.stats[RCP!="hist."]$`Cost ($)`)
quantile.positions.hist<-round(quantile(1:yearsrankedhist,c(0.1,0.5,0.9),names=FALSE))
quantile.positions.fut<-round(quantile(1:yearsrankedfut,c(0.1,0.5,0.9),names=FALSE))
gencost.quantile.years.hist<-annual.gen.stats[CostRankHist %in% quantile.positions.hist][,CostPercentile:=CostRankHist/yearsrankedhist]
gencost.quantile.years.fut <-annual.gen.stats[CostRankFut  %in% quantile.positions.fut ][,CostPercentile:=CostRankFut/yearsrankedfut]
setorderv(gencost.quantile.years.hist,c("GCM","CostPercentile"))
setorderv(gencost.quantile.years.fut,c("GCM","CostPercentile"))

hydro.quantile.years.hist<-hydro.totals[HydroRankHist %in% quantile.positions.hist ][,HydroPercentile:=HydroRankHist/yearsrankedhist]
hydro.quantile.years.fut <-hydro.totals[HydroRankFut  %in% quantile.positions.fut  ][,HydroPercentile:=HydroRankFut/yearsrankedfut]
setorderv(hydro.quantile.years.hist,c("GCM","HydroPercentile"))
setorderv(hydro.quantile.years.fut,c("GCM","HydroPercentile"))


##### EMULATOR #####################################
emulator_total<-lm(`Cost ($)`~`Hydro GWh`,data=annual.gen.stats)
emulator_NW<-lm(`Cost ($)`~`NW Hydro GWh`,data=annual.gen.stats)
emulator_NW_Can<-lm(`Cost ($)`~`NW and Canada Hydro`,data=annual.gen.stats)
emulator_majors<-lm(`Cost ($)`~`NW Hydro GWh`+`Northern California`+`Canada`+`Southern California`+`Southwest`,data=annual.gen.stats)
emulator_NW_NCal<-lm(`Cost ($)`~`NW and NCal Hydro`,data=annual.gen.stats)
emulator_NCal<-lm(`Cost ($)`~`Northern California`,data=annual.gen.stats)

####T TESTS ########################################
# t-test in R uses default var.equal=FALSE so it does not assume equal variance. 
MeanHydro.Test<-testitem(as.formula("`Hydro GWh`~Period"),hydro.totals)

GenCost.Test  <-testitem(as.formula("`Cost ($)`~Period"),annual.gen.stats)

ResShort.Test <-testitem(as.formula("`Reserve Shortage (GWh)`~Period"),annual.gen.stats)

NWHydroPercent.Test  <-testitem(as.formula("`% NW Hydro`~Period"),annual.gen.stats)

NWHydro.Test   <-testitem(as.formula("value~Period"),hydro.regionals[TEPPC.Region=="Northwest"])
NCalHydro.Test <-testitem(as.formula("value~Period"),hydro.regionals[TEPPC.Region=="Northern California"])

#### WSGIF NUMBERS ###################################
WSGIF<-data.table(read.csv('./WSGIF_WECC_list.csv',colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric")))
setnames(WSGIF,"ï..Model","Model")
WSGIF<-melt(WSGIF,id.vars=c("Model","Scenario"),value.name="WSGIF",variable.name="TEPPC.Region")
WSGIF<-add_cols(WSGIF)

WSGIF_hydro<-data.table(read.csv('./WSGIF_hydro_list.csv',colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric")))
setnames(WSGIF_hydro,"ï..Model","Model")
WSGIF_hydro<-melt(WSGIF_hydro,id.vars=c("Model","Scenario"),value.name="WSGIF_hydro",variable.name="TEPPC.Region")

WSGIF_thermo<-data.table(read.csv('./WSGIF_thermal_list.csv',colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric")))
setnames(WSGIF_thermo,"ï..Model","Model")
WSGIF_thermo<-melt(WSGIF_thermo,id.vars=c("Model","Scenario"),value.name="WSGIF_thermo",variable.name="TEPPC.Region")

WSGIF_hydro_thermo<-merge(WSGIF_hydro,WSGIF_thermo,by=c("Model","Scenario","TEPPC.Region"))
WSGIF_hydro_thermo<-add_cols(WSGIF_hydro_thermo)
rm(WSGIF_hydro,WSGIF_thermo)

### RANK YEARS BY WSGIF WECC #####
WSGIF$WSGIFHistRank=0
WSGIF$WSGIFFutRank=0
WSGIF[Period==1995&TEPPC.Region=="WECC",]$WSGIFHistRank<-rank(WSGIF[Period==1995&TEPPC.Region=="WECC"]$WSGIF)
WSGIF[Period==2050&TEPPC.Region=="WECC",]$WSGIFFutRank<-rank(WSGIF[Period==2050&TEPPC.Region=="WECC"]$WSGIF)
quantile.positions.hist<-round(quantile(1:90,c(0.1,0.5,0.9),names=FALSE))
quantile.positions.fut<-round(quantile(1:180,c(0.1,0.5,0.9),names=FALSE))

WSGIF.quantile.years.hist<-WSGIF[WSGIFHistRank %in% quantile.positions.hist][,Percentile:=WSGIFHistRank/90]
WSGIF.quantile.years.fut <-WSGIF[WSGIFFutRank %in% quantile.positions.fut][,Percentile:=WSGIFFutRank/180]

#### WSGIF T-TESTS ######################################
single_ttest = function(WSGIFcol,Periodcol){
  source<-data.table(WSGIF=WSGIFcol,Period=Periodcol)
pval<-t.test(WSGIF~Period,data=source)$p.value
return(pval)
}

mean_delta = function(WSGIFcol,Periodcol){
  source<-data.table(WSGIF=WSGIFcol,Period=Periodcol)
  means<-source[Period!="NA",mean(WSGIF),by=Period]
  delta<-diff(means$V1)
  return(delta)
}

WSGIF.summary.45<-WSGIF[RCP!="rcp85",
                        list(`WSGIF Change from 1995`=mean_delta(WSGIF,Period),
                        `p-value for change in WSGIF`=single_ttest(WSGIF,Period)),
                        by=c("TEPPC.Region","GCM")]
WSGIF.summary.45[,RCP:="rcp45"]

WSGIF.summary.85<-WSGIF[RCP!="rcp45",
                        list(`WSGIF Change from 1995`=mean_delta(WSGIF,Period),
                        `p-value for change in WSGIF`=single_ttest(WSGIF,Period)),
                        by=c("TEPPC.Region","GCM")]
WSGIF.summary.85[,RCP:="rcp85"]
WSGIF.summary<-rbind(WSGIF.summary.45,WSGIF.summary.85)
rm(WSGIF.summary.45,WSGIF.summary.85)
WSGIFhydrothermo.summary.45<-WSGIF_hydro_thermo[RCP!="rcp85",
                        list(`Hydro WSGIF Change from 1995`=mean_delta(WSGIF_hydro,Period),
                        `p-value for change in Hydro WSGIF`=single_ttest(WSGIF_hydro,Period),
                        `Thermal WSGIF Change from 1995`=mean_delta(WSGIF_thermo,Period),
                        `p-value for change in Thermal WSGIF`=single_ttest(WSGIF_thermo,Period)),
                          by=c("TEPPC.Region","GCM")]
WSGIFhydrothermo.summary.45[,RCP:="rcp45"]
WSGIFhydrothermo.summary.85<-WSGIF_hydro_thermo[RCP!="rcp45",
                         list(`Hydro WSGIF Change from 1995`=mean_delta(WSGIF_hydro,Period),
                         `p-value for change in Hydro WSGIF`=single_ttest(WSGIF_hydro,Period),
                         `Thermal WSGIF Change from 1995`=mean_delta(WSGIF_thermo,Period),
                         `p-value for change in Thermal WSGIF`=single_ttest(WSGIF_thermo,Period)),
                         by=c("TEPPC.Region","GCM")]
WSGIFhydrothermo.summary.85[,RCP:="rcp85"]
WSGIFhydrothermo.summary<-rbind(WSGIFhydrothermo.summary.45,WSGIFhydrothermo.summary.85)
rm(WSGIFhydrothermo.summary.45,WSGIFhydrothermo.summary.85)

## Combine WSGIF & Cost at WECC level
WSGIF.cost<-merge(WSGIF[TEPPC.Region=="WECC"],annual.gen.stats[,.(Scenario,Model,`Cost ($)`,GCM,RCP,`Hydro GWh`,CostRankHist,CostRankFut)]
                  ,by=c("Scenario","Model","GCM","RCP"))
WSGIF.cost<-merge(WSGIF.cost,hydro.totals[,.(Scenario,Model,GCM,RCP,HydroRankHist,HydroRankFut)],by=c("Scenario","Model","GCM","RCP"))

####################################################
# Labeling helpers
GCM_names<-c(`ccsm`="CCSM4",`gfdl`="GFDL-CM3",`inm_`="INMCM4",`Basin`="Basin",`Canada`="Canada",`Northern California`="N. Cal.",
             `Northwest`="Northwest",`Rockies`="Roc.",`Southern California`="S. Cal.",
             `Mexico`="Mexico",`Southwest`="Southwest")
RCP_names<-c(`hist.`="Historical",`rcp45`="RCP 4.5",`rcp85`="RCP 8.5")


##### OUTPUT #######################################
# Filename to save .HTML file.
setwd('../')
output.filename = 'IM3_model_comparison_V8_selectFINAL.html'
scenario.name='modelcompare' #prefix for plots

# Call the .Rmd file which creates the resulting HTML - select figures only
render(input='plot_creator_paper_FINAL.Rmd', c("html_document"), 
       output_file=output.filename)


