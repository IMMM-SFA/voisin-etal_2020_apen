## Comparing Scenario Flows

# This script loads all of the flow .csv files into invidiual environments so that results can be compared in one place. 


# Load required R libraries 
library(rmarkdown)
library(data.table)
library(RColorBrewer)
library(plyr)
library(stringr)
library(circlize)

###### SETUP ######################### 
setwd('./Results/')

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
ccsm_rcp45$annual.flows=read.csv('./future_climate_V8/ccsm_rcp45/ccsm_rcp45_annual_flows.csv')
ccsm_rcp85$annual.flows=read.csv('./future_climate_V8/ccsm_rcp85/ccsm_rcp85_annual_flows.csv')
gfdl_rcp45$annual.flows=read.csv('./future_climate_V8/gfdl_rcp45/gfdl_rcp45_annual_flows.csv')
gfdl_rcp85$annual.flows=read.csv('./future_climate_V8/gfdl_rcp85/gfdl_rcp85_annual_flows.csv')
inm_rcp45$annual.flows=read.csv('./future_climate_V8/inm_rcp45/inm_rcp45_annual_flows.csv')
inm_rcp85$annual.flows=read.csv('./future_climate_V8/inm_rcp85/inm_rcp85_annual_flows.csv')
ccsm_hist$annual.flows=read.csv('./historic_climate_V8/ccsm/ccsm_annual_flows.csv')
gfdl_hist$annual.flows=read.csv('./historic_climate_V8/gfdl/gfdl_annual_flows.csv')
inm_hist$annual.flows=read.csv('./historic_climate_V8/inm/inm_annual_flows.csv')

ccsm_rcp45$Q1.flows=read.csv('./future_climate_V8/ccsm_rcp45/ccsm_rcp45_Q1_flows.csv')
ccsm_rcp85$Q1.flows=read.csv('./future_climate_V8/ccsm_rcp85/ccsm_rcp85_Q1_flows.csv')
gfdl_rcp45$Q1.flows=read.csv('./future_climate_V8/gfdl_rcp45/gfdl_rcp45_Q1_flows.csv')
gfdl_rcp85$Q1.flows=read.csv('./future_climate_V8/gfdl_rcp85/gfdl_rcp85_Q1_flows.csv')
inm_rcp45$Q1.flows=read.csv('./future_climate_V8/inm_rcp45/inm_rcp45_Q1_flows.csv')
inm_rcp85$Q1.flows=read.csv('./future_climate_V8/inm_rcp85/inm_rcp85_Q1_flows.csv')
ccsm_hist$Q1.flows=read.csv('./historic_climate_V8/ccsm/ccsm_Q1_flows.csv')
gfdl_hist$Q1.flows=read.csv('./historic_climate_V8/gfdl/gfdl_Q1_flows.csv')
inm_hist$Q1.flows=read.csv('./historic_climate_V8/inm/inm_Q1_flows.csv')

ccsm_rcp45$Q2.flows=read.csv('./future_climate_V8/ccsm_rcp45/ccsm_rcp45_Q2_flows.csv')
ccsm_rcp85$Q2.flows=read.csv('./future_climate_V8/ccsm_rcp85/ccsm_rcp85_Q2_flows.csv')
gfdl_rcp45$Q2.flows=read.csv('./future_climate_V8/gfdl_rcp45/gfdl_rcp45_Q2_flows.csv')
gfdl_rcp85$Q2.flows=read.csv('./future_climate_V8/gfdl_rcp85/gfdl_rcp85_Q2_flows.csv')
inm_rcp45$Q2.flows=read.csv('./future_climate_V8/inm_rcp45/inm_rcp45_Q2_flows.csv')
inm_rcp85$Q2.flows=read.csv('./future_climate_V8/inm_rcp85/inm_rcp85_Q2_flows.csv')
ccsm_hist$Q2.flows=read.csv('./historic_climate_V8/ccsm/ccsm_Q2_flows.csv')
gfdl_hist$Q2.flows=read.csv('./historic_climate_V8/gfdl/gfdl_Q2_flows.csv')
inm_hist$Q2.flows=read.csv('./historic_climate_V8/inm/inm_Q2_flows.csv')


ccsm_rcp45$Q3.flows=read.csv('./future_climate_V8/ccsm_rcp45/ccsm_rcp45_Q3_flows.csv')
ccsm_rcp85$Q3.flows=read.csv('./future_climate_V8/ccsm_rcp85/ccsm_rcp85_Q3_flows.csv')
gfdl_rcp45$Q3.flows=read.csv('./future_climate_V8/gfdl_rcp45/gfdl_rcp45_Q3_flows.csv')
gfdl_rcp85$Q3.flows=read.csv('./future_climate_V8/gfdl_rcp85/gfdl_rcp85_Q3_flows.csv')
inm_rcp45$Q3.flows=read.csv('./future_climate_V8/inm_rcp45/inm_rcp45_Q3_flows.csv')
inm_rcp85$Q3.flows=read.csv('./future_climate_V8/inm_rcp85/inm_rcp85_Q3_flows.csv')
ccsm_hist$Q3.flows=read.csv('./historic_climate_V8/ccsm/ccsm_Q3_flows.csv')
gfdl_hist$Q3.flows=read.csv('./historic_climate_V8/gfdl/gfdl_Q3_flows.csv')
inm_hist$Q3.flows=read.csv('./historic_climate_V8/inm/inm_Q3_flows.csv')

ccsm_rcp45$Q4.flows=read.csv('./future_climate_V8/ccsm_rcp45/ccsm_rcp45_Q4_flows.csv')
ccsm_rcp85$Q4.flows=read.csv('./future_climate_V8/ccsm_rcp85/ccsm_rcp85_Q4_flows.csv')
gfdl_rcp45$Q4.flows=read.csv('./future_climate_V8/gfdl_rcp45/gfdl_rcp45_Q4_flows.csv')
gfdl_rcp85$Q4.flows=read.csv('./future_climate_V8/gfdl_rcp85/gfdl_rcp85_Q4_flows.csv')
inm_rcp45$Q4.flows=read.csv('./future_climate_V8/inm_rcp45/inm_rcp45_Q4_flows.csv')
inm_rcp85$Q4.flows=read.csv('./future_climate_V8/inm_rcp85/inm_rcp85_Q4_flows.csv')
ccsm_hist$Q4.flows=read.csv('./historic_climate_V8/ccsm/ccsm_Q4_flows.csv')
gfdl_hist$Q4.flows=read.csv('./historic_climate_V8/gfdl/gfdl_Q4_flows.csv')
inm_hist$Q4.flows=read.csv('./historic_climate_V8/inm/inm_Q4_flows.csv')


list_of_names=c("ccsm_rcp85","gfdl_rcp85","inm_rcp85",
                "ccsm_rcp45","gfdl_rcp45","inm_rcp45",
                "ccsm_hist." ,"gfdl_hist." ,"inm_hist."  )
env_list=list(ccsm_rcp85,gfdl_rcp85,inm_rcp85,
              ccsm_rcp45,gfdl_rcp45,inm_rcp45,
              ccsm_hist ,gfdl_hist ,inm_hist)

ranked.years<-read.csv('./WSGIF_with_rankings.csv')                 #File containing annual results and rankings by WSGIF, Cost, Hydro

##### FUNCTION TO RBIND FROM ENVIRONMENTS ############
rbindruns = function(item,env_list) {

modellist<-list(data.table(0))
for (i in 1:length(env_list)) {
    modellist[[i]]=get(item,envir=env_list[[i]])                    #Get the specified item from each i environment
}
setattr(modellist,'names',list_of_names)
bound=rbindlist(modellist,use.names=TRUE,idcol="Model")             #Combine
return(bound)

}

###### FUNCTION TO ORGANIZE BOUND RESULTS by adding new columns and cleaning up ##################
add_cols=function(item){
item[, GCM:=str_trunc(item$Model,4,side=c("right"),ellipsis="")]    #Add GCM column
item[, RCP:=str_trunc(item$Model,5,side=c("left"),ellipsis="")]     #Add RCP column
item[, Scenario := gsub('.*85_', '', item$Scenario)]                #Reduce to only year
item[, Scenario := gsub('.*45_', '', item$Scenario)]                #Reduce to only year
item[, Scenario := gsub('.*year_', '', item$Scenario)]              #Reduce to only year
item[, Scenario := gsub('.*inm_', '', item$Scenario)]               #Reduce to only year
item[, Scenario := gsub('.*ccsm_', '', item$Scenario)]              #Reduce to only year
item[, Scenario := gsub('.*gfdl_', '', item$Scenario)]              #Reduce to only year

item[, Scenario:=as.numeric(item$Scenario)]
item[(Scenario>1980&Scenario<2011),Period:='1995']                  #Add labels for 30 year periods
item[(Scenario>2035&Scenario<2066),Period:='2050'] 
item[(Scenario>2065&Scenario<2096),Period:='2080']
item[from_RSG=="Northern California",from_RSG:="N. Cal."]
item[to_RSG=="Northern California",to_RSG:="N. Cal."]
item[from_RSG=="Southern California",from_RSG:="S. Cal."]
item[to_RSG=="Southern California",to_RSG:="S. Cal."]

return(item)
}

###### FUNCTION TO MELT HYDRO GENERATION ITEMS #####################################################
meltitem = function(item,env_list,new_item,id_list){
  for (i in 1:length(env_list)) {
    temp_item=get(item,envir=env_list[[i]])    
    temp_item<-melt(temp_item, id.vars=id_list, variable.name='Scenario')[Type=="Hydro"]
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

## Function to convert transfer table to net interchange matrix ###########
tab_to_mat=function(data){

  mat=as.data.frame(dcast(data,from_RSG~to_RSG,fun=sum,value.var=c("TWh")))
  rownames(mat)=mat$from_RSG #move the from column to the column names
  mat$from_RSG<-NULL #Remove the from column for transpose
  
  return(mat)
  
}

########################################################################
# Function to plot using chord from circilize package ##################
interchange_plot=function(mat,set_gap,custom_order,is.directional,title.char,
                          annotationTracks){
  # mat = mat[rowSums(mat)!=0,colSums(mat)!=0]
  Trackheight=c(0.05,0.1,0.05)
  col.len = length(unique(c(rownames(mat),colnames(mat))))
  col1=RColorBrewer::brewer.pal(8,'Accent')
  col=col1[c(2,3,4,5,6,7,8,1)] #manually adjust the order
 
  link.size=1
  link.visiblility=(mat>0.1) #Only make visible if > 
  circlize::circos.clear() 
  circlize::circos.par(gap.degree=set_gap,canvas.ylim=c(-1,1)) #Create a gap between "sectors" or regions
  circlize::chordDiagram(as.matrix(mat), directional=is.directional, grid.col=col, direction.type="arrows",
                         link.border=1, link.lwd=0.25, link.arr.lwd=link.size,
                         link.arr.length=link.size/4, link.arr.lty=2, reduce=-1,
                         transparency=0.4,order=custom_order,link.visible=link.visiblility,
                         annotationTrack=annotationTracks,annotationTrackHeight=Trackheight)
  circlize::circos.clear()
  title(sub =title.char,
        cex.sub = 1, font.sub = 3, col.sub = "black")    # for(si in get.all.sector.index()) {
  
}

######################################################

###### COMBINE DATA ##################################

annual.flows<-rbindruns('annual.flows',env_list)
annual.flows<-add_cols(annual.flows)
annual.flows<-merge(annual.flows,ranked.years[,c("Scenario","Model","GCM","RCP","WSGIF","WSGIFHistRank","WSGIFFutRank","CostRankHist","CostRankFut","HydroRankHist","HydroRankFut","Hydro.GWh")],by=c("Scenario","GCM","RCP","Model"))

Q1.flows<-rbindruns('Q1.flows',env_list)
Q1.flows<-add_cols(Q1.flows)
Q1.flows<-merge(Q1.flows,ranked.years[,c("Scenario","Model","GCM","RCP","WSGIF","WSGIFHistRank","WSGIFFutRank","CostRankHist","CostRankFut","HydroRankHist","HydroRankFut","Hydro.GWh")],by=c("Scenario","GCM","RCP","Model"))
Q1.flows[,Q:="Q1"]

Q2.flows<-rbindruns('Q2.flows',env_list)
Q2.flows<-add_cols(Q2.flows)
Q2.flows<-merge(Q2.flows,ranked.years[,c("Scenario","Model","GCM","RCP","WSGIF","WSGIFHistRank","WSGIFFutRank","CostRankHist","CostRankFut","HydroRankHist","HydroRankFut","Hydro.GWh")],by=c("Scenario","GCM","RCP","Model"))
Q2.flows[,Q:="Q2"]

Q3.flows<-rbindruns('Q3.flows',env_list)
Q3.flows<-add_cols(Q3.flows)
Q3.flows<-merge(Q3.flows,ranked.years[,c("Scenario","Model","GCM","RCP","WSGIF","WSGIFHistRank","WSGIFFutRank","CostRankHist","CostRankFut","HydroRankHist","HydroRankFut","Hydro.GWh")],by=c("Scenario","GCM","RCP","Model"))
Q3.flows[,Q:="Q3"]

Q4.flows<-rbindruns('Q4.flows',env_list)
Q4.flows<-add_cols(Q4.flows)
Q4.flows<-merge(Q4.flows,ranked.years[,c("Scenario","Model","GCM","RCP","WSGIF","WSGIFHistRank","WSGIFFutRank","CostRankHist","CostRankFut","HydroRankHist","HydroRankFut","Hydro.GWh")],by=c("Scenario","GCM","RCP","Model"))
Q4.flows[,Q:="Q4"]

Q.flows<-rbindlist(list(Q1.flows,Q2.flows,Q3.flows,Q4.flows),use.names=TRUE)

### T-TEST #########################################
NW_SCal.Test<-  testitem(as.formula("TWh~Period"),annual.flows[from_RSG=="Northwest"&to_RSG=="S. Cal."])
NW_NCal.Test<-  testitem(as.formula("TWh~Period"),annual.flows[from_RSG=="Northwest"&to_RSG=="N. Cal."])
NCal_SCal.Test<-testitem(as.formula("TWh~Period"),annual.flows[from_RSG=="N. Cal."&to_RSG=="S. Cal."])
SW_SCal.Test<-  testitem(as.formula("TWh~Period"),annual.flows[from_RSG=="Southwest"&to_RSG=="S. Cal."])
Rock_SW.Test<-  testitem(as.formula("TWh~Period"),annual.flows[from_RSG=="Rockies"&to_RSG=="Southwest"])
Basin_SW.Test<- testitem(as.formula("TWh~Period"),annual.flows[from_RSG=="Basin"&to_RSG=="Southwest"])

NW_SCal.Test.Q1<-  testitem(as.formula("TWh~Period"),Q1.flows[from_RSG=="Northwest"&to_RSG=="S. Cal."])
NW_NCal.Test.Q1<-  testitem(as.formula("TWh~Period"),Q1.flows[from_RSG=="Northwest"&to_RSG=="N. Cal."])
NCal_SCal.Test.Q1<-testitem(as.formula("TWh~Period"),Q1.flows[from_RSG=="N. Cal."&to_RSG=="S. Cal."])
SW_SCal.Test.Q1<-  testitem(as.formula("TWh~Period"),Q1.flows[from_RSG=="Southwest"&to_RSG=="S. Cal."])
Rock_SW.Test.Q1<-  testitem(as.formula("TWh~Period"),Q1.flows[from_RSG=="Rockies"&to_RSG=="Southwest"])
Basin_SW.Test.Q1<- testitem(as.formula("TWh~Period"),Q1.flows[from_RSG=="Basin"&to_RSG=="Southwest"])

NW_SCal.Test.Q2<-  testitem(as.formula("TWh~Period"),Q2.flows[from_RSG=="Northwest"&to_RSG=="S. Cal."])
NW_NCal.Test.Q2<-  testitem(as.formula("TWh~Period"),Q2.flows[from_RSG=="Northwest"&to_RSG=="N. Cal."])
NCal_SCal.Test.Q2<-testitem(as.formula("TWh~Period"),Q2.flows[from_RSG=="N. Cal."&to_RSG=="S. Cal."])
SW_SCal.Test.Q2<-  testitem(as.formula("TWh~Period"),Q2.flows[from_RSG=="Southwest"&to_RSG=="S. Cal."])
Rock_SW.Test.Q2<-  testitem(as.formula("TWh~Period"),Q2.flows[from_RSG=="Rockies"&to_RSG=="Southwest"])
Basin_SW.Test.Q2<- testitem(as.formula("TWh~Period"),Q2.flows[from_RSG=="Basin"&to_RSG=="Southwest"])

NW_SCal.Test.Q3<-  testitem(as.formula("TWh~Period"),Q3.flows[from_RSG=="Northwest"&to_RSG=="S. Cal."])
NW_NCal.Test.Q3<-  testitem(as.formula("TWh~Period"),Q3.flows[from_RSG=="Northwest"&to_RSG=="N. Cal."])
NCal_SCal.Test.Q3<-testitem(as.formula("TWh~Period"),Q3.flows[from_RSG=="N. Cal."&to_RSG=="S. Cal."])
SW_SCal.Test.Q3<-  testitem(as.formula("TWh~Period"),Q3.flows[from_RSG=="Southwest"&to_RSG=="S. Cal."])
Rock_SW.Test.Q3<-  testitem(as.formula("TWh~Period"),Q3.flows[from_RSG=="Rockies"&to_RSG=="Southwest"])
Basin_SW.Test.Q3<- testitem(as.formula("TWh~Period"),Q3.flows[from_RSG=="Basin"&to_RSG=="Southwest"])

NW_SCal.Test.Q4<-  testitem(as.formula("TWh~Period"),Q4.flows[from_RSG=="Northwest"&to_RSG=="S. Cal."])
NW_NCal.Test.Q4<-  testitem(as.formula("TWh~Period"),Q4.flows[from_RSG=="Northwest"&to_RSG=="N. Cal."])
NCal_SCal.Test.Q4<-testitem(as.formula("TWh~Period"),Q4.flows[from_RSG=="N. Cal."&to_RSG=="S. Cal."])
SW_SCal.Test.Q4<-  testitem(as.formula("TWh~Period"),Q4.flows[from_RSG=="Southwest"&to_RSG=="S. Cal."])
Rock_SW.Test.Q4<-  testitem(as.formula("TWh~Period"),Q4.flows[from_RSG=="Rockies"&to_RSG=="Southwest"])
Basin_SW.Test.Q4<- testitem(as.formula("TWh~Period"),Q4.flows[from_RSG=="Basin"&to_RSG=="Southwest"])


####################################################
# Labeling helpers
GCM_names<-c(`ccsm`="CCSM4",`gfdl`="GFDL-CM3",`inm_`="INMCM4",`Basin`="Basin",`Canada`="Canada",`N. Cal.`="N. Cal.",
             `Northwest`="Northwest",`Rockies`="Roc.",`S. Cal.`="S. Cal.",
             `Mexico`="Mexico",`Southwest`="Southwest")
RCP_names<-c(`hist.`="Historical",`rcp45`="RCP 4.5",`rcp85`="RCP 8.5")

# Coloring helpers

chord.colors=c("Basin"="#BEAED4","S. Cal."="#FDC086","Rockies"="#FFFF99",
               "Northwest"="#386CB0","Mexico"="#F0027F","Southwest"="#BF5B17",
               "N. Cal."="#666666","Canada"="#7FC97F") #Colors to match chords exactly


##### OUTPUT SUMMARY OF ANNUAL FLOWS #######################################

output.filename = 'IM3_annual_flows_V8_FINAL.html'              # Filename to save .HTML file.
scenario.name='flowscompare'                                    # prefix for plots
setwd('../')
render(input='plot_creator_flows_FINAL.Rmd', c("html_document"), # Call the .Rmd file which creates the resulting HTML report file. 
  output_file=output.filename)

quantile.positions.hist<-round(quantile(1:90,c(0.1,0.5,0.9),names=FALSE))
quantile.positions.fut<-round(quantile(1:180,c(0.1,0.5,0.9),names=FALSE))
drydatahist<-tab_to_mat(annual.flows[WSGIFHistRank==quantile.positions.hist[[1]],c("from_RSG","to_RSG","TWh")])
meddatahist<-tab_to_mat(annual.flows[WSGIFHistRank==quantile.positions.hist[[2]],c("from_RSG","to_RSG","TWh")])
wetdatahist<-tab_to_mat(annual.flows[WSGIFHistRank==quantile.positions.hist[[3]],c("from_RSG","to_RSG","TWh")])
drydatafut<-tab_to_mat(annual.flows[WSGIFFutRank==quantile.positions.fut[[1]],c("from_RSG","to_RSG","TWh")])
meddatafut<-tab_to_mat(annual.flows[WSGIFFutRank==quantile.positions.fut[[2]],c("from_RSG","to_RSG","TWh")])
wetdatafut<-tab_to_mat(annual.flows[WSGIFFutRank==quantile.positions.fut[[3]],c("from_RSG","to_RSG","TWh")])

drydatahistQ1<-tab_to_mat(Q1.flows[WSGIFHistRank==quantile.positions.hist[[1]],c("from_RSG","to_RSG","TWh")])
meddatahistQ1<-tab_to_mat(Q1.flows[WSGIFHistRank==quantile.positions.hist[[2]],c("from_RSG","to_RSG","TWh")])
wetdatahistQ1<-tab_to_mat(Q1.flows[WSGIFHistRank==quantile.positions.hist[[3]],c("from_RSG","to_RSG","TWh")])
drydatafutQ1<-tab_to_mat(Q1.flows[WSGIFFutRank==quantile.positions.fut[[1]],c("from_RSG","to_RSG","TWh")])
meddatafutQ1<-tab_to_mat(Q1.flows[WSGIFFutRank==quantile.positions.fut[[2]],c("from_RSG","to_RSG","TWh")])
wetdatafutQ1<-tab_to_mat(Q1.flows[WSGIFFutRank==quantile.positions.fut[[3]],c("from_RSG","to_RSG","TWh")])

drydatahistQ2<-tab_to_mat(Q2.flows[WSGIFHistRank==quantile.positions.hist[[1]],c("from_RSG","to_RSG","TWh")])
meddatahistQ2<-tab_to_mat(Q2.flows[WSGIFHistRank==quantile.positions.hist[[2]],c("from_RSG","to_RSG","TWh")])
wetdatahistQ2<-tab_to_mat(Q2.flows[WSGIFHistRank==quantile.positions.hist[[3]],c("from_RSG","to_RSG","TWh")])
drydatafutQ2<-tab_to_mat(Q2.flows[WSGIFFutRank==quantile.positions.fut[[1]],c("from_RSG","to_RSG","TWh")])
meddatafutQ2<-tab_to_mat(Q2.flows[WSGIFFutRank==quantile.positions.fut[[2]],c("from_RSG","to_RSG","TWh")])
wetdatafutQ2<-tab_to_mat(Q2.flows[WSGIFFutRank==quantile.positions.fut[[3]],c("from_RSG","to_RSG","TWh")])

drydatahistQ3<-tab_to_mat(Q3.flows[WSGIFHistRank==quantile.positions.hist[[1]],c("from_RSG","to_RSG","TWh")])
meddatahistQ3<-tab_to_mat(Q3.flows[WSGIFHistRank==quantile.positions.hist[[2]],c("from_RSG","to_RSG","TWh")])
wetdatahistQ3<-tab_to_mat(Q3.flows[WSGIFHistRank==quantile.positions.hist[[3]],c("from_RSG","to_RSG","TWh")])
drydatafutQ3<-tab_to_mat(Q3.flows[WSGIFFutRank==quantile.positions.fut[[1]],c("from_RSG","to_RSG","TWh")])
meddatafutQ3<-tab_to_mat(Q3.flows[WSGIFFutRank==quantile.positions.fut[[2]],c("from_RSG","to_RSG","TWh")])
wetdatafutQ3<-tab_to_mat(Q3.flows[WSGIFFutRank==quantile.positions.fut[[3]],c("from_RSG","to_RSG","TWh")])

drydatahistQ4<-tab_to_mat(Q4.flows[WSGIFHistRank==quantile.positions.hist[[1]],c("from_RSG","to_RSG","TWh")])
meddatahistQ4<-tab_to_mat(Q4.flows[WSGIFHistRank==quantile.positions.hist[[2]],c("from_RSG","to_RSG","TWh")])
wetdatahistQ4<-tab_to_mat(Q4.flows[WSGIFHistRank==quantile.positions.hist[[3]],c("from_RSG","to_RSG","TWh")])
drydatafutQ4<-tab_to_mat(Q4.flows[WSGIFFutRank==quantile.positions.fut[[1]],c("from_RSG","to_RSG","TWh")])
meddatafutQ4<-tab_to_mat(Q4.flows[WSGIFFutRank==quantile.positions.fut[[2]],c("from_RSG","to_RSG","TWh")])
wetdatafutQ4<-tab_to_mat(Q4.flows[WSGIFFutRank==quantile.positions.fut[[3]],c("from_RSG","to_RSG","TWh")])

##### OUTPUT CHORD DIAGRAMS #######################################


output.filename = 'IM3_chords_all_FINAL.html'                         # Filename to save .HTML file.
scenario.name='all_chords'                                            #prefix for plots


render(input='./plot_creator_chords_all_FINAL.Rmd', c("html_document"),# Call the .Rmd file which creates the resulting HTML report file. 
       output_file=output.filename)

