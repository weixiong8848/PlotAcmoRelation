#################################################################################################
#     faceit
#  This script generates a correlation scatterplot of simulated variables with climatic variables
#
#  Author: Wei Xiong
#  Created: 09/29/2014
#################################################################################################

args<-commandArgs(trailingOnly=TRUE)

fileDir="/Users/weixiong/Development/face-it/plot_correlation"
acmocsv<-args[1]
plotformat<-args[2]
varNameX<-args[3]#"ETCP_S"
varNameY<-args[4]#"HWAH_S"
group1<-args[5]
group2<-args[6]
output<-args[7]

Group<-"NO"
if (group1!="NO" && group2!="NO"){
    if (group1==group2) {Group<-group1} else {Group<-paste(group1,group2,sep="+")}
}else if(group1=="NO" || group2=="NO"){
    if (group1!="NO") {Group<-group1} else {Group<-group2}
}else{
    Group<-"NO"
}

name_unit<-function(inputcode){
    name<-c("ID","Name of experiment", "Field Overlay","Seaonal Strategy","Rotational Analysis","","Treatment Name","Climate ID code","Climate replication number",	"Region ID","Regional stratum identification number","RAP ID", "Management regimen ID","Names of institutions","Crop rotation", "Weather station ID","Soil ID", "Site Latitude", "Site Longitude",	"Crop type", "Crop model-specific cultivar ID", "Cultivar name", "Start of simulation date",	"Planting date","Observed harvested yield, dry weight", "Observed total above-ground biomass at harvest",	"Observed harvest date",	"Total number of irrigation events",	"Total amount of irrigation",	"Type of irrigation application",	"Total number of fertilizer applications",	"Total N applied",	"Total P applied",	"Total K applied",	"Manure and applied oganic matter",	"Total number of tillage applications",	"Tillage type (hand, animal or mechanized)",	"Experiment ID",	"Weather ID",	"Soil ID",	"DOME ID for Overlay",	"DOME ID for Seasonal",  "DOME ID for Rotational", "Short name of crop model used for simulations",	"Model name and version number", "Simulated harvest yield, dry matter", "Simulated above-ground biomass at harvest, dry matter",	"Simulated anthesis date",	"Simulated maturity date",	"Simulated harvest date",	"Simulated leaf area index, maximum",	"Total precipitation from planting to harvest",	"Simulated evapotranspiration, planting to harvest",	"Simulated N uptake during season", "Simulated N leached up to harvest maturity")
    unit<-c("text",	"text",	"text",	"text",	"text",	"number",	"text",	"code",	"number",	"code",	"number",	"code",	"code",	"text",	"number",	"text",	"text",	"decimal degrees",	"decimal degrees",	"text",	"text",	"text",	"yyyy-mm-dd",	"yyyy-mm-dd",	"kg/ha",	"kg/ha",	"yyyy-mm-dd",	"number",	"mm",	"text",	"number",	"kg[N]/ha",	"kg[P]/ha",	"kg[K]/ha",	"kg/ha",	"#",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"kg/ha",	"kg/ha",	"das",	"das",	"das",	"m2/m2",	"mm",	"mm",	"kg/ha",	"kg/ha")
    code<-c("SUITE_ID",	"EXNAME",	"FIELD_OVERLAY",	"SEASONAL_STRATEGY",	"ROTATIONAL_ANALYSIS",	"RUN#",	"TRT_NAME",	"CLIM_ID",	"CLIM_REP",	"REG_ID",	"STRATUM",	"RAP_ID",	"MAN_ID",	"INSTITUTION",	"ROTATION",	"WST_ID",	"SOIL_ID",	"FL_LAT",	"FL_LONG",	"CRID_text",	"CUL_ID",	"CUL_NAME",	"SDAT",	"PDATE",	"HWAH",	"CWAH",	"HDATE",	"IR#C",	"IR_TOT",	"IROP_text",	"FE_#",	"FEN_TOT",	"FEP_TOT",	"FEK_TOT",	"OM_TOT","TI_#",	"TIIMP_text",	"EID",	"WID",	"SID",	"DOID",	"DSID",	"DRID",	"CROP_MODEL",	"MODEL_VER",	"HWAH_S",	"CWAH_S",	"ADAT_S",	"MDAT_S",	"HADAT_S",	"LAIX_S",	"PRCP_S",	"ETCP_S",	"NUCM_S",	"NLCM_S")
    for (thisi in 1:length(code)) {
        if (inputcode==code[thisi]) {
            all<-paste(name[thisi],"(",unit[thisi],")")
            break
        }
    }
    return(all)
}
#"FIELD_OVERLAY"

#plot_correction(varNameX,varNameY,Group,acmocsv,fileDir)

#plot_correction<-function(varNameX,varNameY,Group,acmocsv,fileDir){
    library(lattice)
    library(MASS)
    #setwd(fileDir)
    OriData<-read.csv(acmocsv,skip=2,header=T)
    #plot(OriData$PRCP_S,OriData$HWAH_S,main="A scatterplot test", xlab="PRCP_S",ylab="HWAH_S",pch=19)
    if (plotformat=="png") png(output)
    if (plotformat=="pdf") pdf(output)
    if (Group!="NO") {
        form<-as.formula(paste(varNameY,"~",varNameX,"|",Group))
    } else {
        form<-as.formula(paste(varNameY,"~",varNameX))
    }
    xyplot(form, OriData,
    panel=function(x,y,...){
        #panel.abline(h=seq(0,8000,2000),col="gray")
        #panel.abline(v=seq(0,1500,500),col="gray")
         panel.xyplot(x,y,type="p",col="red",pch=20,...)
         panel.abline(fit<-lm(y~x),col="blue")
         panel.text(600,7000,paste("R2=",format(summary(fit)$adj.r.squared,digits=3)))
    },scales=list(cex=1.2),
    xlab=list(name_unit(varNameX),cex=1.4),ylab=list(name_unit(varNameY),ces=1.4))
    dev.off()
#}





