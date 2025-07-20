##############################################################################################################################
##############################################################################################################################
##R CODE FOR MIGRATION COMPARE
##
##EDDIE HUNSINGER (AFFILIATION: ALASKA DEPARTMENT OF LABOR AND WORKFORCE DEVELOPMENT), NOVEMBER 2018
##http://www.demog.berkeley.edu/~eddieh/
##edyhsgr@gmail.com
##############################################################################################################################
##############################################################################################################################

library(shiny)
ui<-fluidPage(

	tags$h3("PFD- vs ACS-Based In-Migration Data: Compare Areas"),
	p(""),
  
hr(),

sidebarLayout(
sidebarPanel(

 selectInput("period", "Time Period",
c(
"2010 to 2015"="2010to2015",
"2005 to 2010"="2005to2010"
),
),

 selectInput("Area", "First Area (state, borough, or census area)",
c(
"Alaska"="Alaska",
"Aleutians East Census Area"="AleutiansEast",
"Aleutians West Census Area"="AleutiansWest",
"Anchorage Municipality"="Anchorage",
"Bethel Census Area"="Bethel",
"Bristol Bay Borough"="BristolBay",
"Denali Borough"="Denali",
"Dillingham Census Area"="Dillingham",
"Fairbanks North Star Borough"="FairbanksNorthStar",
"Haines City and Borough"="Haines",
"Hoonah-Angoon Census Area"="HoonahAngoon",
"Juneau City and Borough"="Juneau",
"Kenai Peninsula Borough"="KenaiPeninsula",
"Ketchikan Gateway Borough"="KetchikanGateway",
"Kodiak Island Borough"="KodiakIsland",
"Kusilvak Census Area"="Kusilvak",
"Lake and Peninsula Borough"="LakeandPeninsula",
"Matanuska-Susitna Borough"="MatanuskaSusitna",
"Nome Census Area"="Nome",
"North Slope Borough"="NorthSlope",
"Northwest Arctic Borough"="NorthwestArctic",
"Petersburg City and Borough"="Petersburg",
"Prince of Wales-Hyder Census Area"="PrinceofWalesHyder",
"Sitka City and Borough"="Sitka",
"Municipality of Skagway Borough"="Skagway",
"Southeast Fairbanks Census Area"="SoutheastFairbanks",
"Valdez-Cordova Census Area"="ValdezCordova",
"Wrangell City and Borough"="Wrangell",
"Yakutat City and Borough"="Yakutat",
"Yukon-Koyukuk Census Area"="YukonKoyukuk"
),
),

 selectInput("Area2", "Second Area (state, borough, or census area)",
c(
"Alaska"="Alaska",
"Aleutians East Census Area"="AleutiansEast",
"Aleutians West Census Area"="AleutiansWest",
"Anchorage Municipality"="Anchorage",
"Bethel Census Area"="Bethel",
"Bristol Bay Borough"="BristolBay",
"Denali Borough"="Denali",
"Dillingham Census Area"="Dillingham",
"Fairbanks North Star Borough"="FairbanksNorthStar",
"Haines City and Borough"="Haines",
"Hoonah-Angoon Census Area"="HoonahAngoon",
"Juneau City and Borough"="Juneau",
"Kenai Peninsula Borough"="KenaiPeninsula",
"Ketchikan Gateway Borough"="KetchikanGateway",
"Kodiak Island Borough"="KodiakIsland",
"Kusilvak Census Area"="Kusilvak",
"Lake and Peninsula Borough"="LakeandPeninsula",
"Matanuska-Susitna Borough"="MatanuskaSusitna",
"Nome Census Area"="Nome",
"North Slope Borough"="NorthSlope",
"Northwest Arctic Borough"="NorthwestArctic",
"Petersburg City and Borough"="Petersburg",
"Prince of Wales-Hyder Census Area"="PrinceofWalesHyder",
"Sitka City and Borough"="Sitka",
"Municipality of Skagway Borough"="Skagway",
"Southeast Fairbanks Census Area"="SoutheastFairbanks",
"Valdez-Cordova Census Area"="ValdezCordova",
"Wrangell City and Borough"="Wrangell",
"Yakutat City and Borough"="Yakutat",
"Yukon-Koyukuk Census Area"="YukonKoyukuk"
),
),

 selectInput("Area3", "Third Area (state, borough, or census area)",
c(
"Alaska"="Alaska",
"Aleutians East Census Area"="AleutiansEast",
"Aleutians West Census Area"="AleutiansWest",
"Anchorage Municipality"="Anchorage",
"Bethel Census Area"="Bethel",
"Bristol Bay Borough"="BristolBay",
"Denali Borough"="Denali",
"Dillingham Census Area"="Dillingham",
"Fairbanks North Star Borough"="FairbanksNorthStar",
"Haines City and Borough"="Haines",
"Hoonah-Angoon Census Area"="HoonahAngoon",
"Juneau City and Borough"="Juneau",
"Kenai Peninsula Borough"="KenaiPeninsula",
"Ketchikan Gateway Borough"="KetchikanGateway",
"Kodiak Island Borough"="KodiakIsland",
"Kusilvak Census Area"="Kusilvak",
"Lake and Peninsula Borough"="LakeandPeninsula",
"Matanuska-Susitna Borough"="MatanuskaSusitna",
"Nome Census Area"="Nome",
"North Slope Borough"="NorthSlope",
"Northwest Arctic Borough"="NorthwestArctic",
"Petersburg City and Borough"="Petersburg",
"Prince of Wales-Hyder Census Area"="PrinceofWalesHyder",
"Sitka City and Borough"="Sitka",
"Municipality of Skagway Borough"="Skagway",
"Southeast Fairbanks Census Area"="SoutheastFairbanks",
"Valdez-Cordova Census Area"="ValdezCordova",
"Wrangell City and Borough"="Wrangell",
"Yakutat City and Borough"="Yakutat",
"Yukon-Koyukuk Census Area"="YukonKoyukuk"
),
),

 selectInput("Area4", "Fourth Area (state, borough, or census area)",
c(
"Alaska"="Alaska",
"Aleutians East Census Area"="AleutiansEast",
"Aleutians West Census Area"="AleutiansWest",
"Anchorage Municipality"="Anchorage",
"Bethel Census Area"="Bethel",
"Bristol Bay Borough"="BristolBay",
"Denali Borough"="Denali",
"Dillingham Census Area"="Dillingham",
"Fairbanks North Star Borough"="FairbanksNorthStar",
"Haines City and Borough"="Haines",
"Hoonah-Angoon Census Area"="HoonahAngoon",
"Juneau City and Borough"="Juneau",
"Kenai Peninsula Borough"="KenaiPeninsula",
"Ketchikan Gateway Borough"="KetchikanGateway",
"Kodiak Island Borough"="KodiakIsland",
"Kusilvak Census Area"="Kusilvak",
"Lake and Peninsula Borough"="LakeandPeninsula",
"Matanuska-Susitna Borough"="MatanuskaSusitna",
"Nome Census Area"="Nome",
"North Slope Borough"="NorthSlope",
"Northwest Arctic Borough"="NorthwestArctic",
"Petersburg City and Borough"="Petersburg",
"Prince of Wales-Hyder Census Area"="PrinceofWalesHyder",
"Sitka City and Borough"="Sitka",
"Municipality of Skagway Borough"="Skagway",
"Southeast Fairbanks Census Area"="SoutheastFairbanks",
"Valdez-Cordova Census Area"="ValdezCordova",
"Wrangell City and Borough"="Wrangell",
"Yakutat City and Borough"="Yakutat",
"Yukon-Koyukuk Census Area"="YukonKoyukuk"
),
),

tags$small(paste0(        
	"This interface was made with Shiny for R (shiny.rstudio.com).       
	Eddie Hunsinger, November 2018.
	Data from live.laborstats.alaska.gov/pop/migration.cfm and www.census.gov/programs-surveys/acs/."
	)),

width=3
),

mainPanel(
	
	plotOutput("plots"),width=3

))
)

#To print full names - thanks https://stackoverflow.com/questions/48106504/r-shiny-how-to-display-choice-label-in-selectinput 
choiceVec <- c(
"Alaska"="Alaska",
"Aleutians East Census Area"="AleutiansEast",
"Aleutians West Census Area"="AleutiansWest",
"Anchorage Municipality"="Anchorage",
"Bethel Census Area"="Bethel",
"Bristol Bay Borough"="BristolBay",
"Denali Borough"="Denali",
"Dillingham Census Area"="Dillingham",
"Fairbanks North Star Borough"="FairbanksNorthStar",
"Haines City and Borough"="Haines",
"Hoonah-Angoon Census Area"="HoonahAngoon",
"Juneau City and Borough"="Juneau",
"Kenai Peninsula Borough"="KenaiPeninsula",
"Ketchikan Gateway Borough"="KetchikanGateway",
"Kodiak Island Borough"="KodiakIsland",
"Kusilvak Census Area"="Kusilvak",
"Lake and Peninsula Borough"="LakeandPeninsula",
"Matanuska-Susitna Borough"="MatanuskaSusitna",
"Nome Census Area"="Nome",
"North Slope Borough"="NorthSlope",
"Northwest Arctic Borough"="NorthwestArctic",
"Petersburg City and Borough"="Petersburg",
"Prince of Wales-Hyder Census Area"="PrinceofWalesHyder",
"Sitka City and Borough"="Sitka",
"Municipality of Skagway Borough"="Skagway",
"Southeast Fairbanks Census Area"="SoutheastFairbanks",
"Valdez-Cordova Census Area"="ValdezCordova",
"Wrangell City and Borough"="Wrangell",
"Yakutat City and Borough"="Yakutat",
"Yukon-Koyukuk Census Area"="YukonKoyukuk"
)

choiceVec2 <- c(
"2005 to 2010"="2005to2010",
"2010 to 2015"="2010to2015"
)

server<-function(input, output) {	
	output$plots<-renderPlot({
par(mfrow=c(2,2))
	
##############################################################################################################################
##############################################################################################################################

##########
##Inputs
#PFD
PFDDataFileInput<-paste(c("https://raw.githubusercontent.com/edyhsgr/AKMigrationProfiles/master/Tables/PFDInMigrationByAgeBCA_",input$period,".csv"),collapse="")
PFDInAgeBCA<-read.table(file=PFDDataFileInput,header=TRUE,sep=",")

#ACS
if (input$period=="2005to2010") {
ACSDataFileInputYear<-"2006to2010"
}
if (input$period=="2010to2015") {
ACSDataFileInputYear<-"2011to2015"
}
ACSEstDataFileInput<-paste(c("https://raw.githubusercontent.com/edyhsgr/AKMigrationProfiles/master/Tables/ACSInMigrationByAgeBCA",ACSDataFileInputYear,"_Est",".csv"),collapse="")
ACSInAgeBCA_Est<-read.table(file=ACSEstDataFileInput,header=TRUE,sep=",")
ACSMOEDataFileInput<-paste(c("https://raw.githubusercontent.com/edyhsgr/AKMigrationProfiles/master/Tables/ACSInMigrationByAgeBCA",ACSDataFileInputYear,"_MOE",".csv"),collapse="")
ACSInAgeBCA_MOE<-read.table(file=ACSMOEDataFileInput,header=TRUE,sep=",")

#Some distinguising characteristics (just Eddie's definitions)
Characteristics<-read.csv(file="https://raw.githubusercontent.com/edyhsgr/AKMigrationProfiles/master/Tables/AKBCACharacteristics.csv",header=TRUE,sep=",")

#Age groups
agegroups <- c("0-4", "5-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+")
#agegroups <- c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+")
##########
##########

##########
##Plot PFD to ACS comparison
ylim<-max(1.1*pmax(ACSInAgeBCA_Est[input$Area][2:15,]+ACSInAgeBCA_MOE[input$Area][2:15,],PFDInAgeBCA[input$Area][2:15,]))
plot(ACSInAgeBCA_Est[input$Area][2:15,] , ylim=c(0,ylim) , ylab="In-Migration" , xlab="", col="orange", type="l", lwd=3, axes=F,panel.first=c(abline(v=c(1.5,2.5,13.5),col="grey65",lty=3)))
lines(ACSInAgeBCA_Est[input$Area][2:15,]+ACSInAgeBCA_MOE[input$Area][2:15,], col="orange", lwd=1)
lines(ACSInAgeBCA_Est[input$Area][2:15,]-ACSInAgeBCA_MOE[input$Area][2:15,], col="orange", lwd=1)
lines(PFDInAgeBCA[input$Area][2:15,], col="blue", lwd=3)
axis(side=1,at=1:14,las=2,labels=agegroups,cex.axis=0.9)
axis(side=2,cex.axis=0.9)
title(c("Average Annual In-Migration by Age",text=names(choiceVec2)[choiceVec2 == input$period],text=names(choiceVec)[choiceVec == input$Area]))
legend(7,ylim/1.1, 
legend=c("ACS Estimate", "ACS Error Bound", "PFD Estimate"), 
col=c("orange", "orange", "blue"), 
lwd=c(3,1,3), cex=.9)
mtext(side=1,line=-16,adj=.75,text="Some relevant characteristics",underline=TRUE,font=2,cex=.8,col="black")
mtext(side=1,line=-15,adj=.75,text=Characteristics[input$Area][1,],font=1,cex=.8,col="black")
mtext(side=1,line=-14,adj=.75,text=Characteristics[input$Area][2,],font=1,cex=.8,col="black")
mtext(side=1,line=-13,adj=.75,text=Characteristics[input$Area][3,],font=1,cex=.8,col="black")
mtext(side=1,line=-12,adj=.75,text=Characteristics[input$Area][4,],font=1,cex=.8,col="black")
##########

##########
##Plot PFD to ACS comparison
ylim<-max(1.1*pmax(ACSInAgeBCA_Est[input$Area2][2:15,]+ACSInAgeBCA_MOE[input$Area2][2:15,],PFDInAgeBCA[input$Area2][2:15,]))
plot(ACSInAgeBCA_Est[input$Area2][2:15,] , ylim=c(0,ylim) , ylab="In-Migration" , xlab="", col="orange", type="l", lwd=3, axes=F,panel.first=c(abline(v=c(1.5,2.5,13.5),col="grey65",lty=3)))
lines(ACSInAgeBCA_Est[input$Area2][2:15,]+ACSInAgeBCA_MOE[input$Area2][2:15,], col="orange", lwd=1)
lines(ACSInAgeBCA_Est[input$Area2][2:15,]-ACSInAgeBCA_MOE[input$Area2][2:15,], col="orange", lwd=1)
lines(PFDInAgeBCA[input$Area2][2:15,], col="blue", lwd=3)
axis(side=1,at=1:14,las=2,labels=agegroups,cex.axis=0.9)
axis(side=2,cex.axis=0.9)
title(c("Average Annual In-Migration by Age",text=names(choiceVec2)[choiceVec2 == input$period],text=names(choiceVec)[choiceVec == input$Area2]))
legend(7,ylim/1.1, 
legend=c("ACS Estimate", "ACS Error Bound", "PFD Estimate"), 
col=c("orange", "orange", "blue"), 
lwd=c(3,1,3), cex=.9)
mtext(side=1,line=-16,adj=.75,text="Some relevant characteristics",underline=TRUE,font=2,cex=.8,col="black")
mtext(side=1,line=-15,adj=.75,text=Characteristics[input$Area2][1,],font=1,cex=.8,col="black")
mtext(side=1,line=-14,adj=.75,text=Characteristics[input$Area2][2,],font=1,cex=.8,col="black")
mtext(side=1,line=-13,adj=.75,text=Characteristics[input$Area2][3,],font=1,cex=.8,col="black")
mtext(side=1,line=-12,adj=.75,text=Characteristics[input$Area2][4,],font=1,cex=.8,col="black")
mtext(side=1,line=-11,adj=.75,text=Characteristics[input$Area2][5,],font=1,cex=.8,col="black")
mtext(side=1,line=-10,adj=.75,text=Characteristics[input$Area2][6,],font=1,cex=.8,col="black")
##########

##########
##Plot PFD to ACS comparison
ylim<-max(1.1*pmax(ACSInAgeBCA_Est[input$Area3][2:15,]+ACSInAgeBCA_MOE[input$Area3][2:15,],PFDInAgeBCA[input$Area3][2:15,]))
plot(ACSInAgeBCA_Est[input$Area3][2:15,] , ylim=c(0,ylim) , ylab="In-Migration" , xlab="", col="orange", type="l", lwd=3, axes=F,panel.first=c(abline(v=c(1.5,2.5,13.5),col="grey65",lty=3)))
lines(ACSInAgeBCA_Est[input$Area3][2:15,]+ACSInAgeBCA_MOE[input$Area3][2:15,], col="orange", lwd=1)
lines(ACSInAgeBCA_Est[input$Area3][2:15,]-ACSInAgeBCA_MOE[input$Area3][2:15,], col="orange", lwd=1)
lines(PFDInAgeBCA[input$Area3][2:15,], col="blue", lwd=3)
axis(side=1,at=1:14,las=2,labels=agegroups,cex.axis=0.9)
axis(side=2,cex.axis=0.9)
title(c("Average Annual In-Migration by Age",text=names(choiceVec2)[choiceVec2 == input$period],text=names(choiceVec)[choiceVec == input$Area3]))
legend(7,ylim/1.1, 
legend=c("ACS Estimate", "ACS Error Bound", "PFD Estimate"), 
col=c("orange", "orange", "blue"), 
lwd=c(3,1,3), cex=.9)
mtext(side=1,line=-16,adj=.75,text="Some relevant characteristics",underline=TRUE,font=2,cex=.8,col="black")
mtext(side=1,line=-15,adj=.75,text=Characteristics[input$Area3][1,],font=1,cex=.8,col="black")
mtext(side=1,line=-14,adj=.75,text=Characteristics[input$Area3][2,],font=1,cex=.8,col="black")
mtext(side=1,line=-13,adj=.75,text=Characteristics[input$Area3][3,],font=1,cex=.8,col="black")
mtext(side=1,line=-12,adj=.75,text=Characteristics[input$Area3][4,],font=1,cex=.8,col="black")
##########

##########
##Plot PFD to ACS comparison
ylim<-max(1.1*pmax(ACSInAgeBCA_Est[input$Area4][2:15,]+ACSInAgeBCA_MOE[input$Area4][2:15,],PFDInAgeBCA[input$Area4][2:15,]))
plot(ACSInAgeBCA_Est[input$Area4][2:15,] , ylim=c(0,ylim) , ylab="In-Migration" , xlab="", col="orange", type="l", lwd=3, axes=F,panel.first=c(abline(v=c(1.5,2.5,13.5),col="grey65",lty=3)))
lines(ACSInAgeBCA_Est[input$Area4][2:15,]+ACSInAgeBCA_MOE[input$Area4][2:15,], col="orange", lwd=1)
lines(ACSInAgeBCA_Est[input$Area4][2:15,]-ACSInAgeBCA_MOE[input$Area4][2:15,], col="orange", lwd=1)
lines(PFDInAgeBCA[input$Area4][2:15,], col="blue", lwd=3)
axis(side=1,at=1:14,las=2,labels=agegroups,cex.axis=0.9)
axis(side=2,cex.axis=0.9)
title(c("Average Annual In-Migration by Age",text=names(choiceVec2)[choiceVec2 == input$period],text=names(choiceVec)[choiceVec == input$Area4]))
legend(7,ylim/1.1, 
legend=c("ACS Estimate", "ACS Error Bound", "PFD Estimate"), 
col=c("orange", "orange", "blue"), 
lwd=c(3,1,3), cex=.9)
mtext(side=1,line=-16,adj=.75,text="Some relevant characteristics",underline=TRUE,font=2,cex=.8,col="black")
mtext(side=1,line=-15,adj=.75,text=Characteristics[input$Area4][1,],font=1,cex=.8,col="black")
mtext(side=1,line=-14,adj=.75,text=Characteristics[input$Area4][2,],font=1,cex=.8,col="black")
mtext(side=1,line=-13,adj=.75,text=Characteristics[input$Area4][3,],font=1,cex=.8,col="black")
mtext(side=1,line=-12,adj=.75,text=Characteristics[input$Area4][4,],font=1,cex=.8,col="black")
##########

},height=800,width=800)
		
}

shinyApp(ui = ui, server = server)
