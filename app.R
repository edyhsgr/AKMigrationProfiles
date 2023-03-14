##############################################################################################################################
##############################################################################################################################
##R CODE FOR PFD MIGRATION REVIEW
##
##EDDIE HUNSINGER, NOVEMBER 2018 (UPDATED MARCH 2023)
##edyhsgr.github.io
##edyhsgr@gmail.com
##############################################################################################################################
##############################################################################################################################

library(shiny)

ui<-fluidPage(

	tags$h3("PFD-Based Migration Data Reviewer"),
	p(""),
  
hr(),

sidebarLayout(
sidebarPanel(

selectInput("Area1", "Area (state, borough, or census area)",
            c(
              "Alaska"="000",
              "Aleutians East Borough"="013",
              "Aleutians West Census Area"="016",
              "Anchorage Municipality"="020",
              "Bethel Census Area"="050",
              "Bristol Bay Borough"="060",
              "Chugach Census Area"="063",
              "Copper River Census Area"="066",
              "Denali Borough"="068",
              "Dillingham Census Area"="070",
              "Fairbanks North Star Borough"="090",
              "Haines City and Borough"="100",
              "Hoonah-Angoon Census Area"="105",
              "Juneau City and Borough"="110",
              "Kenai Peninsula Borough"="122",
              "Ketchikan Gateway Borough"="130",
              "Kodiak Island Borough"="150",
              "Kusilvak Census Area"="158",
              "Lake and Peninsula Borough"="164",
              "Matanuska-Susitna Borough"="170",
              "Nome Census Area"="180",
              "North Slope Borough"="185",
              "Northwest Arctic Borough"="188",
              "Petersburg City and Borough"="195",
              "Prince of Wales-Hyder Census Area"="198",
              "Sitka City and Borough"="220",
              "Municipality of Skagway Borough"="230",
              "Southeast Fairbanks Census Area"="240",
              #"Valdez-Cordova Census Area"="261",
              "Wrangell City and Borough"="275",
              "Yakutat City and Borough"="282",
              "Yukon-Koyukuk Census Area"="290"
            ),
),

selectInput("Sex", "Sex grouping",
            c(
              "Total"="Total",
              "Male"="Male",
              "Female"="Female"
            ),
),

selectInput("Period1", "Time period",
            c(
              "2015 to 2020"="2015 to 2020",
              "2010 to 2015"="2010 to 2015",
              "2005 to 2010"="2005 to 2010",
              "2000 to 2005"="2000 to 2005",
              "1995 to 2000"="1995 to 2000",
              "1990 to 1995"="1990 to 1995"
            ),
),

selectInput("Period2", "Extrapolation starting period (only for bottom-right panel)", selected="1990 to 1995",
            c(
              "2015 to 2020"="2015 to 2020",
              "2010 to 2015"="2010 to 2015",
              "2005 to 2010"="2005 to 2010",
              "2000 to 2005"="2000 to 2005",
              "1995 to 2000"="1995 to 2000",
              "1990 to 1995"="1990 to 1995"
            ),
),

  p("This interface was made with Shiny for R, using population and PFD-based-migration data from the Alaska Department of Labor and Workforce Development, Research and Analysis Section. Eddie Hunsinger, November 2018 (updated March 2023)."),
  p("Printed ratio sums are multiplied by five for age group size."),
  p("'Extrapolated values' (in grey, bottom-right panel) are average linear change between the selected starting period and 2015 to 2020, applied to the 2015 to 2020 period data."),
  p(tags$a(href="https://live.laborstats.alaska.gov/pop/migration.cfm", "Migration Data and Information (PFD-based-migration methods under Methodology)."),
    tags$a(href="https://web.archive.org/web/20210611231927/https://live.laborstats.alaska.gov/pop/index.cfm", "Population Estimates Data and Information."),
  p(tags$a(href="https://edyhsgr.github.io/documents/alaskan-migration_SlidesWithNotes.pdf", "Associated presentation on Alaska migration."),
    tags$a(href="https://raw.githubusercontent.com/edyhsgr/edyhsgr.github.io/master/documents/JSM2018_EddieH.pdf", "Related presentation on use of administrative data."),
    tags$a(href="https://applieddemogtoolbox.github.io/#AKMigration", "Alaska PFD-Based Migration Data listing on the Applied Demography Toolbox page."))),
  p(tags$a(href="https://github.com/edyhsgr/AKMigrationProfiles", "GitHub repository with code for the Shiny application."),
    tags$a(href="shiny.rstudio.com", "Shiny for R information.")),
    width=3),

mainPanel(
	plotOutput("plots"),width=3
))
)

##Inputs
PFDMigrAgeSexBCA<-read.table(file="https://raw.githubusercontent.com/edyhsgr/AKMigrationProfiles/master/Tables/PFDMigrationByAgeBySexBCA_v2020.csv",header=TRUE,sep=",",
                             colClasses=c("character","character","character","numeric","numeric","numeric","numeric","numeric","numeric","character"))

PopAgeSexBCA19902000<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/AKMigrationProfiles/master/Tables/PopAgeBySexBCA_1990to2000v2012.csv",header=TRUE,sep=",",colClasses=c("Borough.Census.Area.FIPS"="character")))
PopAgeSexBCA20002010<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/AKMigrationProfiles/master/Tables/PopAgeBySexBCA_2000to2010v2010.csv",header=TRUE,sep=",",colClasses=c("Borough.Census.Area.FIPS"="character")))
PopAgeSexBCA20102020<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/AKMigrationProfiles/master/Tables/PopAgeBySexBCA_2010to2020v2020.csv",header=TRUE,sep=",",colClasses=c("Borough.Census.Area.FIPS"="character")))
PopAgeSexBCA19902000$Borough.Census.Area.FIPS[PopAgeSexBCA19902000$Borough.Census.Area.FIPS=="270"]<-"158"
PopAgeSexBCA20002010$Borough.Census.Area.FIPS[PopAgeSexBCA20002010$Borough.Census.Area.FIPS=="270"]<-"158"
PopAgeSexBCA20102020$Borough.Census.Area.FIPS[PopAgeSexBCA20102020$Borough.Census.Area.FIPS=="270"]<-"158"
PopAgeSexBCA19902000[PopAgeSexBCA19902000==0]<-1
PopAgeSexBCA20002010[PopAgeSexBCA20002010==0]<-1
PopAgeSexBCA20102020[PopAgeSexBCA20102020==0]<-1


server<-function(input, output) {	
	output$plots<-renderPlot({
par(mfrow=c(2,2))
	
##############################################################################################################################
##############################################################################################################################
if(input$Area1=="063" | input$Area1=="066" | input$Area1=="105" | input$Area1=="195" | input$Area1=="198" | input$Area1=="230" | input$Area1=="275") {plot.new()
	    legend("topleft",legend=c("I need to manage some geog info to add this area."),cex=1.5,bty="n")
	  }
	  
if(input$Area1!="063" & input$Area1!="066" & input$Area1!="105" & input$Area1!="195" & input$Area1!="198" & input$Area1!="230" & input$Area1!="275") {	  

if(input$Sex=="Total") {
  PopAgeSexBCA19902000$Select<-PopAgeSexBCA19902000$Total
  PopAgeSexBCA20002010$Select<-PopAgeSexBCA20002010$Total
  PopAgeSexBCA20102020$Select<-PopAgeSexBCA20102020$Total
  PFDMigrAgeSexBCA$In_Select<-PFDMigrAgeSexBCA$In_Total
  PFDMigrAgeSexBCA$Out_Select<-PFDMigrAgeSexBCA$Out_Total
}

if(input$Sex=="Male") {
  PopAgeSexBCA19902000$Select<-PopAgeSexBCA19902000$Male
  PopAgeSexBCA20002010$Select<-PopAgeSexBCA20002010$Male
  PopAgeSexBCA20102020$Select<-PopAgeSexBCA20102020$Male
  PFDMigrAgeSexBCA$In_Select<-PFDMigrAgeSexBCA$In_Male
  PFDMigrAgeSexBCA$Out_Select<-PFDMigrAgeSexBCA$Out_Male
}

if(input$Sex=="Female") {
  PopAgeSexBCA19902000$Select<-PopAgeSexBCA19902000$Female
  PopAgeSexBCA20002010$Select<-PopAgeSexBCA20002010$Female
  PopAgeSexBCA20102020$Select<-PopAgeSexBCA20102020$Female
  PFDMigrAgeSexBCA$In_Select<-PFDMigrAgeSexBCA$In_Female
  PFDMigrAgeSexBCA$Out_Select<-PFDMigrAgeSexBCA$Out_Female
}
  
#To print full names - thanks https://stackoverflow.com/questions/48106504/r-shiny-how-to-display-choice-label-in-selectinput 
	  AreaNames <- c(
	    "Alaska"="000",
	    "Aleutians East Census Area"="013",
	    "Aleutians West Census Area"="016",
	    "Anchorage Municipality"="020",
	    "Bethel Census Area"="050",
	    "Bristol Bay Borough"="060",
	    "Denali Borough"="068",
	    "Dillingham Census Area"="070",
	    "Fairbanks North Star Borough"="090",
	    "Haines City and Borough"="100",
	    "Hoonah-Angoon Census Area"="105",
	    "Juneau City and Borough"="110",
	    "Kenai Peninsula Borough"="122",
	    "Ketchikan Gateway Borough"="130",
	    "Kodiak Island Borough"="150",
	    "Kusilvak Census Area"="158",
	    "Lake and Peninsula Borough"="164",
	    "Matanuska-Susitna Borough"="170",
	    "Nome Census Area"="180",
	    "North Slope Borough"="185",
	    "Northwest Arctic Borough"="188",
	    "Petersburg City and Borough"="195",
	    "Prince of Wales-Hyder Census Area"="198",
	    "Sitka City and Borough"="220",
	    "Municipality of Skagway Borough"="230",
	    "Southeast Fairbanks Census Area"="240",
	    "Valdez-Cordova Census Area"="261",
	    "Wrangell City and Borough"="275",
	    "Yakutat City and Borough"="282",
	    "Yukon-Koyukuk Census Area"="290"
	  )
	  
	  choiceVec3 <- c(
	    "Total"="Total",
	    "Male"="Male",
	    "Female"="Female"
	  )	  

#Age groups
agegroups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84")
agelength<-17 #seventeen age groups
agesize<-5

##########
##Selections
InSelect<-PFDMigrAgeSexBCA$In_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period==input$Period1 & PFDMigrAgeSexBCA$Age!="Total"][1:agelength]
OutSelect<-PFDMigrAgeSexBCA$Out_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period==input$Period1 & PFDMigrAgeSexBCA$Age!="Total"][1:agelength]
NetSelect<-InSelect-OutSelect
Pop<-rbind(PopAgeSexBCA20102020[PopAgeSexBCA20102020$Borough.Census.Area.FIPS==input$Area1,c(1:6,8)],
                   PopAgeSexBCA20002010[PopAgeSexBCA20002010$Borough.Census.Area.FIPS==input$Area1 & PopAgeSexBCA20002010$Year!="2010",c(1:6,8)],
                   PopAgeSexBCA19902000[PopAgeSexBCA19902000$Borough.Census.Area.FIPS==input$Area1 & PopAgeSexBCA19902000$Year!="2000",c(1:6,8)])

PopSelect<-Pop[Pop$Year==as.numeric(substr(input$Period1,1,4))+2,]
PopSelect<-PopSelect[1:agelength,]

In2015to2020<-PFDMigrAgeSexBCA$In_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="2015 to 2020" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength]
In2010to2015<-PFDMigrAgeSexBCA$In_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="2010 to 2015" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength]
In2005to2010<-PFDMigrAgeSexBCA$In_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="2005 to 2010" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength]
In2000to2005<-PFDMigrAgeSexBCA$In_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="2000 to 2005" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength]
In1995to2000<-PFDMigrAgeSexBCA$In_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="1995 to 2000" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength]
In1990to1995<-PFDMigrAgeSexBCA$In_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="1990 to 1995" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength]

Out2015to2020<-PFDMigrAgeSexBCA$Out_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="2015 to 2020" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength]
Out2010to2015<-PFDMigrAgeSexBCA$Out_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="2010 to 2015" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength]
Out2005to2010<-PFDMigrAgeSexBCA$Out_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="2005 to 2010" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength]
Out2000to2005<-PFDMigrAgeSexBCA$Out_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="2000 to 2005" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength]
Out1995to2000<-PFDMigrAgeSexBCA$Out_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="1995 to 2000" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength]
Out1990to1995<-PFDMigrAgeSexBCA$Out_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="1990 to 1995" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength]

Net2015to2020<-In2015to2020-Out2015to2020
Net2010to2015<-In2010to2015-Out2010to2015
Net2005to2010<-In2005to2010-Out2005to2010
Net2000to2005<-In2000to2005-Out2000to2005
Net1995to2000<-In1995to2000-Out1995to2000
Net1990to1995<-In1990to1995-Out1990to1995

InRateSelect<-PFDMigrAgeSexBCA$In_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period==input$Period1 & PFDMigrAgeSexBCA$Age!="Total"][1:agelength] / PopSelect$Select[1:agelength]
InRate2015to2020<-PFDMigrAgeSexBCA$In_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="2015 to 2020" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength] / 
  Pop$Select[Pop$Year==2017][1:agelength]
InRate2010to2015<-PFDMigrAgeSexBCA$In_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="2010 to 2015" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength] / 
  Pop$Select[Pop$Year==2012][1:agelength]
InRate2005to2010<-PFDMigrAgeSexBCA$In_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="2005 to 2010" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength] / 
  Pop$Select[Pop$Year==2007][1:agelength]
InRate2000to2005<-PFDMigrAgeSexBCA$In_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="2000 to 2005" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength] / 
  Pop$Select[Pop$Year==2002][1:agelength]
InRate1995to2000<-PFDMigrAgeSexBCA$In_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="1995 to 2000" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength] / 
  Pop$Select[Pop$Year==1997][1:agelength]
InRate1990to1995<-PFDMigrAgeSexBCA$In_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="1990 to 1995" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength] / 
  Pop$Select[Pop$Year==1992][1:agelength]

OutRateSelect<-PFDMigrAgeSexBCA$Out_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period==input$Period1 & PFDMigrAgeSexBCA$Age!="Total"][1:agelength] / PopSelect$Select[1:agelength]
OutRate2015to2020<-PFDMigrAgeSexBCA$Out_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="2015 to 2020" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength] / 
  Pop$Select[Pop$Year==2017][1:agelength]
OutRate2010to2015<-PFDMigrAgeSexBCA$Out_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="2010 to 2015" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength] / 
  Pop$Select[Pop$Year==2012][1:agelength]
OutRate2005to2010<-PFDMigrAgeSexBCA$Out_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="2005 to 2010" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength] / 
  Pop$Select[Pop$Year==2007][1:agelength]
OutRate2000to2005<-PFDMigrAgeSexBCA$Out_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="2000 to 2005" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength] / 
  Pop$Select[Pop$Year==2002][1:agelength]
OutRate1995to2000<-PFDMigrAgeSexBCA$Out_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="1995 to 2000" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength] / 
  Pop$Select[Pop$Year==1997][1:agelength]
OutRate1990to1995<-PFDMigrAgeSexBCA$Out_Select[PFDMigrAgeSexBCA$Borough.Census.Area.FIPS==input$Area1 & PFDMigrAgeSexBCA$Period=="1990 to 1995" & PFDMigrAgeSexBCA$Age!="Total"][1:agelength] / 
  Pop$Select[Pop$Year==1992][1:agelength]

NetRateSelect<-InRateSelect-OutRateSelect
NetRate2015to2020<-InRate2015to2020-OutRate2015to2020
NetRate2010to2015<-InRate2010to2015-OutRate2010to2015
NetRate2005to2010<-InRate2005to2010-OutRate2005to2010
NetRate2000to2005<-InRate2000to2005-OutRate2000to2005
NetRate1995to2000<-InRate1995to2000-OutRate1995to2000
NetRate1990to1995<-InRate1990to1995-OutRate1990to1995

InRateScaledSelect<-InRateSelect/sum(InRateSelect)
InRateScaled2015to2020<-InRate2015to2020/sum(InRate2015to2020)
InRateScaled2010to2015<-InRate2010to2015/sum(InRate2010to2015)
InRateScaled2005to2010<-InRate2005to2010/sum(InRate2005to2010)
InRateScaled2000to2005<-InRate2000to2005/sum(InRate2000to2005)
InRateScaled1995to2000<-InRate1995to2000/sum(InRate1995to2000)
InRateScaled1990to1995<-InRate1990to1995/sum(InRate1990to1995)

OutRateScaledSelect<-OutRateSelect/sum(OutRateSelect)
OutRateScaled2015to2020<-OutRate2015to2020/sum(OutRate2015to2020)
OutRateScaled2010to2015<-OutRate2010to2015/sum(OutRate2010to2015)
OutRateScaled2005to2010<-OutRate2005to2010/sum(OutRate2005to2010)
OutRateScaled2000to2005<-OutRate2000to2005/sum(OutRate2000to2005)
OutRateScaled1995to2000<-OutRate1995to2000/sum(OutRate1995to2000)
OutRateScaled1990to1995<-OutRate1990to1995/sum(OutRate1990to1995)

NetRateScaledSelect<-InRateScaledSelect-OutRateScaledSelect
NetRateScaled2015to2020<-InRateScaled2015to2020-OutRateScaled2015to2020
NetRateScaled2010to2015<-InRateScaled2010to2015-OutRateScaled2010to2015
NetRateScaled2005to2010<-InRateScaled2005to2010-OutRateScaled2005to2010
NetRateScaled2000to2005<-InRateScaled2000to2005-OutRateScaled2000to2005
NetRateScaled1995to2000<-InRateScaled1995to2000-OutRateScaled1995to2000
NetRateScaled1990to1995<-InRateScaled1990to1995-OutRateScaled1990to1995

if(input$Period2=="1990 to 1995") {NetRateScaledExtrap<-(NetRateScaled2015to2020-NetRateScaled1990to1995)/5+NetRateScaled2015to2020}
if(input$Period2=="1995 to 2000") {NetRateScaledExtrap<-(NetRateScaled2015to2020-NetRateScaled1995to2000)/4+NetRateScaled2015to2020}
if(input$Period2=="2000 to 2005") {NetRateScaledExtrap<-(NetRateScaled2015to2020-NetRateScaled2000to2005)/3+NetRateScaled2015to2020}
if(input$Period2=="2005 to 2010") {NetRateScaledExtrap<-(NetRateScaled2015to2020-NetRateScaled2005to2010)/2+NetRateScaled2015to2020}
if(input$Period2=="2010 to 2015") {NetRateScaledExtrap<-(NetRateScaled2015to2020-NetRateScaled2005to2010)/1+NetRateScaled2015to2020}
if(input$Period2=="2015 to 2020") {NetRateScaledExtrap<-NetRateScaled2015to2020}

YMin<--1.5*abs(min(c(NetSelect,Net2015to2020,Net2010to2015,Net2005to2010,Net2005to2010,Net2000to2005,Net1995to2000,Net1990to1995)))
YMax<-1.25*max(c(InSelect,In2015to2020,In2010to2015,In2005to2010,In2005to2010,In2000to2005,In1995to2000,In1990to1995,
                    Out2015to2020,Out2010to2015,Out2005to2010,Out2005to2010,Out2000to2005,Out1995to2000,Out1990to1995))

YRateMin<--1.5*abs(min(c(NetRateSelect,NetRate2015to2020,NetRate2010to2015,NetRate2005to2010,NetRate2005to2010,NetRate2000to2005,NetRate1995to2000,NetRate1990to1995)))
YRateMax<-1.25*max(c(InRateSelect,InRate2015to2020,InRate2010to2015,InRate2005to2010,InRate2005to2010,InRate2000to2005,InRate1995to2000,InRate1990to1995,
                    OutRate2015to2020,OutRate2010to2015,OutRate2005to2010,OutRate2005to2010,OutRate2000to2005,OutRate1995to2000,OutRate1990to1995))

YRateScaledMin<--1.5*abs(min(c(NetRateScaledSelect,NetRateScaled2015to2020,NetRateScaled2010to2015,NetRateScaled2005to2010,NetRateScaled2005to2010,NetRateScaled2000to2005,NetRateScaled1995to2000,NetRateScaled1990to1995)))
YRateScaledMax<-1.25*max(c(InRateScaledSelect,InRateScaled2015to2020,InRateScaled2010to2015,InRateScaled2005to2010,InRateScaled2005to2010,InRateScaled2000to2005,InRateScaled1995to2000,InRateScaled1990to1995,
                    OutRateScaled2015to2020,OutRateScaled2010to2015,OutRateScaled2005to2010,OutRateScaled2005to2010,OutRateScaled2000to2005,OutRateScaled1995to2000,OutRateScaled1990to1995))

YRateScaledExtrapMin<--1.5*abs(min(c(NetRateScaledSelect,NetRateScaled2015to2020,NetRateScaled2010to2015,NetRateScaled2005to2010,NetRateScaled2005to2010,NetRateScaled2000to2005,NetRateScaled1995to2000,NetRateScaled1990to1995)))
YRateScaledExtrapMax<-1.5*abs(max(c(NetRateScaledSelect,NetRateScaled2015to2020,NetRateScaled2010to2015,NetRateScaled2005to2010,NetRateScaled2005to2010,NetRateScaled2000to2005,NetRateScaled1995to2000,NetRateScaled1990to1995)))

##########
#MIGRATION
plot(InSelect,ylab="",xlab="",axes=F,col="blue",type="l",lwd=4,panel.first=c(abline(h=0)),ylim=c(YMin,YMax))
lines(In2015to2020, col="blue", lwd=1, lty=3)
lines(In2010to2015, col="blue", lwd=1, lty=3)
lines(In2005to2010, col="blue", lwd=1, lty=3)
lines(In2000to2005, col="blue", lwd=1, lty=3)
lines(In1995to2000, col="blue", lwd=1, lty=3)
lines(In1990to1995, col="blue", lwd=1, lty=3)
lines(OutSelect, col="orange", lwd=4)
lines(Out2015to2020, col="orange", lwd=1, lty=3)
lines(Out2010to2015, col="orange", lwd=1, lty=3)
lines(Out2005to2010, col="orange", lwd=1, lty=3)
lines(Out2000to2005, col="orange", lwd=1, lty=3)
lines(Out1995to2000, col="orange", lwd=1, lty=3)
lines(Out1990to1995, col="orange", lwd=1, lty=3)
lines(NetSelect, col="forestgreen", lwd=4)
lines(Net2015to2020, col="forestgreen", lwd=1, lty=3)
lines(Net2010to2015, col="forestgreen", lwd=1, lty=3)
lines(Net2005to2010, col="forestgreen", lwd=1, lty=3)
lines(Net2000to2005, col="forestgreen", lwd=1, lty=3)
lines(Net1995to2000, col="forestgreen", lwd=1, lty=3)
lines(Net1990to1995, col="forestgreen", lwd=1, lty=3)
axis(side=1,at=1:agelength,las=2,labels=agegroups,cex.axis=0.9)
axis(side=2,cex.axis=0.9)
title(c("Average Annual Migration by Age Grouping",
        paste(c(text=names(AreaNames)[AreaNames == input$Area1]," ",text=input$Sex),collapse=""),
        paste(c(text=input$Period1," in Bold"),collapse="")))
legend(10,YMax, 
  legend=c("In-Migration", "Out-Migration", "Net Migration"), 
  col=c("blue", "orange", "forestgreen"), 
  lwd=c(4,4,4), cex=1)
mtext(side=1,line=-23,adj=.7,text="Sum 0 to 84: ",font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-23,adj=.9,text=sum(NetSelect[1:agelength]),font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-22,adj=.7,text="Sum 0 to 14: ",font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-22,adj=.9,text=sum(NetSelect[1:3]),font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-21,adj=.7,text="Sum 15 to 19: ",font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-21,adj=.9,text=sum(NetSelect[4]),font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-20,adj=.7,text="Sum 20 to 49: ",font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-20,adj=.9,text=sum(NetSelect[5:10]),font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-19,adj=.7,text="Sum 50 to 84: ",font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-19,adj=.9,text=sum(NetSelect[11:agelength]),font=2,cex=.8,col="forestgreen")

#RATES
plot(InRateSelect,ylab="",xlab="",axes=F,col="blue",type="l",lwd=4,panel.first=c(abline(h=0)),ylim=c(YRateMin,YRateMax))
lines(InRate2015to2020, col="blue", lwd=1, lty=3)
lines(InRate2010to2015, col="blue", lwd=1, lty=3)
lines(InRate2005to2010, col="blue", lwd=1, lty=3)
lines(InRate2000to2005, col="blue", lwd=1, lty=3)
lines(InRate1995to2000, col="blue", lwd=1, lty=3)
lines(InRate1990to1995, col="blue", lwd=1, lty=3)
lines(OutRateSelect, col="orange", lwd=4)
lines(OutRate2015to2020, col="orange", lwd=1, lty=3)
lines(OutRate2010to2015, col="orange", lwd=1, lty=3)
lines(OutRate2005to2010, col="orange", lwd=1, lty=3)
lines(OutRate2000to2005, col="orange", lwd=1, lty=3)
lines(OutRate1995to2000, col="orange", lwd=1, lty=3)
lines(OutRate1990to1995, col="orange", lwd=1, lty=3)
lines(NetRateSelect, col="forestgreen", lwd=4)
lines(NetRate2015to2020, col="forestgreen", lwd=1, lty=3)
lines(NetRate2010to2015, col="forestgreen", lwd=1, lty=3)
lines(NetRate2005to2010, col="forestgreen", lwd=1, lty=3)
lines(NetRate2000to2005, col="forestgreen", lwd=1, lty=3)
lines(NetRate1995to2000, col="forestgreen", lwd=1, lty=3)
lines(NetRate1990to1995, col="forestgreen", lwd=1, lty=3)
axis(side=1,at=1:agelength,las=2,labels=agegroups,cex.axis=0.9)
axis(side=2,cex.axis=0.9)
title(c("Average Annual Migration by Age Grouping",
        paste(c(text=names(AreaNames)[AreaNames == input$Area1]," ",text=input$Sex),collapse=""),
        paste(c(text=input$Period1," in Bold"),collapse="")))
legend(10,YRateMax, 
       legend=c("In-Migration Ratios", "Out-Migration Rates", "Net Migration Ratios"), 
       col=c("blue", "orange", "forestgreen"), 
       lwd=c(4,4,4), cex=1)
mtext(side=1,line=-23,adj=.7,text="Sum 0 to 84: ",font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-23,adj=.9,text=round(sum(NetRateSelect[1:agelength])*agesize,2),font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-22,adj=.7,text="Sum 0 to 14: ",font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-22,adj=.9,text=round(sum(NetRateSelect[1:3])*agesize,2),font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-21,adj=.7,text="Sum 15 to 19: ",font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-21,adj=.9,text=round(sum(NetRateSelect[4])*agesize,2),font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-20,adj=.7,text="Sum 20 to 49: ",font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-20,adj=.9,text=round(sum(NetRateSelect[5:10])*agesize,2),font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-19,adj=.7,text="Sum 50 to 84: ",font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-19,adj=.9,text=round(sum(NetRateSelect[11:agelength])*agesize,2),font=2,cex=.8,col="forestgreen")

#SCALED RATES
plot(InRateScaledSelect,ylab="",xlab="",axes=F,col="blue",type="l",lwd=4,panel.first=c(abline(h=0)),ylim=c(YRateScaledMin,YRateScaledMax))
lines(InRateScaled2015to2020, col="blue", lwd=1, lty=3)
lines(InRateScaled2010to2015, col="blue", lwd=1, lty=3)
lines(InRateScaled2005to2010, col="blue", lwd=1, lty=3)
lines(InRateScaled2000to2005, col="blue", lwd=1, lty=3)
lines(InRateScaled1995to2000, col="blue", lwd=1, lty=3)
lines(InRateScaled1990to1995, col="blue", lwd=1, lty=3)
lines(OutRateScaledSelect, col="orange", lwd=4)
lines(OutRateScaled2015to2020, col="orange", lwd=1, lty=3)
lines(OutRateScaled2010to2015, col="orange", lwd=1, lty=3)
lines(OutRateScaled2005to2010, col="orange", lwd=1, lty=3)
lines(OutRateScaled2000to2005, col="orange", lwd=1, lty=3)
lines(OutRateScaled1995to2000, col="orange", lwd=1, lty=3)
lines(OutRateScaled1990to1995, col="orange", lwd=1, lty=3)
lines(NetRateScaledSelect, col="forestgreen", lwd=4)
lines(NetRateScaled2015to2020, col="forestgreen", lwd=1, lty=3)
lines(NetRateScaled2010to2015, col="forestgreen", lwd=1, lty=3)
lines(NetRateScaled2005to2010, col="forestgreen", lwd=1, lty=3)
lines(NetRateScaled2000to2005, col="forestgreen", lwd=1, lty=3)
lines(NetRateScaled1995to2000, col="forestgreen", lwd=1, lty=3)
lines(NetRateScaled1990to1995, col="forestgreen", lwd=1, lty=3)
axis(side=1,at=1:agelength,las=2,labels=agegroups,cex.axis=0.9)
axis(side=2,cex.axis=0.9)
title(c("Average Annual Migration by Age Grouping",
        paste(c(text=names(AreaNames)[AreaNames == input$Area1]," ",text=input$Sex),collapse=""),
        paste(c(text=input$Period1," in Bold"),collapse="")))
legend(8,YRateScaledMax, 
       legend=c("Scaled In-Migration Ratios", "Scaled Out-Migration Rates", "Scaled Net Migration Ratios"), 
       col=c("blue", "orange", "forestgreen"), 
       lwd=c(4,4,4), cex=1)
mtext(side=1,line=-23,adj=.7,text="Sum 0 to 84: ",font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-23,adj=.9,text=round(sum(NetRateScaledSelect[1:agelength])*agesize,2),font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-22,adj=.7,text="Sum 0 to 14: ",font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-22,adj=.9,text=round(sum(NetRateScaledSelect[1:3])*agesize,2),font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-21,adj=.7,text="Sum 15 to 19: ",font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-21,adj=.9,text=round(sum(NetRateScaledSelect[4])*agesize,2),font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-20,adj=.7,text="Sum 20 to 49: ",font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-20,adj=.9,text=round(sum(NetRateScaledSelect[5:10])*agesize,2),font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-19,adj=.7,text="Sum 50 to 84: ",font=2,cex=.8,col="forestgreen")
mtext(side=1,line=-19,adj=.9,text=round(sum(NetRateScaledSelect[11:agelength])*agesize,2),font=2,cex=.8,col="forestgreen")

#SCALED AND EXTRAPOLATED RATES
plot(NetRateScaledSelect,ylab="",xlab="",axes=F,col="forestgreen",type="l",lwd=4,panel.first=c(abline(h=0)),ylim=c(YRateScaledExtrapMin,YRateScaledExtrapMax))
lines(NetRateScaled2015to2020, col="forestgreen", lwd=1, lty=3)
lines(NetRateScaled2010to2015, col="forestgreen", lwd=1, lty=3)
lines(NetRateScaled2005to2010, col="forestgreen", lwd=1, lty=3)
lines(NetRateScaled2000to2005, col="forestgreen", lwd=1, lty=3)
lines(NetRateScaled1995to2000, col="forestgreen", lwd=1, lty=3)
lines(NetRateScaled1990to1995, col="forestgreen", lwd=1, lty=3)
lines(NetRateScaledExtrap, col="gray49", lwd=2, lty=2)
axis(side=1,at=1:agelength,las=2,labels=agegroups,cex.axis=0.9)
axis(side=2,cex.axis=0.9)
title(c("Average Annual Migration by Age Grouping",
        paste(c(text=names(AreaNames)[AreaNames == input$Area1]," ",text=input$Sex),collapse=""),
        paste(c(text=input$Period1," in Bold"),collapse="")))
legend(8,YRateScaledExtrapMax, 
       legend=c("Scaled Net Migration Ratios", "Extrapolated Values"), 
       col=c("forestgreen", "gray49"), 
       lwd=c(4,2), lty=c(1,2), cex=1)
mtext(side=1,line=-24,adj=.85,text="Extrapolated Values ",font=2,cex=.9,col="gray49")
mtext(side=1,line=-23,adj=.7,text="Sum 0 to 84: ",font=2,cex=.8,col="gray49")
mtext(side=1,line=-23,adj=.9,text=round(sum(NetRateScaledExtrap[1:agelength])*agesize,2),font=2,cex=.8,col="gray49")
mtext(side=1,line=-22,adj=.7,text="Sum 0 to 14: ",font=2,cex=.8,col="gray49")
mtext(side=1,line=-22,adj=.9,text=round(sum(NetRateScaledExtrap[1:3])*agesize,2),font=2,cex=.8,col="gray49")
mtext(side=1,line=-21,adj=.7,text="Sum 15 to 19: ",font=2,cex=.8,col="gray49")
mtext(side=1,line=-21,adj=.9,text=round(sum(NetRateScaledExtrap[4])*agesize,2),font=2,cex=.8,col="gray49")
mtext(side=1,line=-20,adj=.7,text="Sum 20 to 49: ",font=2,cex=.8,col="gray49")
mtext(side=1,line=-20,adj=.9,text=round(sum(NetRateScaledExtrap[5:10])*agesize,2),font=2,cex=.8,col="gray49")
mtext(side=1,line=-19,adj=.7,text="Sum 50 to 84: ",font=2,cex=.8,col="gray49")
mtext(side=1,line=-19,adj=.9,text=round(sum(NetRateScaledExtrap[11:agelength])*agesize,2),font=2,cex=.8,col="gray49")
}

},height=950,width=950)
		
}

shinyApp(ui = ui, server = server)

