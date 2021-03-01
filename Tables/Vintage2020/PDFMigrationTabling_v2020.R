#install.packages(c("tidyverse","readxl"))
library("readxl")

##Tabling function
TableAKMigration<-function(data){
	##Rename variable names
	colnames(data)<-c("FIPS","AreaName","Age","space","Total","Male","Female")
	#head(data)

	##Formatting (area name for each row...
	area_name<-subset(data[,1:3],data$Age=="Total")
	data_area_name_merge<-merge(data,area_name,by="FIPS")
	##	...and numeric age codes)
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="Total"]<-0
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="0-4"]<-1
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="5-9"]<-2
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="10-14"]<-3
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="15-19"]<-4
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="20-24"]<-5
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="25-29"]<-6
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="30-34"]<-7
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="35-39"]<-8
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="40-44"]<-9
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="45-49"]<-10
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="50-54"]<-11
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="55-59"]<-12
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="60-64"]<-13
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="65-69"]<-14
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="70-74"]<-15
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="75-79"]<-16
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="80-84"]<-17
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="85-89"]<-18
	data_area_name_merge$AgeNumber[data_area_name_merge$Age.x=="90+"]<-19
	#head(data_area_name_merge)

	##Make output dataframe...
	Migration<-data.frame(
		FIPS=data_area_name_merge$FIPS,
		AreaName=data_area_name_merge$AreaName.y,
		AgeNumber=data_area_name_merge$AgeNumber,	
		Age=data_area_name_merge$Age.x,
		Total=data_area_name_merge$Total,
		Male=data_area_name_merge$Male,
		Female=data_area_name_merge$Female)
	##	...and sort (order) dataframe
	Migration<-data.frame(
		Migration[order(
			Migration$FIPS,
			Migration$AreaName,
			Migration$AgeNumber),])
	#head(Migration)

	return(Migration)}

##Read Excel file in and make dataframes for in- and out-migration by period
	##2015to2020
	data<-read_excel(
		"C:/Users/ehunsing/Desktop/PFDMigrationByAgeBySexBCA_v2020.xls", 
		range="2015-2020!A5:K656")
	indata<-data[,c(1:7)]
	outdata<-data[,c(1:4,9:11)]	
	InMigrationAKBCA2015to2020<-TableAKMigration(indata)
	OutMigrationAKBCA2015to2020<-TableAKMigration(outdata)

	##2010to2015
	data<-read_excel(
		"C:/Users/ehunsing/Desktop/PFDMigrationByAgeBySexBCA_v2020.xls", 
		range="2010-2015!A5:K656")
	indata<-data[,c(1:7)]
	outdata<-data[,c(1:4,9:11)]	
	InMigrationAKBCA2010to2015<-TableAKMigration(indata)
	OutMigrationAKBCA2010to2015<-TableAKMigration(outdata)

	##2005to2010
	data<-read_excel(
		"C:/Users/ehunsing/Desktop/PFDMigrationByAgeBySexBCA_v2020.xls", 
		range="2005-2010!A5:K656")
	indata<-data[,c(1:7)]
	outdata<-data[,c(1:4,9:11)]	
	InMigrationAKBCA2005to2010<-TableAKMigration(indata)
	OutMigrationAKBCA2005to2010<-TableAKMigration(outdata)

	##2000to2005
	data<-read_excel(
		"C:/Users/ehunsing/Desktop/PFDMigrationByAgeBySexBCA_v2020.xls", 
		range="2000-2005!A5:K656")
	indata<-data[,c(1:7)]
	outdata<-data[,c(1:4,9:11)]	
	InMigrationAKBCA2000to2005<-TableAKMigration(indata)
	OutMigrationAKBCA2000to2005<-TableAKMigration(outdata)

	##1995to2000
	data<-read_excel(
		"C:/Users/ehunsing/Desktop/PFDMigrationByAgeBySexBCA_v2020.xls", 
		range="1995-2000!A5:K656")
	indata<-data[,c(1:7)]
	outdata<-data[,c(1:4,9:11)]	
	InMigrationAKBCA1995to2000<-TableAKMigration(indata)
	OutMigrationAKBCA1995to2000<-TableAKMigration(outdata)

	##1990to1995
	data<-read_excel(
		"C:/Users/ehunsing/Desktop/PFDMigrationByAgeBySexBCA_v2020.xls", 
		range="1990-1995!A5:K656")
	indata<-data[,c(1:7)]
	outdata<-data[,c(1:4,9:11)]	
	InMigrationAKBCA1990to1995<-TableAKMigration(indata)
	OutMigrationAKBCA1990to1995<-TableAKMigration(outdata)

	##1985to1990
	data<-read_excel(
		"C:/Users/ehunsing/Desktop/PFDMigrationByAgeBySexBCA_v2020.xls", 
		range="1985-1990!A5:K656")
	indata<-data[,c(1:7)]
	outdata<-data[,c(1:4,9:11)]	
	InMigrationAK1985to1990<-TableAKMigration(indata)
	OutMigrationAK1985to1990<-TableAKMigration(outdata)

##Tables (complete) by period
	##2015to2020
	Migration2015to2020<-data.frame(
		FIPS=InMigrationAKBCA2015to2020$FIPS,
		AreaName=InMigrationAKBCA2015to2020$AreaName,
		AgeNumber=InMigrationAKBCA2015to2020$AgeNumber,	
		Age=InMigrationAKBCA2015to2020$Age,
		Total_In2015to2020=InMigrationAKBCA2015to2020$Total,
		Male_In2015to2020=InMigrationAKBCA2015to2020$Male,
		Female_In2015to2020=InMigrationAKBCA2015to2020$Female,
		Total_Out2015to2020=OutMigrationAKBCA2015to2020$Total,
		Male_Out2015to2020=OutMigrationAKBCA2015to2020$Male,
		Female_Out2015to2020=OutMigrationAKBCA2015to2020$Female)
		Migration2015to2020$Total_Net2015to2020=Migration2015to2020$Total_In2015to2020-Migration2015to2020$Total_Out2015to2020
		Migration2015to2020$Male_Net2015to2020=Migration2015to2020$Male_In2015to2020-Migration2015to2020$Male_Out2015to2020
		Migration2015to2020$Female_Net2015to2020=Migration2015to2020$Female_In2015to2020-Migration2015to2020$Female_Out2015to2020

	##2010to2015	
	Migration2010to2015<-data.frame(
		FIPS=InMigrationAKBCA2010to2015$FIPS,
		AreaName=InMigrationAKBCA2010to2015$AreaName,
		AgeNumber=InMigrationAKBCA2010to2015$AgeNumber,	
		Age=InMigrationAKBCA2010to2015$Age,
		Total_In2010to2015=InMigrationAKBCA2010to2015$Total,
		Male_In2010to2015=InMigrationAKBCA2010to2015$Male,
		Female_In2010to2015=InMigrationAKBCA2010to2015$Female,
		Total_Out2010to2015=OutMigrationAKBCA2010to2015$Total,
		Male_Out2010to2015=OutMigrationAKBCA2010to2015$Male,
		Female_Out2010to2015=OutMigrationAKBCA2010to2015$Female)
		Migration2010to2015$Total_Net2010to2015=Migration2010to2015$Total_In2010to2015-Migration2010to2015$Total_Out2010to2015
		Migration2010to2015$Male_Net2010to2015=Migration2010to2015$Male_In2010to2015-Migration2010to2015$Male_Out2010to2015
		Migration2010to2015$Female_Net2010to2015=Migration2010to2015$Female_In2010to2015-Migration2010to2015$Female_Out2010to2015
	
	##2005to2010
	Migration2005to2010<-data.frame(
		FIPS=InMigrationAKBCA2005to2010$FIPS,
		AreaName=InMigrationAKBCA2005to2010$AreaName,
		AgeNumber=InMigrationAKBCA2005to2010$AgeNumber,	
		Age=InMigrationAKBCA2005to2010$Age,
		Total_In2005to2010=InMigrationAKBCA2005to2010$Total,
		Male_In2005to2010=InMigrationAKBCA2005to2010$Male,
		Female_In2005to2010=InMigrationAKBCA2005to2010$Female,
		Total_Out2005to2010=OutMigrationAKBCA2005to2010$Total,
		Male_Out2005to2010=OutMigrationAKBCA2005to2010$Male,
		Female_Out2005to2010=OutMigrationAKBCA2005to2010$Female)
		Migration2005to2010$Total_Net2005to2010=Migration2005to2010$Total_In2005to2010-Migration2005to2010$Total_Out2005to2010
		Migration2005to2010$Male_Net2005to2010=Migration2005to2010$Male_In2005to2010-Migration2005to2010$Male_Out2005to2010
		Migration2005to2010$Female_Net2005to2010=Migration2005to2010$Female_In2005to2010-Migration2005to2010$Female_Out2005to2010

	##2000to2005
	Migration2000to2005<-data.frame(
		FIPS=InMigrationAKBCA2000to2005$FIPS,
		AreaName=InMigrationAKBCA2000to2005$AreaName,
		AgeNumber=InMigrationAKBCA2000to2005$AgeNumber,	
		Age=InMigrationAKBCA2000to2005$Age,
		Total_In2000to2005=InMigrationAKBCA2000to2005$Total,
		Male_In2000to2005=InMigrationAKBCA2000to2005$Male,
		Female_In2000to2005=InMigrationAKBCA2000to2005$Female,
		Total_Out2000to2005=OutMigrationAKBCA2000to2005$Total,
		Male_Out2000to2005=OutMigrationAKBCA2000to2005$Male,
		Female_Out2000to2005=OutMigrationAKBCA2000to2005$Female)
		Migration2000to2005$Total_Net2000to2005=Migration2000to2005$Total_In2000to2005-Migration2000to2005$Total_Out2000to2005
		Migration2000to2005$Male_Net2000to2005=Migration2000to2005$Male_In2000to2005-Migration2000to2005$Male_Out2000to2005
		Migration2000to2005$Female_Net2000to2005=Migration2000to2005$Female_In2000to2005-Migration2000to2005$Female_Out2000to2005
	
	##1995to2000
	Migration1995to2000<-data.frame(
		FIPS=InMigrationAKBCA1995to2000$FIPS,
		AreaName=InMigrationAKBCA1995to2000$AreaName,
		AgeNumber=InMigrationAKBCA1995to2000$AgeNumber,	
		Age=InMigrationAKBCA1995to2000$Age,
		Total_In1995to2000=InMigrationAKBCA1995to2000$Total,
		Male_In1995to2000=InMigrationAKBCA1995to2000$Male,
		Female_In1995to2000=InMigrationAKBCA1995to2000$Female,
		Total_Out1995to2000=OutMigrationAKBCA1995to2000$Total,
		Male_Out1995to2000=OutMigrationAKBCA1995to2000$Male,
		Female_Out1995to2000=OutMigrationAKBCA1995to2000$Female)
		Migration1995to2000$Total_Net1995to2000=Migration1995to2000$Total_In1995to2000-Migration1995to2000$Total_Out1995to2000
		Migration1995to2000$Male_Net1995to2000=Migration1995to2000$Male_In1995to2000-Migration1995to2000$Male_Out1995to2000
		Migration1995to2000$Female_Net1995to2000=Migration1995to2000$Female_In1995to2000-Migration1995to2000$Female_Out1995to2000

	##1990to1995
	Migration1990to1995<-data.frame(
		FIPS=InMigrationAKBCA1990to1995$FIPS,
		AreaName=InMigrationAKBCA1990to1995$AreaName,
		AgeNumber=InMigrationAKBCA1990to1995$AgeNumber,	
		Age=InMigrationAKBCA1990to1995$Age,
		Total_In1990to1995=InMigrationAKBCA1990to1995$Total,
		Male_In1990to1995=InMigrationAKBCA1990to1995$Male,
		Female_In1990to1995=InMigrationAKBCA1990to1995$Female,
		Total_Out1990to1995=OutMigrationAKBCA1990to1995$Total,
		Male_Out1990to1995=OutMigrationAKBCA1990to1995$Male,
		Female_Out1990to1995=OutMigrationAKBCA1990to1995$Female)
		Migration1990to1995$Total_Net1990to1995=Migration1990to1995$Total_In1990to1995-Migration1990to1995$Total_Out1990to1995
		Migration1990to1995$Male_Net1990to1995=Migration1990to1995$Male_In1990to1995-Migration1990to1995$Male_Out1990to1995
		Migration1990to1995$Female_Net1990to1995=Migration1990to1995$Female_In1990to1995-Migration1990to1995$Female_Out1990to1995

	##Table (complete) for 1985to1990 (statewide-only)
	Migration1985to1990<-data.frame(
		FIPS=InMigrationAK1985to1990$FIPS,
		AreaName=InMigrationAK1985to1990$AreaName,
		AgeNumber=InMigrationAK1985to1990$AgeNumber,	
		Age=InMigrationAK1985to1990$Age,
		Total_In1985to1990=InMigrationAK1985to1990$Total,
		Male_In1985to1990=InMigrationAK1985to1990$Male,
		Female_In1985to1990=InMigrationAK1985to1990$Female,
		Total_Out1985to1990=OutMigrationAK1985to1990$Total,
		Male_Out1985to1990=OutMigrationAK1985to1990$Male,
		Female_Out1985to1990=OutMigrationAK1985to1990$Female)
		Migration1985to1990$Total_Net1985to1990=Migration1985to1990$Total_In1985to1990-Migration1985to1990$Total_Out1985to1990
		Migration1985to1990$Male_Net1985to1990=Migration1985to1990$Male_In1985to1990-Migration1985to1990$Male_Out1985to1990
		Migration1985to1990$Female_Net1985to1990=Migration1985to1990$Female_In1985to1990-Migration1985to1990$Female_Out1985to1990
	
##Table with all years (1985to1990 is statewide-only)
Migration2010to2020<-merge(Migration2015to2020,Migration2010to2015,by=c("FIPS","AreaName","AgeNumber","Age"))
Migration2005to2020<-merge(Migration2010to2020,Migration2005to2010,by=c("FIPS","AreaName","AgeNumber","Age"))
Migration2000to2020<-merge(Migration2005to2020,Migration2000to2005,by=c("FIPS","AreaName","AgeNumber","Age"))
Migration1995to2020<-merge(Migration2000to2020,Migration1995to2000,by=c("FIPS","AreaName","AgeNumber","Age"))
Migration1990to2020<-merge(Migration1995to2020,Migration1990to1995,by=c("FIPS","AreaName","AgeNumber","Age"))
Migration1985to2020<-merge(Migration1990to2020,Migration1985to1990,by=c("FIPS","AreaName","AgeNumber","Age"),all.x=TRUE)
##Sort (order()) the variables and write table to csv file
Migration1985to2020<-data.frame(
	Migration1985to2020[order(
		Migration1985to2020$FIPS,
		Migration1985to2020$AreaName,
		Migration1985to2020$AgeNumber),])
write.table(Migration1985to2020,file="C:/Users/ehunsing/Desktop/PFDMigrationByAgeSexBCA1985to2020_v2020.csv", sep=",", row.names=FALSE)



