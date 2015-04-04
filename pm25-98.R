#setwd("M:/Coursera/Exploratory Data Analysis/pm25_data")
#
# From the week4 air Pollution Stdy Case, we are left with open questions:
#       
# Although less than 10% of the data is missing, is it important?
# Considering the extend of work placed into reporting, even missing data represent
# a large number of records (> 1.5 Million between 1998 and 2013), and there must be
# data worth mining in these, so I developed this script to attempt an answer at...
#
# What can be determined from PM25 missing data? Specifically between 1998-2013:
#
# 1) What is the trend globally in US and by state?
#    In absolute and relative amount, quantify and categorize Records, Missing, Geolocalized data.
# 
# 2) Regrouping the more than 50 different values assigned for Missing PM2.5 Null.Data.Code data
#    in 5 event groups: [D]amage, [L]imits Exceeded, [O]perator, [Q]uality and [N]on-controllable,
#    How is the spatial distribution of these events evolving in absolute and relative terms,
#    At the US states, county and geopositions levels?
#
# PM2.5 data files available at the US EPA web site
# address: http://www.epa.gov/ttn/airs/airsaqs/detaildata/downloadaqsdata.htm
# of type 1: RD_501_88101_YYYY-0.txt contain the PM2.5 data 
# Annual reports between 1999 and 2013 at the US EPA web site
# address: http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html#Annual
# of type 2: annual_all_YYYY.zip contain also the geolocalization per site. After extraction, we used
# as type 3: annual_all_YYYY.csv
# missing categories are provided in QualifierCodesNULL.csv provided also at US EPA web site
# address: https://aqs.epa.gov/aqsweb/codes/data/QualifierCodesNULL.csv
#
# Step 1: for each year between 1999 and 2013: 
#       read type 1 and type 3 files, combine info and generate basic statistics
#       retain ONLY missing data for this study
#       me a tidy data set
#
#install.packages("lubridate")
#install.packages("RCurl") # needed to handle Certified (SSL) URL
#install.packages("data.table") # for faster mergeing
#install.packages("ggmap")
#install.packages("mapproj")
#install.packages("rworldmap")
#
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RCurl)
library(data.table)
library(ggmap)
library(mapproj)
library(rworldmap)
#
download.flag<-FALSE    # TRUE for URL download, FALSE for Local data read
savefile.flag<-TRUE    # TRUE for local save (in case of power failures...)
#
url1A<-"http://www.epa.gov/ttn/airs/airsaqs/detaildata/501files/Rd_501_88101_YYYY.Zip"
url1B<-"http://www.epa.gov/ttn/airs/airsaqs/detaildata/501files/RD_501_88101_YYYY.zip"
url2<-"http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/annual_all_YYYY.zip"
url3<-"https://aqs.epa.gov/aqsweb/codes/data/QualifierCodesNULL.csv"
url4<-"http://www2.census.gov/geo/docs/reference/state.txt"
url5A<-"http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt"
url5B<-"http://www2.census.gov/geo/docs/reference/codes/files/st60_as_cou.txt"
url5C<-"http://www2.census.gov/geo/docs/reference/codes/files/st66_gu_cou.txt"
url5D<-"http://www2.census.gov/geo/docs/reference/codes/files/st69_mp_cou.txt"
url5E<-"http://www2.census.gov/geo/docs/reference/codes/files/st72_pr_cou.txt"
url5F<-"http://www2.census.gov/geo/docs/reference/codes/files/st74_um_cou.txt"
url5G<-"http://www2.census.gov/geo/docs/reference/codes/files/st78_vi_cou.txt"
#
assemble <- function (x) {
        year.analyzed<-x$a 
        year.statistics<-vector() # initialize a vector to return the year's dfstat data
        if(download.flag==TRUE){datafile1<-paste0("RD_501_88101_",as.character(year.analyzed),"-0.zip",sep='')
                                if(y<=2007){url<-url1A}else{url<-url1B}
                                url<-gsub("YYYY",as.character(year.analyzed),url)
                                download.file(url, dest=datafile1, mode="wb")
                                unzip (datafile1)       # unzip creates and populates the data structure 
                                unlink(datafile1)
        }
        datafile1<-paste0("RD_501_88101_",as.character(year.analyzed),"-0.txt",sep='')
        pm<-read.table(datafile1, comment.char = "#", header = FALSE, sep = "|", na.strings = "")
        cnames<-readLines(datafile1, 1)
        cnames<-strsplit(cnames, "|", fixed = TRUE)
        names(pm)<-make.names(cnames[[1]])
        # populate year statistics vector with year, records, missing... 
        year.statistics<-c(year.analyzed,nrow(pm),sum(is.na(pm$Sample.Value)))
        pm<-subset(pm,is.na(pm$Sample.Value))   # retain only the missing Sample.Value data subset
        # trim RD, Action Code, Parameter Code==88101, Start.Time and Sample Value==NA entries 
        pm<-pm[,-c(1,2,6,13)]
        # cast Date to date format and substitute Start.Time with Year variable to facilitate merging later...
        pm$Date<-parse_date_time(paste(pm$Date,pm$Start.Time,sep=" "),"%Y%m%d %H:%M")
        colnames(pm)[9]<-"Year"
        pm$Year<-year.analyzed
        # retain only entries that have full localization data...
        # must have complete State.Code, County.Code and Site.ID
        pm<-pm[complete.cases(pm[,1:3]),]
        pm<-merge(pm,x$b, by="Null.Data.Code")       # all missing events are now categorized
        pm$key<-paste(as.character(pm$State.Code),as.character(pm$County.Code),as.character(pm$Site.ID),sep=":")
        # and keyed
        # read in Annual data to extract geographic positions
        if(download.flag==TRUE){datafile2<-paste0("annual_all_",as.character(year.analyzed),".zip",sep='')
                                url<-gsub("YYYY",as.character(year.analyzed),url2)
                                download.file(url, dest=datafile2, mode="wb")
                                unzip (datafile2)       # unzip creates and populates the data structure 
                                unlink(datafile2)
        }
        datafile2<-paste0("annual_all_",as.character(year.analyzed),".csv",sep='')
        df<-read.csv(datafile2,stringsAsFactors=FALSE) 
        df<-df[which(df$Parameter.Code==88101),]        # subset on PM2.5 only
        df<-df[,c(1:3,6,7)]                     # only retain State,County and Site.Num, Latitude and Longitude...
        df<-df[complete.cases(df[,1:3]),]
        df<-unique(df)                          # retain only unique entries to merge
        # build a key and remove State.Code, County.Code, Site.Num since...
        df$key<-paste(as.character(df$State.Code),as.character(df$County.Code),as.character(df$Site.Num),sep=":")
        df<-df[-c(1:3)] 
        #
        # most likely, some State.Code fields were populated with a State.Abr code...
        # it would be interesting to verify this hypothesis... on the to-do list!
        #
        suppressWarnings(pm$State.Code<-as.integer(pm$State.Code))        # this will flag a few additional invalid localization
        pm<-subset(pm,!is.na(pm$State.Code))            # retain only non-missing State.Codes in pm data subset
        # do not use all=TRUE since we only want to retain the sites reported in pm subset        
        localized<-merge(pm,df,by="key")                        # ... subset on localized factor,
        pm$Geolocalized<-factor(pm$key %in% localized$key)      # create a Geolocalized factor variable
        pm<-merge(pm,df,by="key")                       # so pm can easily be subset later for display  
        pm<-pm[,-1]                                     # drop the key we do not need anymore
        # now add the State and County info to the pm25 data frame
        pm$State.Code<-sapply(pm$State.Code, updateStateCode)
        pm$key<-paste(pm$State.Code,pm$County.Code,sep=":")     # build a key
        pm<-pm[,-c(2,3)]                                # drop State.Code and County.Code
        pm<-merge(pm,x$c,by="key")                      # likely will drop some records due to State.Code 7...(Panama Canal)
        year.statistics<-c(year.statistics,nrow(pm))    # ...and report clean localized count in year.statistics
        pm<-pm[,-1]                                     # drop the key we do not need anymore
        x$a<-pm;x$b<-year.statistics                    # rebuild the list and return it
        x
}
#
# helper function for text conversion when calling maps...
#
simpleCap <- function(x) {
        s <- tolower(x)
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
}
#
# helper to update State.Code to match current FIPS
updateStateCode <- function(x) {
        x[x==3]<-60         # American Samoa
        x[x==14]<-66        # Guam
        x[x==52]<-78        # US Virgin Islands
        x[x==43]<-72        # Puerto Rico
        x[x==7]<-NA         # Panama Canal
        x
}
#
# helper to strip 'county' ending of the County.Name
stripCounty <- function(x) {
        x<-gsub(" County","",x)
}
#
# start with building the Qualifiers data frame
#
code.D<-c('AC','AD','AJ','AK','AN','BA','BE','BI','BK','FI','MC','SC')
code.L<-c('AA','AE','AG','AH','BN','BR','DL','TS')
code.N<-c('AO','AP','AV','AW','SA')
code.O<-c('AB','AF','AL','AM','AQ','AR','AU','BB','BG','BH','BJ')
code.Q<-c('AI','AS','AT','AX','AY','AZ','BC','BD','BF','BL','BM','CS','DA','MB','ST','TC','XX')
codes<-c(code.D,code.L,code.N,code.O,code.Q)
type<-c(rep('Damage Causes',length(code.D)),
        rep('Limits Exceeded',length(code.L)),
        rep('Non-Controllable',length(code.N)),
        rep('Operators',length(code.O)),
        rep('Quality Related',length(code.Q)))
q.group<-cbind(codes,type)
# retrieve info using url or locally
filename<-"QualifierCodesNULL.csv"
if(download.flag==TRUE){download.file(url3, dest=filename, mode="wb")}
Qualifiers<-readLines(filename,2)
Qualifiers<-Qualifiers[-1]        # get rid of the title line
Qualifiers <- strsplit(Qualifiers, ",", fixed = TRUE)
null.code <- read.table(filename, skip=2, header = FALSE, sep = ",", na.strings = "",stringsAsFactors=FALSE)
names(null.code) <- make.names(Qualifiers[[1]])
null.code<-null.code[,1:2]      # drop what we don't need
colnames(q.group)[1]<-colnames(null.code)[1]
colnames(q.group)[2]<-'Event.Type'
Qualifiers<-merge(q.group,null.code)
colnames(Qualifiers)[1]<-'Null.Data.Code'
rm(null.code,q.group,codes,type,code.D,code.L,code.N,code.O,code.Q) 
#
# we can retrieve the state and county data from the Census Bureau
#
# retrieve info using url or locally
filename<-"state.txt"
if(download.flag==TRUE){download.file(url4, dest=filename, mode="wb")}
States.Info<-read.table(filename, header = TRUE, sep = "|", na.strings = "")
States.Info<-States.Info[,-4]
colnames(States.Info)<-c("State.Code","State.Abr","State.Name")
#
# note the quote excludes sigle quotes a delimiter which can pose problems in some names...
#
filename<-"national_county.txt"
if(download.flag==TRUE){download.file(url5A, dest=filename, mode="wb")}
suppressWarnings(County.Info<-read.table(filename, quote="\"",sep = ","))
filename<-"st60_as_cou.txt"
if(download.flag==TRUE){download.file(url5B, dest=filename, mode="wb")}
appendix<-read.table(filename, quote="\"",sep = ",")
suppressWarnings(County.Info<-rbind(County.Info,appendix))
filename<-"st66_gu_cou.txt"
if(download.flag==TRUE){download.file(url5C, dest=filename, mode="wb")}
suppressWarnings(appendix<-read.table(filename, quote="\"",sep = ","))
County.Info<-rbind(County.Info,appendix)
filename<-"st69_mp_cou.txt"
if(download.flag==TRUE){download.file(url5D, dest=filename, mode="wb")}
suppressWarnings(appendix<-read.table(filename, quote="\"",sep = ","))
County.Info<-rbind(County.Info,appendix)
filename<-"st72_pr_cou.txt"
if(download.flag==TRUE){download.file(url5E, dest=filename, mode="wb")}
suppressWarnings(appendix<-read.table(filename, quote="\"",sep = ","))
County.Info<-rbind(County.Info,appendix)
filename<-"st74_um_cou.txt"
if(download.flag==TRUE){download.file(url5F, dest=filename, mode="wb")}
suppressWarnings(appendix<-read.table(filename, quote="\"",sep = ","))
County.Info<-rbind(County.Info,appendix)
filename<-"st78_vi_cou.txt"
if(download.flag==TRUE){download.file(url5G, dest=filename, mode="wb")}
suppressWarnings(appendix<-read.table(filename, quote="\"",sep = ","))
County.Info<-rbind(County.Info,appendix)
# trimoff what is redundant
County.Info<-County.Info[,c(1,3,4)]       # retain State.Abr, County.Code and County.Name
colnames(County.Info)<-c("State.Abr","County.Code","County.Name")
# now merge with States.Info
County.Info<-merge(County.Info,States.Info,by="State.Abr")
# now make a key for mergeing
County.Info<-mutate(County.Info,key=paste(State.Code,County.Code,sep=":"))
# trim the County.Name to bare name
County.Info$County.Name<-sapply(County.Info$County.Name,stripCounty)
#
rm(appendix,filename,url4,url5A,url5B,url5C,url5D,url5E,url5F,url5G)    # cleanup
#
# now initialize data frame to collect missing data from pm data files
#
a<-data.frame()
b<-data.frame()
c<-data.frame()
dfstat<-data.frame(Year=1998,Records=0,Missing=0,Geolocalized=0)
x<-list(a,b,c)
#
# populate (iteratively)
#
for(y in 1998:2013) {print(y);x$a<-y;x$b<-Qualifiers;x$c<-County.Info;x<-assemble(x)
                     if (y==1998){pm25<-as.data.frame(x$a);dfstat[1,]<-x$b} else{pm25<-rbind(pm25,x$a);dfstat<-rbind(dfstat,x$b)}
}
#
rm(a,b,c,x,y,url1A,url1B,url2,url3) # cleanup and save this data for now
County.Info<-County.Info[,-6]   # drop the key
#
# pm25 is fully Geolocalized to allow mapping down to the US county level
#
if (savefile.flag==TRUE){write.csv(Qualifiers,"qualifiers.csv",row.names=FALSE)
                         write.csv(States.Info,"statesinfo.csv",row.names=FALSE)
                         write.csv(County.Info,"countyinfo.csv",row.names=FALSE)
                         write.table(dfstat,"dfstat.dat",row.names=FALSE)
                         write.table(pm25,"pm25.dat",row.names=FALSE)
}
#
# pm25<-read.table(file="pm25.dat",header=TRUE,stringsAsFactors=FALSE)  # in case of power outage...
#
#
pm25<-pm25[,-c(2:6,10:22)]  # trim what we won't use for now
#
# Begin plots
#
myPNGfile<-"plot0.png"
png(filename=myPNGfile,width=480,height=480) ## open png device for plot0.png 480x480 pix
ggplot (dfstat,
        aes(Year,Records*1e-6))+
        geom_bar(stat="identity",
                 color="blue",fill="blue")+
        stat_smooth(data=dfstat[which(dfstat$Year>=2008),],
                    aes(x=Year,y=Records*1e-6,group=1),
                    fill="blue",
                    color="orange",
                    size=1,
                    method=lm,
                    se=FALSE)+
        labs(title="US Sites Records (in Millions) reporting missing PM2.5 data \n with Linear Trendline since Y1998")+
        xlab("Year")+
        ylab("Millions of Sites Records reporting missing PM2.5 data")
dev.off() # close png device
#
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot0.png 6116 FALSE  666 2015-04-03 22:18:30 2015-04-03 21:30:14 2015-04-03 21:30:14  no
#
dfstat2<-mutate(dfstat,PM2.5.Reported=Records-Missing,Non.Mappable=Missing-Geolocalized)
dfstat2A<-select(dfstat2,Year,PM2.5.Reported,Missing)   # for Missing vs Non-Missing plots
dfstat3<-gather(dfstat2A,"Type","Count",2,3)
dfstat3<-group_by(dfstat3,Year)
dfstat3<-as.data.frame(dfstat3)
#
myPNGfile<-"plot1A.png"
png(filename=myPNGfile,width=480,height=480) ## open png device for plot1A.png 480x480 pix
ggplot (data=dfstat3,
        aes(x=Year,y=Count/1e6,fill=Type))+
        geom_bar(stat="identity")+
        stat_smooth(data=dfstat3[which(dfstat3$Year>=2008),],
                    aes(x=Year,y=Count/1e6),
                    color="Black",
                    size=1,
                    method=lm,
                    se=FALSE)+
        facet_wrap (~ Type,ncol=1)+
        labs(title="Annual Count of US PM2.5 Data Reports - US\n with Linear Trendline since Y1998")+
        xlab("Year")+
        ylab("Millions of Annual PM2.5 Data Reported")

dev.off() # close png device
#
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot1A.png 7455 FALSE  666 2015-04-03 22:18:31 2015-04-03 21:30:14 2015-04-03 21:30:14  no
#
dfstat2B<-select(dfstat2,Year,Geolocalized,Non.Mappable) # for Geolocalized vs Non.Mappable
dfstat3<-gather(dfstat2B,"Type","Count",2,3)
dfstat3<-group_by(dfstat3,Year)
dfstat3<-as.data.frame(dfstat3)
#
myPNGfile<-"plot1B.png"
png(filename=myPNGfile,width=480,height=480) ## open png device for plot1B.png 480x480 pix
ggplot (data=dfstat3,
        aes(x=Year,y=Count/1e6,fill=Type))+
        geom_bar(stat="identity")+
        stat_smooth(data=dfstat3[which(dfstat3$Year>=2008),],
                    aes(x=Year,y=Count/1e6),
                    color="Black",
                    size=1,
                    method=lm,
                    se=FALSE)+
        facet_wrap (~ Type,ncol=1)+
        labs(title="Annual Count of US Missing PM2.5 Data Reports - US\n with Linear Trendline since Y1998")+
        xlab("Year")+
        ylab("Millions of Annual PM2.5 Missing Data Reported")

dev.off() # close png device
#
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot1B.png 7974 FALSE  666 2015-04-03 22:18:32 2015-04-03 21:37:05 2015-04-03 21:37:05  no
#
dfstat4<-mutate(dfstat,Missing.Ratio=Missing/Records,Geolocalized.Ratio=Geolocalized/Records)
dfstat4<-dfstat4[,-(2:4)]
dfstat5<-gather(dfstat4,"Ratios","Percentages",2,3)
dfstat5<-group_by(dfstat5,Year)
dfstat5<-as.data.frame(dfstat5)
myPNGfile<-"plot2.png"
png(filename=myPNGfile,width=480,height=480) ## open png device for plot2.png 480x480 pix
ggplot (dfstat5,
        aes(x=Year,y=Percentages*1e2,fill=Ratios))+
        geom_bar(stat="identity")+
        geom_hline(aes(yintercept=1e2*(sum(dfstat$Missing)/sum(dfstat$Records))))+
        facet_wrap(~Ratios,ncol=1)+
        labs(title="Missing PM2.5 Data Records % - US\nwith Average Missing % Level since Y1998")+
        xlab("Year")+
        ylab("% of US Missing PM2.5 Data Records")
        
dev.off() # close png device
#
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot2.png 7160 FALSE  666 2015-04-03 22:18:33 2015-04-03 21:30:14 2015-04-03 21:30:14  no
#
rm(dfstat2A,dfstat2B,dfstat3,dfstat4,dfstat5)
#
dfstat2<-select(pm25,Year,Event.Type,State.Name,County.Name)
by_year_Event.Type<-group_by(dfstat2,Year,Event.Type)
dfstat3<-summarize(by_year_Event.Type,Events=n())
dfstat3<-as.data.frame(dfstat3)
myPNGfile<-"plot2A.png"
png(filename=myPNGfile,width=480,height=480) ## open png device for plot2A.png 480x480 pix
ggplot (dfstat3,
        aes(x=Year,y=Events/1e3))+
        geom_bar(stat="identity",
                 color="blue",fill="blue")+
        stat_smooth(data=dfstat3[which(dfstat3$Year>=2008),],
                    aes(x=Year,y=Events/1e3,group=1),
                    fill="blue",
                    color="orange",
                    size=1,
                    method=lm,
                    se=FALSE)+
        facet_wrap(~Event.Type)+
        labs(title="Geolocalized US Sites (in Thousandss) Reporting Missing PM2.5 Data \nCategorized by Event Type\n with Linear Trendline since Y2008")+
        xlab("Year")+
        ylab("Thousands of US Geolocalized Sites Reporting Missing PM2.5 Data")
dev.off() # close png device
#
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot2A.png 9302 FALSE  666 2015-04-03 22:18:34 2015-04-03 21:30:14 2015-04-03 21:30:14  no
#
dfstat2<-select(pm25,Year,Event.Type,State.Name,County.Name,Longitude,Latitude)
by_year_Event.Type<-group_by(dfstat2,Year,Event.Type,State.Name,County.Name,Longitude,Latitude)
dfstat3<-summarize(by_year_Event.Type,Events=n())
dfstat3<-as.data.frame(dfstat3)
dfstat3<-mutate(dfstat3,Event.Magnitude=round(1.5+log10(Events)))
##
## we want to chart the coordinates on the map after summation and use
## a color chart to indicate density
myPNGfile<-"plot3A.png"
png(filename=myPNGfile,width=2560,height=2560)        # open png device for plot3A.png 2560x2560 pix
USAmap<-get_map('United States',zoom=4,maptype="toner-lite")
ggmap(USAmap,extent='device',color='bw')+
        scale_color_manual(values=c("dark blue","orange","red","dark green","light blue"))+
        geom_point(data=dfstat3, aes(x=Longitude, y=Latitude, color=Event.Type, size=Event.Magnitude, alpha=0.2))+
        facet_wrap(~Year)+
        labs(title="US Locations of Missing PM_25 Emission Records 1998-2013\nAll Missing Data Codes Reported")
dev.off() # close png device
##
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot3A.png 942534 FALSE  666 2015-04-03 22:18:50 2015-04-03 21:30:14 2015-04-03 21:30:14  no
#
by_Event.Type<-group_by(dfstat2,Event.Type,State.Name)
dfstat4<-summarize(by_Event.Type,Events=n())
dfstat4<-as.data.frame(dfstat4)
colnames(dfstat4)<-c("Type","state","Count")
dfstat4<-mutate(dfstat4,logCount=log10(Count))
dfstat4$state = tolower(dfstat4$state)
myPNGfile<-"plot3B.png"
png(filename=myPNGfile,width=1920,height=1280)        # open png device for plot3B.png 1920x1280 pix
if (require(maps)) {
        states_map<-map_data("state")
        ggplot(dfstat4,aes(map_id=state))+
                geom_map(aes(fill=0),map=states_map)+expand_limits(x=states_map$long,y=states_map$lat)
        last_plot()+coord_map()
        ggplot(dfstat4,aes(map_id=state))+geom_map(aes(fill=logCount),map=states_map)+expand_limits(x=states_map$long,y=states_map$lat)+
        facet_wrap(~ Type)+
        labs(title="US Locations of Missing PM_25 Emission Records 1998-2013\nAll Missing Data Codes Reported") 
}
dev.off() # close png device
##
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot3B.png 70513 FALSE  666 2015-04-03 22:18:53 2015-04-03 21:30:13 2015-04-03 21:30:13  no
#
by_Event.Type2<-group_by(dfstat2,Event.Type,State.Name,County.Name)
dfstat6<-summarize(by_Event.Type2,Events=n())
dfstat6<-as.data.frame(dfstat6)
colnames(dfstat6)<-c("Type","state","county","Count")
dfstat6$state = tolower(dfstat6$state)
dfstat6$county=tolower(dfstat6$county)
dfstat6<-mutate(dfstat6,logCount=log10(Count))
myPNGfile<-"plot3C.png"
png(filename=myPNGfile,width=1920,height=1280)        # open png device for plot3B.png 1920x1280 pix
if (require(maps)) {
        map.county<-map_data('county')
        counties<-unique(map.county[,5:6])
        map.county<-data.table(map_data('county'))
        setkey(map.county,region,subregion)
        my_map<-data.frame(region=counties$region,
                           subregion=counties$subregion)
        dfstat7<-rename(dfstat6,region=state,subregion=county)
        dfstat7<-data.table(dfstat7)
        setkey(dfstat7,region,subregion)
        my_map<-data.table(my_map)
        my_map<-left_join(my_map,dfstat7,by=c("region","subregion"))
        my_map<-my_map[!is.na(my_map$Type),]
        setkey(my_map,region,subregion)
        my_map<-left_join(map.county,my_map,by=c("region","subregion"))
        my_map$Count[is.na(my_map$Count)]<-0
        my_map$logCount[is.na(my_map$logCount)]<-0
        levels(my_map$Type)<-append(levels(my_map$Type),"None Missing")
        my_map$Type[is.na(my_map$Type)]<-"None Missing"
        states_map<-map_data("county")
        ggplot(states_map,aes(x=long,y=lat,group=group,fill=logCount))+geom_polygon()
        last_plot()+coord_map()
        ggplot(my_map,aes(x=long,y=lat,group=group,fill=logCount))+geom_polygon()+
        facet_wrap(~ Type)+
        labs(title="US Locations of Missing PM_25 Emission Records 1998-2013\nAll Missing Data Codes Reported") 
}
dev.off() # close png device
#
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot3C.png 149826 FALSE  666 2015-04-03 22:19:00 2015-04-03 21:29:59 2015-04-03 21:29:59  no
#
dfstat6<-dfstat6[,-5]
by_state_Type<-group_by(dfstat6,state,Type)
dfstat6<-summarize(by_state_Type,StateCount=sum(Count))
dfstat6$PercentCount<-100*dfstat6$StateCount/sum(dfstat6$StateCount)
myPNGfile<-"plot3D.png"
png(filename=myPNGfile,width=1920,height=1280)        # open png device for plot3B.png 1920x1280 pix
if (require(maps)) {
        states_map<-map_data("state")
        ggplot(dfstat6,aes(map_id=state))+
                geom_map(aes(fill=0),map=states_map)+expand_limits(x=states_map$long,y=states_map$lat)
        last_plot()+coord_map()
        ggplot(dfstat6,aes(map_id=state))+geom_map(aes(fill=PercentCount),map=states_map)+expand_limits(x=states_map$long,y=states_map$lat)+
                facet_wrap(~ Type)+
                labs(title="PM_25 Emission Records 1998-2013 US Locations Categorized by State\nPercentages of All Data Reported") 
}
dev.off() # close png device
##
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot3D.png 51629 FALSE  666 2015-04-03 22:19:04 2015-04-03 21:30:14 2015-04-03 21:30:14  no
#
dfstat8<-data.frame(dfstat7)
dfstat8<-dfstat8[,-5]
dfstat8$PercentCount<-100*dfstat8$Count/sum(dfstat8$Count)
myPNGfile<-"plot3E.png"
png(filename=myPNGfile,width=1920,height=1280)        # open png device for plot3B.png 1920x1280 pix
if (require(maps)) {
        map.county<-map_data('county')
        counties<-unique(map.county[,5:6])
        map.county<-data.table(map_data('county'))
        setkey(map.county,region,subregion)
        my_map<-data.frame(region=counties$region,
                           subregion=counties$subregion)
        my_map<-data.table(my_map)
        dfstat8<-data.table(dfstat8)
        my_map<-left_join(my_map,dfstat8,by=c("region","subregion"))
        my_map<-my_map[!is.na(my_map$Type),]
        setkey(my_map,region,subregion)
        my_map<-left_join(map.county,my_map,by=c("region","subregion"))
        my_map$Count[is.na(my_map$Count)]<-0
        my_map$PercenCount[is.na(my_map$PercentCount)]<-0
        levels(my_map$Type)<-append(levels(my_map$Type),"None Missing")
        my_map$Type[is.na(my_map$Type)]<-"None Missing"
        states_map<-map_data("county")
        ggplot(states_map,aes(x=long,y=lat,group=group,fill=PercentCount))+geom_polygon()
        last_plot()+coord_map()
        ggplot(my_map,aes(x=long,y=lat,group=group,fill=PercentCount))+geom_polygon()+
                facet_wrap(~ Type)+
                labs(title="PM_25 Emission Records 1998-2013 US Locations Categorized by County\nPercentages of All Data Reported") 
}
dev.off() # close png device
#
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot3E.png 106318 FALSE  666 2015-04-03 22:19:10 2015-04-03 21:30:14 2015-04-03 21:30:14  no
#
myPNGfile<-"plot3.png"
png(filename=myPNGfile,width=1280,height=1280)        # open png device for plot3.png 480x480 pix
USAmap<-get_map('Ohio',zoom=8,maptype="toner-lite")
ggmap(USAmap,extent='device',color='bw')+
        scale_color_manual(values=c("dark blue","orange","red","dark green","light blue"))+
        geom_point(data=dfstat3, aes(x=Longitude, y=Latitude, color=Event.Type, size=Event.Magnitude))+
        facet_wrap(~Year)+
        labs(title="Ohio Locations of Missing PM_25 Emission Records 1998-2013\nAll Missing Data Codes Reported")
dev.off() # close png device
##
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot3.png 250576 FALSE  666 2015-04-03 22:19:20 2015-04-03 21:36:57 2015-04-03 21:36:57  no
#
myPNGfile<-"plot4.png"
png(filename=myPNGfile,width=1280,height=1280)        # open png device for plot3.png 480x480 pix
USAmap<-get_map('Ohio',zoom=9,maptype="toner-hybrid")
ggmap(USAmap,extent='device',color='bw')+
        scale_color_manual(values=c("dark blue","orange","red","dark green","light blue"))+
        geom_point(data=dfstat3, aes(x=Longitude, y=Latitude, color=Event.Type, size=Event.Magnitude))+
        facet_wrap(~Year)+
        labs(title="Ohio Locations of Missing PM_25 Emission Records 1998-2013\nAll Missing Data Codes Reported")
dev.off() # close png device
##
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot4.png 202974 FALSE  666 2015-04-03 22:19:44 2015-04-03 21:36:57 2015-04-03 21:36:57  no
#
myPNGfile<-"plot4A.png"
png(filename=myPNGfile,width=1280,height=1280)        # open png device for plot3.png 480x480 pix
USAmap<-get_map('Baltimore, MD',zoom=12,maptype="toner-hybrid")
ggmap(USAmap,extent='device',color='bw')+
        scale_color_manual(values=c("dark blue","orange","red","dark green","light blue"))+
        geom_point(data=dfstat3, aes(x=Longitude, y=Latitude, color=Event.Type, size=Event.Magnitude))+
        facet_wrap(~Year)+
        labs(title="Baltimore, MD Locations of Missing PM_25 Emission Records 1998-2013\nAll Missing Data Codes Reported")
dev.off() # close png device
##
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot4A.png 485956 FALSE  666 2015-04-03 22:20:07 2015-04-03 21:36:57 2015-04-03 21:36:57  no

#
