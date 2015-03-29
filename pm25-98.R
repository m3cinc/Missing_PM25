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
# 2) How is the spatial distribution of these events evolving in absolute and relative terms?
# 3) As more than 50 different values are assigned in the Null.Data.Code, can they be regrouped
#    in 5 event groups: Limits Exceeded, Quality ,Damage ,Operator and Uncontrollable?
# 3) How is the spatial distribution of these events evolving in absolute and
#    relative terms?
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
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
#
assemble <- function (x) {
        year <- x$a
        y <- x$b
        z <- x$c
        datafile1 <- paste0("RD_501_88101_",year,"-0.txt",sep='')
        datafile2 <- paste0("annual_all_",year,".csv",sep='')
        year <- as.numeric(year)
        pm <- read.table(datafile1, comment.char = "#", header = FALSE, sep = "|", na.strings = "")
        cnames <- readLines(datafile1, 1)
        cnames <- strsplit(cnames, "|", fixed = TRUE)
        names(pm) <- make.names(cnames[[1]])
        y <- c(year,sum(is.na(pm$Sample.Value)),nrow(pm))
        pm <- unique(pm)                # eliminate duplicates if any
        pm <- subset(pm,is.na(pm$Sample.Value))   # only the missing Sample.Value data
        pm <- pm[,c(3:5,11,14)]        # trim what we won't need
        pm <- merge(pm,z)
        pm <- pm[complete.cases(pm[,1:3]),]
        pm$Date<-ymd(pm$Date)
        pm$Year<-year
        pm <- unique(pm)
        # read in Annual data to extract geographic positions
        df <- read.csv(datafile2,stringsAsFactors=FALSE) 
        df <- df[which(df$Parameter.Code==88101),]        # subset on PM2.5 only
        df <- df[,c(1:3,6,7)]                     # only retain State,County and Site.Num, Latitude and Longitude...
        df <- unique(df)                          # retain only unique ...
        # add year info to df data
        df$year<-year
        df < -df[complete.cases(df[,1:3]),]
        df[,1] <- as.integer(df[,1])
        df <- df[complete.cases(df[,1:3]),]
        pm <- merge(pm,df)        # do not use all=TRUE since we only want to retain the relevant sites
        pm <- unique(pm)
        x$b <- y
        x$c <- pm[]
        x
}
#
# Retrieve Null.Data.Code events from QualifierCodesNULL.csv file, grouped as:
# 
#       D: Damage
#       L: Limits Exceeded
#       Q: Quality
#       N: Non-controllable
#       O: Operator
#
# return the Qualifiers data frame
#
code.D<-c('AC','AD','AJ','AK','AN','BA','BE','BI','BK','FI','MC','SC')
code.L<-c('AA','AE','AG','AH','BN','BR','DL','TS')
code.N<-c('AO','AP','AV','AW','SA')
code.O<-c('AB','AF','AL','AM','AQ','AR','AU','BB','BG','BH','BJ')
code.Q<-c('AI','AS','AT','AX','AY','AZ','BC','BD','BF','BL','BM','CS','DA','MB','ST','TC','XX')
codes<-c(code.D,code.L,code.N,code.O,code.Q)
type<-c(rep('D',length(code.D)),
        rep('L',length(code.L)),
        rep('N',length(code.N)),
        rep('O',length(code.O)),
        rep('Q',length(code.Q)))
q.group<-cbind(codes,type)
Qualifiers<-readLines("QualifierCodesNULL.csv",2)
Qualifiers<-Qualifiers[-1]        # get rid of the title line
Qualifiers <- strsplit(Qualifiers, ",", fixed = TRUE)
null.code <- read.table("QualifierCodesNULL.csv", skip=2, header = FALSE, sep = ",", na.strings = "",stringsAsFactors=FALSE)
names(null.code) <- make.names(Qualifiers[[1]])
null.code<-null.code[,1:2]      # drop what we don't need
colnames(q.group)[1]<-colnames(null.code)[1]
colnames(q.group)[2]<-'Event.Type'
Qualifiers<-merge(q.group,null.code)
colnames(Qualifiers)[1]<-'Null.Data.Code'
rm(null.code,q.group,codes,type,code.D,code.L,code.N,code.O,code.Q)     # cleanup
#
# now initialize data frame to collect missing data from pm data files
#
pm25<-data.frame()      # start with a clean data.frame to accumulate all data
dfstat<-data.frame()
result<-list()
x<-list()
#
# populate (iteratively)
#
xs<-1998:2013
for(year in xs) {print(year)
                x$a<-as.character(year)
                x$b<-dfstat[]
                x$c<-Qualifiers[]
                result<-assemble(x)
                if (year==1998) {dfstat<-result$b
                                pm25<-result$c
                                }
                else    {pm25<-rbind(pm25,result$c)
                        dfstat<-rbind(dfstat,result$b)
                        }
}
colnames(dfstat)<-c("year","missing","records")
# recast as needed         
pm25$State.Code<-as.integer(pm25$State.Code)
# save this data for now
write.csv(Qualifiers,"qualifiers.csv",row.names=FALSE)
write.table(dfstat,"dfstat.dat",row.names=FALSE)
write.table(pm25,"pm25.dat",row.names=FALSE)
#
# cleanup
rm(year,xs,x,result)
#
# now, let's place questionable data for which site.ID!=Site.Num into a Special Event.Type S
# and replace their Qualifier.Desc to describe inconsistency...
#
levels(pm25$Event.Type)<-c(levels(pm25$Event.Type),"S") # create additional factor level
pm25[which(pm25$Site.ID!=pm25$Site.Num),6]<-"S"
pm25[which(pm25$Site.ID!=pm25$Site.Num),7]<-"Inconsistent Data Site Identification"
#
# determine inconsistent records per year and add to dfstat
#
pm<-filter(pm25,Event.Type=="S")
pm<-select(pm,State.Code,year,Longitude,Latitude)
inconsistent<-group_by(pm,year)
inconsistent<-summarize(inconsistent,count=n())
merge(dfstat,inconsistent)





stop()
myPNGfile<-"plot1.png"
png(filename=myPNGfile,width=480,height=480) ## open png device for plot1.png 480x480 pix
ggplot (dfstat,
        aes(year,y=(missing/records)*1e2))+
        geom_bar(stat="identity",
                 color="blue",fill="blue")+
        stat_smooth(data=dfstat,
                    aes(x=year,y=(missing/records)*1e2,group=1),
                    fill="blue",
                    color="orange",
                    size=1,
                    method=lm,
                    se=FALSE)+
        labs(title="Annual % of missing PM2.5 data in EPA reports - US\n with Linear Trendline since Y1998")+
        xlab("Year")+
        ylab("% of missing data in EPA PM2.5 annual reports")
dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot1.png 5420 FALSE  666 2015-03-28 21:26:04 2015-03-27 21:32:28 2015-03-27 21:32:28  no
#
myPNGfile<-"plot2.png"
png(filename=myPNGfile,width=480,height=480) ## open png device for plot1.png 480x480 pix
ggplot (dfstat,
        aes(year,records*1e-6))+
        geom_bar(stat="identity",
                 color="blue",fill="blue")+
        stat_smooth(data=dfstat[which(dfstat$year>=2008),],
                    aes(x=year,y=records*1e-6,group=1),
                    fill="blue",
                    color="orange",
                    size=1,
                    method=lm,
                    se=FALSE)+
        labs(title="US Sites Records (in Millions) reporting missing PM2.5 data \n in Annual EPA reports\n with Linear Trendline since Y1998")+
        xlab("Year")+
        ylab("Millions of Sites Records reporting missing PM2.5 data")
dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot2.png 6378 FALSE  666 2015-03-28 21:39:38 2015-03-28 21:38:48 2015-03-28 21:38:48  no
#
# pm25<-read.table(file="pm25.dat",header=TRUE,stringsAsFactors=FALSE)  # in case of power outage...
#
selection<-select(pm25,year,Event.Type,Longitude,Latitude,State.Code,County.Code,Site.ID)
by_year_Location_Event.Type<-group_by(selection,year,Longitude,Latitude,Event.Type)
display1<-summarize(by_year_Location_Event.Type,Events=n())
myPNGfile<-"plot2A.png"
png(filename=myPNGfile,width=480,height=480) ## open png device for plot1.png 480x480 pix
by_year_Event.Type<-group_by(display1,year,Event.Type,Events)
display2<-summarize(by_year_Event.Type,All.Events=sum(Events))
ggplot (display2,
        aes(year,y=All.Events/1e6))+
        geom_bar(stat="identity",
                 color="blue",fill="blue")+
        stat_smooth(data=display2,
                    aes(x=year,y=All.Events/1e6,group=1),
                    fill="blue",
                    color="orange",
                    size=1,
                    method=lm,
                    se=FALSE)+
        facet_wrap(~Event.Type)+
        labs(title="US Sites (in thousands) reporting missing PM2.5 data \n in Annual EPA reports by Event Type\n with Linear Trendline since Y1999")+
        xlab("Year")+
        ylab("Thousands Sites reporting missing PM2.5 data")
dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot2.png 6738 FALSE  666 2015-03-26 20:24:18 2015-03-26 20:23:21 2015-03-26 20:23:21  no
stop()
display1.D<-display1[display1$Group=="D",];display1.D<-display1.D[,-4]
display1.L<-display1[display1$Group=="L",];display1.L<-display1.L[,-4]
display1.N<-display1[display1$Group=="N",];display1.N<-display1.N[,-4]
display1.O<-display1[display1$Group=="O",];display1.O<-display1.O[,-4]
display1.Q<-display1[display1$Group=="Q",];display1.Q<-display1.Q[,-4]
display1<-display1[,-4]
by_year_Location<-group_by(display1,year,Longitude,Latitude)
display2<-summarize(by_year_Location,all.events=sum(events))
display2$year<-factor(display2$year)
##
## we want to chart the coordinates on the map after summation and use
## a color chart to indicate density
#install.packages("ggmap")
#install.packages("mapproj")
#install.packages("rworldmap")
#install.packages("RColorBrewer")
library(ggmap)
library(mapproj)
library(rworldmap)
myPNGfile<-"plot3A.png"
png(filename=myPNGfile,width=2560,height=2560)        # open png device for plot1.png 480x480 pix
USAmap<-get_map('United States',zoom=4)
ggmap(USAmap,extent='device',color='bw',legend='bottomright')+
        scale_color_brewer(palette="Accent")+
        geom_point(aes(x=Longitude, y=Latitude, size=events, color=Group), data=display0)+
        facet_wrap(~year)+
        labs(title="US Locations of Missing PM_25 Emission data 1998-2013/na All Missing Data Codes Reported")
dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot2.png 7081 FALSE  666 2015-03-22 10:35:03 2015-03-16 20:20:57 2015-03-16 20:20:57  nomyPNGfile<-"plot3D.png"myPNGfile<-"plot3.png"
colnames(display1)[4]<-"level"
data3d<-display1[,2:4]
data3d<-cbind(data3d,display1[,1])
myPNGfile<-"plot3.png"
png(filename=myPNGfile,width=2560,height=2560)        # open png device for plot1.png 480x480 pix
USAmap<-get_map('United States',zoom=4)
ggmap(USAmap,extent='device',color='bw',legend='bottomright')+
        stat_density2d(aes(x=Longitude, y=Latitude, fill=..level..,alpha=..level..),
                       size=1,bins=10,contour=TRUE,geom='polygon', data=data3d)+
        scale_alpha(range=c(.1,.9),guide=FALSE)+
        guides(fill=guide_colorbar(barwidth=5,barheight=30))+
        facet_wrap(~year)+
        labs(title="US Locations of Missing PM_25 Emission data 1998-2013/na All Missing Data Codes Reported")
dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot2.png 7081 FALSE  666 2015-03-22 10:35:03 2015-03-16 20:20:57 2015-03-16 20:20:57  nomyPNGfile<-"plot3D.png"
colnames(display1)[4]<-"level"
data3d<-display1.D[,2:4]
data3d<-cbind(data3d,display1.D[,1])
myPNGfile<-"plot3D.png"
png(filename=myPNGfile,width=2560,height=2560)        # open png device for plot1.png 480x480 pix
USAmap<-get_map('United States',zoom=4)
ggmap(USAmap,extent='device',color='bw',legend='bottomright')+
        stat_density2d(aes(x=Longitude, y=Latitude, fill=..level..,alpha=..level..),
                       size=2,bins=10, data=data3d, geom='polygon')+
        scale_alpha(range=c(.1,.9),guide=FALSE)+
        guides(fill=guide_colorbar(barwidth=5,barheight=30))+
        facet_wrap(~year)+
        labs(title="US Locations of Missing PM_25 Emission data 1999-2013/nMissing Data Code reports linked to DAMAGES")
dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot2.png 7081 FALSE  666 2015-03-22 10:35:03 2015-03-16 20:20:57 2015-03-16 20:20:57  no
colnames(display1)[4]<-"level"
data3d<-display1.L[,2:4]
data3d<-cbind(data3d,display1.L[,1])
myPNGfile<-"plot3D.png"
png(filename=myPNGfile,width=2560,height=2560)        # open png device for plot1.png 480x480 pix
USAmap<-get_map('United States',zoom=4)
ggmap(USAmap,extent='device',color='bw',legend='bottomright')+
        stat_density2d(aes(x=Longitude, y=Latitude, fill=..level..,alpha=..level..),
                       size=2,bins=10, data=data3d, geom='polygon')+
        scale_alpha(range=c(.1,.9),guide=FALSE)+
        guides(fill=guide_colorbar(barwidth=5,barheight=30))+
        facet_wrap(~year)+
        labs(title="US Locations of Missing PM_25 Emission data 1999-2013/nMissing Data Code reports linked to OUT-OF-LIMITS")
dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot2.png 7081 FALSE  666 2015-03-22 10:35:03 2015-03-16 20:20:57 2015-03-16 20:20:57  no
colnames(display1)[4]<-"level"
data3d<-display1.N[,2:4]
data3d<-cbind(data3d,display1.N[,1])
myPNGfile<-"plot3N.png"
png(filename=myPNGfile,width=2560,height=2560)        # open png device for plot1.png 480x480 pix
USAmap<-get_map('United States',zoom=4)
ggmap(USAmap,extent='device',color='bw',legend='bottomright')+
        stat_density2d(aes(x=Longitude, y=Latitude, fill=..level..,alpha=..level..),
                       size=2,bins=10, data=data3d, geom='polygon')+
        scale_alpha(range=c(.1,.9),guide=FALSE)+
        guides(fill=guide_colorbar(barwidth=5,barheight=30))+
        facet_wrap(~year)+
        labs(title="US Locations of Missing PM_25 Emission data 1999-2013/nMissing Data Code reports linked to NON-CONTROLLABLE EVENTS")
dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot2.png 7081 FALSE  666 2015-03-22 10:35:03 2015-03-16 20:20:57 2015-03-16 20:20:57  no
colnames(display1)[4]<-"level"
data3d<-display1.O[,2:4]
data3d<-cbind(data3d,display1.O[,1])
myPNGfile<-"plot3D.png"
png(filename=myPNGfile,width=2560,height=2560)        # open png device for plot1.png 480x480 pix
USAmap<-get_map('United States',zoom=4)
ggmap(USAmap,extent='device',color='bw',legend='bottomright')+
        stat_density2d(aes(x=Longitude, y=Latitude, fill=..level..,alpha=..level..),
                       size=2,bins=10, data=data3d, geom='polygon')+
        scale_alpha(range=c(.1,.9),guide=FALSE)+
        guides(fill=guide_colorbar(barwidth=5,barheight=30))+
        facet_wrap(~year)+
        labs(title="US Locations of Missing PM_25 Emission data 1999-2013/nMissing Data Code reports linked to OPERATOR")
dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot2.png 7081 FALSE  666 2015-03-22 10:35:03 2015-03-16 20:20:57 2015-03-16 20:20:57  no
colnames(display1)[4]<-"level"
data3d<-display1.Q[,2:4]
data3d<-cbind(data3d,display1.Q[,1])
myPNGfile<-"plot3D.png"
png(filename=myPNGfile,width=2560,height=2560)        # open png device for plot1.png 480x480 pix
USAmap<-get_map('United States',zoom=4)
ggmap(USAmap,extent='device',color='bw',legend='bottomright')+
        stat_density2d(aes(x=Longitude, y=Latitude, fill=..level..,alpha=..level..),
                       size=2,bins=10, data=data3d, geom='polygon')+
        scale_alpha(range=c(.1,.9),guide=FALSE)+
        guides(fill=guide_colorbar(barwidth=5,barheight=30))+
        facet_wrap(~year)+
        labs(title="US Locations of Missing PM_25 Emission data 1999-2013/nMissing Data Code reports linked to QUALITY")
dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot2.png 7081 FALSE  666 2015-03-22 10:35:03 2015-03-16 20:20:57 2015-03-16 20:20:57  no
