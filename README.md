# Missing_PM25
Exploratory Missing Data analysis project

From the week4 air Pollution Stdy Case, we are left with open questions:
       
Although less than 10% of the data is missing, is it important?
Considering the extend of work placed into reporting, even missing data represent
a large number of records (> 1.5 Million between 1998 and 2013), and there must be
data worth mining in these, so I developed this script to attempt an answer at...

What can be determined from PM25 missing data? Specifically between 1998-2013:

        1) What is the trend globally in US and by state?
        2) How is the spatial distribution of these events evolving in absolute and relative terms?
        3) More than 50 different values are assigned in the Null.Data.Code table.
        Can they be regrouped in 5 event groups: Limits Exceeded, Quality ,Damage ,Operator and Uncontrollable?
        4) How is the spatial distribution of these events evolving in absolute and relative terms?

 Type 1 PM2.5 data files available at the US EPA web site address: http://www.epa.gov/ttn/airs/airsaqs/detaildata/downloadaqsdata.htm: RD_501_88101_YYYY-0.txt contain the PM2.5 data.
 
 Type 2 Annual reports between 1999 and 2013 at the US EPA web site address: http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html#Annual: annual_all_YYYY.zip contain also the geolocalization for all the EPA sites. After extraction, we used annual_all_YYYY.csv files (Type 3)
 
 Missing categories are provided in QualifierCodesNULL.csv available also at US EPA web site address: https://aqs.epa.gov/aqsweb/codes/data/QualifierCodesNULL.csv
