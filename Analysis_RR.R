#Michelle Stuhlmacher

#Environmental Gentrification (Chicago)

#STEPS:
#1. Import data and libraries
#2. Create yearly census variables
#3. Create yearly greenspace variables
#3A. Prepare variables
#3B. Format LU codes
#3C. Create variables
#4. Add final variables to megatract
#4A Format with Year column
#4B Format for Panel Regression
#5. Determine gentrification eligible (GE) census tracts
#   1990-2000, 2000-2010, and 2010-2015
#6. Identify gentrifying census tracts from GE census tracts
#   1990-2000, 2000-2010, and 2010-2015
#7. Prepare variables
#8. Calculate percent change for variables
#8A. Export data to make figures
#9. Run logit regression
#10. Check if residuals are spatially correlated

# STEP 1 -----------------------------------------------
#Import data and libraries

#Libraries
library(raster)
library(dplyr)
library(sf)
library(tidyr)
library(caret)
library(car)
library(DescTools)
library(rgeos)

#Set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/EnvGentrification/Data") #work laptop
#Import megatract
MT = shapefile('./RawCensusData/Megatracts/megatract_output_20210408.shp')

#Import land use and greespace files
g1990_80th = read.csv('./Greenspace/LU1990_NDVI80thPer_L5_20210524.csv')
g2000_80th = read.csv('./Greenspace/LU2001_NDVI80thPer_L5_20210524.csv')
g2010_80th = read.csv('./Greenspace/LU2010_NDVI80thPer_L5_20210524.csv')
g2015_80th = read.csv('./Greenspace/LU2015_NDVI80thPer_L8_20210524.csv')

#Downtown polygon
DwnTwn = shapefile('C:/Users/mstuhlm1/Dropbox/Envt Gentrification/Data/ChicagoBoundaries/DowntownPoly.shp')

#Transit
cta = st_read('./Transit/CTA_RailStations/CTA_RailStations.shp')
metra = st_read('./Transit/Metra_Stations/MetraStations_Chicago10kmClipPts.shp')

# STEP 2 -----------------------------------------------
#Create yearly census variables

#percent white (pct_w)
MT@data$pct_w = MT@data$race_w / MT@data$orgn_tot

#percent college educated (pct_cedu)
MT@data$edu_total = MT@data$edu_9 + MT@data$edu_12 + MT@data$edu_hs + MT@data$edu_scol + 
  MT@data$edu_asc + MT@data$edu_bch + MT@data$edu_grad

MT@data$pct_cedu = (MT@data$edu_asc + MT@data$edu_bch + MT@data$edu_grad)/MT@data$edu_total

#percent vacant housing units (pct_v)
MT@data$pct_v = MT@data$ohous_vac/MT@data$ohous_tot

#percent Hispanic origin (pct_h)
MT@data$pct_h = MT@data$orgn_hisp/MT@data$orgn_tot

#percent housing units older than 30 years (pct_h30)
MT@data$pct_h30 = MT@data$hous_30yr/MT@data$hous_tot

#percent black (pct_b)
MT@data$pct_b = MT@data$race_b / MT@data$orgn_tot

# STEP 3A -----------------------------------------------
#Create yearly greenspace variables: Prepare variables

#Calculate the area of each census tract
#raster area function: area if each spatial object in squared meters if the CRS is longitude/latitude, or in squared map units (typically meter)
proj4string(MT)
#Our map units are m --> proj4string(tract)
MT$areaSqM = area(MT)
chicagoSqM = sum(MT@data$areaSqM)

#Calculate population density (ppl_sqm)
MT@data$ppl_sqm = MT@data$orgn_tot/MT@data$areaSqM

#Make cluster_id a number
MT@data$cluster_id = as.numeric(MT@data$cluster_id)

# Separate the megatract out by year
MT_1990 = subset(MT, YEAR == "1990")
MT_2000 = subset(MT, YEAR == "2000")
MT_2010 = subset(MT, YEAR == "2006-2010")
MT_2015 = subset(MT, YEAR == "2011-2015")

#Adjust for inflation (make rent and income comparable through time)
#Census Metadata:
#1990 income is in 1989 dollars (rent doesn't specify)
#2000 income is in 1999 dollars (rent doesn't specify)
#2010 income is in 2010 inflation adjusted dollars (rent doesn't specify)
#2015 income is in 2015 inflation adjusted dollars (rent doesn't specify)

#U.S. Bureau of Labor Statistics Inflation Calculator:
#https://www.bls.gov/data/inflation_calculator.htm

#$1 in Dec. 1989 has the same buying power as $1.88 in Dec. 2015
#$1 in Dec. 1999 has the same buying power as $1.41 in Dec. 2015
#$1 in Dec. 2010 has the same buying power as $1.08 in Dec. 2015

MT_1990$income_inf = MT_1990$income * 1.88
MT_2000$income_inf = MT_2000$income * 1.41
MT_2010$income_inf = MT_2010$income * 1.08
MT_2015$income_inf = MT_2015$income

MT_1990$rent_inf = MT_1990$rent * 1.88
MT_2000$rent_inf = MT_2000$rent * 1.41
MT_2010$rent_inf = MT_2010$rent * 1.08
MT_2015$rent_inf = MT_2015$rent

MT_1990$hmVal_inf = MT_1990$hous_val * 1.88
MT_2000$hmVal_inf = MT_2000$hous_val * 1.41
MT_2010$hmVal_inf = MT_2010$hous_val * 1.08
MT_2015$hmVal_inf = MT_2015$hous_val

#rename LANDUSE so it matches for recoding
names(g1990_80th) = sub("LANDUSE*", "", names(g1990_80th))
names(g2000_80th) = sub("LANDUSE_n*", "", names(g2000_80th))
names(g2010_80th) = sub("LANDUSE_n*", "", names(g2010_80th))
names(g2015_80th) = sub("LANDUSE_n*", "", names(g2015_80th))

#change to long format for recode
#1990 LANDUSE
g1990_80th_l = reshape(data = g1990_80th,
                       varying = c("1110","1120","1130","1140","1210","1220","1230",
                                   "1241","1242","1243","1250","1260","1311","1312","1313","1320",
                                   "1330","1340","1360","1370","1380","1390","1410","1420","1430",
                                   "1440","1510","1520","1530","1540","1550","1560","2000","3110",
                                   "3120","3130","3210","3220","4110","4120","4210","4220","4300",
                                   "5100","5200","5300","9999"),
                       times = c("1110","1120","1130","1140","1210","1220","1230",
                                 "1241","1242","1243","1250","1260","1311","1312","1313","1320",
                                 "1330","1340","1360","1370","1380","1390","1410","1420","1430",
                                 "1440","1510","1520","1530","1540","1550","1560","2000","3110",
                                 "3120","3130","3210","3220","4110","4120","4210","4220","4300",
                                 "5100","5200","5300","9999"),
                       idvar = "cluster_id",
                       v.names = "SqM",
                       direction = "long")

#2000 LANDUSE
g2000_80th_l = reshape(data = g2000_80th,
                       varying = c("1110","1120","1130","1140","1211","1212","1221","1222","1223","1231","1232","1240",
                                   "1250","1310","1320","1330","1340","1350","1360","1370","1410","1420","1430","1440",
                                   "1511","1512","1520","1530","1540","1550","1560","2100","2200","3100","3200","3300",
                                   "3400","3500","3600","4110","4120","4210","4220","4300","5100","5200","5300"),
                       times = c("1110","1120","1130","1140","1211","1212","1221","1222","1223","1231","1232","1240",
                                 "1250","1310","1320","1330","1340","1350","1360","1370","1410","1420","1430","1440",
                                 "1511","1512","1520","1530","1540","1550","1560","2100","2200","3100","3200","3300",
                                 "3400","3500","3600","4110","4120","4210","4220","4300","5100","5200","5300"),
                       idvar = "cluster_id",
                       v.names = "SqM",
                       direction = "long")

#2010 LANDUSE
g2010_80th_l = reshape(data = g2010_80th,
                       varying = c("1111","1112","1130","1140","1151","1211","1212","1214","1215","1216","1220","1240","1250",
                                   "1310","1321","1322","1330","1340","1350","1360","1370","1410","1420","1431","1432","1433",
                                   "1450","1511","1512","1520","1530","1540","1550","1561","1562","1563","1564","1565","1570",
                                   "2000","3100","3200","3300","3400","3500","4110","4120","4130","4140","4210","4220","4230",
                                   "4240","5000","6100","6200","6300","6400","9999"),
                       times = c("1111","1112","1130","1140","1151","1211","1212","1214","1215","1216","1220","1240","1250",
                                 "1310","1321","1322","1330","1340","1350","1360","1370","1410","1420","1431","1432","1433",
                                 "1450","1511","1512","1520","1530","1540","1550","1561","1562","1563","1564","1565","1570",
                                 "2000","3100","3200","3300","3400","3500","4110","4120","4130","4140","4210","4220","4230",
                                 "4240","5000","6100","6200","6300","6400","9999"),
                       idvar = "cluster_id",
                       v.names = "SqM",
                       direction = "long")

#2015 LANDUSE
# "1111","1112","1130","1151","1215","1216","1321","1420","1511","1512","1520","1564","3100","4120","4130","5000","6000" 
g2015_80th_l = reshape(data = g2015_80th,
                       varying = c("1111","1112","1130","1140","1151","1211","1212","1214","1215","1216","1220","1240","1250",
                                   "1310","1321","1322","1330","1340","1350","1360","1370","1410","1420","1431","1432","1433",
                                   "1450","1511","1512","1520","1530","1540","1550","1561","1562","1563","1564","1565","1570",
                                   "2000","3100","3200","3300","3400","3500","4110","4120","4130","4140","4210","4220","4230",
                                   "4240","5000","6000","9999"),
                       times = c("1111","1112","1130","1140","1151","1211","1212","1214","1215","1216","1220","1240","1250",
                                 "1310","1321","1322","1330","1340","1350","1360","1370","1410","1420","1431","1432","1433",
                                 "1450","1511","1512","1520","1530","1540","1550","1561","1562","1563","1564","1565","1570",
                                 "2000","3100","3200","3300","3400","3500","4110","4120","4130","4140","4210","4220","4230",
                                 "4240","5000","6000","9999"),
                       idvar = "cluster_id",
                       v.names = "SqM",
                       direction = "long")

# STEP 3B -----------------------------------------------
# Create yearly green space variables: 
#1) park green space, 2) green space on vacant land, 3) all other green space  

#Add columns with LU codes in one column and the string name in another
##1990
g1990_80th_l$GreenType = ifelse(g1990_80th_l$time == 3110 | g1990_80th_l$time == 3120 | g1990_80th_l$time == 3130 | g1990_80th_l$time == 3210 | g1990_80th_l$time == 3220, 1,
                                ifelse(g1990_80th_l$time == 4300 | g1990_80th_l$time == 4110, 2,
                                       3))

g1990_80th_l$CATEGORY_t = ifelse(g1990_80th_l$GreenType == 1, "Park", 
                               ifelse(g1990_80th_l$GreenType == 2, "Vacant",
                                      ifelse(g1990_80th_l$GreenType == 3, "Other",
                                             "ERROR")))

##2000
g2000_80th_l$GreenType = ifelse(g2000_80th_l$time == 3100 | g2000_80th_l$time == 3200 | g2000_80th_l$time == 3300, 1, 
                                ifelse(g2000_80th_l$time == 4300 | g2000_80th_l$time == 4110, 2,
                                       3))

g2000_80th_l$CATEGORY_t = ifelse(g2000_80th_l$GreenType == 1, "Park", 
                               ifelse(g2000_80th_l$GreenType == 2, "Vacant",
                                      ifelse(g2000_80th_l$GreenType == 3, "Other",
                                             "ERROR")))

#2010
g2010_80th_l$GreenType = ifelse(g2010_80th_l$time == 3100 | g2010_80th_l$time == 3200 | g2010_80th_l$time == 3300 | g2010_80th_l$time == 3500, 1, 
                                ifelse(g2010_80th_l$time == 4110 | g2010_80th_l$time == 4120 | g2010_80th_l$time == 4130 | g2010_80th_l$time == 4140, 2, 
                                       3))

g2010_80th_l$CATEGORY_t = ifelse(g2010_80th_l$GreenType == 1, "Park", 
                               ifelse(g2010_80th_l$GreenType == 2, "Vacant",
                                      ifelse(g2010_80th_l$GreenType == 3, "Other",
                                             "ERROR")))

#2015
g2015_80th_l$GreenType = ifelse(g2015_80th_l$time == 3100 | g2015_80th_l$time == 3200 | g2015_80th_l$time == 3300 | g2015_80th_l$time == 3500, 1, 
                                ifelse(g2015_80th_l$time == 4110 | g2015_80th_l$time == 4120 | g2015_80th_l$time == 4130 | g2015_80th_l$time == 4140, 2, 
                                       3))

g2015_80th_l$CATEGORY_t = ifelse(g2015_80th_l$GreenType == 1, "Park", 
                               ifelse(g2015_80th_l$GreenType == 2, "Vacant",
                                      ifelse(g2015_80th_l$GreenType == 3, "Other",
                                             "ERROR")))

#Create greenspace variables
#A. Drop rows with NA and 0 values to speed processing time
g1990_80th_l = g1990_80th_l[!is.na(g1990_80th_l$SqM),] 
g1990_80th_l = g1990_80th_l[g1990_80th_l$SqM != 0,] 

g2000_80th_l = g2000_80th_l[!is.na(g2000_80th_l$SqM),] 
g2000_80th_l = g2000_80th_l[g2000_80th_l$SqM != 0,] 

g2010_80th_l = g2010_80th_l[!is.na(g2010_80th_l$SqM),] 
g2010_80th_l = g2010_80th_l[g2010_80th_l$SqM != 0,] 

g2015_80th_l = g2015_80th_l[!is.na(g2015_80th_l$SqM),] 
g2015_80th_l = g2015_80th_l[g2015_80th_l$SqM != 0,] 

#B. Summarize by greenspace type and census tract (a.k.a cluster id)
summary1990_NDVI80th = g1990_80th_l %>%
  group_by(cluster_id, GreenType) %>%
  summarise(sum = sum(SqM))
colnames(summary1990_NDVI80th) = c("cluster_id","CATEGORY","GreenSqM")

summary2000_NDVI80th = g2000_80th_l %>%
  group_by(cluster_id, GreenType) %>%
  summarise(sum = sum(SqM))
colnames(summary2000_NDVI80th) = c("cluster_id","CATEGORY","GreenSqM")

summary2010_NDVI80th = g2010_80th_l %>%
  group_by(cluster_id, GreenType) %>%
  summarise(sum = sum(SqM))
colnames(summary2010_NDVI80th) = c("cluster_id","CATEGORY","GreenSqM")

summary2015_NDVI80th = g2015_80th_l %>%
  group_by(cluster_id, GreenType) %>%
  summarise(sum = sum(SqM))
colnames(summary2015_NDVI80th) = c("cluster_id","CATEGORY","GreenSqM")

#C. Reshape from long to wide
summary1990_NDVI80th_w = pivot_wider(data=summary1990_NDVI80th,
                                     id_cols = "cluster_id",
                                     names_from = "CATEGORY",
                                     values_from = "GreenSqM")

summary2000_NDVI80th_w = pivot_wider(data=summary2000_NDVI80th,
                                     id_cols = "cluster_id",
                                     names_from = "CATEGORY",
                                     values_from = "GreenSqM")

summary2010_NDVI80th_w = pivot_wider(data=summary2010_NDVI80th,
                                     id_cols = "cluster_id",
                                     names_from = "CATEGORY",
                                     values_from = "GreenSqM")

summary2015_NDVI80th_w = pivot_wider(data=summary2015_NDVI80th,
                                     id_cols = "cluster_id",
                                     names_from = "CATEGORY",
                                     values_from = "GreenSqM")

#D. Rename
#1 = "Formal",2 = "Informal"
#1) park green space, 2) green space on vacant land, 3) all other green space
oldnames = c('1','2','3')
newnames = c('ParkGreenSqM','VacGreenSqM','OtherGreenSqM')

summary1990_NDVI80th_w = summary1990_NDVI80th_w %>% rename_at(vars(oldnames), ~ newnames)
summary2000_NDVI80th_w = summary2000_NDVI80th_w %>% rename_at(vars(oldnames), ~ newnames)
summary2010_NDVI80th_w = summary2010_NDVI80th_w %>% rename_at(vars(oldnames), ~ newnames)
summary2015_NDVI80th_w = summary2015_NDVI80th_w %>% rename_at(vars(oldnames), ~ newnames)

#E.Replace NAs with 0s (otherwise the sum in step F won't work)
summary1990_NDVI80th_w[is.na(summary1990_NDVI80th_w)] = 0 
summary2000_NDVI80th_w[is.na(summary2000_NDVI80th_w)] = 0 
summary2010_NDVI80th_w[is.na(summary2010_NDVI80th_w)] = 0 
summary2015_NDVI80th_w[is.na(summary2015_NDVI80th_w)] = 0 

#F. Add up all the greenspace in each census tract
summary1990_NDVI80th_w$GreenSqM = summary1990_NDVI80th_w$ParkGreenSqM + summary1990_NDVI80th_w$VacGreenSqM + summary1990_NDVI80th_w$OtherGreenSqM
summary2000_NDVI80th_w$GreenSqM = summary2000_NDVI80th_w$ParkGreenSqM + summary2000_NDVI80th_w$VacGreenSqM + summary2000_NDVI80th_w$OtherGreenSqM
summary2010_NDVI80th_w$GreenSqM = summary2010_NDVI80th_w$ParkGreenSqM + summary2010_NDVI80th_w$VacGreenSqM + summary2010_NDVI80th_w$OtherGreenSqM
summary2015_NDVI80th_w$GreenSqM = summary2015_NDVI80th_w$ParkGreenSqM + summary2015_NDVI80th_w$VacGreenSqM + summary2015_NDVI80th_w$OtherGreenSqM

#G. Merge with census data
MT_1990_G80th = merge(MT_1990,summary1990_NDVI80th_w,by=c("cluster_id"),all.x=TRUE) 
MT_2000_G80th = merge(MT_2000,summary2000_NDVI80th_w,by=c("cluster_id"),all.x=TRUE) 
MT_2010_G80th = merge(MT_2010,summary2010_NDVI80th_w,by=c("cluster_id"),all.x=TRUE) 
MT_2015_G80th = merge(MT_2015,summary2015_NDVI80th_w,by=c("cluster_id"),all.x=TRUE) 

#H. Calculate greenspace variables
# pctA_G = % of census tract covered by any type of greenspace (including the excluded class)
MT_1990_G80th$pctA_G = MT_1990_G80th$GreenSqM/MT_1990_G80th$areaSqM 
MT_2000_G80th$pctA_G = MT_2000_G80th$GreenSqM/MT_2000_G80th$areaSqM 
MT_2010_G80th$pctA_G = MT_2010_G80th$GreenSqM/MT_2010_G80th$areaSqM 
MT_2015_G80th$pctA_G = MT_2015_G80th$GreenSqM/MT_2015_G80th$areaSqM 

# pctA_parkG = % of census tract covered by park greenspace
MT_1990_G80th$pctA_parkG = MT_1990_G80th$ParkGreenSqM/MT_1990_G80th$areaSqM 
MT_2000_G80th$pctA_parkG = MT_2000_G80th$ParkGreenSqM/MT_2000_G80th$areaSqM 
MT_2010_G80th$pctA_parkG = MT_2010_G80th$ParkGreenSqM/MT_2010_G80th$areaSqM 
MT_2015_G80th$pctA_parkG = MT_2015_G80th$ParkGreenSqM/MT_2015_G80th$areaSqM 

# pctA_vacG = % of census tract covered by greenspace on vacant land
MT_1990_G80th$pctA_vacG = MT_1990_G80th$VacGreenSqM/MT_1990_G80th$areaSqM 
MT_2000_G80th$pctA_vacG = MT_2000_G80th$VacGreenSqM/MT_2000_G80th$areaSqM 
MT_2010_G80th$pctA_vacG = MT_2010_G80th$VacGreenSqM/MT_2010_G80th$areaSqM 
MT_2015_G80th$pctA_vacG = MT_2015_G80th$VacGreenSqM/MT_2015_G80th$areaSqM

# pctA_othG = % of census tract covered by all other greenspace
MT_1990_G80th$pctA_othG = MT_1990_G80th$OtherGreenSqM/MT_1990_G80th$areaSqM 
MT_2000_G80th$pctA_othG = MT_2000_G80th$OtherGreenSqM/MT_2000_G80th$areaSqM 
MT_2010_G80th$pctA_othG = MT_2010_G80th$OtherGreenSqM/MT_2010_G80th$areaSqM 
MT_2015_G80th$pctA_othG = MT_2015_G80th$OtherGreenSqM/MT_2015_G80th$areaSqM

# STEP 4 -----------------------------------------------
# Add final variables to the megatract 

#Replace NAs with 0s for greenspace and census variables
MT_1990_G80th@data[c('income','income_inf','rent','rent_inf','pct_w','pct_cedu','hous_tot','hmVal_inf','pct_v','pct_h','pct_h30','pct_b','ppl_sqm','ParkGreenSqM','VacGreenSqM','OtherGreenSqM',
                     'GreenSqM','pctA_G','pctA_parkG','pctA_vacG','pctA_othG')][is.na(MT_1990_G80th@data[c('income','income_inf','rent','rent_inf','pct_w','pct_cedu','hous_tot','hmVal_inf','pct_v',
                                                                                               'pct_h','pct_h30','pct_b','ppl_sqm','ParkGreenSqM','VacGreenSqM','OtherGreenSqM','GreenSqM',
                                                                                               'pctA_G','pctA_parkG','pctA_vacG','pctA_othG')])] = 0

MT_2000_G80th@data[c('income','income_inf','rent','rent_inf','pct_w','pct_cedu','hous_tot','hmVal_inf','pct_v','pct_h','pct_h30','pct_b','ppl_sqm','ParkGreenSqM','VacGreenSqM','OtherGreenSqM',
                     'GreenSqM','pctA_G','pctA_parkG','pctA_vacG','pctA_othG')][is.na(MT_2000_G80th@data[c('income','income_inf','rent','rent_inf','pct_w','pct_cedu','hous_tot','hmVal_inf','pct_v',
                                                                                                           'pct_h','pct_h30','pct_b','ppl_sqm','ParkGreenSqM','VacGreenSqM','OtherGreenSqM','GreenSqM',
                                                                                                           'pctA_G','pctA_parkG','pctA_vacG','pctA_othG')])] = 0


MT_2010_G80th@data[c('income','income_inf','rent','rent_inf','pct_w','pct_cedu','hous_tot','hmVal_inf','pct_v','pct_h','pct_h30','pct_b','ppl_sqm','ParkGreenSqM','VacGreenSqM','OtherGreenSqM',
                     'GreenSqM','pctA_G','pctA_parkG','pctA_vacG','pctA_othG')][is.na(MT_2010_G80th@data[c('income','income_inf','rent','rent_inf','pct_w','pct_cedu','hous_tot','hmVal_inf','pct_v',
                                                                                                           'pct_h','pct_h30','pct_b','ppl_sqm','ParkGreenSqM','VacGreenSqM','OtherGreenSqM','GreenSqM',
                                                                                                           'pctA_G','pctA_parkG','pctA_vacG','pctA_othG')])] = 0

MT_2015_G80th@data[c('income','income_inf','rent','rent_inf','pct_w','pct_cedu','hous_tot','hmVal_inf','pct_v','pct_h','pct_h30','pct_b','ppl_sqm','ParkGreenSqM','VacGreenSqM','OtherGreenSqM',
                     'GreenSqM','pctA_G','pctA_parkG','pctA_vacG','pctA_othG')][is.na(MT_2015_G80th@data[c('income','income_inf','rent','rent_inf','pct_w','pct_cedu','hous_tot','hmVal_inf','pct_v',
                                                                                                           'pct_h','pct_h30','pct_b','ppl_sqm','ParkGreenSqM','VacGreenSqM','OtherGreenSqM','GreenSqM',
                                                                                                           'pctA_G','pctA_parkG','pctA_vacG','pctA_othG')])] = 0

# STEP 4A -----------------------------------------------
#Format with year column

#csv
MT_G80th = rbind(MT_1990_G80th@data,MT_2000_G80th@data,MT_2010_G80th@data,MT_2015_G80th@data) #stack the DFs

#Keep only final variables
keep = c('cluster_id','YEAR','areaSqM','income','income_inf','rent','rent_inf','pct_w','pct_cedu','hous_tot','hmVal_inf','pct_v','pct_h',
         'pct_h30','pct_b','ppl_sqm','ParkGreenSqM','VacGreenSqM','OtherGreenSqM','GreenSqM','pctA_G','pctA_parkG','pctA_vacG','pctA_othG')
MT_G80th = MT_G80th[keep]

#shp
MT_G80th_SHP = rbind(MT_1990_G80th,MT_2000_G80th,MT_2010_G80th,MT_2015_G80th)
MT_G80th_SHP = MT_G80th_SHP[keep]

# STEP 4B -----------------------------------------------
#Format for Panel Data
#Panel form drops all census tracts for which we don't have observations for every year, but these are mostly outside of the city boundaries
#(except one by O'Hare)

keep2 = c('cluster_id','income_inf','rent_inf','pct_w','pct_cedu','hous_tot','hmVal_inf','pct_v','pct_h',
         'pct_h30','pct_b','ppl_sqm','ParkGreenSqM','VacGreenSqM','OtherGreenSqM','pctA_G','pctA_parkG','pctA_vacG','pctA_othG')

MT_1990_G80th_p = MT_1990_G80th[keep2]
MT_2000_G80th_p = MT_2000_G80th[keep2]
MT_2010_G80th_p = MT_2010_G80th[keep2]
MT_2015_G80th_p = MT_2015_G80th[keep2]

#Add year to the end of relevant variable names
names(MT_1990_G80th_p) = c('cluster_id','income90','rent90','pW90','pCedu90','hous_tot90','hmVal90','pct_v90',
                           'pct_h90','pct_h30_90','pct_b90','ppl_sqm90','parkGsqM_90','vacGsqM_90','othGsqM_90',
                           'pA_G90','pA_prkG90','pA_vacG90','pA_othG90')

names(MT_2000_G80th_p) = c('cluster_id','income00','rent00','pW00','pCedu00','hous_tot00','hmVal00','pct_v00',
                           'pct_h00','pct_h30_00','pct_b00','ppl_sqm00','parkGsqM_00','vacGsqM_00','othGsqM_00',
                           'pA_G00','pA_prkG00','pA_vacG00','pA_othG00')

names(MT_2010_G80th_p) = c('cluster_id','income10','rent10','pW10','pCedu10','hous_tot10','hmVal10','pct_v10',
                           'pct_h10','pct_h30_10','pct_b10','ppl_sqm10','parkGsqM_10','vacGsqM_10','othGsqM_10',
                           'pA_G10','pA_prkG10','pA_vacG10','pA_othG10')

names(MT_2015_G80th_p) = c('cluster_id','income15','rent15','pW15','pCedu15','hous_tot15','hmVal15','pct_v15',
                           'pct_h15','pct_h30_15','pct_b15','ppl_sqm15','parkGsqM_15','vacGsqM_15','othGsqM_15',
                           'pA_G15','pA_prkG15','pA_vacG15','pA_othG15')

#Merge on constant columns
MTpanel_G80th_1 = merge(MT_1990_G80th_p,MT_2000_G80th_p,by = "cluster_id")
MTpanel_G80th_2 = merge(MTpanel_G80th_1,MT_2010_G80th_p,by = "cluster_id")
MTpanel_G80th = merge(MTpanel_G80th_2,MT_2015_G80th_p,by = "cluster_id")

# STEP 5 -----------------------------------------------
# Determine gentrification eligible (GE) census tracts [1990-2000, 2000-2010, and 2010-2015]

#Data
DF = as.data.frame(MTpanel_G80th)
DFshp_all = MT_G80th_SHP

#Reduce shp DF to 1990 cluster_id and geometry for joining
DFshp_all = subset(DFshp_all, YEAR == "1990")
DFshp_all = DFshp_all[c("cluster_id","areaSqM")]

#Remove airports
DF = filter(DF, !(cluster_id %in% c(563, 536)))
DFshp = subset(DFshp_all, cluster_id != c(563, 536))

#Public housing
hud_wgs = st_read('./PublicHousingBuildings/Public_Housing_Buildings.shp')
hud = st_transform(hud_wgs,crs(DFshp)) #reproject to match DFshp

#GE definition from Rigolon and Nemeth (2020), pg. 7
#Median household income below the city's median

#Using the first year of the three time periods to determine GE like they did (pdf pg. 9):
##--1990--##
medInc90 = median(DF$income90) #calculate median income
DF$GE90 = DF$income90 < medInc90 #create boolean column
#391 that are gentrification eligible

##--2000--##
medInc00 = median(DF$income00) #calculate median income
DF$GE00 = DF$income00 < medInc00 #create boolean column
#391 that are gentrification eligible

##--2010--##
medInc10 = median(DF$income10) #calculate median income
DF$GE10 = DF$income10 < medInc10 #create boolean column
#391 that are gentrification eligible

# STEP 6 -----------------------------------------------
# Identify gentrifying census tracts from GE census tracts [1990-2000, 2000-2010, and 2010-2015]

#####Mean Gentrification Definition (no race)#####
#0. Gentrification eligible
#1. Change in median household income > city (absolute value)
#2. Change in % college educated > city (percentage points)
#3a. Change in Median Gross Rent > city (absolute value) OR
#3b. Change in Median Home Value > city (absolute value)

##--1990--##
#1. Change in median household income >  city mean (absolute value)
DF$pct_inc_delt90 = DF$income00 - DF$income90
cityMean_inc90 = mean(DF$pct_inc_delt90)
DF$delt_inc90 = DF$pct_inc_delt90 > cityMean_inc90

#2. Change in % college educated > city mean (percentage points)
DF$pct_cedu_delt90 = DF$pCedu00 - DF$pCedu90 
cityMean_cedu90 = mean(DF$pct_cedu_delt90)
DF$delt_cedu90 = DF$pct_cedu_delt90 > cityMean_cedu90

#3. Change in housing costs:
#a. Change in Median Gross Rent > city mean (absolute value)
DF$rent_delt90 = DF$rent00 - DF$rent90
cityMean_rent90 = mean(DF$rent_delt90)
DF$delt_rent90 = DF$rent_delt90 > cityMean_rent90

#b. Change in Median Home Value > city mean (absolute value)
DF$hval_delt90 = DF$hmVal00 - DF$hmVal90
cityMean_hval = mean(DF$hval_delt90)
DF$delt_hval90 = DF$hval_delt90 > cityMean_hval

#Determine which census tracts have a TRUE for 1, 2, 3 (a or b) & 4 (a or b)
DF$gent90_00 = ifelse(DF$GE90 == TRUE & DF$delt_inc90 == TRUE & DF$delt_cedu90 == TRUE & 
                        (DF$delt_rent90 == TRUE | DF$delt_hval90 == TRUE), 1, 0)
#79 gentrifying census tracts

#Set as factor
DF$gent90_00 = as.factor(DF$gent90_00)

##--2000--##
#1. Change in median household income > city mean (absolute value)
DF$pct_inc_delt00 = DF$income10 - DF$income00
cityMean_inc00 = mean(DF$pct_inc_delt00)
DF$delt_inc00 = DF$pct_inc_delt00 > cityMean_inc00

#2. Change in % college educated > city mean (percentage points)
DF$pct_cedu_delt00 = DF$pCedu10 - DF$pCedu00
cityMean_cedu00 = mean(DF$pct_cedu_delt00)
DF$delt_cedu00 = DF$pct_cedu_delt00 > cityMean_cedu00

#3. Change in housing costs:
#a. Change in Median Gross Rent > city mean (absolute value)
DF$rent_delt00 = DF$rent10 - DF$rent00
cityMean_rent00 = mean(DF$rent_delt00)
DF$delt_rent00 = DF$rent_delt00 > cityMean_rent00

#b. Change in Median Home Value > city mean (absolute value)
DF$hval_delt00 = DF$hmVal10 - DF$hmVal00
cityMean_hval = mean(DF$hval_delt00)
DF$delt_hval00 = DF$hval_delt00 > cityMean_hval

#Determine which census tracts have a TRUE for 1, 2, 3 (a or b) & 4 (a or b)
DF$gent00_10 = ifelse(DF$GE00 == TRUE & DF$delt_inc00 == TRUE & DF$delt_cedu00 == TRUE &
                        (DF$delt_rent00 == TRUE | DF$delt_hval00 == TRUE), 1, 0)
#85 gentrifying census tracts

#Set as factor
DF$gent00_10 = as.factor(DF$gent00_10)

##--2010--##
#1. Change in median household income > city mean (absolute value)
DF$pct_inc_delt10 = DF$income15 - DF$income10
cityMean_inc10 = mean(DF$pct_inc_delt10)
DF$delt_inc10 = DF$pct_inc_delt10 > cityMean_inc10

#2. Change in % college educated > city mean(percentage points)
DF$pct_cedu_delt10 = DF$pCedu15 - DF$pCedu10
cityMean_cedu10 = mean(DF$pct_cedu_delt10)
DF$delt_cedu10 = DF$pct_cedu_delt10 > cityMean_cedu10

#3. Change in housing costs:
#a. Change in Median Gross Rent > city mean (absolute value)
DF$rent_delt10 = DF$rent15 - DF$rent10
cityMean_rent10 = mean(DF$rent_delt10)
DF$delt_rent10 = DF$rent_delt10 > cityMean_rent10

#b. Change in Median Home Value > city mean (absolute value)
DF$hval_delt10 = DF$hmVal15 - DF$hmVal10
cityMean_hval = mean(DF$hval_delt10)
DF$delt_hval10 = DF$hval_delt10 > cityMean_hval

#Determine which census tracts have a TRUE for 1, 2, 3 (a or b) & 4 (a or b)
DF$gent10_15 = ifelse(DF$GE10 == TRUE & DF$delt_inc10 == TRUE & DF$delt_cedu10 == TRUE &
                        (DF$delt_rent10 == TRUE | DF$delt_hval10 == TRUE), 1, 0)
#86 gentrifying census tracts

#Set as factor
DF$gent10_15 = as.factor(DF$gent10_15)

#####Mean Gentrification Definition (with race)#####
#0. Gentrification eligible (median household income below the city's median) AND
#1a. Increase in % Non-Hispanic White Population OR
#1b. Decrease in % Hispanic Population AND
#2a. Change in Median Gross Rent > city change OR
#2b. Change in Median Home Value > city change

##--1990--##
#1a. One standard deviation increase in the non-Hispanic % white OR
DF$pct_w_delt90 = DF$pW00 - DF$pW90
cityMean_w90 = mean(DF$pct_w_delt90)
citySD_w90 = sd(DF$pct_w_delt90)
DF$deltSD_w90 = DF$pct_w_delt90 > (cityMean_w90 + citySD_w90)
#62

#1b. Decrease in % hispanic 
DF$pct_h_delt90 = DF$pct_h00 - DF$pct_h90
cityMean_h90 = mean(DF$pct_h_delt90)
citySD_h90 = sd(DF$pct_h_delt90)
DF$deltSD_h90 = DF$pct_h_delt90 < (cityMean_h90 - citySD_h90)
#65

#Increase greater than 1 SD:
DF$gentSDR90_00 = ifelse(DF$GE90 == TRUE & (DF$deltSD_w90 == TRUE | DF$deltSD_h90 == TRUE) &
                           (DF$delt_rent90 == TRUE | DF$delt_hval90 == TRUE), 1, 0)
#41 gentrifying census tracts
DF$gentSDR90_00 = as.factor(DF$gentSDR90_00) #Set as factor

##--2000--##
DF$pct_w_delt00 = DF$pW10 - DF$pW00
cityMean_w00 = mean(DF$pct_w_delt00)
citySD_w00 = sd(DF$pct_w_delt00)
DF$deltSD_w00 = DF$pct_w_delt00 > (cityMean_w00 + citySD_w00)
#77

DF$pct_h_delt00 = DF$pct_h10 - DF$pct_h00
cityMean_h00 = mean(DF$pct_h_delt00)
citySD_h00 = sd(DF$pct_h_delt00)
DF$deltSD_h00 = DF$pct_h_delt00 < (cityMean_h00 - citySD_h00)
#87

#Increase greater than 1 SD:
DF$gentSDR00_10 = ifelse(DF$GE00 == TRUE & (DF$deltSD_w00 == TRUE | DF$deltSD_h00 == TRUE) &
                           (DF$delt_rent00 == TRUE | DF$delt_hval00 == TRUE), 1, 0)
#53 gentrifying census tracts
DF$gentSDR00_10 = as.factor(DF$gentSDR00_10) #Set as factor

##--2010--##
DF$pct_w_delt10 = DF$pW15 - DF$pW10
cityMean_w10 = mean(DF$pct_w_delt10)
citySD_w10 = sd(DF$pct_w_delt10)
DF$deltSD_w10 = DF$pct_w_delt10 > (cityMean_w10 + citySD_w10)
#97

DF$pct_h_delt10 = DF$pct_h15 - DF$pct_h10
cityMean_h10 = mean(DF$pct_h_delt10)
citySD_h10 = sd(DF$pct_h_delt10)
DF$deltSD_h10 = DF$pct_h_delt10 < (cityMean_h10 - citySD_h10)
#97

#Increase greater than 1 SD:
DF$gentSDR10_15 = ifelse(DF$GE10 == TRUE & (DF$deltSD_w10 == TRUE | DF$deltSD_h10 == TRUE) &
                           (DF$delt_rent10 == TRUE | DF$delt_hval10 == TRUE), 1, 0)
#44 gentrifying census tracts
DF$gentSDR10_15 = as.factor(DF$gentSDR10_15)

# STEP 7 -----------------------------------------------
#Prepare the variables

###---Yearly Variables---###
DFsf_GE = st_as_sf(DFshp_all) #convert to sf format for st intersects

#-1990-#
#PERCENT HUD UNITS
#Select only those units with DOFA_ACTUA earlier than or equal to 1990
#DOFA_ACTUA = "Actual date the physical development reached the Date of Full Availability (DOFA)." (see metadata)
hud$DOFA_YEAR = format(as.Date(hud$DOFA_ACTUA, format="%Y-%m-%d"),"%Y") #Make a year column
hud_90 = subset(hud, DOFA_YEAR <= 1990)

#Sum the number of units (TOTAL_UNIT) per cluster id:
#First, add cluster ID to hud data
hud_90$cluster_id = apply(st_intersects(DFsf_GE, hud_90, sparse = FALSE), 2, 
                       function(col) { 
                         DFsf_GE[which(col), ]$cluster_id
                       })

#Second, sum the TOTAL_UNIT by cluster_id
#TOTAL_UNIT = "total_units_count  The number of total units associated with the participant." (see metadata)
summary_hud90 = hud_90 %>%
  group_by(cluster_id) %>%
  summarise(unitsHUD90 = sum(TOTAL_UNIT))

#Third, add the cluster_id/unit data back to DF
summary_hud90 = st_drop_geometry(summary_hud90) #remove geometry
summary_hud90 = subset(summary_hud90, cluster_id != c(563)) #remove airports (already doesn't include 536)
DF = merge(DF, summary_hud90, by.x = "cluster_id", by.y = "cluster_id", all.x = TRUE)

#Add zeros for missing data in unitsHUD90 column
DF[c('unitsHUD90')][is.na(DF[c('unitsHUD90')])] = 0

#Divide the number of public housing units (TOTAL_UNIT) by the number of units in that cluster id
DF$pct_hud90 = DF$unitsHUD90/DF$hous_tot90

#-2000-#
#PERCENT HUD UNITS
#Select only those units with DOFA_ACTUA earlier than or equal to 2000
#DOFA_ACTUA = "Actual date the physical development reached the Date of Full Availability (DOFA)." (see metadata)
hud_00 = subset(hud, DOFA_YEAR <= 2000)

#Sum the number of units (TOTAL_UNIT) per cluster id:
#First, add cluster ID to hud data
hud_00$cluster_id = apply(st_intersects(DFsf_GE, hud_00, sparse = FALSE), 2, 
                       function(col) { 
                         DFsf_GE[which(col), ]$cluster_id
                       })

#Second, sum the TOTAL_UNIT by cluster_id
#TOTAL_UNIT = "total_units_count  The number of total units associated with the participant." (see metadata)
summary_hud00 = hud_00 %>%
  group_by(cluster_id) %>%
  summarise(unitsHUD00 = sum(TOTAL_UNIT))

#Third, add the cluster_id/unit data back to DF
summary_hud00 = st_drop_geometry(summary_hud00) #remove geometry
summary_hud00 = subset(summary_hud00, cluster_id != c(563)) #remove airports (already doesn't include 536)
DF = merge(DF, summary_hud00, by.x = "cluster_id", by.y = "cluster_id", all.x = TRUE)

#Add zeros for missing data in unitsHUD00 column
DF[c('unitsHUD00')][is.na(DF[c('unitsHUD00')])] = 0

#Divide the number of public housing units (TOTAL_UNIT) by the number of units in that cluster id
DF$pct_hud00 = DF$unitsHUD00/DF$hous_tot00

#-2010-#
#PERCENT HUD UNITS
#Select only those units with DOFA_ACTUA earlier than or equal to 2010
#DOFA_ACTUA = "Actual date the physical development reached the Date of Full Availability (DOFA)." (see metadata)
hud_10 = subset(hud, DOFA_YEAR <= 2010)

#Sum the number of units (TOTAL_UNIT) per cluster id:
#First, add cluster ID to hud data
hud_10$cluster_id = apply(st_intersects(DFsf_GE, hud_10, sparse = FALSE), 2, 
                       function(col) { 
                         DFsf_GE[which(col), ]$cluster_id
                       })

#Second, sum the TOTAL_UNIT by cluster_id
#TOTAL_UNIT = "total_units_count  The number of total units associated with the participant." (see metadata)
summary_hud10 = hud_10 %>%
  group_by(cluster_id) %>%
  summarise(unitsHUD10 = sum(TOTAL_UNIT))

#Third, add the cluster_id/unit data back to DF
summary_hud10 = st_drop_geometry(summary_hud10) #remove geometry
summary_hud10 = subset(summary_hud10, cluster_id != c(563)) #remove airports (already doesn't include 536)
DF = merge(DF, summary_hud10, by.x = "cluster_id", by.y = "cluster_id", all.x = TRUE)

#Add zeros for missing data in unitsHUD10 column
DF[c('unitsHUD10')][is.na(DF[c('unitsHUD10')])] = 0

#Divide the number of public housing units (TOTAL_UNIT) by the number of units in that cluster id
DF$pct_hud10 = DF$unitsHUD10/DF$hous_tot10

#-2015-#
#PERCENT HUD UNITS
#Select only those units with DOFA_ACTUA earlier than or equal to 2015
#DOFA_ACTUA = "Actual date the physical development reached the Date of Full Availability (DOFA)." (see metadata)
hud_15 = subset(hud, DOFA_YEAR <= 2015)

#Sum the number of units (TOTAL_UNIT) per cluster id:
#First, add cluster ID to hud data
hud_15$cluster_id = apply(st_intersects(DFsf_GE, hud_15, sparse = FALSE), 2, 
                       function(col) { 
                         DFsf_GE[which(col), ]$cluster_id
                       })

#Second, sum the TOTAL_UNIT by cluster_id
#TOTAL_UNIT = "total_units_count  The number of total units associated with the participant." (see metadata)
summary_hud15 = hud_15 %>%
  group_by(cluster_id) %>%
  summarise(unitsHUD15 = sum(TOTAL_UNIT))

#Third, add the cluster_id/unit data back to DF
summary_hud15 = st_drop_geometry(summary_hud15) #remove geometry
summary_hud15 = subset(summary_hud15, cluster_id != c(563)) #remove airports (already doesn't include 536)
DF = merge(DF, summary_hud15, by.x = "cluster_id", by.y = "cluster_id", all.x = TRUE)

#Add zeros for missing data in unitsHUD15 column
DF[c('unitsHUD15')][is.na(DF[c('unitsHUD15')])] = 0

#Divide the number of public housing units (TOTAL_UNIT) by the number of units in that cluster id
DF$pct_hud15 = DF$unitsHUD15/DF$hous_tot15

###---Multi-year Variables---###
#-Distance from downtown-#
DFsf = st_as_sf(DFshp) #convert to sf format for distance calc
DwnTwnsf = st_as_sf(DwnTwn)

#Calculate centroids
DFsf_cntrd = st_centroid(DFsf)
DwnTwnsf_cntrd = st_centroid(DwnTwnsf)

#Calculate distance
dist = st_distance(DFsf_cntrd,DwnTwnsf_cntrd,which="Euclidean")
distDF = as.data.frame(dist)
distDF[-2] = lapply(distDF[-2], function(x) as.numeric(sub("\\s+\\D+$", "", x))) #remove units

#Add to DF
DFsf$dwntwnM = distDF$dist

#Visualize to check that it's working:
#ggplot() + 
#  geom_sf(data=DFsf, aes(fill = dwntwnM)) +
#  ggtitle("Downtown distance") +
#  theme_void()

#-Access to rail transit-#
#Remove all columns except STATION_ID and LONGNAME
cta = cta[c("STATION_ID","LONGNAME")]
metra = metra[c("STATION_ID","LONGNAME")]

#Add a column for type of rail station
cta$TYPE = "CTA"
metra$TYPE = "METRA"

#Combine metra and cta into one rail transit layer
transit = rbind(cta,metra)

#Reproject transit to match DFsf
transit_rp = st_transform(transit,crs(DFsf_cntrd))

#Calculate distance from centroid to closest transit stop
#gDistance: https://www.rdocumentation.org/packages/rgeos/versions/0.5-5/topics/gDistance

transit_sp = as(transit_rp, "Spatial")
centroid_sp = as(DFsf_cntrd, "Spatial")
row.names(centroid_sp) = centroid_sp$cluster_id #make the cluster ID the index, used this for testing

#Transit is the rows and cluster IDs are the columns
DFsf$trnstDistM = apply(gDistance(centroid_sp, transit_sp, byid = TRUE), 2, min) #unit is m bc proj is m

#Add to DF
DFsf_m = st_drop_geometry(DFsf) #remove geometry
DF = merge(DF, DFsf_m, by.x = "cluster_id", by.y = "cluster_id", all.x = TRUE)

#Round columns
is.num = sapply(DF, is.numeric)
DF[is.num] = lapply(DF[is.num], round, 3)

# STEP 8 -----------------------------------------------
#Calculate percent change for the variables (subtract the two years) 

#--1990-2000--#
#% 2000 - % 1990

#Park greenspace
DF$pctC_prkG90_00 = (DF$pA_prkG00 - DF$pA_prkG90)
#Binary park greenspace
#1 means "increased from the beginning year". 0 means not increased or decreased.
DF$BpctC_prkG90_00 = DF$pctC_prkG90_00 > 0
DF$BpctC_prkG90_00 = as.integer(DF$BpctC_prkG90_00)

#Vacant greenspace
DF$pctC_vacG90_00 = (DF$pA_vacG00 - DF$pA_vacG90)
hist(DF$pctC_vacG90_00)
#Binary vacant greenspace
DF$BpctC_vacG90_00 = DF$pctC_vacG90_00 > 0
DF$BpctC_vacG90_00 = as.integer(DF$BpctC_vacG90_00)
hist(DF$BpctC_vacG90_00)

#All other greenspace
DF$pctC_othG90_00 = (DF$pA_othG00 - DF$pA_othG90)
#Binary all other greenspace
DF$BpctC_othG90_00 = DF$pctC_othG90_00 > 0
DF$BpctC_othG90_00 = as.integer(DF$BpctC_othG90_00)

#% Vacant housing
DF$pctC_pctV90_00 = (DF$pct_v00 - DF$pct_v90)
#Increase vacant housing (binary)
DF$B_pctV90_00 = DF$pctC_pctV90_00 > 0
DF$B_pctV90_00 = as.integer(DF$B_pctV90_00)

#% HUD units
DF$pctC_pctHUD90_00 = (DF$pct_hud00 - DF$pct_hud90)
#Increase HUD units (binary)
DF$B_pctHUD90_00 = DF$pctC_pctHUD90_00 > 0
DF$B_pctHUD90_00 = as.integer(DF$B_pctHUD90_00)

#Population density
DF$pctC_pplSqM90_00 = (DF$ppl_sqm00 - DF$ppl_sqm90)
#Increase population density (binary)
DF$B_pplSqM90_00 = DF$pctC_pplSqM90_00 > 0
DF$B_pplSqM90_00 = as.integer(DF$B_pplSqM90_00)

#% Older housing stock
DF$pctC_pct30H90_00 = (DF$pct_h30_00 - DF$pct_h30_90)
#Increase in older housing stock (binary)
DF$B_pct30H90_00 = DF$pctC_pct30H90_00 > 0
DF$B_pct30H90_00 = as.integer(DF$B_pct30H90_00)

#--2000-2010--#
#% 2010 - % 2000

#Park greenspace
DF$pctC_prkG00_10 = (DF$pA_prkG10 - DF$pA_prkG00)
#Binary park greenspace
#1 means "increased from the beginning year". 0 means not increased or decreased.
DF$BpctC_prkG00_10 = DF$pctC_prkG00_10 > 0
DF$BpctC_prkG00_10 = as.integer(DF$BpctC_prkG00_10)

#Vacant greenspace
DF$pctC_vacG00_10 = (DF$pA_vacG10 - DF$pA_vacG00)
hist(DF$pctC_vacG00_10,breaks = 50)
#Binary vacant greenspace
DF$BpctC_vacG00_10 = DF$pctC_vacG00_10 > 0
DF$BpctC_vacG00_10 = as.integer(DF$BpctC_vacG00_10)
hist(DF$BpctC_vacG00_10)

#All other greenspace
DF$pctC_othG00_10 = (DF$pA_othG10 - DF$pA_othG00)
#Binary all other greenspace
DF$BpctC_othG00_10 = DF$pctC_othG00_10 > 0
DF$BpctC_othG00_10 = as.integer(DF$BpctC_othG00_10)

#% Vacant housing
DF$pctC_pctV00_10 = (DF$pct_v10 - DF$pct_v00)
#Increase vacant housing (binary)
DF$B_pctV00_10 = DF$pctC_pctV00_10 > 0
DF$B_pctV00_10 = as.integer(DF$B_pctV00_10)

#% HUD units
DF$pctC_pctHUD00_10 = (DF$pct_hud10 - DF$pct_hud00)
#Increase HUD units (binary)
DF$B_pctHUD00_10 = DF$pctC_pctHUD00_10 > 0
DF$B_pctHUD00_10 = as.integer(DF$B_pctHUD00_10)

#Population density
DF$pctC_pplSqM00_10 = (DF$ppl_sqm10 - DF$ppl_sqm00)
#Increase population density (binary)
DF$B_pplSqM00_10 = DF$pctC_pplSqM00_10 > 0
DF$B_pplSqM00_10 = as.integer(DF$B_pplSqM00_10)

#% Older housing stock
DF$pctC_pct30H00_10 = (DF$pct_h30_10 - DF$pct_h30_00)
#Increase in older housing stock (binary)
DF$B_pct30H00_10 = DF$pctC_pct30H00_10 > 0
DF$B_pct30H00_10 = as.integer(DF$B_pct30H00_10)

#--2010-2015--#
# (% 2015 - % 2010) / %2010

#Park greenspace
DF$pctC_prkG10_15 = (DF$pA_prkG15 - DF$pA_prkG10)
#Binary park greenspace
#1 means "increased from the beginning year". 0 means not increased or decreased.
DF$BpctC_prkG10_15 = DF$pctC_prkG10_15 > 0
DF$BpctC_prkG10_15 = as.integer(DF$BpctC_prkG10_15)

#Vacant greenspace
DF$pctC_vacG10_15 = (DF$pA_vacG15 - DF$pA_vacG10) 
#plot histogram
hist(DF$pctC_vacG10_15)
#Binary vacant greenspace
DF$BpctC_vacG10_15 = DF$pctC_vacG10_15 > 0
DF$BpctC_vacG10_15 = as.integer(DF$BpctC_vacG10_15)
#plot histogram
hist(DF$BpctC_vacG10_15)

#All other greenspace
DF$pctC_othG10_15 = (DF$pA_othG15 - DF$pA_othG10)
#Binary all other greenspace
DF$BpctC_othG10_15 = DF$pctC_othG10_15 > 0
DF$BpctC_othG10_15 = as.integer(DF$BpctC_othG10_15)

#% Vacant housing
DF$pctC_pctV10_15 = (DF$pct_v15 - DF$pct_v10)
#Increase vacant housing (binary)
DF$B_pctV10_15 = DF$pctC_pctV10_15 > 0
DF$B_pctV10_15 = as.integer(DF$B_pctV10_15)

#% HUD units
DF$pctC_pctHUD10_15 = (DF$pct_hud15 - DF$pct_hud10)
#Increase HUD units (binary)
DF$B_pctHUD10_15 = DF$pctC_pctHUD10_15 > 0
DF$B_pctHUD10_15 = as.integer(DF$B_pctHUD10_15)

#Population density
DF$pctC_pplSqM10_15 = (DF$ppl_sqm15 - DF$ppl_sqm10)
#Increase population density (binary)
DF$B_pplSqM10_15 = DF$pctC_pplSqM10_15 > 0
DF$B_pplSqM10_15 = as.integer(DF$B_pplSqM10_15)

#% Older housing stock
DF$pctC_pct30H10_15 = (DF$pct_h30_15 - DF$pct_h30_10)
#Increase in older housing stock (binary)
DF$B_pct30H10_15 = DF$pctC_pct30H10_15 > 0
DF$B_pct30H10_15 = as.integer(DF$B_pct30H10_15)

#Add in 0s for missing pct_HUD values and population density
DF[c('pct_hud90','pct_hud00','pct_hud10','pctC_pctHUD90_00','pctC_pctHUD00_10',
     'pctC_pctHUD10_15')][is.na(DF[c('pct_hud90','pct_hud00','pct_hud10','pctC_pctHUD90_00','pctC_pctHUD00_10','pctC_pctHUD10_15')])] = 0

# STEP 8A -----------------------------------------------
#Set up data for exporting to make figures

# #Create a column for each year that shows what type of gentrification occurred:
# #0 = not GE,
# #1 = GE, gentrified (non-race def),
# #2 = GE, gentrified (race def),
# #3 = GE, gentrified (both defs),
# #4 = GE, did not gentrify (both defs)
# 
# #1990-2000
# DF$GType90_00 = ifelse(DF$GE90 == 0, 0, 
#                        ifelse(DF$GE90 == 1 & DF$gent90_00 == 1 & DF$gentSDR90_00 == 0, 1,
#                               ifelse(DF$GE90 == 1 & DF$gent90_00 == 0 & DF$gentSDR90_00 == 1, 2,
#                                      ifelse(DF$GE90 == 1 & DF$gent90_00 == 1 & DF$gentSDR90_00 == 1, 3,
#                                             ifelse(DF$GE90 == 1 & DF$gent90_00 == 0 & DF$gentSDR90_00 == 0, 4,
#                                                    "ERROR")))))
# #2000-2010
# DF$GType00_10 = ifelse(DF$GE00 == 0, 0, 
#                        ifelse(DF$GE00 == 1 & DF$gent00_10 == 1 & DF$gentSDR00_10 == 0, 1,
#                               ifelse(DF$GE00 == 1 & DF$gent00_10 == 0 & DF$gentSDR00_10 == 1, 2,
#                                      ifelse(DF$GE00 == 1 & DF$gent00_10 == 1 & DF$gentSDR00_10 == 1, 3,
#                                             ifelse(DF$GE00 == 1 & DF$gent00_10 == 0 & DF$gentSDR00_10 == 0, 4,
#                                                    "ERROR")))))
# #2010-2015
# DF$GType10_15 = ifelse(DF$GE10 == 0, 0, 
#                        ifelse(DF$GE10 == 1 & DF$gent10_15 == 1 & DF$gentSDR10_15 == 0, 1,
#                               ifelse(DF$GE10 == 1 & DF$gent10_15 == 0 & DF$gentSDR10_15 == 1, 2,
#                                      ifelse(DF$GE10 == 1 & DF$gent10_15 == 1 & DF$gentSDR10_15 == 1, 3,
#                                             ifelse(DF$GE10 == 1 & DF$gent10_15 == 0 & DF$gentSDR10_15 == 0, 4,
#                                                    "ERROR")))))
# 
# #Count the number of gentrification eligible census tracts that were the same across the three periods
# DF$GE_allYears = ifelse(DF$GE90 == 1 & DF$GE00 == 1 & DF$GE10 == 1, 1,
#                         0)
# sum(DF$GE_allYears)
# 
# #Create new dataframe for export, select only new variables and the greenspace variables
# figDF = DF[, c('cluster_id','GType90_00','GType00_10','GType10_15','pctC_infG90_00','pctC_formG90_00',
#                'pctC_infG00_10','pctC_formG00_10','pctC_infG10_15','pctC_formG10_15')]
# 
# #Rename to shorten column names
# oldnames_fig = c('cluster_id','GType90_00','GType00_10','GType10_15','pctC_infG90_00','pctC_formG90_00',
#              'pctC_infG00_10','pctC_formG00_10','pctC_infG10_15','pctC_formG10_15')
# newnames_fig = c('cluster_id','GType90_00','GType00_10','GType10_15','pCinf90_00','pCfrm90_00',
#              'pCinf00_10','pCfrm00_10','pCinf10_15','pCfrm10_15')
# 
# figDF = figDF %>% rename_at(vars(oldnames_fig), ~ newnames_fig)
# 
# #Descriptive stats for results section
# sum(figDF$pCinf90_00 > 0.01)
# sum(figDF$pCinf90_00 < -0.01)
# 
# sum(figDF$pCinf00_10 > 0.01)
# sum(figDF$pCinf00_10 < -0.01)
# 
# sum(figDF$pCinf10_15 > 0.01)
# sum(figDF$pCinf10_15 < -0.01)
# 
# #Increase in first period, decrease in second period, increase in third period
# sum(figDF$pCinf90_00 > 0.01 & figDF$pCinf00_10 < -0.01 & figDF$pCinf10_15 > 0.01)
# 
# #Decrease in first period, increase in second period, decrease in third period
# sum(figDF$pCinf90_00 < -0.01 & figDF$pCinf00_10 > 0.01 & figDF$pCinf10_15 < -0.01)
# 
# sum(figDF$pCfrm90_00 > 0.01)
# sum(figDF$pCfrm90_00 < -0.01)
# 
# sum(figDF$pCfrm00_10 > 0.01)
# sum(figDF$pCfrm00_10 < -0.01)
# 
# sum(figDF$pCfrm10_15 > 0.01)
# sum(figDF$pCfrm10_15 < -0.01)
# 
# #Saw change in all three periods
# sum((figDF$pCfrm90_00 > 0.01 | figDF$pCfrm90_00 < -0.01) & (figDF$pCfrm00_10 > 0.01 | figDF$pCfrm00_10 < -0.01) & (figDF$pCfrm10_15 > 0.01 | figDF$pCfrm10_15 < -0.01))
# 
# #Export DF to make figures for paper
# #write.csv(figDF, 'C:/Users/mstuhlm1/Dropbox/Envt Gentrification/Data/Combined/DF4Figures_v6_80th.csv')

# STEP 9 -----------------------------------------------
#Run logit regression

##--Subset to only gentrification eligible census tracts--##
DF_GE90 = DF[DF$GE90==TRUE,]

DF_GE00 = DF[DF$GE00==TRUE,]

DF_GE10 = DF[DF$GE10==TRUE,]

options(scipen = 999) 

#----1990-2000 GE only: GE, gentrified----
#1990-2000 Change Model with binary greenspace, % change variables replaced with binary increase. 
#Remove area of census tract, add in amount of each type of greenspace in 1990
model90_00_GE = glm(gent90_00 ~ BpctC_prkG90_00 + BpctC_vacG90_00 + BpctC_othG90_00 + dwntwnM + trnstDistM + B_pctV90_00 + B_pctHUD90_00 + B_pplSqM90_00 +
                      B_pct30H90_00 + pA_prkG90 + pA_vacG90 + pA_othG90, family = "binomial", data = DF_GE90)

summary(model90_00_GE)
OddsRatio(model90_00_GE)

#Calculate McFadden R2
PseudoR2(model90_00_GE,c("McFadden","McFaddenAdj"))
#In a footnote, McFadden (1977, p.35) wrote that "values of .2 to .4 [...] represent an excellent fit."
#The paper is available online: http://cowles.yale.edu/sites/default/files/files/pub/d04/d0474.pdf

#Determine variable importance
varImp(model90_00_GE)

#Check for multicollinearity
vif(model90_00_GE) #need VIF values to be below 5

#----1990-2000 GE only: GE, gentrified (with race)----
#1990-2000 Change Model with binary greenspace, % change variables replaced with binary increase. 
model90_00GE_R = glm(gentSDR90_00 ~ BpctC_prkG90_00 + BpctC_vacG90_00 + BpctC_othG90_00 + dwntwnM + trnstDistM + B_pctV90_00 + B_pctHUD90_00 + B_pplSqM90_00 +
                       B_pct30H90_00 + pA_prkG90 + pA_vacG90 + pA_othG90, family = "binomial", data = DF_GE90)

summary(model90_00GE_R)
OddsRatio(model90_00GE_R) #glm.fit: fitted probabilities numerically 0 or 1 occurred

model90_00GE_R_nb = glm(gentSDR90_00 ~ pctC_prkG90_00 + pctC_vacG90_00 + pctC_othG90_00 + dwntwnM + trnstDistM + pctC_pctV90_00 + pctC_pctHUD90_00 + pctC_pplSqM90_00 +
                          pctC_pct30H90_00, family = "binomial", data = DF_GE90)

summary(model90_00GE_R_nb)
OddsRatio(model90_00GE_R_nb) #don't get the fit error with all non-binary variables and the percent area of vacant removed

#Calculate McFadden R2
PseudoR2(model90_00GE_R,c("McFadden","McFaddenAdj"))

#Determine variable importance
varImp(model90_00GE_R)

#Check for multicollinearity
vif(model90_00GE_R) #need VIF values to be below 5

#----2000-2010 GE only: GE, gentrified----
#1990-2000 Change Model with binary greenspace, % change variables replaced with binary increase. 
model00_10_GE = glm(gent00_10 ~ BpctC_prkG00_10 + BpctC_vacG00_10 + BpctC_othG00_10 + dwntwnM + trnstDistM + B_pctV00_10 + B_pctHUD00_10 + B_pplSqM00_10 +
                      B_pct30H00_10 + pA_prkG00 + pA_vacG00 + pA_othG00, family = "binomial", data = DF_GE00)

summary(model00_10_GE)
OddsRatio(model00_10_GE)

#Calculate McFadden R2
PseudoR2(model00_10_GE,c("McFadden","McFaddenAdj"))

#Determine variable importance
varImp(model00_10_GE)

#Check for multicollinearity
vif(model00_10_GE)

#----2000-2010 GE only: GE, gentrified (with race)----
#1990-2000 Change Model with binary greenspace, % change variables replaced with binary increase. 
model00_10_GE_R = glm(gentSDR00_10 ~ BpctC_prkG00_10 + BpctC_vacG00_10 + BpctC_othG00_10 + dwntwnM + trnstDistM + B_pctV00_10 + B_pctHUD00_10 + B_pplSqM00_10 +
                        B_pct30H00_10 + pA_prkG00 + pA_vacG00 + pA_othG00, family = "binomial", data = DF_GE00)

summary(model00_10_GE_R)
OddsRatio(model00_10_GE_R)

#Calculate McFadden R2
PseudoR2(model00_10_GE_R,c("McFadden","McFaddenAdj"))

#Determine variable importance
varImp(model00_10_GE_R)

#Check for multicollinearity
vif(model00_10_GE_R)

#----2010-2015 GE only: GE, gentrified----
#2010-2015 Change Model with binary greenspace, % change variables replaced with binary increase
model10_15_GE = glm(gent10_15 ~ BpctC_prkG10_15 + BpctC_vacG10_15 + BpctC_othG10_15 + dwntwnM + trnstDistM + B_pctV10_15 + B_pctHUD10_15 + B_pplSqM10_15 +
                      B_pct30H10_15 + pA_prkG10 + pA_vacG10 + pA_othG10, family = "binomial", data = DF_GE10)
summary(model10_15_GE)
OddsRatio(model10_15_GE)

#Calculate McFadden R2
PseudoR2(model10_15_GE,c("McFadden","McFaddenAdj"))

#Determine variable importance
varImp(model10_15_GE)

#Check for multicollinearity
vif(model10_15_GE)

#----2010-2015 GE only: GE, gentrified (with race)----
#2010-2015 Change Model with binary greenspace, % change variables replaced with binary increase
model10_15_GE_R = glm(gentSDR10_15 ~ BpctC_prkG10_15 + BpctC_vacG10_15 + BpctC_othG10_15 + dwntwnM + trnstDistM + B_pctV10_15 + B_pctHUD10_15 + B_pplSqM10_15 +
                        B_pct30H10_15 + pA_prkG10 + pA_vacG10 + pA_othG10, family = "binomial", data = DF_GE10)
summary(model10_15_GE_R)
OddsRatio(model10_15_GE_R)

#Calculate McFadden R2
PseudoR2(model10_15_GE_R,c("McFadden","McFaddenAdj"))

#Determine variable importance
varImp(model10_15_GE_R)

#Check for multicollinearity
vif(model10_15_GE_R)

# REMOVING VACANT FROM THE MODEL -----------------------

#----1990-2000 GE only: GE, gentrified, no vacant----
#1990-2000 Change Model with binary greenspace, % change variables replaced with binary increase. 
#Remove area of census tract, add in amount of each type of greenspace in 1990
model90_00_GE_nv = glm(gent90_00 ~ BpctC_prkG90_00 + BpctC_othG90_00 + dwntwnM + trnstDistM + B_pctV90_00 + B_pctHUD90_00 + B_pplSqM90_00 +
                      B_pct30H90_00 + pA_prkG90 + pA_othG90, family = "binomial", data = DF_GE90)

summary(model90_00_GE_nv)
OddsRatio(model90_00_GE_nv)

#Calculate McFadden R2
PseudoR2(model90_00_GE_nv,c("McFadden","McFaddenAdj"))
#In a footnote, McFadden (1977, p.35) wrote that "values of .2 to .4 [...] represent an excellent fit."
#The paper is available online: http://cowles.yale.edu/sites/default/files/files/pub/d04/d0474.pdf

#Determine variable importance
varImp(model90_00_GE_nv)

#Check for multicollinearity
vif(model90_00_GE_nv) #need VIF values to be below 5

#----1990-2000 GE only: GE, gentrified (with race), no vacant----
#1990-2000 Change Model with binary greenspace, % change variables replaced with binary increase. 
model90_00GE_R_nv = glm(gentSDR90_00 ~ BpctC_prkG90_00 + BpctC_othG90_00 + dwntwnM + trnstDistM + B_pctV90_00 + B_pctHUD90_00 + B_pplSqM90_00 +
                       B_pct30H90_00 + pA_prkG90 + pA_othG90, family = "binomial", data = DF_GE90)

summary(model90_00GE_R_nv)
OddsRatio(model90_00GE_R_nv)

#Calculate McFadden R2
PseudoR2(model90_00GE_R_nv,c("McFadden","McFaddenAdj"))

#Determine variable importance
varImp(model90_00GE_R_nv)

#Check for multicollinearity
vif(model90_00GE_R_nv) #need VIF values to be below 5

#----2000-2010 GE only: GE, gentrified, no vacant----
#1990-2000 Change Model with binary greenspace, % change variables replaced with binary increase. 
model00_10_GE_nv = glm(gent00_10 ~ BpctC_prkG00_10 + BpctC_othG00_10 + dwntwnM + trnstDistM + B_pctV00_10 + B_pctHUD00_10 + B_pplSqM00_10 +
                      B_pct30H00_10 + pA_prkG00 + pA_othG00, family = "binomial", data = DF_GE00)

summary(model00_10_GE_nv)
OddsRatio(model00_10_GE_nv)

#Calculate McFadden R2
PseudoR2(model00_10_GE_nv,c("McFadden","McFaddenAdj"))

#Determine variable importance
varImp(model00_10_GE_nv)

#Check for multicollinearity
vif(model00_10_GE_nv)

#----2000-2010 GE only: GE, gentrified (with race), no vacant----
#1990-2000 Change Model with binary greenspace, % change variables replaced with binary increase. 
model00_10_GE_R_nv = glm(gentSDR00_10 ~ BpctC_prkG00_10 + BpctC_othG00_10 + dwntwnM + trnstDistM + B_pctV00_10 + B_pctHUD00_10 + B_pplSqM00_10 +
                        B_pct30H00_10 + pA_prkG00 + pA_othG00, family = "binomial", data = DF_GE00)

summary(model00_10_GE_R_nv)
OddsRatio(model00_10_GE_R_nv)

#Calculate McFadden R2
PseudoR2(model00_10_GE_R_nv,c("McFadden","McFaddenAdj"))

#Determine variable importance
varImp(model00_10_GE_R_nv)

#Check for multicollinearity
vif(model00_10_GE_R_nv)

#----2010-2015 GE only: GE, gentrified, no vacant----
#2010-2015 Change Model with binary greenspace, % change variables replaced with binary increase
model10_15_GE_nv = glm(gent10_15 ~ BpctC_prkG10_15 + BpctC_othG10_15 + dwntwnM + trnstDistM + B_pctV10_15 + B_pctHUD10_15 + B_pplSqM10_15 +
                      B_pct30H10_15 + pA_prkG10 + pA_othG10, family = "binomial", data = DF_GE10)
summary(model10_15_GE_nv)
OddsRatio(model10_15_GE_nv)

#Calculate McFadden R2
PseudoR2(model10_15_GE_nv,c("McFadden","McFaddenAdj"))

#Determine variable importance
varImp(model10_15_GE_nv)

#Check for multicollinearity
vif(model10_15_GE_nv)

#----2010-2015 GE only: GE, gentrified (with race), no vacant----
#2010-2015 Change Model with binary greenspace, % change variables replaced with binary increase
model10_15_GE_R_nv = glm(gentSDR10_15 ~ BpctC_prkG10_15 + BpctC_othG10_15 + dwntwnM + trnstDistM + B_pctV10_15 + B_pctHUD10_15 + B_pplSqM10_15 +
                        B_pct30H10_15 + pA_prkG10 + pA_othG10, family = "binomial", data = DF_GE10)
summary(model10_15_GE_R_nv)
OddsRatio(model10_15_GE_R_nv)

#Calculate McFadden R2
PseudoR2(model10_15_GE_R_nv,c("McFadden","McFaddenAdj"))

#Determine variable importance
varImp(model10_15_GE_R_nv)

#Check for multicollinearity
vif(model10_15_GE_R_nv)

# STEP 10 -----------------------------------------------
#Check if the residuals are spatially correlated
#Using https://maczokni.github.io/crimemapping_textbook_bookdown/spatial-regression-models.html as reference

#add residuals and standard deviation breaks to DF:
##--1990-2000 GE only--##
#1 deleted due to missingness = 668. Cluster_ID = 673
DF_GE90res = DF_GE90[DF_GE90$cluster_id != 673, ] #Remove 673 from DF because it was removed from logit regression

DF_GE90res$res90_00 = residuals(model90_00_GE)
DF_GE90res$res_SDbreaks90_00 = scale(DF_GE90res$res90_00)[,1]

##--2000-2010 GE only--##
#1 deleted due to missingness = 668. Cluster_ID = 673 -- [This might actually be 3 deleted due to missingness]
DF_GE90res$res00_10 = residuals(model00_10_GE)
DF_GE90res$res_SDbreaks00_10 = scale(DF_GE90res$res00_10)[,1]

##--2010-2015 GE only--##
#3 deleted due to missingness = 183, 324, 668. cluster_ID = 184, 325, 673
DF_GE10res = filter(DF_GE10, !(cluster_id %in% c(184, 325, 673)))

DF_GE10res$res10_15 = residuals(model10_15_GE)
DF_GE10res$res_SDbreaks10_15 = scale(DF_GE10res$res10_15)[,1]
#DFres2 = DFres2[c("cluster_id","res10_15","res_SDbreaks10_15")] #shorten DFres2 to only the residual columns

##--Plot GE only--##
#add to spatial DF
DFshp1_GE = merge(DFshp, DF_GE90res, by.x = "cluster_id", by.y = "cluster_id", all.y = TRUE) #merge based on cluster_id and cluster_id
DFresGE_shp = st_as_sf(DFshp1_GE) #convert to sf format for plotting

DFshp2_GE = merge(DFshp, DF_GE10res, by.x = "cluster_id", by.y = "cluster_id", all.y = TRUE) #merge based on cluster_id and cluster_id
DFresGE_shp2 = st_as_sf(DFshp2_GE) #convert to sf format for plotting

res90_00GEPlot = ggplot() + 
  geom_sf(data=DFresGE_shp, aes(fill = res90_00)) +
  ggtitle("Residuals (1990-2000), GE only") +
  theme_void()

res00_10GEPlot = ggplot() + 
  geom_sf(data=DFresGE_shp, aes(fill = res00_10)) +
  ggtitle("Residuals (2000-2010), GE only") +
  theme_void()

res10_15GEPlot = ggplot() + 
  geom_sf(data=DFresGE_shp2, aes(fill = res10_15)) +
  ggtitle("Residuals (2010-2015), GE only") +
  theme_void()