#Michelle Stuhlmacher

#Code to clean census data for analysis

#STEPS:
#1. Import data and libraries
#2. Reduce dataset to only the census tracts in study area (Cook County, IL)
#3. Clean data to make it comparable between years
#4. Subset to final outputs
#5. Combine data tables with their shapefiles
#6. Export cleaned data as a csv and shapefile

# STEP 1 -----------------------------------------------
#Import data and libraries

#Libraries
library(dplyr)
library(rgdal)
library(raster)

# STEP 2 -----------------------------------------------
#Download census data for 1990, 2000, 2010 and 2015 from NHGIS: https://www.nhgis.org/ 
#Variable List:
#1. Median household income
#2. Educational attainment
#3. Median gross rent
#4. Median home value for owner-occupied units
#5. Total population
#6. Race
#7. Ethnicity
#8. Vacant housing units
#9. Year structure built

#Reduce dataset to only the census tracts in study area (Cook County, IL)
#Dataset names:
#csv1990_cook = census data for 1990 in Cook County
#csv2000_cook = census data for 2000 in Cook County
#csv2010_cook = census data for 2010 in Cook County
#csv2015_cook = census data for 2015 in Cook County

# STEP 3 -----------------------------------------------
#Clean the data to make it comparable between years

##RACE, INCOME & RENT categories----
#Total
#White
#Black or African American
#American Indian and Alaska Native 
#Asian
#Pacific Islander (including Native Hawaiian)
#Other
#Two or more races

#1990#
#race_w: ET2001
#race_b: ET2002
#race_n: EUZ003-EUZ005
csv1990_cook$race_n = csv1990_cook$EUZ003 + csv1990_cook$EUZ004 +
  csv1990_cook$EUZ005
#race_a: EUZ006-EUZ016
csv1990_cook$race_a = csv1990_cook$EUZ006 + csv1990_cook$EUZ007 +
  csv1990_cook$EUZ008 + csv1990_cook$EUZ009 + csv1990_cook$EUZ010 +
  csv1990_cook$EUZ011 + csv1990_cook$EUZ012 + csv1990_cook$EUZ013 +
  csv1990_cook$EUZ014 + csv1990_cook$EUZ015 + csv1990_cook$EUZ016
#race_p: EUZ017-EUZ025
csv1990_cook$race_p = csv1990_cook$EUZ017 + csv1990_cook$EUZ018 +
  csv1990_cook$EUZ019 + csv1990_cook$EUZ020 + csv1990_cook$EUZ021 +
  csv1990_cook$EUZ022 + csv1990_cook$EUZ023 + csv1990_cook$EUZ024 +
  csv1990_cook$EUZ025
#race_o: NA
csv1990_cook$race_o = NA
#race_2: NA
csv1990_cook$race_2 = NA

#income: E4U001
#rent: EYU001

csv1990_cook = csv1990_cook %>% 
  rename(
    race_w = ET2001,
    race_b = ET2002,
    income = E4U001,
    rent = EYU001
  )

#2000#
#race_w: FMS001
#race_b: FMS002
#race_n: FMR003
#race_a: FMR004
#race_p: FMR005
#race_o: FMR006
#race_2: FMR007

#income: GMY001
#rent: GBO001

csv2000_cook = csv2000_cook %>% 
  rename(
    race_w = FMS001,
    race_b = FMS002,
    race_n = FMR003,
    race_a = FMR004,
    race_p = FMR005,
    race_o = FMR006,
    race_2 = FMR007,
    income = GMY001,
    rent = GBO001
  )

#2010#
#race_total: JMBE001
#race_w: JMJE003
#race_b: JMJE004
#race_n: JMBE004
#race_a: JMBE005
#race_p: JMBE006
#race_o: JMBE007
#race_2: JMBE008

csv2010_cook = csv2010_cook %>% 
  rename(
    race_total = JMBE001,
    race_w = JMJE003,
    race_b = JMJE004,
    race_n = JMBE004,
    race_a = JMBE005,
    race_p = JMBE006,
    race_o = JMBE007,
    race_2 = JMBE008,
    income = JOIE001,
    rent = JS5E001
  )

#2015#
#race_total: ADKXE001
#race_w: ADK5E003
#race_b: ADK5E004
#race_n: ADKXE004
#race_a: ADKXE005
#race_p: ADKXE006
#race_o: ADKXE007
#race_2: ADKXE008

csv2015_cook = csv2015_cook %>% 
  rename(
    race_total = ADKXE001,
    race_w = ADK5E003,
    race_b = ADK5E004,
    race_n = ADKXE004,
    race_a = ADKXE005,
    race_p = ADKXE006,
    race_o = ADKXE007,
    race_2 = ADKXE008,
    income = ADNKE001,
    rent = ADRKE001
  )


##EDUCATION categories----

#edu_total: Total
#edu_9: Less than 9th grade
#edu_12: 9th to 12th grade, no diploma
#edu_hs: High school graduate (includes equivalency)
#edu_scol: Some college, no degree
#edu_asc: Associate degree
#edu_bch: Bachelor's degree
#edu_grad: Graduate or professional degree

#1990#
csv1990_cook = csv1990_cook %>% 
  rename(
    edu_9 = E33001,
    edu_12 = E33002,
    edu_hs = E33003,
    edu_scol = E33004,
    edu_asc = E33005,
    edu_bch = E33006,
    edu_grad = E33007
  )

#2000#
#edu_9: GKT001 to GKT004 & GKT017 to GKT020
csv2000_cook$edu_9 = csv2000_cook$GKT001 + csv2000_cook$GKT002 +
  csv2000_cook$GKT003 + csv2000_cook$GKT004 + csv2000_cook$GKT017 +
  csv2000_cook$GKT018 + csv2000_cook$GKT019 + csv2000_cook$GKT020
#edu_12: GKT005, GKT006, GKT007, GKT008, GKT021, GKT022, GKT023, GKT024
csv2000_cook$edu_12 = csv2000_cook$ GKT005+ csv2000_cook$ GKT006+
  csv2000_cook$ GKT007+ csv2000_cook$ GKT008+ csv2000_cook$ GKT021+
  csv2000_cook$ GKT022+ csv2000_cook$ GKT023+ csv2000_cook$ GKT024
#edu_hs: GKT009, GKT025
csv2000_cook$edu_hs = csv2000_cook$ GKT009+ csv2000_cook$ GKT025
#edu_scol: GKT010, GKT011, GKT026, GKT027
csv2000_cook$edu_scol = csv2000_cook$ GKT010+ csv2000_cook$ GKT011+
  csv2000_cook$ GKT026+ csv2000_cook$ GKT027
#edu_asc: GKT012, GKT028
csv2000_cook$edu_asc = csv2000_cook$ GKT012+ csv2000_cook$ GKT028
#edu_bch: GKT013, GKT029
csv2000_cook$edu_bch = csv2000_cook$ GKT013+ csv2000_cook$ GKT029
#edu_grad: GKT014, GKT015, GKT016, GKT030, GKT031, GKT032
csv2000_cook$edu_grad = csv2000_cook$ GKT014 + csv2000_cook$ GKT030 +
  csv2000_cook$ GKT015 + csv2000_cook$ GKT031 + csv2000_cook$ GKT016 +
  csv2000_cook$ GKT032

#2010#
#edu_total: JN9E001
csv2010_cook$edu_total = csv2010_cook$JN9E001
#edu_9: JN9E003-JN9E006 & JN9E020-JN9E023
csv2010_cook$edu_9 = csv2010_cook$JN9E003 + csv2010_cook$JN9E020 +
  csv2010_cook$JN9E004 + csv2010_cook$JN9E005 + csv2010_cook$JN9E021 +
  csv2010_cook$JN9E022 + csv2010_cook$JN9E006 + csv2010_cook$JN9E023
#edu_12: JN9E007,JN9E008,JN9E009,JN9E010,JN9E024,JN9E025,JN9E026,JN9E027
csv2010_cook$edu_12 = csv2010_cook$JN9E007 + csv2010_cook$JN9E008 +
  csv2010_cook$JN9E009 + csv2010_cook$JN9E010 + csv2010_cook$JN9E024 +
  csv2010_cook$JN9E025 + csv2010_cook$JN9E026 + csv2010_cook$JN9E027
#edu_hs: JN9E011,JN9E028
csv2010_cook$edu_hs = csv2010_cook$JN9E011 + csv2010_cook$JN9E028
#edu_scol: JN9E012,JN9E013,JN9E029,JN9E030
csv2010_cook$edu_scol = csv2010_cook$JN9E012 + csv2010_cook$JN9E013 +
  csv2010_cook$JN9E029 + csv2010_cook$JN9E030
#edu_asc: JN9E014, JN9E031
csv2010_cook$edu_asc = csv2010_cook$JN9E014 + csv2010_cook$JN9E031
#edu_bch: JN9E015, JN9E032
csv2010_cook$edu_bch = csv2010_cook$JN9E015 + csv2010_cook$JN9E032
#edu_grad: JN9E016, JN9E033, JN9E017, JN9E034, JN9E018, JN9E035
csv2010_cook$edu_grad = csv2010_cook$JN9E016 + csv2010_cook$JN9E033 + 
  csv2010_cook$JN9E017 + csv2010_cook$JN9E034 + csv2010_cook$JN9E018 + 
  csv2010_cook$JN9E035

#2015#
#edu_total: ADMZE001
csv2015_cook$edu_total = csv2015_cook$ADMZE001
#edu_9: ADMZE002-ADMZE012
csv2015_cook$edu_9 = csv2015_cook$ADMZE002 + csv2015_cook$ADMZE003 + 
  csv2015_cook$ADMZE004 + csv2015_cook$ADMZE005 + csv2015_cook$ADMZE006 +
  csv2015_cook$ADMZE007 + csv2015_cook$ADMZE008 + csv2015_cook$ADMZE009 + 
  csv2015_cook$ADMZE010 + csv2015_cook$ADMZE011 + csv2015_cook$ADMZE012
#edu_12: ADMZE013-ADMZE016
csv2015_cook$edu_12 = csv2015_cook$ADMZE013 + csv2015_cook$ADMZE014 +
  csv2015_cook$ADMZE015 + csv2015_cook$ADMZE016
#edu_hs: ADMZE017, ADMZE018
csv2015_cook$edu_hs = csv2015_cook$ADMZE017 + csv2015_cook$ADMZE018
#edu_scol: ADMZE019, ADMZE020
csv2015_cook$edu_scol = csv2015_cook$ADMZE019 + csv2015_cook$ADMZE020
#edu_asc: ADMZE021
csv2015_cook$edu_asc = csv2015_cook$ADMZE021
#edu_bch: ADMZE022
csv2015_cook$edu_bch = csv2015_cook$ADMZE022
#edu_grad: ADMZE023-ADMZE025
csv2015_cook$edu_grad = csv2015_cook$ADMZE023 + csv2015_cook$ADMZE024 + 
  csv2015_cook$ADMZE025

##YEAR STRUCTURE BUILT categories----
#need to be able to calculate percentage of housing units older than 30 years 

#hous_30yr = Housing older than 30 years 
#hous_tot = Total housing

#1990#
#hous_30yr
csv1990_cook$hous_30yr = csv1990_cook$EX7006 + csv1990_cook$EX7007 +
  csv1990_cook$EX7008 
#hous_tot
csv1990_cook$hous_tot = csv1990_cook$EX7001 + csv1990_cook$EX7002 +
  csv1990_cook$EX7003 + csv1990_cook$EX7004 + csv1990_cook$EX7005 +
  csv1990_cook$EX7006 + csv1990_cook$EX7007 + csv1990_cook$EX7008

#2000#
#hous_30yr: GAJ006, GAJ007, GAJ008, GAJ009
csv2000_cook$hous_30yr = csv2000_cook$GAJ006 + csv2000_cook$GAJ007 +
  csv2000_cook$GAJ008 + csv2000_cook$GAJ009
#hous_tot: GAJ001, GAJ002, GAJ003, GAJ004, GAJ005, GAJ006, GAJ007, GAJ008, GAJ009
csv2000_cook$hous_tot = csv2000_cook$GAJ001 + csv2000_cook$GAJ002 +
  csv2000_cook$GAJ003 + csv2000_cook$GAJ004 + csv2000_cook$GAJ005 + 
  csv2000_cook$GAJ006 + csv2000_cook$GAJ007 + csv2000_cook$GAJ008 +
  csv2000_cook$GAJ009
  
#2010#
#hous_30yr: JSDE006, JSDE007, JSDE008, JSDE009, JSDE010
csv2010_cook$hous_30yr = csv2010_cook$JSDE006 + csv2010_cook$JSDE007 +
  csv2010_cook$JSDE008 + csv2010_cook$JSDE009 + csv2010_cook$JSDE010
#hous_tot: JSDE001
csv2010_cook$hous_tot = csv2010_cook$JSDE001

#2015#
#hous_30yr: ADQSE007, ADQSE008, ADQSE009, ADQSE010, ADQSE011 (35 yr instead of 30 because data is collected in 10yr increments)
csv2015_cook$hous_30yr = csv2015_cook$ADQSE007 + csv2015_cook$ADQSE008 + 
  csv2015_cook$ADQSE009 + csv2015_cook$ADQSE010 + csv2015_cook$ADQSE011
#hous_tot: ADQSE001
csv2015_cook$hous_tot = csv2015_cook$ADQSE001

##HISPANIC categories----
#need to be able to calculate percent Hispanic

#orgn_hisp: Population of Hispanic origin 
#orgn_tot: Total population (all)

#1990#
#orgn_hisp
csv1990_cook$orgn_hisp = csv1990_cook$ET2006 + csv1990_cook$ET2007 +
  csv1990_cook$ET2008 + csv1990_cook$ET2009 + csv1990_cook$ET2010
#orgn_tot
csv1990_cook$orgn_tot = csv1990_cook$race_w + csv1990_cook$race_b + 
  csv1990_cook$ET2003 + csv1990_cook$ET2004 + csv1990_cook$ET2005 + 
  csv1990_cook$ET2006 + csv1990_cook$ET2007 + csv1990_cook$ET2008 +
  csv1990_cook$ET2009 + csv1990_cook$ET2010

#2000#
#orgn_hisp
csv2000_cook$orgn_hisp = csv2000_cook$FMS008 + csv2000_cook$FMS009 +
  csv2000_cook$FMS010 + csv2000_cook$FMS011 + csv2000_cook$FMS012 +
  csv2000_cook$FMS013 + csv2000_cook$FMS014
#orgn_tot
csv2000_cook$orgn_tot = csv2000_cook$race_w + csv2000_cook$race_b +
  csv2000_cook$FMS003 + csv2000_cook$FMS004 + csv2000_cook$FMS005 + 
  csv2000_cook$FMS006 + csv2000_cook$FMS007 + csv2000_cook$FMS008 + 
  csv2000_cook$FMS009 + csv2000_cook$FMS010 + csv2000_cook$FMS011 + 
  csv2000_cook$FMS012 + csv2000_cook$FMS013 + csv2000_cook$FMS014

#2010#
#orgn_hisp: JMJE012
csv2010_cook$orgn_hisp = csv2010_cook$JMJE012
#orgn_tot: JMJE001
csv2010_cook$orgn_tot = csv2010_cook$JMJE001

#2015#
#orgn_hisp: ADK5E012
csv2015_cook$orgn_hisp = csv2015_cook$ADK5E012
#orgn_tot: ADK5E001
csv2015_cook$orgn_tot = csv2015_cook$ADK5E001

##HOUSING VALUE AND OCCUPANCY categories----
#need to be able to calculate percent vacant

#hous_val = median housing value
#ohous_vac = number of vacant housing units
#ohous_tot = number of housing units

#1990#
#hous_val: EST001
csv1990_cook$hous_val = csv1990_cook$EST001
#ohous_vac: ESN002
csv1990_cook$ohous_vac = csv1990_cook$ESN002
#ohous_tot: ESN001 & ESN002
csv1990_cook$ohous_tot = csv1990_cook$ESN001 + csv1990_cook$ESN002

#2000#
#hous_val: GB7001
csv2000_cook$hous_val = csv2000_cook$GB7001
#ohous_vac: FKL002
csv2000_cook$ohous_vac = csv2000_cook$FKL002
#ohous_tot: FKL001 & FKL002
csv2000_cook$ohous_tot = csv2000_cook$FKL001 + csv2000_cook$FKL002

#2010#
#hous_val: JTIE001
csv2010_cook$hous_val = csv2010_cook$JTIE001
#ohous_vac: JRJE003
csv2010_cook$ohous_vac = csv2010_cook$JRJE003
#ohous_tot: JRJE001
csv2010_cook$ohous_tot = csv2010_cook$JRJE001

#2015#
#hous_val: ADRWE001
csv2015_cook$hous_val = csv2015_cook$ADRWE001
#ohous_vac: ADPZE003
csv2015_cook$ohous_vac = csv2015_cook$ADPZE003
#ohous_tot: ADPZE001
csv2015_cook$ohous_tot = csv2015_cook$ADPZE001

# STEP 4 -----------------------------------------------

#Subset to final outputs
keep00 = c("GISJOIN","YEAR","TRACTA","race_w","race_b","race_n","race_a","race_p","race_o","race_2",
           "age0_4","age5_9","age10_14","age15_19","age20_24","age25_29","age30_34","age35_39","age40_44",
           "age45_49","age50_54","age55_59","age60_64","age65_69","age70_74","age75_79","age80_84","age85up",
           "income","rent","edu_9","edu_12","edu_hs","edu_scol","edu_asc","edu_bch","edu_grad","hous_30yr",
           "hous_tot","orgn_hisp","orgn_tot","hous_val","ohous_vac","ohous_tot")

keep = c("GISJOIN","YEAR","TRACTA","race_total","race_w","race_b","race_n","race_a","race_p","race_o","race_2",
         "age_total","age0_4","age5_9","age10_14","age15_19","age20_24","age25_29","age30_34","age35_39","age40_44",
         "age45_49","age50_54","age55_59","age60_64","age65_69","age70_74","age75_79","age80_84","age85up","income",
         "rent","edu_total","edu_9","edu_12","edu_hs","edu_scol","edu_asc","edu_bch","edu_grad","hous_30yr","hous_tot",
         "orgn_hisp","orgn_tot","hous_val","ohous_vac","ohous_tot")

csv1990_cook = csv1990_cook[ , keep00]
csv2000_cook = csv2000_cook[ , keep00]
csv2010_cook = csv2010_cook[ , keep]
csv2015_cook = csv2015_cook[ , keep]

# STEP 5 -----------------------------------------------
#Combine dfs with their shapefiles (drop all other census tracts outside of Chicago)

#1990#
#Clip using Chicago boundaries
chicago = shapefile('C:/Users/mstuhlm1/Dropbox/Envt Gentrification/Data/ChicagoBoundaries/geo_export_ec26e5d3-be74-418b-a9b1-99fad21daba3.shp')

#make sure projections match before doing the clip
proj4string(chicago)
proj4string(shp1990)
chicago = spTransform(chicago, CRS("+proj=aea +lat_0=37.5 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

#clip
shp1990_cook = shp1990[chicago, ]
export1990 = merge(shp1990_cook,csv1990_cook, by='GISJOIN')

#export
write.csv(csv1990_cook,"C:/Users/mstuhlm1/Dropbox/Envt Gentrification/Data/Census/1990/census1990.csv") #csv
shapefile(export1990,"C:/Users/mstuhlm1/Dropbox/Envt Gentrification/Data/Census/1990/SHP/census1990.shp", overwrite = T) #shp

#2000#
#clip
shp2000_cook = shp2000[chicago, ]
export2000 = merge(shp2000_cook,csv2000_cook, by='GISJOIN')

#export
write.csv(csv2000_cook,"C:/Users/mstuhlm1/Dropbox/Envt Gentrification/Data/Census/2000/census2000.csv") #csv
shapefile(export2000,"C:/Users/mstuhlm1/Dropbox/Envt Gentrification/Data/Census/2000/SHP/census2000.shp", overwrite = T) #shp

#2010#
shp2010_cook = shp2010[chicago, ]
export2010 = merge(shp2010_cook,csv2010_cook, by='GISJOIN')

#export
write.csv(csv2010_cook,"C:/Users/mstuhlm1/Dropbox/Envt Gentrification/Data/Census/2010/census2010.csv") #csv
shapefile(export2010,"C:/Users/mstuhlm1/Dropbox/Envt Gentrification/Data/Census/2010/SHP/census2010.shp", overwrite = T) #shp

#2015#
shp2015_cook = shp2015[chicago, ]
export2015 = merge(shp2015_cook,csv2015_cook, by='GISJOIN')

#export
write.csv(csv2015_cook,"C:/Users/mstuhlm1/Dropbox/Envt Gentrification/Data/Census/2015/census2015.csv") #csv
shapefile(export2015,"C:/Users/mstuhlm1/Dropbox/Envt Gentrification/Data/Census/2015/SHP/census2015.shp", overwrite = T) #shp