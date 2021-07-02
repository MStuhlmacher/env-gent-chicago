#Michelle Stuhlmacher

#Appendix code

#(A) NDVI threshold testing:
#1. Import data and libraries
#2. Plot % greenspace for each threshold by year (formal vs. informal)
#3. Calculate the smoothness of the trends

#(B) Land use consistency check:
#4. Import data and libraries
#5. Get the data into the same format so it can be compared

# STEP 1 -----------------------------------------------
#Import data and libraries

#Import libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

#Set working directory
setwd("C:/Users/mstuhlm1/Dropbox/Envt Gentrification") #work laptop

#Import threshold rmcorr csv
ndvi70th = read.csv('./Data/Combined/CensusGreenspace70th_RmcorForm_parkCorrected_v6.csv')
ndvi75th = read.csv('./Data/Combined/CensusGreenspace75th_RmcorForm_parkCorrected_v6.csv')
ndvi80th = read.csv('./Data/Combined/CensusGreenspace80th_RmcorForm_parkCorrected_v6.csv')
ndvi85th = read.csv('./Data/Combined/CensusGreenspace85th_RmcorForm_parkCorrected_v6.csv')
ndvi90th = read.csv('./Data/Combined/CensusGreenspace90th_RmcorForm_parkCorrected_v6.csv')

# STEP 2 -----------------------------------------------
#Plot % greenspace for each threshold by year (all LU groupings)

#Summarize greenspace values by year
summary_ndvi70th = ndvi70th %>%
  group_by(YEAR) %>%
  summarise(SqM = sum(areaSqM), GreenSqM = sum(GreenSqM), ParkGreenSqM = sum(ParkGreenSqM), PubGreenSqM = sum(PubGreenSqM),
            PriGreenSqM = sum(PriGreenSqM), ExcGreenSqM = sum(Exclude))

summary_ndvi75th = ndvi75th %>%
  group_by(YEAR) %>%
  summarise(SqM = sum(areaSqM), GreenSqM = sum(GreenSqM), ParkGreenSqM = sum(ParkGreenSqM), PubGreenSqM = sum(PubGreenSqM),
            PriGreenSqM = sum(PriGreenSqM), ExcGreenSqM = sum(Exclude))

summary_ndvi80th = ndvi80th %>%
  group_by(YEAR) %>%
  summarise(SqM = sum(areaSqM), GreenSqM = sum(GreenSqM), ParkGreenSqM = sum(ParkGreenSqM), PubGreenSqM = sum(PubGreenSqM),
            PriGreenSqM = sum(PriGreenSqM), ExcGreenSqM = sum(Exclude))

summary_ndvi85th = ndvi85th %>%
  group_by(YEAR) %>%
  summarise(SqM = sum(areaSqM), GreenSqM = sum(GreenSqM), ParkGreenSqM = sum(ParkGreenSqM), PubGreenSqM = sum(PubGreenSqM),
            PriGreenSqM = sum(PriGreenSqM), ExcGreenSqM = sum(Exclude))

summary_ndvi90th = ndvi90th %>%
  group_by(YEAR) %>%
  summarise(SqM = sum(areaSqM), GreenSqM = sum(GreenSqM), ParkGreenSqM = sum(ParkGreenSqM), PubGreenSqM = sum(PubGreenSqM),
            PriGreenSqM = sum(PriGreenSqM), ExcGreenSqM = sum(Exclude))

#Add column with threshold
summary_ndvi70th$Percentile = "70th"
summary_ndvi75th$Percentile = "75th"
summary_ndvi80th$Percentile = "80th"
summary_ndvi85th$Percentile = "85th"
summary_ndvi90th$Percentile = "90th"

#Combine threshold dfs
summary_ndvi = rbind(summary_ndvi70th,summary_ndvi75th,summary_ndvi80th,summary_ndvi85th,summary_ndvi90th)

#Create variables for plotting
summary_ndvi$Year = ifelse(summary_ndvi$YEAR == "1990", 1990,
                           ifelse(summary_ndvi$YEAR == "2000", 2000,
                                  ifelse(summary_ndvi$YEAR == "2006-2010", 2010,
                                         ifelse(summary_ndvi$YEAR == "2011-2015", 2015, 0))))

summary_ndvi$InfGreenSqM = summary_ndvi$PubGreenSqM + summary_ndvi$PriGreenSqM + summary_ndvi$ExcGreenSqM

#Plot by sum year and threshold
pGreenSqM = ggplot(data = summary_ndvi, aes(x = Year, y = GreenSqM, group = Percentile, color = Percentile)) +
  geom_line(size = 2)+
  ylab("Greenspace (m²)")+
  scale_color_brewer(palette = 'Greens',direction = -1)+
  theme(text=element_text(size = 15),
        legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal')+
  ggtitle("Total Greenspace")

pParkGreenSqM = ggplot(data = summary_ndvi, aes(x = Year, y = ParkGreenSqM, group = Percentile, color = Percentile)) +
  geom_line(size = 2)+
  ylab("Greenspace (m²)")+
  scale_color_brewer(palette = 'Greens',direction = -1)+
  theme(text=element_text(size = 15),
        legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal')+
  ggtitle("Formal Greenspace")

pInfGreenSqM = ggplot(data = summary_ndvi, aes(x = Year, y = InfGreenSqM, group = Percentile, color = Percentile)) +
  geom_line(size = 2)+
  ylab("Greenspace (m²)")+
  scale_color_brewer(palette = 'Greens',direction = -1)+
  theme()+
  theme(text=element_text(size = 15),
        legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +
  ggtitle("Informal Greenspace")

#Plot the three used in the paper together:
grid.arrange(pGreenSqM,pParkGreenSqM,pInfGreenSqM,ncol=3) #change to rows not columns

# STEP 3 -----------------------------------------------
#Calculate the smoothness of the trend

#Add column with threshold
ndvi70th$Percentile = "70th"
ndvi75th$Percentile = "75th"
ndvi80th$Percentile = "80th"
ndvi85th$Percentile = "85th"
ndvi90th$Percentile = "90th"

#Combine
ndviPanel = rbind(ndvi70th,ndvi75th,ndvi80th,ndvi85th,ndvi90th)

#Add year column
ndviPanel$Year = ifelse(ndviPanel$YEAR == "1990", 1990,
                        ifelse(ndviPanel$YEAR == "2000", 2000,
                               ifelse(ndviPanel$YEAR == "2006-2010", 2010,
                                      ifelse(ndviPanel$YEAR == "2011-2015", 2015, 0))))

#Define informal greenspace variables
ndviPanel$InfGreenSqM = ndviPanel$PubGreenSqM + ndviPanel$PriGreenSqM + ndviPanel$Exclude

ndviPanel$pctA_infG = ndviPanel$InfGreenSqM/ndviPanel$areaSqM #InfGreenSqM/SqM

#Calculate the standard deviation & variance
grouped = group_by(ndviPanel, Year,Percentile)
SD_DF = summarise(grouped, GreenSqM_mean = mean(GreenSqM), GreenSqM_sd = sd(GreenSqM), GreenSqM_var = var(GreenSqM),
                  ParkGreenSqM_mean = mean(ParkGreenSqM), ParkGreenSqM_sd = sd(ParkGreenSqM), ParkGreenSqM_var = var(ParkGreenSqM),
                  InfGreenSqM_mean = mean(InfGreenSqM), InfGreenSqM_sd = sd(InfGreenSqM), InfGreenSqM_var = var(InfGreenSqM),
                  pctA_G_mean = mean(pctA_G), pctA_G_sd = sd(pctA_G), pctA_G_var = var(pctA_G),
                  pctA_parkG_mean = mean(pctA_parkG), pctA_parkG_sd = sd(pctA_parkG), pctA_parkG_var = var(pctA_parkG),
                  pctA_infG_mean = mean(pctA_infG), pctA_infG_sd = sd(pctA_infG), pctA_infG_var = var(pctA_infG))

#Calculate sm and ac
#sm - Calculate scale-independent measure of smoothness (small number = smoother series)
#ac - Calculate lag-one autocorrelation (1 = smooth trend, 0 = random, -1 = jagged trend)
#Source: https://stats.stackexchange.com/questions/24607/how-to-measure-smoothness-of-a-time-series-in-r
grouped2 = group_by(ndviPanel, cluster_id,Percentile)
LAG_DF = summarise(grouped2, GreenSqM_sm = sd(diff(GreenSqM))/abs(mean(diff(GreenSqM))),
                   ParkGreenSqM_sm = sd(diff(ParkGreenSqM))/abs(mean(diff(ParkGreenSqM))),
                   InfGreenSqM_sm = sd(diff(InfGreenSqM))/abs(mean(diff(InfGreenSqM))),
                   pctA_G_sm = sd(diff(pctA_G))/abs(mean(diff(pctA_G))),
                   pctA_parkG_sm = sd(diff(pctA_parkG))/abs(mean(diff(pctA_parkG))),
                   pctA_infG_sm = sd(diff(pctA_infG))/abs(mean(diff(pctA_infG))),
                   #ac
                   GreenSqM_ac = cor(GreenSqM[-length(GreenSqM)],GreenSqM[-1]),
                   ParkGreenSqM_ac = cor(ParkGreenSqM[-length(ParkGreenSqM)],ParkGreenSqM[-1]),
                   InfGreenSqM_ac = cor(InfGreenSqM[-length(InfGreenSqM)],InfGreenSqM[-1]),
                   pctA_G_ac = cor(pctA_G[-length(pctA_G)],pctA_G[-1]),
                   pctA_parkG_ac = cor(pctA_parkG[-length(pctA_parkG)],pctA_parkG[-1]),
                   pctA_infG_ac = cor(pctA_infG[-length(pctA_infG)],pctA_infG[-1])
                   ) #Get lots of NAs and Inf for sm because of the census tracts with no greenspace

#Average the year-over-year smoothness by census tract
summary_sm = LAG_DF %>%
  group_by(Percentile) %>%
  summarise(GreenSqM_avgSm = mean(GreenSqM_sm[is.finite(GreenSqM_sm)]),
            ParkGreenSqM_avgSm = mean(ParkGreenSqM_sm[is.finite(ParkGreenSqM_sm)]),
            InfGreenSqM_avgSm = mean(InfGreenSqM_sm[is.finite(InfGreenSqM_sm)]),
            pctA_G_avgSm = mean(pctA_G_sm[is.finite(pctA_G_sm)]),
            pctA_parkG_avgSm = mean(pctA_parkG_sm[is.finite(pctA_parkG_sm)]),
            pctA_infG_avgSm = mean(pctA_infG_sm[is.finite(pctA_infG_sm)]),
            
            GreenSqM_avgAc = mean(GreenSqM_ac[is.finite(GreenSqM_ac)]),
            ParkGreenSqM_avgAc = mean(ParkGreenSqM_ac[is.finite(ParkGreenSqM_ac)]),
            InfGreenSqM_avgAc = mean(InfGreenSqM_ac[is.finite(InfGreenSqM_ac)]),
            pctA_G_avgAc = mean(pctA_G_ac[is.finite(pctA_G_ac)]),
            pctA_parkG_avgAc = mean(pctA_parkG_ac[is.finite(pctA_parkG_ac)]),
            pctA_infG_avgAc = mean(pctA_infG_ac[is.finite(pctA_infG_ac)])
            )

# STEP 4 -----------------------------------------------
#Import data and libraries
library(raster)
library(sf)
library(rgeos)

#Import CMAP land use clipped to megatract boundary
LU90 = shapefile('./Data/LandUse/Chicago_LandUse_1990/CMAPLandUse1990_ChicagoMT.shp')
LU00 = shapefile('./Data/LandUse/Chicago_LandUse_2000/CMAPLandUse2001_ChicagoMT.shp')
LU10 = shapefile('./Data/LandUse/Chicago_LandUse_2010/CMAPLandUse2010_ChicagoMT.shp')
LU15 = shapefile('./Data/LandUse/Chicago_LandUse_2015/CMAPLandUse2015_ChicagoMT.shp')

# STEP 5 -----------------------------------------------
#Simplify the data and get it into the same format

#Simplify to formal and informal 
##1990
LU90$GreenType = ifelse(LU90$LANDUSE == "3110" | LU90$LANDUSE == "3120" | LU90$LANDUSE == "3130" | LU90$LANDUSE == "3210" | LU90$LANDUSE == "3220", 1, 
                                2)
LU90$CATEGORY_t = ifelse(LU90$GreenType == 1, "Formal", 
                                 ifelse(LU90$GreenType == 2, "Informal",
                                        "ERROR"))

##2000
LU00$GreenType = ifelse(LU00$LANDUSE == "3100" | LU00$LANDUSE == "3200" | LU00$LANDUSE == "3300", 1, 
                                2)
LU00$CATEGORY_t = ifelse(LU00$GreenType == 1, "Formal", 
                                 ifelse(LU00$GreenType == 2, "Informal",
                                        "ERROR"))

#2010
LU10$GreenType = ifelse(LU10$LANDUSE == "3100" | LU10$LANDUSE == "3200" | LU10$LANDUSE == "3300" | LU10$LANDUSE == "3500", 1, 
                                2)
LU10$CATEGORY_t = ifelse(LU10$GreenType == 1, "Formal", 
                                 ifelse(LU10$GreenType == 2, "Informal",
                                        "ERROR"))

#2015
LU15$GreenType = ifelse(LU15$LANDUSE == "3100" | LU15$LANDUSE == "3200" | LU15$LANDUSE == "3300" | LU15$LANDUSE == "3500", 1, 
                                2)
LU15$CATEGORY_t = ifelse(LU15$GreenType == 1, "Formal", 
                                 ifelse(LU15$GreenType == 2, "Informal",
                                        "ERROR"))

#simplify
keep = c('LANDUSE','GreenType','CATEGORY_t')
SHP90 = LU90[keep]
SHP00 = LU00[keep]
SHP10 = LU10[keep]
SHP15 = LU15[keep]

#rename columns to include year
names(SHP90) = c('LANDUSE_90','CAT_90','CAT_t_90')
names(SHP00) = c('LANDUSE_00','CAT_00','CAT_t_00')
names(SHP10) = c('LANDUSE_10','CAT_10','CAT_t_10')
names(SHP15) = c('LANDUSE_15','CAT_15','CAT_t_15')

#clean up ring self-intersection (otherwise get an error with the join)
SHP90c = gBuffer(SHP90,byid=TRUE,width=0)
SHP00c = gBuffer(SHP00,byid=TRUE,width=0)
SHP10c = gBuffer(SHP10,byid=TRUE,width=0)
SHP15c = gBuffer(SHP15,byid=TRUE,width=0)

#convert to sf format
sf90 = st_as_sf(SHP90c)
sf00 = st_as_sf(SHP00c)
sf10 = st_as_sf(SHP10c)
sf15 = st_as_sf(SHP15c)

#combine based on location
join1 = st_join(sf90,sf00,left=FALSE,largest=TRUE)
join2 = st_join(join1,sf10,left=FALSE,largest=TRUE)
join3 = st_join(join2,sf15,left=FALSE,largest=TRUE)

# STEP 6 -----------------------------------------------
#Make corrections to improve consistency for informal vs. formal

#create a loop that will remove formal oscillations
join3$CAT_t_00c = ifelse(join3$CAT_t_90 == "Formal" & join3$CAT_t_00 == "Informal" & join3$CAT_t_10 == "Formal" & join3$CAT_t_15 == "Formal", 1, 0) #more stringent
join3$CAT_t_10c = ifelse(join3$CAT_t_00 == "Formal" & join3$CAT_t_10 == "Informal" & join3$CAT_t_15 == "Formal", 1, 0) #less stringent

#things categorized as informal in 2010 but not in surrounding years
join3$CATinf_10c = ifelse(join3$CAT_t_00 == "Informal" & join3$CAT_t_10 == "Formal" & join3$CAT_t_15 == "Informal", 1, 0)

#plot the locations
plot(join3["CAT_t_00c"])
plot(join3["CAT_t_10c"])
plot(join3["CATinf_10c"])

#select only the census tracts with 2010 disagreement (less stringent) for export
export10 = join3[join3$CAT_t_10c == 1,]
plot(export10)

#Export a shapefile of just the 2010 spots with problems for comparison to the 2010 MT census tract file
st_write(export10,'./LandUse/Chicago_LandUse_2010/MisclassifiedChicagoParks_v4.shp')