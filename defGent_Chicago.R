#Michelle Stuhlmacher

#Environmental Gentrification (Chicago)

#STEPS:
#1. Import data and libraries
#2. Create yearly census variables
#3. Create yearly tables
#4. Clean and recombine as panel data
#5. Determine GE census tracts
#6. Gentrification Definition (no race)
#7. Gentrification Definition (with race)

# STEP 1 -----------------------------------------------
#Import data and libraries

#Libraries
library(raster)
library(dplyr)

#Set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Admin/2022-2023/RWorkshop/DataCode") 
#Import megatract
MT = shapefile('./censusMultiYear.shp')

# STEP 2 -----------------------------------------------
#Create yearly census variables

#percent white (pct_w)
MT@data$pct_w = MT@data$race_w / MT@data$orgn_tot

#percent college educated (pct_cedu)
MT@data$edu_total = MT@data$edu_9 + MT@data$edu_12 + MT@data$edu_hs + MT@data$edu_scol + 
  MT@data$edu_asc + MT@data$edu_bch + MT@data$edu_grad

#percent with a 4 year college degree
MT@data$pct_cedu = (MT@data$edu_bch + MT@data$edu_grad)/MT@data$edu_total

#percent vacant housing units (pct_v)
MT@data$pct_v = MT@data$ohous_vac/MT@data$ohous_tot

#percent Hispanic origin (pct_h)
MT@data$pct_h = MT@data$orgn_hisp/MT@data$orgn_tot

#percent housing units older than 30 years (pct_h30)
MT@data$pct_h30 = MT@data$hous_30yr/MT@data$hous_tot

#Calculate the area of each census tract
#raster area function: area if each spatial object in squared meters if the CRS is longitude/latitude, or in squared map units (typically meter)
proj4string(MT)
#Our map units are m --> proj4string(tract)
MT$areaSqM = area(MT)
chicagoSqM = sum(MT@data$areaSqM)

#Calculate population density (ppl_sqm)
MT@data$ppl_sqm = MT@data$orgn_tot/MT@data$areaSqM

#Calculate population density (ppl_acre)
MT@data$ppl_acre = MT@data$orgn_tot/(MT@data$areaSqM/4046.85642)

# STEP 3 -----------------------------------------------
#Create yearly tables

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

# STEP 4 -----------------------------------------------
#Clean and recombine as panel data

#Keep only final variables
keep = c('cluster_id','income','income_inf','rent','rent_inf','pct_w','pct_cedu','hous_tot','hmVal_inf','pct_v','pct_h',
         'pct_h30','ppl_sqm','ppl_acre')
MT_1990 = MT_1990[keep]
MT_2000 = MT_2000[keep]
MT_2010 = MT_2010[keep]
MT_2015 = MT_2015[keep]

#Add year to the end of relevant variable names
names(MT_1990) = c('cluster_id','income90','income_inf90','rent90','rent_inf90','pct_w90','pct_cedu90','hous_tot90','hmVal_inf90',
                   'pct_v90','pct_h90','pct_h30_90','ppl_sqm90','ppl_acre90')

names(MT_2000) = c('cluster_id','income00','income_inf00','rent00','rent_inf00','pct_w00','pct_cedu00','hous_tot00','hmVal_inf00',
                   'pct_v00','pct_h00','pct_h30_00','ppl_sqm00','ppl_acre00')

names(MT_2010) = c('cluster_id','income10','income_inf10','rent10','rent_inf10','pct_w10','pct_cedu10','hous_tot10','hmVal_inf10',
                   'pct_v10','pct_h10','pct_h30_10','ppl_sqm10','ppl_acre10')

names(MT_2015) = c('cluster_id','income15','income_inf15','rent15','rent_inf15','pct_w15','pct_cedu15','hous_tot15','hmVal_inf15',
                   'pct_v15','pct_h15','pct_h30_15','ppl_sqm15','ppl_acre15')

#Merge on constant columns
MTpanel_1 = merge(MT_1990@data,MT_2000@data,by = 'cluster_id')
MTpanel_2 = merge(MTpanel_1,MT_2010@data,by = "cluster_id")
MTpanel = merge(MTpanel_2,MT_2015@data,by = "cluster_id")

# STEP 5 -----------------------------------------------
# Determine gentrification eligible (GE) census tracts [1990-2000, 2000-2010, and 2010-2015]

#Remove airports
DF = filter(MTpanel, !(cluster_id %in% c(563, 536)))

#GE definition from Rigolon and Nemeth (2020), pg. 7
#Median household income below the city's median

#Using the first year of the three time periods to determine GE like they did (pdf pg. 9):
##--1990--##
medInc90 = median(DF$income90) #calculate median income
DF$GE90 = DF$income90 < medInc90 #create boolean column

##--2000--##
medInc00 = median(DF$income00) #calculate median income
DF$GE00 = DF$income00 < medInc00 #create boolean column

##--2010--##
medInc10 = median(DF$income10) #calculate median income
DF$GE10 = DF$income10 < medInc10 #create boolean column

#Should be 391 that are gentrification eligible (i.e., half)

# STEP 6 -----------------------------------------------
# Identify gentrifying census tracts from GE census tracts [1990-2000]

#####Mean Gentrification Definition (no race)#####
#0. Gentrification eligible
#1. Change in median household income > city (absolute value)
#2. Change in % college educated > city (percentage points)
#3a. Change in Median Gross Rent > city (absolute value) OR
#3b. Change in Median Home Value > city (absolute value)

##--1990-2000--##
#1. Change in median household income >  city mean (absolute value)
DF$pct_inc_delt90 = DF$income_inf00 - DF$income_inf90
cityMean_inc90 = mean(DF$pct_inc_delt90, na.rm = TRUE)
DF$delt_inc90 = DF$pct_inc_delt90 > cityMean_inc90

#2. Change in % college educated > city mean (percentage points)
DF$pct_cedu_delt90 = DF$pct_cedu00 - DF$pct_cedu90 
cityMean_cedu90 = mean(DF$pct_cedu_delt90, na.rm = TRUE)
DF$delt_cedu90 = DF$pct_cedu_delt90 > cityMean_cedu90

#3. Change in housing costs:
#a. Change in Median Gross Rent > city mean (absolute value)
DF$rent_delt90 = DF$rent_inf00 - DF$rent_inf90
cityMean_rent90 = mean(DF$rent_delt90, na.rm = TRUE)
DF$delt_rent90 = DF$rent_delt90 > cityMean_rent90

#b. Change in Median Home Value > city mean (absolute value)
DF$hval_delt90 = DF$hmVal_inf00 - DF$hmVal_inf90
cityMean_hval = mean(DF$hval_delt90, na.rm = TRUE)
DF$delt_hval90 = DF$hval_delt90 > cityMean_hval

#Determine which census tracts have a TRUE for 1, 2, 3 (a or b) & 4 (a or b)
DF$gent90_00 = ifelse(DF$GE90 == TRUE & DF$delt_inc90 == TRUE & DF$delt_cedu90 == TRUE & 
                        (DF$delt_rent90 == TRUE | DF$delt_hval90 == TRUE), 1, 0)
#check
DF %>% group_by(gent90_00) %>% summarize(num_of_gent = n())
#77 gentrifying census tracts

#Set as factor
DF$gent90_00 = as.factor(DF$gent90_00)


# STEP 7 -----------------------------------------------
# Identify gentrifying census tracts from GE census tracts [1990-2000]

#####Mean Gentrification Definition (with race)#####
#0. Gentrification eligible (median household income below the city's median) AND
#1a. Increase in % Non-Hispanic White Population OR
#1b. Decrease in % Hispanic Population AND
#2a. Change in Median Gross Rent > city change OR
#2b. Change in Median Home Value > city change

##--1990-2000--##
#1a. One standard deviation increase in the non-Hispanic % white OR
DF$pct_w_delt90 = DF$pct_w00 - DF$pct_w90
cityMean_w90 = mean(DF$pct_w_delt90, na.rm = TRUE)
citySD_w90 = sd(DF$pct_w_delt90)
DF$deltSD_w90 = DF$pct_w_delt90 > (cityMean_w90 + citySD_w90)

#1b. Decrease in % hispanic 
DF$pct_h_delt90 = DF$pct_h00 - DF$pct_h90
cityMean_h90 = mean(DF$pct_h_delt90, na.rm = TRUE)
citySD_h90 = sd(DF$pct_h_delt90)
DF$deltSD_h90 = DF$pct_h_delt90 < (cityMean_h90 - citySD_h90)

#Increase greater than 1 SD:
DF$gentSDR90_00 = ifelse(DF$GE90 == TRUE & (DF$deltSD_w90 == TRUE | DF$deltSD_h90 == TRUE) &
                           (DF$delt_rent90 == TRUE | DF$delt_hval90 == TRUE), 1, 0)
#check
DF %>% group_by(gentSDR90_00) %>% summarize(num_of_gent = n())
#41 gentrifying census tracts

DF$gentSDR90_00 = as.factor(DF$gentSDR90_00) #Set as factor

# STEP 8 -----------------------------------------------
#Export DF
write.csv(DF,"./exportDF.csv")
