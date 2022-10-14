#Michelle Stuhlmacher
#2022.10.14

#Clean Census Data (Chicago)

#Variables = 
#Population
#White
#Latino
#Educational attainment (percent of population with a bachelor's degree or higher)
#Year housing built (percent of housing older than 30yrs)
#Percent vacant housing
#Median household income
#Median gross rent
#Median home value

#STEPS:
#1. Read in data and import libraries
#2. Subset to Cook County
#3. Subset variables of interest
#4. Merge with SHP and export

# STEP 1 -----------------------------------------------
#Import data and libraries

#Libraries
library(dplyr)
library(sf)

#Set working directory
#setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Admin/2022-2023/RWorkshop/DataCode") 
setwd("INSERTYOURFILEPATHHERE") 

#Import files
csv2020 = read.csv('./nhgis0026_csv/nhgis0026_ds249_20205_tract.csv')
shp2020 = read_sf('./nhgis0026_shape/US_tract_2020.shp')

# STEP 2 -----------------------------------------------
#Subset to Cook County

csv2020_cook = subset(csv2020, STATE == "Illinois" & COUNTY == "Cook County")

# STEP 3 -----------------------------------------------
#Subset to variables of interest

#AMPVE001:    Total Population
#AMP3E003:    Not Hispanic or Latino: White alone
#AMP3E012:    Hispanic or Latino
#AMRZE022:    Bachelor's degree
#AMRZE023:    Master's degree
#AMRZE024:    Professional school degree
#AMRZE025:    Doctorate degree
#AMUEE001:    Total Housing Units
#AMUEE003:    Vacant Housing Units
#AMU7E005:    Built 1990 to 1999
#AMU7E006:    Built 1980 to 1989
#AMU7E007:    Built 1970 to 1979
#AMU7E008:    Built 1960 to 1969
#AMU7E009:    Built 1950 to 1959
#AMU7E010:    Built 1940 to 1949
#AMU7E011:    Built 1939 or earlier
#AMR8E001:    Median household income in the past 12 months (in 2020 inflation-adjusted dollars)
#AMVZE001:    Median gross rent
#AMWBE001:    Median home value (dollars)

#Add educational attainment columns
csv2020_cook$gteBachDeg = csv2020_cook$AMRZE022 + csv2020_cook$AMRZE023 + csv2020_cook$AMRZE024 + csv2020_cook$AMRZE025

#Add housing age columns
csv2020_cook$gteH30yr = csv2020_cook$AMU7E005 + csv2020_cook$AMU7E006 + csv2020_cook$AMU7E007 + csv2020_cook$AMU7E008 +
  csv2020_cook$AMU7E009 + csv2020_cook$AMU7E010 + csv2020_cook$AMU7E011

#Subset to columns of interest
csv2020_cookC = csv2020_cook[ , c("GISJOIN","YEAR","AMPVE001","AMP3E003","AMP3E012","AMUEE001","AMUEE003","AMR8E001",
                                  "AMVZE001","AMWBE001","gteBachDeg","gteH30yr")]

#Rename                                 
colnames(csv2020_cookC) = c("GISJOIN", "YEAR", "totPop","white","hisp","totHous","vacHous","income","rent","hmVal","gteBachDeg","gteH30yr")

# STEP 4 -----------------------------------------------    
#Merge with SHP and export
export2020 = inner_join(shp2020,csv2020_cookC, by='GISJOIN')

st_write(export2020, "./Data/Census/Cleaned/census2020_Chicago.shp")
                                       
