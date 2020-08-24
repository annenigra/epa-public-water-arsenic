#########------------------  Contact and database information------------------#########
##  Title:                                                                          
#   "Inequalities in public water arsenic concentrations in counties and community water systems    
#   across the United States, 2006-2011                                             
#   Contact:                                                                      
#   Anne Nigra, PhD                                                                
#   Columbia University Mailman School of Public Health                             
#   Environmental Health Sciences                                                   
#   aen2136@cumc.columbia.edu 
#
#
##  Data files required for the main analysis:
# 1. arsenic.txt: EPA SYR3 data for arsenic - available for download as indicated in code
# 2. MySDWIS: created in section 1 from individual SDWIS primacy agency files- available for download as indicated in code
# 3. ManualPWSIDs480a.csv - a manually edited Excel file that you can recreate yourself as indicated in code
# 4. ZIP_COUNTY_032010.csv and ZIP_COUNTY_122011.csv - USPS Zip code files, available for download as indicated in code
# 5. FIPScodes2010.csv - County FIPS codes, available for download as indicated in code
# 6. correctedcounties.csv - created manually, you can recreate yourself as indicated in code
# 7. co-est00int-tot.csv - Census population data, available for download as indicated in code
# 8. nhgis0001_ds123_1990_county.csv - Census public drinking water estimates, available for download as indicated in code
# 9. clustershare.csv - county sociodemographic cluster data from Wallace et al. 2019 received via email from author
### end
#### Some package and fxns ######
library(Hmisc)
library(reshape)
library(ggplot2)
library(plyr)
library(reshape2)
library(RColorBrewer)
library(colorspace)
library(openxlsx)
library(extrafont)
library(extrafontdb)
loadfonts()

# Function to convert to confident interval
convert.to.ci <- function( vector )
{
  vector <- paste0( vector[1] , " (" , vector[ 2 ] , "," , vector[ 3 ], ")" ) 
  return( vector )
}

to.prop <- function( vector )
{
  vector <- paste0( vector[1] , " / " , vector[ 2 ] ) 
  return( vector )
}

to.n.prop <- function( vector )
{
  vector <- paste0( vector[1] , " (" , vector[ 2 ],"%) " ) 
  return( vector )
}
to.mean.se <- function( vector )
{
  vector <- paste0( vector[1] , " (" , vector[ 2 ],") " ) 
  return( vector )
}
to.div.col <- function( vector )
{
  vector <- paste0( vector[1] , " / " , vector[ 2 ] ) 
  return( vector )
}
### end
####------------------ DATABASE CREATION ----------------------####

####### 1. Create a SDWIS database from online database  ######
# We downloaded individual state files as Excell files from EPA website using the Envirofacts RESTful Data Service API, which queries SDWIS data tables through a URL 
# Datafiles were downloaded using the following example script: http://iaspub.epa.gov/enviro/efservice/WATER_SYSTEM/PRIMACY_AGENCY_CODE/=/PA/EXCEL
# See: United States Environmental Protection Agency. 2017b. Web services: Envirofacts data service api. Available: https://www.epa.gov/enviro/web-services.
# Note: the larger states had to be downloaded in multiple files because the number of rows was too large for the RESTful service to handle
## States with multiple SDWIS files: CA (2), CT(2), FL(3), IL(3), MI(3), MN(3), NJ(2), NY(3), NC(3), OH(2), PA(3), TX(2), WI(3)

## First,Create 1 file for each state #
setwd("~/Google Drive/Research/County level exposure and outcomes/SDWIS data ")
## CA 
CA1 <- read.csv("CA 1.csv", stringsAsFactors = FALSE)
CA2 <- read.csv("CA 2.csv", stringsAsFactors = FALSE) #18188 matches number of rows in SDWIS data
CA <-rbind(CA1, CA2) #18188
anyDuplicated(CA$WATER_SYSTEM.PWSID) #18188
#anyDuplicated(): an integer or real vector of length one with value the 1-based index of the first duplicate if any, otherwise 0.
describe(CA$WATER_SYSTEM.PWSID)
CA <- CA[which(CA$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] #5954 community water systems in CA  (5954 total)
anyDuplicated(CA$WATER_SYSTEM.PWSID) #3057  expected unique at the end)


## Next we evaluate duplicates to see if any were duplicated in the download
## Check that duplicates are true duplicates, then remove each duplicate row 
dups <- data.frame(table(CA$WATER_SYSTEM.PWSID))
dups[dups$Freq > 1,]      #14 duplicates
# Var1 Freq
# 3645 CA3100006    2
# 3646 CA3100007    2
# 3648 CA3100009    2
# 3649 CA3100012    2
# 3651 CA3100015    2
# 3652 CA3100016    2
# 3653 CA3100017    2
# 3655 CA3100021    2
# 3656 CA3100022    2
# 3658 CA3100024    2
# 3659 CA3100025    2
# 4920 CA3600444    2
# 4921 CA3600445    2
# 4923 CA3600447    2

#This will check begin dates among duplicates
describe(CA1$WATER_SYSTEM.SEASON_BEGIN_DATE[which(CA1$WATER_SYSTEM.PWSID=="CA3600447")],)
describe(CA2$WATER_SYSTEM.SEASON_BEGIN_DATE[which(CA2$WATER_SYSTEM.PWSID=="CA3600447")],)

#This will check total pop served count among duplicates
describe(CA1$WATER_SYSTEM.POPULATION_SERVED_COUNT[which(CA1$WATER_SYSTEM.PWSID=="CA3600447")],)
describe(CA2$WATER_SYSTEM.POPULATION_SERVED_COUNT[which(CA2$WATER_SYSTEM.PWSID=="CA3600447")],)


y<-CA1[which(CA1$WATER_SYSTEM.PWSID=="CA3100006"),]
describe(y$WATER_SYSTEM.SEASON_BEGIN_DATE) #01-01
describe(y$WATER_SYSTEM.SEASON_END_DATE) #12-31
describe(y$WATER_SYSTEM.POPULATION_SERVED_COUNT) #48
describe(y$WATER_SYSTEM.PWS_DEACTIVATION_DATE) #01-Nov-93


x<-CA2[which(CA2$WATER_SYSTEM.PWSID=="CA3100006"),]
describe(x$WATER_SYSTEM.SEASON_BEGIN_DATE) #01-01
describe(x$WATER_SYSTEM.SEASON_END_DATE) #12-31
describe(x$WATER_SYSTEM.POPULATION_SERVED_COUNT) #48
describe(x$WATER_SYSTEM.PWS_DEACTIVATION_DATE)
#These are the same, okay to delete either one

#Need to evalute/look at these manually to see if they are the same dates 
# for every duplicate system


y<-CA1[which(CA1$WATER_SYSTEM.PWSID=="CA3100007"),]
x<-CA2[which(CA2$WATER_SYSTEM.PWSID=="CA3100007"),]
y$WATER_SYSTEM.COUNTIES_SERVED
x$WATER_SYSTEM.COUNTIES_SERVED
y$WATER_SYSTEM.POPULATION_SERVED_COUNT #32
x$WATER_SYSTEM.POPULATION_SERVED_COUNT

y<-CA1[which(CA1$WATER_SYSTEM.PWSID=="CA3100009"),]
x<-CA2[which(CA2$WATER_SYSTEM.PWSID=="CA3100009"),]
y$WATER_SYSTEM.COUNTIES_SERVED
x$WATER_SYSTEM.COUNTIES_SERVED
y$WATER_SYSTEM.POPULATION_SERVED_COUNT #455
x$WATER_SYSTEM.POPULATION_SERVED_COUNT

y<-CA1[which(CA1$WATER_SYSTEM.PWSID=="CA3100012"),]
x<-CA2[which(CA2$WATER_SYSTEM.PWSID=="CA3100012"),]
y$WATER_SYSTEM.COUNTIES_SERVED
x$WATER_SYSTEM.COUNTIES_SERVED
y$WATER_SYSTEM.POPULATION_SERVED_COUNT #45
x$WATER_SYSTEM.POPULATION_SERVED_COUNT

y<-CA1[which(CA1$WATER_SYSTEM.PWSID=="CA3100015"),]
x<-CA2[which(CA2$WATER_SYSTEM.PWSID=="CA3100015"),]
y$WATER_SYSTEM.COUNTIES_SERVED
x$WATER_SYSTEM.COUNTIES_SERVED
y$WATER_SYSTEM.POPULATION_SERVED_COUNT #25
x$WATER_SYSTEM.POPULATION_SERVED_COUNT

y<-CA1[which(CA1$WATER_SYSTEM.PWSID=="CA3100016"),]
x<-CA2[which(CA2$WATER_SYSTEM.PWSID=="CA3100016"),]
y$WATER_SYSTEM.COUNTIES_SERVED
x$WATER_SYSTEM.COUNTIES_SERVED
y$WATER_SYSTEM.POPULATION_SERVED_COUNT #32
x$WATER_SYSTEM.POPULATION_SERVED_COUNT

y<-CA1[which(CA1$WATER_SYSTEM.PWSID=="CA3100017"),]
x<-CA2[which(CA2$WATER_SYSTEM.PWSID=="CA3100017"),]
y$WATER_SYSTEM.COUNTIES_SERVED
x$WATER_SYSTEM.COUNTIES_SERVED
y$WATER_SYSTEM.POPULATION_SERVED_COUNT #49
x$WATER_SYSTEM.POPULATION_SERVED_COUNT

y<-CA1[which(CA1$WATER_SYSTEM.PWSID=="CA3100021"),]
x<-CA2[which(CA2$WATER_SYSTEM.PWSID=="CA3100021"),]
y$WATER_SYSTEM.COUNTIES_SERVED
x$WATER_SYSTEM.COUNTIES_SERVED
y$WATER_SYSTEM.POPULATION_SERVED_COUNT #252
x$WATER_SYSTEM.POPULATION_SERVED_COUNT

y<-CA1[which(CA1$WATER_SYSTEM.PWSID=="CA3100022"),]
x<-CA2[which(CA2$WATER_SYSTEM.PWSID=="CA3100022"),]
y$WATER_SYSTEM.COUNTIES_SERVED
x$WATER_SYSTEM.COUNTIES_SERVED
y$WATER_SYSTEM.POPULATION_SERVED_COUNT #86
x$WATER_SYSTEM.POPULATION_SERVED_COUNT

y<-CA1[which(CA1$WATER_SYSTEM.PWSID=="CA3100024"),]
x<-CA2[which(CA2$WATER_SYSTEM.PWSID=="CA3100024"),]
y$WATER_SYSTEM.COUNTIES_SERVED
x$WATER_SYSTEM.COUNTIES_SERVED
y$WATER_SYSTEM.POPULATION_SERVED_COUNT #98
x$WATER_SYSTEM.POPULATION_SERVED_COUNT

y<-CA1[which(CA1$WATER_SYSTEM.PWSID=="CA3100025"),]
x<-CA2[which(CA2$WATER_SYSTEM.PWSID=="CA3100025"),]
y$WATER_SYSTEM.COUNTIES_SERVED
x$WATER_SYSTEM.COUNTIES_SERVED
y$WATER_SYSTEM.POPULATION_SERVED_COUNT #25
x$WATER_SYSTEM.POPULATION_SERVED_COUNT

y<-CA1[which(CA1$WATER_SYSTEM.PWSID=="CA3600444"),]
x<-CA2[which(CA2$WATER_SYSTEM.PWSID=="CA3600444"),]
y$WATER_SYSTEM.COUNTIES_SERVED
x$WATER_SYSTEM.COUNTIES_SERVED
y$WATER_SYSTEM.POPULATION_SERVED_COUNT #52
x$WATER_SYSTEM.POPULATION_SERVED_COUNT

y<-CA1[which(CA1$WATER_SYSTEM.PWSID=="CA3600445"),]
x<-CA2[which(CA2$WATER_SYSTEM.PWSID=="CA3600445"),]
y$WATER_SYSTEM.COUNTIES_SERVED
x$WATER_SYSTEM.COUNTIES_SERVED
y$WATER_SYSTEM.POPULATION_SERVED_COUNT #52
x$WATER_SYSTEM.POPULATION_SERVED_COUNT

y<-CA1[which(CA1$WATER_SYSTEM.PWSID=="CA3600447"),]
x<-CA2[which(CA2$WATER_SYSTEM.PWSID=="CA3600447"),]
y$WATER_SYSTEM.COUNTIES_SERVED
x$WATER_SYSTEM.COUNTIES_SERVED
y$WATER_SYSTEM.POPULATION_SERVED_COUNT #15
x$WATER_SYSTEM.POPULATION_SERVED_COUNT

duplicated(CA)
CA[duplicated(CA), ]
CA <-CA[!duplicated(CA), ] #5940 final (had expected 2897...)

## CT ##
### States with multiple SDWIS files: CA (2), CT(2), FL(3), IL(3), MI(3), MN(3), NJ(2), NY(3), NC(3), OH(2), PA(3), TX(2), WI(3)
CT1 <- read.csv("CT 1.csv", stringsAsFactors = FALSE)
CT2 <- read.csv("CT 2.csv", stringsAsFactors = FALSE) #11069 matches number of rows in SDWIS data
CT <-rbind(CT1, CT2) 
anyDuplicated(CT$WATER_SYSTEM.PWSID) #9036
describe(CT$WATER_SYSTEM.PWSID)
CT <- CT[which(CT$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] #993 community water systems in CT
anyDuplicated(CT$WATER_SYSTEM.PWSID) #748 duplicated

### Check that duplicates are true duplicates, then remove each duplicate row ###
dups <- data.frame(table(CT$WATER_SYSTEM.PWSID))
dups[dups$Freq > 1,]      #3 duplicates
# Var1 Freq
# 6359 CT1094201    2
# 8547 CT1631031    2
# 8845 CT1680041    2
duplicated(CT)
CT[duplicated(CT), ]
CT <-CT[!duplicated(CT), ]  # Still two duplicates, CT1094201 and CT1631031
describe(CT$WATER_SYSTEM.PWSID)
CTnew <-CT[!duplicated(CT$WATER_SYSTEM.PWSID), ]
CT <- CTnew

#This will check begin dates among duplicates
describe(CT$WATER_SYSTEM.SEASON_BEGIN_DATE[which(CT$WATER_SYSTEM.PWSID=="CT1094201")],)
describe(CT$WATER_SYSTEM.SEASON_BEGIN_DATE[which(CT$WATER_SYSTEM.PWSID=="CT1631031")],)
describe(CT$WATER_SYSTEM.SEASON_BEGIN_DATE[which(CT$WATER_SYSTEM.PWSID=="CT1680041")],)

#This will check total pop served count among duplicates
describe(CT$WATER_SYSTEM.POPULATION_SERVED_COUNT[which(CT$WATER_SYSTEM.PWSID=="CT1094201")],)
describe(CT$WATER_SYSTEM.POPULATION_SERVED_COUNT[which(CT$WATER_SYSTEM.PWSID=="CT1631031")],)
describe(CT$WATER_SYSTEM.POPULATION_SERVED_COUN[which(CT$WATER_SYSTEM.PWSID=="CT1680041")],)

#Confirmed all three are identical duplicates by date and pop served count

## FL ##
### States with multiple SDWIS files: CA (2), CT(2), FL(3), IL(3), MI(3), MN(3), NJ(2), NY(3), NC(3), OH(2), PA(3), TX(2), WI(3)
FL1 <- read.csv("FL 1.csv", stringsAsFactors = FALSE)
FL2 <- read.csv("FL 2.csv", stringsAsFactors = FALSE) 
FL3 <- read.csv ("FL 3.csv", stringsAsFactors = FALSE) #19718 matches number of rows in SDWIS data
FL <-rbind(FL1, FL2, FL3) 
anyDuplicated(FL$WATER_SYSTEM.PWSID)
describe(FL$WATER_SYSTEM.PWSID)
FL <- FL[which(FL$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 4,666 community water systems in FL

## Check that duplicates are true duplicates, then remove each duplicate row ##
dups <- data.frame(table(FL$WATER_SYSTEM.PWSID))
dups[dups$Freq > 1,]      #many duplicates
# Var1 Freq
# 173  FL1370027    2
# 174  FL1370031    2
# 175  FL1370034    2
# 177  FL1370039    2
# 179  FL1370053    2
# 180  FL1370061    2
# 184  FL1370071    2
# 187  FL1370097    2
# 188  FL1370110    2
# 189  FL1370114    2
# 190  FL1370120    2
# 192  FL1370132    2
# 193  FL1370152    2
# 196  FL1370164    2
# 197  FL1370166    2
# 199  FL1370172    2
# 201  FL1370188    2
# 202  FL1370195    2
# 203  FL1370198    2
# 6190 FL6290277    2
# 6192 FL6290285    2
# 6194 FL6290299    2
# 6195 FL6290301    2
# 6203 FL6290334    2
# 6205 FL6290352    2
# 6206 FL6290365    2
# 6208 FL6290370    2
# 6316 FL6291985    2
# 6317 FL6291989    2
#This will check begin dates among duplicates
describe(FL$WATER_SYSTEM.SEASON_BEGIN_DATE[which(FL$WATER_SYSTEM.PWSID=="FL1370039")],)

#This will check total pop served count among duplicates
describe(FL$WATER_SYSTEM.POPULATION_SERVED_COUNT[which(FL$WATER_SYSTEM.PWSID=="FL1370039")],)

### NOTE ### 
# should check dupliates this way for all rows to ensure exact duplicates
# but stopping here with FL1370039
# need to do for all PWSs with duplicates across all states 

duplicated(FL)
FL[duplicated(FL), ]
FL <-FL[!duplicated(FL), ]  #
describe(FL$WATER_SYSTEM.PWSID)
FLnew <-FL[!duplicated(FL$WATER_SYSTEM.PWSID), ] # 4637
FL <- FLnew
describe(FL$WATER_SYSTEM.PWSID)

## IL ##
### States with multiple SDWIS files: CA (2), CT(2), IL(3), IL(3), MI(3), MN(3), NJ(2), NY(3), NC(3), OH(2), PA(3), TX(2), WI(3)
IL1 <- read.csv("IL 1.csv", stringsAsFactors = FALSE)
IL2 <- read.csv("IL 2.csv", stringsAsFactors = FALSE) 
IL3 <- read.csv ("IL 3.csv", stringsAsFactors = FALSE) #26742 matches number of rows in SDWIS data
IL <-rbind(IL1, IL2, IL3) 
anyDuplicated(IL$WATER_SYSTEM.PWSID)
describe(IL$WATER_SYSTEM.PWSID)
IL <- IL[which(IL$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 2,816 community water systems in IL
# no duplicates

## MI ##
### States with multiple SDWIS files: CA (2), CT(2), FL(3), IL(3), MI(3), MN(3), NJ(2), NY(3), NC(3), OH(2), PA(3), TX(2), WI(3)
MI1 <- read.csv("MI 1.csv", stringsAsFactors = FALSE)
MI2 <- read.csv("MI 2.csv", stringsAsFactors = FALSE) 
MI3 <- read.csv ("MI 3.csv", stringsAsFactors = FALSE) #25832 matches number of rows in SDWIS data
MI <-rbind(MI1, MI2, MI3) 
anyDuplicated(MI$WATER_SYSTEM.PWSID)
describe(MI$WATER_SYSTEM.PWSID)
MI <- MI[which(MI$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 2,143 community water systems in MI

### Check that duplicates are true duplicates, then remove each duplicate row ###
dups <- data.frame(table(MI$WATER_SYSTEM.PWSID))
dups[dups$Freq > 1,]      #5 duplicates
duplicated(MI)
MI[duplicated(MI), ]
MI <-MI[!duplicated(MI), ]  #
describe(MI$WATER_SYSTEM.PWSID)


## MN ##
### States with multiple SDWIS files: CA (2), CT(2), MN(3), IL(3), MI(3), MN(3), NJ(2), NY(3), NC(3), OH(2), PA(3), TX(2), WI(3)
MN1 <- read.csv("MN 1.csv", stringsAsFactors = FALSE)
MN2 <- read.csv("MN 2.csv", stringsAsFactors = FALSE) 
MN3 <- read.csv ("MN 3.csv", stringsAsFactors = FALSE) #21004 matches number of rows in SDWIS data
MN <-rbind(MN1, MN2, MN3) 
anyDuplicated(MN$WATER_SYSTEM.PWSID)
describe(MN$WATER_SYSTEM.PWSID)
MN <- MN[which(MN$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 1233 community water systems in MN

## NJ ##
### States with multiple SDWIS files: CA (2), CT(2), NJ(3), IL(3), MI(3), MN(3), NJ(2), NY(3), NC(3), OH(2), PA(3), TX(2), WI(3)
NJ1 <- read.csv("NJ 1.csv", stringsAsFactors = FALSE)
NJ2 <- read.csv("NJ 2.csv", stringsAsFactors = FALSE)  #14691 matches number of rows in SDWIS data
NJ <-rbind(NJ1, NJ2) 
anyDuplicated(NJ$WATER_SYSTEM.PWSID)
describe(NJ$WATER_SYSTEM.PWSID)
NJ <- NJ[which(NJ$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 879 community water systems in NJ

## NY ##
### States with multiple SDWIS files: CA (2), CT(2), NY(3), IL(3), MI(3), MN(3), NJ(2), NY(3), NC(3), OH(2), PA(3), TX(2), WI(3)
NY1 <- read.csv("NY 1.csv", stringsAsFactors = FALSE)
NY2 <- read.csv("NY 2.csv", stringsAsFactors = FALSE) 
NY3 <- read.csv ("NY 3.csv", stringsAsFactors = FALSE) #26018 matches number of rows in SDWIS data
NY <-rbind(NY1, NY2, NY3) 
anyDuplicated(NY$WATER_SYSTEM.PWSID)
describe(NY$WATER_SYSTEM.PWSID)
NY <- NY[which(NY$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 5149 community water systems in NY

### Check that duplicates are true duplicates, then remove each duplicate row ###
dups <- data.frame(table(NY$WATER_SYSTEM.PWSID))
dups[dups$Freq > 1,]      #4 duplicates
duplicated(NY)
NY[duplicated(NY), ]
NY <-NY[!duplicated(NY), ]  #
describe(NY$WATER_SYSTEM.PWSID)

## NC ##
### States with multiple SDWIS files: CA (2), CT(2), NC(3), IL(3), MI(3), MN(3), NJ(2), NY(3), NC(3), OH(2), PA(3), TX(2), WI(3)
NC1 <- read.csv("NC 1.csv", stringsAsFactors = FALSE)
NC2 <- read.csv("NC 2.csv", stringsAsFactors = FALSE) 
NC3 <- read.csv ("NC 3.csv", stringsAsFactors = FALSE) #23196 matches number of rows in SDWIS data
NC <-rbind(NC1, NC2, NC3) 
anyDuplicated(NC$WATER_SYSTEM.PWSID)
describe(NC$WATER_SYSTEM.PWSID)
NC <- NC[which(NC$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 6089 community water systems in NC

### Check that duplicates are true duplicates, then remove each duplicate row ###
dups <- data.frame(table(NC$WATER_SYSTEM.PWSID))
dups[dups$Freq > 1,]      #many duplicates
duplicated(NC)
NC[duplicated(NC), ]
NC <-NC[!duplicated(NC), ]  #
describe(NC$WATER_SYSTEM.PWSID)
NCnew <-NC[!duplicated(NC$WATER_SYSTEM.PWSID), ] # 6067
NC <- NCnew
describe(NC$WATER_SYSTEM.PWSID)

## OH ##
### States with multiple SDWIS files: CA (2), CT(2), OH(3), IL(3), MI(3), MN(3), NJ(2), NY(3), NC(3), OH(2), PA(3), TX(2), WI(3)
OH1 <- read.csv("OH 1.csv", stringsAsFactors = FALSE)
OH2 <- read.csv("OH 2.csv", stringsAsFactors = FALSE)  #15963 matches number of rows in SDWIS data
OH <-rbind(OH1, OH2) 
anyDuplicated(OH$WATER_SYSTEM.PWSID)
describe(OH$WATER_SYSTEM.PWSID)
OH <- OH[which(OH$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 2355 community water systems in OH

### Check that duplicates are true duplicates, then remove each duplicate row ###
dups <- data.frame(table(OH$WATER_SYSTEM.PWSID))
dups[dups$Freq > 1,]      #many duplicates
duplicated(OH)
OH[duplicated(OH), ]
OH <-OH[!duplicated(OH), ]  #
describe(OH$WATER_SYSTEM.PWSID)
OHnew <-OH[!duplicated(OH$WATER_SYSTEM.PWSID), ] # 2342
OH <- OHnew
describe(OH$WATER_SYSTEM.PWSID)

## PA ##
### States with multiple SDWIS files: CA (2), CT(2), PA(3), IL(3), MI(3), MN(3), NJ(2), NY(3), NC(3), OH(2), PA(3), TX(2), WI(3)
PA1 <- read.csv("PA 1.csv", stringsAsFactors = FALSE)
PA2 <- read.csv("PA 2.csv", stringsAsFactors = FALSE) 
PA3 <- read.csv ("PA 3.csv", stringsAsFactors = FALSE) #22506 matches number of rows in SDWIS data
PA <-rbind(PA1, PA2, PA3) 
anyDuplicated(PA$WATER_SYSTEM.PWSID)
describe(PA$WATER_SYSTEM.PWSID)
PA <- PA[which(PA$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 4310 community water systems in PA


## TX ##
### States with multiple SDWIS files: CA (2), CT(2), TX(3), IL(3), MI(3), MN(3), NJ(2), NY(3), NC(3), OH(2), PA(3), TX(2), WI(3)
TX1 <- read.csv("TX 1.csv", stringsAsFactors = FALSE)
TX2 <- read.csv("TX 2.csv", stringsAsFactors = FALSE)  #15451 matches number of rows in SDWIS data
TX <-rbind(TX1, TX2) 
anyDuplicated(TX$WATER_SYSTEM.PWSID)
describe(TX$WATER_SYSTEM.PWSID)
TX <- TX[which(TX$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 7641 community water systems in TX

## WI ##
### States with multiple SDWIS files: CA (2), CT(2), WI(3), IL(3), MI(3), MN(3), NJ(2), NY(3), NC(3), OH(2), PA(3), TX(2), WI(3)
WI1 <- read.csv("WI 1.csv", stringsAsFactors = FALSE)
WI2 <- read.csv("WI 2.csv", stringsAsFactors = FALSE) 
WI3 <- read.csv ("WI 3.csv", stringsAsFactors = FALSE) #23834 matches number of rows in SDWIS data
WI <-rbind(WI1, WI2, WI3) 
anyDuplicated(WI$WATER_SYSTEM.PWSID)
describe(WI$WATER_SYSTEM.PWSID)
WI <- WI[which(WI$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 1808 community water systems in WI

### Check that duplicates are true duplicates, then remove each duplicate row ###
dups <- data.frame(table(WI$WATER_SYSTEM.PWSID))
dups[dups$Freq > 1,]      #1 duplicates
duplicated(WI)
WI[duplicated(WI), ]
WI <-WI[!duplicated(WI), ]  #1807
describe(WI$WATER_SYSTEM.PWSID)

## LOAD OTHER STATES - these only have one file (fewer number of systems) ##
AL <- read.csv("AL.csv", stringsAsFactors = FALSE)
AL <- AL[which(AL$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 935 community water systems 

AK <- read.csv("AK.csv", stringsAsFactors = FALSE)
AK <- AK[which(AK$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 831 community water systems

AZ <- read.csv("AZ.csv", stringsAsFactors = FALSE)
AZ <- AZ[which(AZ$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 1659 community water systems

AR <- read.csv("AR.csv", stringsAsFactors = FALSE)
AR <- AR[which(AR$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 920 community water systems

CO <- read.csv("CO.csv", stringsAsFactors = FALSE)
CO <- CO[which(CO$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 831 community water systems

DE <- read.csv("DE.csv", stringsAsFactors = FALSE)
DE <- DE[which(DE$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 416 community water systems

GA <- read.csv("GA.csv", stringsAsFactors = FALSE)
GA <- GA[which(GA$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 2850 community water systems

HI <- read.csv("HI.csv", stringsAsFactors = FALSE)
HI <- HI[which(HI$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 173 community water systems

ID <- read.csv("ID.csv", stringsAsFactors = FALSE)
ID <- ID[which(ID$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 1194 community water systems

IN <- read.csv("IN.csv", stringsAsFactors = FALSE)
IN <- IN[which(IN$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 831 community water systems

IA <- read.csv("IA.csv", stringsAsFactors = FALSE)
IA <- IA[which(IA$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 1725 community water systems

KS <- read.csv("KS.csv", stringsAsFactors = FALSE)
KS <- KS[which(KS$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 1140 community water systems

KY <- read.csv("KY.csv", stringsAsFactors = FALSE)
KY <- KY[which(KY$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 870 community water systems

LA <- read.csv("LA.csv", stringsAsFactors = FALSE)
LA <- LA[which(LA$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 2194 community water systems

ME <- read.csv("ME.csv", stringsAsFactors = FALSE)
ME <- ME[which(ME$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 740 community water systems

MD <- read.csv("MD.csv", stringsAsFactors = FALSE)
MD <- MD[which(MD$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 1538 community water systems

MA <- read.csv("MA.csv", stringsAsFactors = FALSE)
MA <- MA[which(MA$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 781 community water systems

MS <- read.csv("MS.csv", stringsAsFactors = FALSE)
MS <- MS[which(MS$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 1898 community water systems

MO <- read.csv("MO.csv", stringsAsFactors = FALSE)
MO <- MO[which(MO$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 2076 community water systems

MT <- read.csv("MT.csv", stringsAsFactors = FALSE)
MT <- MT[which(MT$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 1189 community water systems

NE <- read.csv("NE.csv", stringsAsFactors = FALSE)
NE <- NE[which(NE$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 808 community water systems

NV <- read.csv("NV.csv", stringsAsFactors = FALSE)
NV <- NV[which(NV$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 457 community water systems

NH <- read.csv("NH.csv", stringsAsFactors = FALSE)
NH <- NH[which(NH$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 989 community water systems

NM <- read.csv("NM.csv", stringsAsFactors = FALSE)
NM <- NM[which(NM$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 918 community water systems

ND <- read.csv("ND.csv", stringsAsFactors = FALSE)
ND <- ND[which(ND$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 503 community water systems

OK <- read.csv("OK.csv", stringsAsFactors = FALSE)
OK <- OK[which(OK$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 1945 community water systems

OR <- read.csv("OR.csv", stringsAsFactors = FALSE)
OR <- OR[which(OR$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 1436 community water systems

RI <- read.csv("RI.csv", stringsAsFactors = FALSE)
RI <- RI[which(RI$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 161 community water systems

SC <- read.csv("SC.csv", stringsAsFactors = FALSE)
SC <- SC[which(SC$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 1884 community water systems

SD <- read.csv("SD.csv", stringsAsFactors = FALSE)
SD <- SD[which(SD$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 743 community water systems

TN <- read.csv("TN.csv", stringsAsFactors = FALSE)
TN <- TN[which(TN$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 1195 community water systems

UT <- read.csv("UT.csv", stringsAsFactors = FALSE)
UT <- UT[which(UT$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 593 community water systems

VT <- read.csv("VT.csv", stringsAsFactors = FALSE)
VT <- VT[which(VT$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 631 community water systems

VA <- read.csv("VA.csv", stringsAsFactors = FALSE)
VA <- VA[which(VA$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 2774 community water systems

WA <- read.csv("WA.csv", stringsAsFactors = FALSE)
WA <- WA[which(WA$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 3830 community water systems

WV <- read.csv("WV.csv", stringsAsFactors = FALSE)
WV <- WV[which(WV$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 1252 community water systems

WY <- read.csv("WY.csv", stringsAsFactors = FALSE)
WY <- WY[which(WY$WATER_SYSTEM.PWS_TYPE_CODE=="CWS"),] # 617 community water systems

#total of these should be 92455
### MERGE ALL STATES TOGETHER ###
USPWS <- rbind(AL, AK, AZ, AR, CA, CO, CT, DE, FL, GA, HI, ID, IL, IN, IA, KS, KY,
               LA, ME, MD, MA, MI, MN, MS, MO ,MT, NE, NV, NH, NJ, NM, NY, NC, ND,
               OH, OK, OR, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY) #doesn't include PA- issue with PA(missing a var)
# 88145 so far, 48 vars

newuspws <- USPWS[,c("WATER_SYSTEM.PWSID", "WATER_SYSTEM.PWS_NAME","WATER_SYSTEM.PRIMACY_AGENCY_CODE", "WATER_SYSTEM.PWS_ACTIVITY_CODE", 
                     "WATER_SYSTEM.PWS_TYPE_CODE", "WATER_SYSTEM.GW_SW_CODE", "WATER_SYSTEM.OWNER_TYPE_CODE","WATER_SYSTEM.POPULATION_SERVED_COUNT",
                     "WATER_SYSTEM.PRIMACY_TYPE", "WATER_SYSTEM.PRIMARY_SOURCE_CODE", "WATER_SYSTEM.SERVICE_CONNECTIONS_COUNT",
                     "WATER_SYSTEM.ADDRESS_LINE1",  "WATER_SYSTEM.ADDRESS_LINE2", "WATER_SYSTEM.CITY_NAME", "WATER_SYSTEM.ZIP_CODE", "WATER_SYSTEM.CITIES_SERVED", "WATER_SYSTEM.COUNTIES_SERVED",
                     "WATER_SYSTEM.PHONE_NUMBER", "WATER_SYSTEM.ALT_PHONE_NUMBER", "WATER_SYSTEM.STATE_CODE"  )]
# 20 vars
newPA <- PA[,c("WATER_SYSTEM.PWSID", "WATER_SYSTEM.PWS_NAME","WATER_SYSTEM.PRIMACY_AGENCY_CODE", "WATER_SYSTEM.PWS_ACTIVITY_CODE", 
               "WATER_SYSTEM.PWS_TYPE_CODE", "WATER_SYSTEM.GW_SW_CODE", "WATER_SYSTEM.OWNER_TYPE_CODE","WATER_SYSTEM.POPULATION_SERVED_COUNT",
               "WATER_SYSTEM.PRIMACY_TYPE", "WATER_SYSTEM.PRIMARY_SOURCE_CODE", "WATER_SYSTEM.SERVICE_CONNECTIONS_COUNT",
               "WATER_SYSTEM.ADDRESS_LINE1", "WATER_SYSTEM.ADDRESS_LINE2", "WATER_SYSTEM.CITY_NAME", "WATER_SYSTEM.ZIP_CODE", "WATER_SYSTEM.CITIES_SERVED", "WATER_SYSTEM.COUNTIES_SERVED",
               "WATER_SYSTEM.PHONE_NUMBER", "WATER_SYSTEM.ALT_PHONE_NUMBER",  "WATER_SYSTEM.STATE_CODE"  )]
#4310 obs, 20 vars
USPWS <- rbind(newuspws, newPA) ; dim(USPWS)#92455, 20 vars
MySDWIS<-USPWS
save("MySDWIS",file="MySDWIS")

describe(MySDWIS$WATER_SYSTEM.COUNTIES_SERVED) #1987 distinct
describe(MySDWIS$WATER_SYSTEM.PRIMACY_AGENCY_CODE)

load("MySDWIS"); dim(MySDWIS)
#[1] 92455    20

### end


####### 2. Load and pre-clean EPA Six Year Review 3 data for arsenic #########
# Load EPA Third Six Year Review Data ##
setwd("~/Google Drive/Research/EPAAsTrajectories/Data")
As6YR3importedR<-read.delim2("arsenic.txt", header = TRUE, sep = "\t", dec = ".")
save("As6YR3importedR", file="As6YR3importedR")
As6YR3<-As6YR3importedR

# Pre-clean some data ##
As6YR3$PWSID <- gsub(" ", "", As6YR3$PWSID) #Removes excess space from PWSID variable
As6YR3$Sample.Collection.Date <- gsub(" 00:00:00.000", "", As6YR3$Sample.Collection.Date) #remove timestamps
describe(As6YR3$Sample.Collection.Date)
As6YR3$Sample.Collection.Date <- substr(As6YR3$Sample.Collection.Date, 0, 10) #only keep first 10characters in character vector (keep date, not time)

# Create new variable converting [As] to ug/L ##
describe(As6YR3$Unit) # All records are reported in mg/L
table(As6YR3$Detect)
#     0      1 
# 162127 135227 

# Exclude water systems which are not community water systems (CWS) ##
describe(As6YR3$PWSID) #54845 unique PWSs, 297354 unique records
As6YR3<-As6YR3[which(As6YR3$System.Type=="C"|As6YR3$System.Type=="C   "),] ; dim(As6YR3)
table(As6YR3$System.Type)
describe(As6YR3$PWSID) #37098 unique PWSs, 230265 unique records ; excluded 67089 records from 17747 systemes 

## Notes to self from EPA's Publication: The Data Management and Quality
# Assurrance/Quality Control Process for the Third Six-Year Review
# Information Collection Rule Dataset (December 2016):
#" A “DETECT” field was added to all of the state datasets to 
#identify the results sign. Wherever the analytical result was
#greater than zero and the result sign indicated a detection, 
#then DETECT was set equal to 1, representing a detection. 
#When the analytical result was equal to zero and/or the 
#result sign indicated a non-detection, then DETECT was set 
#equal to 0 (i.e., a non-detect).
#For this analysis, all data for IOCs were converted to mg/L,

#For all samples, any detected concentrations that were greater 
#than four times the contaminant’s MCL and any that were greater 
#than 10 times the MCL were flagged and sent to states for comment.
#Any changes suggested by the states were implemented for these records.
#For the states that did not respond, all concentrations less than or 
#equal to 100 times the MCL were included; any greater than 100 times 
#the MCL were excluded. Similarly, all detected concentrations less 
#than the contaminant’s method detection limit (MDL)6 and all less than
#one tenth of the MDL were sent to states for comment and any changes 
#suggested by the states were implemented. For states that did not respond, 
#all detected concentrations greater than or equal to 1/100 of the MDL were
#included in the analysis and any with concentrations less than 1/100 of 
#the MDL were excluded.

# Who are the detects reporting values below 1ug/L?
who<-As6YR3[which(As6YR3$Detect==1),]
min(who$Value) #0.0051ug/L
anyprobs<-who[which(who$Value<0.001),] 
describe(anyprobs$Unit) #all in mg/L
describe(anyprobs$Detection.Limit.Value)
describe(anyprobs$Detection.Limit.Unit)
anyprobs$ugLdetection<-NA
anyprobs$ugLdetection[(anyprobs$Detection.Limit.Unit=="UG/L")]<-anyprobs$Detection.Limit.Value
describe(anyprobs$ugLdetection)
anyprobs$ugLdetection[(anyprobs$Detection.Limit.Unit!="UG/L")]<-anyprobs$Detection.Limit.Value*1000
describe(anyprobs$ugLdetection)
describe(anyprobs$Unit)
#We  retain these 6,190 records with detects below 1ug/L and consider them valid

### Handling non-detects
# If the system reports a LOD, we impute LOD/sqrt(2) for values below the LOD
# If a system does not report a LOD, we impute the EPA  method LOD (0.5ug/L) divided by squarert(2) = 0.35
# Any system which reports a ug/L LOD greater than 5 gets replaced wtih 0.35
# If value is non-detect (<1ug/L), it is currently recorded as NA in database

describe(As6YR3$Value)
As6YR3$ugLValue <- 0
As6YR3$ugLValue <- (As6YR3$Value)*1000

detects<-As6YR3[which(As6YR3$Detect==1),] #104,730
nondetects<-As6YR3[which(As6YR3$Detect==0),];dim(nondetects) #125,535

describe(nondetects$Detection.Limit.Code) #Indicates the type of Detection Limit reported
#in the Detection Limit Value column (e.g., the Minimum Reporting Level, Laboratory Reporting Level, etc.) 
describe(nondetects$Detection.Limit.Value)
describe(nondetects$Detection.Limit.Unit)
#some records report Detection.Limit.Codes of MRL, meaning the detection limit reported is the MRL, not the LOD

# if record in 'nondetect' does not have a value for DL limit, impute 0.35 ug/L
nondetects$ugLValue[is.na(nondetects$Detection.Limit.Value)]<-0.35
describe(nondetects$ugLValue) #112123 still not imputed

# if record in 'nondetect' does not have units for Detection limit, impute 0.35 ug/L
nondetects$Detection.Limit.Unit <- gsub(" ", "", nondetects$Detection.Limit.Unit) #Removes excess 
nondetects$ugLValue[nondetects$Detection.Limit.Unit==""]<-0.35
describe(nondetects$ugLValue) #103477 still not imputed

# if record in 'nondetect' is reporting the MRL, impute 0.35 ug/L
nondetects$ugLValue[nondetects$Detection.Limit.Code=="MRL"]<-0.35
describe(nondetects$ugLValue) #47886 still not imputed

# if record in 'nondetect' is reporting the MDL as zero, impute 0.35 ug/L
nondetects$ugLValue[nondetects$Detection.Limit.Code=="MDL"&nondetects$Detection.Limit.Value==0]<-0.35
describe(nondetects$ugLValue) #43254 still not imputed
who<-nondetects[is.na(nondetects$ugLValue),]
describe(who$Detection.Limit.Code)

# if record is missing Detection.Limit.Code, impute 0.35 ug/L (we don't know if this is a DL or MRL)
nondetects$Detection.Limit.Code <- gsub(" ", "", nondetects$Detection.Limit.Code) #Removes excess 
nondetects$ugLValue[nondetects$Detection.Limit.Code==""]<-0.35
describe(nondetects$ugLValue) #36895 still not imputed
who<-nondetects[is.na(nondetects$ugLValue),]
describe(who$Detection.Limit.Code) #All DL or MDL, so all good!
describe(who$Detection.Limit.Unit)
# The rest have a unit (mg or ug) and are the DL; we can impute these now as LOD/sqrt(2)
nondetectsfixed<-nondetects[!is.na(nondetects$ugLValue),]
who<-nondetects[is.na(nondetects$ugLValue),]

# Within 'who' Create ugLOD; these are 36,895 records with specific LODs 
who$ugLOD<-NA
who$ugLOD[who$Detection.Limit.Unit=="MG/L"]<-who$Detection.Limit.Value[who$Detection.Limit.Unit=="MG/L"]*1000
who$ugLOD[who$Detection.Limit.Unit=="UG/L"]<-who$Detection.Limit.Value[who$Detection.Limit.Unit=="UG/L"]
describe(who$ugLOD)

# A problem: 6,639 systems have ugLODs greater than or equal to 5ug/L
howmany<-who[which(who$ugLOD>=5),];dim(howmany)

# Records with LODs greater than 10ug/L likely have reported the wrong units
#Records for WV: (checked http://129.71.204.189:1977/DWWpublic/)
#Records for WI: that are greater than 10 are likely incorrect units (checked https://dnr.wi.gov/dwsviewer/)

describe(howmany$Detection.Limit.Unit)
table(howmany$State.Code)
# 339 in WV; 1204 in WI; 1 in OR; 23 in CT; 2 MT; 4,942 NC;38 AZ
# Extract one row for each PWSID
problems<-howmany[!duplicated(howmany$PWSID),]
write.csv(howmany, file="ProblemLODs.csv") 
# Any files from WV that are greater than 10?
ten<-howmany[which(howmany$ugLOD>10),] #1202
table(ten$State.Code)
who$ugLOD[who$ugLOD>10]<-who$ugLOD[who$ugLOD>10]/1000
howmany2<-who[which(who$ugLOD>=5),] #5,437
##Now we have a ug/L LOD for each record in who, assuming those >10ugL were incorrect units

who$ugLValue[is.na(who$ugLValue)]<-who$ugLOD/1.414214
describe(who$ugLValue) #0 missing
table(who$ugLOD)

#Now, for systems rporting an LOD at 5 ug/L or greater, we replace with 0.35
who$ugLValue[who$ugLOD>=5]<-0.35
who$ugLOD<-NULL
#Merge back together
nondetects<-rbind(nondetectsfixed,who)

#Merge detects back with nondetects
As6YR3<-rbind(nondetects,detects)
describe(As6YR3$ugLValue)
min(As6YR3$ugLValue)
describe(As6YR3$ugLValue)
describe(As6YR3$PWSID) # 230265 total records

# Check date format 
describe(As6YR3$Sample.Collection.Date)

# Subset to keep only necessary data ##
myvars<- c("PWSID", "System.Name", "State.Code", "System.Type","Retail.Population.Served","Adjusted.Total.Population.Served",  "Source.Water.Type",  
           "Sample.ID", "Sample.Collection.Date",  "ugLValue","Sampling.Point.Type","Source.Type.Code" )
As6YR3 <- As6YR3[myvars]
My6YR<-As6YR3

# Why no CWS records for Maryland?
md<-As6YR3importedR[which(As6YR3importedR$State.Code=="MD"),]
#View(md)
table(md$System.Type)
# they do not report any records for CWSs

# Save all records as they are to use in Table 1 to extract number of records (in 1000s) by characteristic
dat<-My6YR
### end
####### 3. Assigning mean of finished vs gross yearly mean values #########

# If a PWSID has one raw sample (As6YR3$Source.Type.Code=="RW")
# and one finished sample (As6YR3$Source.Type.Code=="FN") in the same year, keep mean of finished samples
# if the finished averages is less than raw average by 1 ug/L

## First, yearly-avearges
#Create yearly date --
As6YR3$YearlyDate<-format(As6YR3$Sample.Collection.Date, format="%Y")
class(As6YR3$YearlyDate) #remove last 6 characters
As6YR3$YearlyDate <- substr(As6YR3$YearlyDate, 0, 4) #only keep first 10characters in character vector (keep date, not time)
describe(As6YR3$YearlyDate)
table(As6YR3$YearlyDate)

# Create one dataset for each year, and pull CWSs with lower final vs. raw samples:
# 2006
Y06<-As6YR3[which(As6YR3$YearlyDate=="2006"),];Y06<-Y06[which(Y06$Source.Type.Code=="RW"|Y06$Source.Type.Code=="FN"),]
table(Y06$Source.Type.Code)
#        FN    RW     x
#    0 14819  8234     0
# Create vector for each PWSID of the average raw and average finished sample in 2006
vector.id <- c( unique(Y06$PWSID))
count06 <- as.data.frame(vector.id)
for ( i in unique(Y06$PWSID)){
  raw <- cbind ( mean(Y06$ugLValue[Y06$PWSID==i&Y06$Source.Type.Code=="RW"]) )
  fin <- cbind ( mean(Y06$ugLValue[Y06$PWSID==i&Y06$Source.Type.Code=="FN"]) )
  count06$rawvalue[count06$vector.id==i] <- raw
  count06$finvalue[count06$vector.id==i] <- fin

}
colnames(count06)[colnames(count06)=="vector.id"] <- "PWSID"
# Change NaN to "NA" values; then restrict to systems with both raw and fin available
count06$rawvalue[which(count06$rawvalue=="NaN")]<-NA
count06$finvalue[which(count06$finvalue=="NaN")]<-NA
sen06<-count06[which(!is.na(count06$rawvalue)&!is.na(count06$finvalue)),]
lower06<-sen06[which(sen06$finvalue<sen06$rawvalue),];nrow(lower06) #94 systems with lower finished values than raw values
colnames(lower06)[2] <- "rawvalue06"
colnames(lower06)[3] <- "finvalue06"

# How many CWSs reported both raw and finished samples in 2006?
describe(sen06$PWSID) #356
nms06<-as.data.frame(sen06$PWSID)

# 2007
Y07<-As6YR3[which(As6YR3$YearlyDate=="2007"),];Y07<-Y07[which(Y07$Source.Type.Code=="RW"|Y07$Source.Type.Code=="FN"),]
table(Y07$Source.Type.Code)
#      FN    RW     x
# 0 15699  9692     0
# Create vector for each PWSID of the average raw and average finished sample in 2007
vector.id <- c( unique(Y07$PWSID))
count07 <- as.data.frame(vector.id)
for ( i in unique(Y07$PWSID)){
  raw <- cbind ( mean(Y07$ugLValue[Y07$PWSID==i&Y07$Source.Type.Code=="RW"]) )
  fin <- cbind ( mean(Y07$ugLValue[Y07$PWSID==i&Y07$Source.Type.Code=="FN"]) )
  count07$rawvalue[count07$vector.id==i] <- raw
  count07$finvalue[count07$vector.id==i] <- fin

}
colnames(count07)[colnames(count07)=="vector.id"] <- "PWSID"
# Change NaN to "NA" values; then restrict to systems with both raw and fin available
count07$rawvalue[which(count07$rawvalue=="NaN")]<-NA
count07$finvalue[which(count07$finvalue=="NaN")]<-NA
sen07<-count07[which(!is.na(count07$rawvalue)&!is.na(count07$finvalue)),]
lower07<-sen07[which(sen07$finvalue<sen07$rawvalue),];nrow(lower07) #145 systems with lower finished values than raw values
colnames(lower07)[2] <- "rawvalue07"
colnames(lower07)[3] <- "finvalue07"


# How many CWSs reported both raw and finished samples in 2007?
describe(sen07$PWSID) #452
nms07<-as.data.frame(sen07$PWSID)


# 2008
Y08<-As6YR3[which(As6YR3$YearlyDate=="2008"),];Y08<-Y08[which(Y08$Source.Type.Code=="RW"|Y08$Source.Type.Code=="FN"),]
table(Y08$Source.Type.Code)
#      FN    RW     x
# 0 16451  9521     0
# Create vector for each PWSID of the average raw and average finished sample in 2008
vector.id <- c( unique(Y08$PWSID))
count08 <- as.data.frame(vector.id)
for ( i in unique(Y08$PWSID)){
  raw <- cbind ( mean(Y08$ugLValue[Y08$PWSID==i&Y08$Source.Type.Code=="RW"]) )
  fin <- cbind ( mean(Y08$ugLValue[Y08$PWSID==i&Y08$Source.Type.Code=="FN"]) )
  count08$rawvalue[count08$vector.id==i] <- raw
  count08$finvalue[count08$vector.id==i] <- fin

}
colnames(count08)[colnames(count08)=="vector.id"] <- "PWSID"
# Change NaN to "NA" values; then restrict to systems with both raw and fin available
count08$rawvalue[which(count08$rawvalue=="NaN")]<-NA
count08$finvalue[which(count08$finvalue=="NaN")]<-NA
sen08<-count08[which(!is.na(count08$rawvalue)&!is.na(count08$finvalue)),]
lower08<-sen08[which(sen08$finvalue<sen08$rawvalue),];nrow(lower08) #154 systems with lower finished values than raw values
colnames(lower08)[2] <- "rawvalue08"
colnames(lower08)[3] <- "finvalue08"


# How many CWSs reported both raw and finished samples in 2008?
describe(sen08$PWSID) #413
nms08<-as.data.frame(sen08$PWSID)


# 2009
Y09<-As6YR3[which(As6YR3$YearlyDate=="2009"),];Y09<-Y09[which(Y09$Source.Type.Code=="RW"|Y09$Source.Type.Code=="FN"),]
table(Y09$Source.Type.Code)
#      FN    RW     x
# 0 20197  9578     0
# Create vector for each PWSID of the average raw and average finished sample in 2009
vector.id <- c( unique(Y09$PWSID))
count09 <- as.data.frame(vector.id)
for ( i in unique(Y09$PWSID)){
  raw <- cbind ( mean(Y09$ugLValue[Y09$PWSID==i&Y09$Source.Type.Code=="RW"]) )
  fin <- cbind ( mean(Y09$ugLValue[Y09$PWSID==i&Y09$Source.Type.Code=="FN"]) )
  count09$rawvalue[count09$vector.id==i] <- raw
  count09$finvalue[count09$vector.id==i] <- fin

}
colnames(count09)[colnames(count09)=="vector.id"] <- "PWSID"
# Change NaN to "NA" values; then restrict to systems with both raw and fin available
count09$rawvalue[which(count09$rawvalue=="NaN")]<-NA
count09$finvalue[which(count09$finvalue=="NaN")]<-NA
sen09<-count09[which(!is.na(count09$rawvalue)&!is.na(count09$finvalue)),]
lower09<-sen09[which(sen09$finvalue<sen09$rawvalue),];nrow(lower09) #187 systems with lower finished values than raw values
colnames(lower09)[2] <- "rawvalue09"
colnames(lower09)[3] <- "finvalue09"

# How many CWSs reported both raw and finished samples in 2009?
describe(sen09$PWSID) #465
nms09<-as.data.frame(sen09$PWSID)



# 2010
Y10<-As6YR3[which(As6YR3$YearlyDate=="2010"),];Y10<-Y10[which(Y10$Source.Type.Code=="RW"|Y10$Source.Type.Code=="FN"),]
table(Y10$Source.Type.Code)
#
#      FN    RW     x
# 0 20322 10287     0
# Create vector for each PWSID of the average raw and average finished sample in 2010
vector.id <- c( unique(Y10$PWSID))
count10 <- as.data.frame(vector.id)
for ( i in unique(Y10$PWSID)){
  raw <- cbind ( mean(Y10$ugLValue[Y10$PWSID==i&Y10$Source.Type.Code=="RW"]) )
  fin <- cbind ( mean(Y10$ugLValue[Y10$PWSID==i&Y10$Source.Type.Code=="FN"]) )
  count10$rawvalue[count10$vector.id==i] <- raw
  count10$finvalue[count10$vector.id==i] <- fin

}
colnames(count10)[colnames(count10)=="vector.id"] <- "PWSID"
# Change NaN to "NA" values; then restrict to systems with both raw and fin available
count10$rawvalue[which(count10$rawvalue=="NaN")]<-NA
count10$finvalue[which(count10$finvalue=="NaN")]<-NA
sen10<-count10[which(!is.na(count10$rawvalue)&!is.na(count10$finvalue)),]
lower10<-sen10[which(sen10$finvalue<sen10$rawvalue),];nrow(lower10) #154  systems with lower finished values than raw values
colnames(lower10)[2] <- "rawvalue10"
colnames(lower10)[3] <- "finvalue10"


# How many CWSs reported both raw and finished samples in 2010?
describe(sen10$PWSID) #463
nms10<-as.data.frame(sen10$PWSID)



# 2011
Y11<-As6YR3[which(As6YR3$YearlyDate=="2011"),];Y11<-Y11[which(Y11$Source.Type.Code=="RW"|Y11$Source.Type.Code=="FN"),]
table(Y11$Source.Type.Code)
#        FN    RW     x
#    0 20642 10509     0
# Create vector for each PWSID of the average raw and average finished sample in 2011
vector.id <- c( unique(Y11$PWSID))
count11 <- as.data.frame(vector.id)
for ( i in unique(Y11$PWSID)){
  raw <- cbind ( mean(Y11$ugLValue[Y11$PWSID==i&Y11$Source.Type.Code=="RW"]) )
  fin <- cbind ( mean(Y11$ugLValue[Y11$PWSID==i&Y11$Source.Type.Code=="FN"]) )
  count11$rawvalue[count11$vector.id==i] <- raw
  count11$finvalue[count11$vector.id==i] <- fin

}
colnames(count11)[colnames(count11)=="vector.id"] <- "PWSID"
# Change NaN to "NA" values; then restrict to systems with both raw and fin available
count11$rawvalue[which(count11$rawvalue=="NaN")]<-NA
count11$finvalue[which(count11$finvalue=="NaN")]<-NA
sen11<-count11[which(!is.na(count11$rawvalue)&!is.na(count11$finvalue)),]
lower11<-sen11[which(sen11$finvalue<sen11$rawvalue),];nrow(lower11) # 153 systems with lower finished values than raw values
colnames(lower11)[2] <- "rawvalue11"
colnames(lower11)[3] <- "finvalue11"

# How many CWSs reported both raw and finished samples in 2011?
describe(sen11$PWSID) #397
nms11<-as.data.frame(sen11$PWSID)


# Total number of systems which reported both raw and finished samples for any of the 6 years:
colnames(nms06)[1] <- "PWSID"
colnames(nms07)[1] <- "PWSID"
colnames(nms08)[1] <- "PWSID"
colnames(nms09)[1] <- "PWSID"
colnames(nms10)[1] <- "PWSID"
colnames(nms11)[1] <- "PWSID"
tnms<-rbind(nms06,nms07,nms08,nms09,nms10,nms11)
tnms2<-as.data.frame(tnms[!duplicated(tnms$PWSID),]) #1182


# Save these new yearly means to replace yearly averages calculated below:
lower06$Flag06<-1
lower07$Flag07<-1
lower08$Flag08<-1
lower09$Flag09<-1
lower10$Flag10<-1
lower11$Flag11<-1

# Absolute difference between raw and finished samples
lower06$diff<-lower06$rawvalue06-lower06$finvalue06
lower07$diff<-lower07$rawvalue07-lower07$finvalue07
lower08$diff<-lower08$rawvalue08-lower08$finvalue08
lower09$diff<-lower09$rawvalue09-lower09$finvalue09
lower10$diff<-lower10$rawvalue10-lower10$finvalue10
lower11$diff<-lower11$rawvalue11-lower11$finvalue11

# Ignore those with a difference of 1 ug/L or less
nrow(lower06);lower06<-lower06[which(lower06$diff>1),];nrow(lower06) #46
nrow(lower07);lower07<-lower07[which(lower07$diff>1),];nrow(lower07) #70
nrow(lower08);lower08<-lower08[which(lower08$diff>1),];nrow(lower08) #73
nrow(lower09);lower09<-lower09[which(lower09$diff>1),];nrow(lower09) #91
nrow(lower10);lower10<-lower10[which(lower10$diff>1),];nrow(lower10) #69
nrow(lower11);lower11<-lower11[which(lower11$diff>1),];nrow(lower11) #71


## Now, lower06 through lower11 have average corrected values for PWSIDs 
# we will replace gross yearly averages with these values in the next section.


## Identify the number of raw vs finished samples for these PWSIDs
# 2006
vector.id <- c( unique(Y06$PWSID))
count06 <- as.data.frame(vector.id)
for ( i in unique(Y06$PWSID)){
  rawn <- nrow(Y06[which(Y06$PWSID==i&Y06$Source.Type.Code=="RW"),])
  finn <- nrow(Y06[which(Y06$PWSID==i&Y06$Source.Type.Code=="FN"),])
  count06$rawn[count06$vector.id==i] <- rawn
  count06$finn[count06$vector.id==i] <- finn
}
colnames(count06)[colnames(count06)=="vector.id"] <- "PWSID"

# 2007
vector.id <- c( unique(Y07$PWSID))
count07 <- as.data.frame(vector.id)
for ( i in unique(Y07$PWSID)){
  rawn <- nrow(Y07[which(Y07$PWSID==i&Y07$Source.Type.Code=="RW"),])
  finn <- nrow(Y07[which(Y07$PWSID==i&Y07$Source.Type.Code=="FN"),])
  count07$rawn[count07$vector.id==i] <- rawn
  count07$finn[count07$vector.id==i] <- finn
}
colnames(count07)[colnames(count07)=="vector.id"] <- "PWSID"

# 2008
vector.id <- c( unique(Y08$PWSID))
count08 <- as.data.frame(vector.id)
for ( i in unique(Y08$PWSID)){
  rawn <- nrow(Y08[which(Y08$PWSID==i&Y08$Source.Type.Code=="RW"),])
  finn <- nrow(Y08[which(Y08$PWSID==i&Y08$Source.Type.Code=="FN"),])
  count08$rawn[count08$vector.id==i] <- rawn
  count08$finn[count08$vector.id==i] <- finn
}
colnames(count08)[colnames(count08)=="vector.id"] <- "PWSID"

# 2009
vector.id <- c( unique(Y09$PWSID))
count09 <- as.data.frame(vector.id)
for ( i in unique(Y09$PWSID)){
  rawn <- nrow(Y09[which(Y09$PWSID==i&Y09$Source.Type.Code=="RW"),])
  finn <- nrow(Y09[which(Y09$PWSID==i&Y09$Source.Type.Code=="FN"),])
  count09$rawn[count09$vector.id==i] <- rawn
  count09$finn[count09$vector.id==i] <- finn
}
colnames(count09)[colnames(count09)=="vector.id"] <- "PWSID"

# 2010
vector.id <- c( unique(Y10$PWSID))
count10 <- as.data.frame(vector.id)
for ( i in unique(Y10$PWSID)){
  rawn <- nrow(Y10[which(Y10$PWSID==i&Y10$Source.Type.Code=="RW"),])
  finn <- nrow(Y10[which(Y10$PWSID==i&Y10$Source.Type.Code=="FN"),])
  count10$rawn[count10$vector.id==i] <- rawn
  count10$finn[count10$vector.id==i] <- finn
}
colnames(count10)[colnames(count10)=="vector.id"] <- "PWSID"

# 2011
vector.id <- c( unique(Y11$PWSID))
count11 <- as.data.frame(vector.id)
for ( i in unique(Y11$PWSID)){
  rawn <- nrow(Y11[which(Y11$PWSID==i&Y11$Source.Type.Code=="RW"),])
  finn <- nrow(Y11[which(Y11$PWSID==i&Y11$Source.Type.Code=="FN"),])
  count11$rawn[count11$vector.id==i] <- rawn
  count11$finn[count11$vector.id==i] <- finn
}
colnames(count11)[colnames(count11)=="vector.id"] <- "PWSID"

# Merge these numbers back with lower06, lower07, etc. so number is with concentration
lower06<-merge(lower06, count06,by="PWSID",all.x=T)
lower07<-merge(lower07, count07,by="PWSID",all.x=T)
lower08<-merge(lower08, count08,by="PWSID",all.x=T)
lower09<-merge(lower09, count09,by="PWSID",all.x=T)
lower10<-merge(lower10, count10,by="PWSID",all.x=T)
lower11<-merge(lower11, count11,by="PWSID",all.x=T)

## Next, repeat this for 3-yearly avearges:
#Create three-year date
As6YR3$ThreeYearlyDate<-NA
As6YR3$ThreeYearlyDate[(As6YR3$YearlyDate==2006)]<-1
As6YR3$ThreeYearlyDate[(As6YR3$YearlyDate==2007)]<-1
As6YR3$ThreeYearlyDate[(As6YR3$YearlyDate==2008)]<-1
As6YR3$ThreeYearlyDate[(As6YR3$YearlyDate==2009)]<-2
As6YR3$ThreeYearlyDate[(As6YR3$YearlyDate==2010)]<-2
As6YR3$ThreeYearlyDate[(As6YR3$YearlyDate==2011)]<-2
table(As6YR3$ThreeYearlyDate)
describe(As6YR3$ThreeYearlyDate)

# Create one dataset for each 3-year period , and pull CWSs with lower final vs. raw samples:
# Time period 1
Y063<-As6YR3[which(As6YR3$ThreeYearlyDate==1),];Y063<-Y063[which(Y063$Source.Type.Code=="RW"|Y063$Source.Type.Code=="FN"),]
table(Y063$Source.Type.Code)
#          FN    RW     x
#     0 46969 27447     0
# Create vector for each PWSID of the average raw and average finished sample in first time period
vector.id <- c( unique(Y063$PWSID))
count06 <- as.data.frame(vector.id)
for ( i in unique(Y063$PWSID)){
  raw <- cbind ( mean(Y063$ugLValue[Y063$PWSID==i&Y063$Source.Type.Code=="RW"]) )
  fin <- cbind ( mean(Y063$ugLValue[Y063$PWSID==i&Y063$Source.Type.Code=="FN"]) )
  count06$rawvalue[count06$vector.id==i] <- raw
  count06$finvalue[count06$vector.id==i] <- fin

}
colnames(count06)[colnames(count06)=="vector.id"] <- "PWSID"
# Change NaN to "NA" values; then restrict to systems with both raw and fin available
count06$rawvalue[which(count06$rawvalue=="NaN")]<-NA
count06$finvalue[which(count06$finvalue=="NaN")]<-NA
sen06<-count06[which(!is.na(count06$rawvalue)&!is.na(count06$finvalue)),]
lower063<-sen06[which(sen06$finvalue<sen06$rawvalue),];nrow(lower063) #343 systems with lower finished values than raw values
colnames(lower063)[2] <- "rawvaluetime1"
colnames(lower063)[3] <- "finvaluetime1"

# Time period 2
Y073<-As6YR3[which(As6YR3$ThreeYearlyDate==2),];Y073<-Y073[which(Y073$Source.Type.Code=="RW"|Y073$Source.Type.Code=="FN"),]
table(Y073$Source.Type.Code)
#         FN    RW     x
#    0 61161 30374     0
# Create vector for each PWSID of the average raw and average finished sample in second time period
vector.id <- c( unique(Y073$PWSID))
count07 <- as.data.frame(vector.id)
for ( i in unique(Y073$PWSID)){
  raw <- cbind ( mean(Y073$ugLValue[Y073$PWSID==i&Y073$Source.Type.Code=="RW"]) )
  fin <- cbind ( mean(Y073$ugLValue[Y073$PWSID==i&Y073$Source.Type.Code=="FN"]) )
  count07$rawvalue[count07$vector.id==i] <- raw
  count07$finvalue[count07$vector.id==i] <- fin

}
colnames(count07)[colnames(count07)=="vector.id"] <- "PWSID"
# Change NaN to "NA" values; then restrict to systems with both raw and fin available
count07$rawvalue[which(count07$rawvalue=="NaN")]<-NA
count07$finvalue[which(count07$finvalue=="NaN")]<-NA
sen07<-count07[which(!is.na(count07$rawvalue)&!is.na(count07$finvalue)),]
lower073<-sen07[which(sen07$finvalue<sen07$rawvalue),];nrow(lower073) #407 systems with lower finished values than raw values
colnames(lower073)[2] <- "rawvaluetime2"
colnames(lower073)[3] <- "finvaluetime2"

# Save these new yearly means to replace yearly averages calculated below:
lower063$Flagtime1<-1
lower073$Flagtime2<-1

# Absolute difference between raw and finished samples
lower063$diff<-lower063$rawvaluetime1-lower063$finvaluetime1
lower073$diff<-lower073$rawvaluetime2-lower073$finvaluetime2

# Ignore those with a difference of 1 ug/L or less
nrow(lower063);lower063<-lower063[which(lower063$diff>1),];nrow(lower063) #181
nrow(lower073);lower073<-lower073[which(lower073$diff>1),];nrow(lower073) #206

# How many CWSs had a lower finished value in either time period?
mm1<-as.data.frame(lower063$PWSID)
mm2<-as.data.frame(lower073$PWSID)
colnames(mm1)[colnames(mm1)=="lower063$PWSID"] <- "PWSID"
colnames(mm2)[colnames(mm2)=="lower073$PWSID"] <- "PWSID"
mm<-rbind(mm1,mm2)
describe(mm$PWSID) #336 unique PWSIDs which have 3-year averages replaced with lower (finished) sample averages


## Identify the number of raw vs finished samples for these PWSIDs
# First time period
vector.id <- c( unique(Y063$PWSID))
count <- as.data.frame(vector.id)
for ( i in unique(Y063$PWSID)){
  rawn <- nrow(Y063[which(Y063$PWSID==i&Y063$Source.Type.Code=="RW"),])
  finn <- nrow(Y063[which(Y063$PWSID==i&Y063$Source.Type.Code=="FN"),])
  count$rawn[count$vector.id==i] <- rawn
  count$finn[count$vector.id==i] <- finn
}
colnames(count)[colnames(count)=="vector.id"] <- "PWSID"

# Second time period
vector.id <- c( unique(Y073$PWSID))
count2 <- as.data.frame(vector.id)
for ( i in unique(Y073$PWSID)){
  rawn <- nrow(Y073[which(Y073$PWSID==i&Y073$Source.Type.Code=="RW"),])
  finn <- nrow(Y073[which(Y073$PWSID==i&Y073$Source.Type.Code=="FN"),])
  count2$rawn[count2$vector.id==i] <- rawn
  count2$finn[count2$vector.id==i] <- finn
}
colnames(count2)[colnames(count2)=="vector.id"] <- "PWSID"

# Merge these numbers back with lower063 and lower073
lower063<-merge(lower063, count,by="PWSID",all.x=T)
lower073<-merge(lower073, count2,by="PWSID",all.x=T)

# For yearly averages we will replace with values from lower06-lower11 as appropriate
# For three-year average we will replace with values from lower063 (time period 1) and lower073 (time period 2) as appropriate
# Six year averages should just be the mean of the two three-year time periods

##Old:  For six-year averages, use the yearly averages corrected


### end

#######   3.A. Create yearly and 3-year averages, save as "corrected" ######
### Create yearly and 3-year averages for each CWS, replacing the mean with the finished mean for these systems above^
## First for yearly averages:
t6yr<-My6YR[c("PWSID","Sample.Collection.Date","ugLValue","System.Name","State.Code","System.Type","Retail.Population.Served", "Adjusted.Total.Population.Served",
              "Source.Water.Type", "Sample.ID")]
testing<-t6yr[!duplicated(t6yr[1:2]),] #One row for each sampling day within each PWSID; 150,176

#Create yearly date --
My6YR$YearlyDate<-format(My6YR$Sample.Collection.Date, format="%Y")
class(My6YR$YearlyDate) #remove last 6 characters
My6YR$YearlyDate <- substr(My6YR$YearlyDate, 0, 4) #only keep first 10characters in character vector (keep date, not time)
describe(My6YR$YearlyDate)
table(My6YR$YearlyDate)
#Create yearly average - these are biased by sampling protocol / reporting
My6YR$Yearly <- NA
YearlyAverage <- aggregate(x = My6YR["ugLValue"],
                           by = My6YR[c("PWSID", "YearlyDate")],
                           FUN = "mean")
YearlyAverage
colnames(YearlyAverage)[colnames(YearlyAverage)=="ugLValue"] <- "YearlyAverage"

#Create Yearly database
t6yr<-My6YR[c("PWSID","YearlyDate", "Sample.Collection.Date","ugLValue","System.Name","State.Code","System.Type","Retail.Population.Served","Adjusted.Total.Population.Served",
              "Source.Water.Type", "Sample.ID")]
testing<-t6yr[!duplicated(t6yr[1:2]),] #One row for each sampling year within each PWSID; N= 90,290

# Save yearly averages in "YearlyValues"
YearlyValues<-merge(testing, YearlyAverage, by=c("PWSID", "YearlyDate")); dim(YearlyValues) #[1] 90290    12

## START HERE: maybe rename columns in 06,07, etc so they are not identical for merge
# Merge flags and correct yearly averages back in from above
# Rename each uniquely prior to merge
colnames(lower06)[colnames(lower06)=="diff"] <- "diff06"
colnames(lower07)[colnames(lower07)=="diff"] <- "diff07"
colnames(lower08)[colnames(lower08)=="diff"] <- "diff08"
colnames(lower09)[colnames(lower09)=="diff"] <- "diff09"
colnames(lower10)[colnames(lower10)=="diff"] <- "diff10"
colnames(lower11)[colnames(lower11)=="diff"] <- "diff11"
colnames(lower06)[colnames(lower06)=="rawn"] <- "rawn06"
colnames(lower07)[colnames(lower07)=="rawn"] <- "rawn07"
colnames(lower08)[colnames(lower08)=="rawn"] <- "rawn08"
colnames(lower09)[colnames(lower09)=="rawn"] <- "rawn09"
colnames(lower10)[colnames(lower10)=="rawn"] <- "rawn10"
colnames(lower11)[colnames(lower11)=="rawn"] <- "rawn11"
colnames(lower06)[colnames(lower06)=="finn"] <- "finn06"
colnames(lower07)[colnames(lower07)=="finn"] <- "finn07"
colnames(lower08)[colnames(lower08)=="finn"] <- "finn08"
colnames(lower09)[colnames(lower09)=="finn"] <- "finn09"
colnames(lower10)[colnames(lower10)=="finn"] <- "finn10"
colnames(lower11)[colnames(lower11)=="finn"] <- "finn11"


corrected<-merge(YearlyValues,lower06,by="PWSID",all=T)
corrected<-merge(corrected,lower07,by="PWSID",all=T)
corrected<-merge(corrected,lower08,by="PWSID",all=T)
corrected<-merge(corrected,lower09,by="PWSID",all=T)
corrected<-merge(corrected,lower10,by="PWSID",all=T)
corrected<-merge(corrected,lower11,by="PWSID",all=T)

## Replace yearly average with corrected value
# 2006
corrected$YearlyAverage[which(corrected$YearlyDate=="2006"&corrected$PWSID=="NV0000167")] #[1] 30.7
describe(corrected$YearlyAverage[which(corrected$YearlyDate=="2006")])
#corrected$YearlyAverage[which(corrected$YearlyDate == "2006")]
#     n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95
# 16584        0     1921    0.752    2.168    3.138    0.350    0.350    0.350    0.350    1.414    5.700    9.338

#lowest :   0.1000   0.1600   0.1610   0.1650   0.1670, highest: 133.7794 147.0000 158.0000 220.0000 233.8727
corrected$YearlyAverage[which(corrected$YearlyDate=="2006"&corrected$Flag06==1)]<-corrected$finvalue06[which(corrected$YearlyDate=="2006"&corrected$Flag06==1)]
corrected$YearlyAverage[which(corrected$YearlyDate=="2006"&corrected$PWSID=="NV0000167")] #[1] 5.4
describe(corrected$YearlyAverage[which(corrected$YearlyDate=="2006")])
#corrected$YearlyAverage[which(corrected$YearlyDate == "2006")]
#     n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95
# 16584        0     1869     0.75    2.159    3.126    0.350    0.350    0.350    0.350    1.414    5.679    9.309
#
# lowest :   0.1000   0.1600   0.1610   0.1650   0.1670, highest: 133.7794 147.0000 158.0000 220.0000 233.8727

# 2007
describe(corrected$YearlyAverage[which(corrected$YearlyDate=="2007")])
corrected$YearlyAverage[which(corrected$YearlyDate=="2007"&corrected$Flag07==1)]<-corrected$finvalue07[which(corrected$YearlyDate=="2007"&corrected$Flag07==1)]
# 2008
describe(corrected$YearlyAverage[which(corrected$YearlyDate=="2008")])
corrected$YearlyAverage[which(corrected$YearlyDate=="2008"&corrected$Flag08==1)]<-corrected$finvalue08[which(corrected$YearlyDate=="2008"&corrected$Flag08==1)]
# 2009
describe(corrected$YearlyAverage[which(corrected$YearlyDate=="2009")])
corrected$YearlyAverage[which(corrected$YearlyDate=="2009"&corrected$Flag09==1)]<-corrected$finvalue09[which(corrected$YearlyDate=="2009"&corrected$Flag09==1)]
# 2010
describe(corrected$YearlyAverage[which(corrected$YearlyDate=="2010")])
corrected$YearlyAverage[which(corrected$YearlyDate=="2010"&corrected$Flag10==1)]<-corrected$finvalue10[which(corrected$YearlyDate=="2010"&corrected$Flag10==1)]
# 2011
describe(corrected$YearlyAverage[which(corrected$YearlyDate=="2011")])
corrected$YearlyAverage[which(corrected$YearlyDate=="2011"&corrected$Flag11==1)]<-corrected$finvalue11[which(corrected$YearlyDate=="2011"&corrected$Flag11==1)]

## Next, for 3-year averages
My6YR$ThreeYearlyDate<-NA
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2006)]<-1
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2007)]<-1
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2008)]<-1
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2009)]<-2
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2010)]<-2
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2011)]<-2
table(My6YR$ThreeYearlyDate)
describe(My6YR$ThreeYearlyDate)


#Create 3 year average - these are what we believe are most reliable
My6YR$ThreeYearly <- NA
YearlyAverage3 <- aggregate(x = My6YR["ugLValue"],
                            by = My6YR[c("PWSID", "ThreeYearlyDate")],
                            FUN = "mean")
YearlyAverage3
colnames(YearlyAverage3)[colnames(YearlyAverage3)=="ugLValue"] <- "ThreeYearAverage"

#Create Three-year avearge database
t6yr<-My6YR[c("PWSID","ThreeYearlyDate", "Sample.Collection.Date","ugLValue","System.Name","State.Code","System.Type","Retail.Population.Served","Adjusted.Total.Population.Served",
              "Source.Water.Type", "Sample.ID")]
testing<-t6yr[!duplicated(t6yr[1:2]),] #One row for each 3-year sampling time frame within each PWSID; N= 64,425

# Save three year averages in "ThreeYearlyValues"
ThreeYearlyValues<-merge(testing, YearlyAverage3, by=c("PWSID", "ThreeYearlyDate")); dim(ThreeYearlyValues) #[1] 64425    12
# ThreeYearlyValues is the CWS-level database with 3-year averages at the CWS level

# Merge flags and correct yearly averages back in from above
corrected3<-merge(ThreeYearlyValues,lower063,by="PWSID",all=T)
corrected3<-merge(corrected3,lower073,by="PWSID",all=T)

## Replace yearly average with corrected value
# Time period 1
corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate==1&corrected3$PWSID=="CA2100579")] #[1] 31.2857
describe(corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate==1)])
#corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate ==      1)]
#   n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95
#31395        0     4101    0.786    1.937    2.682    0.350    0.350    0.350    0.350    1.500    5.000    8.029
#
#lowest :   0.04949746   0.07071066   0.10000000   0.13000000   0.16000000, highest:  97.42222222 134.66666667 141.81818182 286.66666667 453.94782609
corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate==1&corrected3$Flagtime1==1)]<-corrected3$finvaluetime1[which(corrected3$ThreeYearlyDate==1&corrected3$Flagtime1==1)]
corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate==1&corrected3$PWSID=="CA2100579")] #[1] 3.575489
describe(corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate==1)])
#corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate ==      1)]
#    n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50      .75      .90      .95
#31395        0     3948    0.784    1.918    2.654     0.35     0.35     0.35     0.35     1.45     5.00     8.00
#
#lowest :   0.04949746   0.07071066   0.10000000   0.13000000   0.16000000, highest:  97.42222222 134.66666667 141.81818182 286.66666667 453.94782609

# Time period 2
describe(corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate==2)])
corrected3$ThreeYearAverage[which(corrected3$ThreeYearlyDate==2&corrected3$Flagtime2==1)]<-corrected3$finvaluetime2[which(corrected3$ThreeYearlyDate==2&corrected3$Flagtime2==1)]

# The databases corrected (yearly averages) and corrected3 (3-year averages) now have these corrected values for analysis

## Next, for six-year average:
# Use "corrected" which has yearly values for each PWSID corrected for source water type
# Note that this averages the 6 yearly avearges; we assessed this as sensitivity analysis but in our
# main maps we will use the average of the two three-year averages for descriptive purposes
corrected$SixYearlyDate<-1
corrected$SixYearly <- NA
YearlyAverage6 <- aggregate(x = corrected["YearlyAverage"],
                            by = corrected[c("PWSID", "SixYearlyDate")],
                            FUN = "mean")
YearlyAverage6
colnames(YearlyAverage6)[colnames(YearlyAverage6)=="YearlyAverage"] <- "SixYearAverage"

#Create six-year avearge database
t6yr<-corrected[c("PWSID","SixYearlyDate", "Sample.Collection.Date","ugLValue","System.Name","State.Code","System.Type","Retail.Population.Served","Adjusted.Total.Population.Served",
                  "Source.Water.Type", "Sample.ID")]
testing<-t6yr[!duplicated(t6yr[1:2]),] #One row for each 3-year sampling time frame within each PWSID; N= 37098

# Save six year averages in "SixYearlyValues"
SixYearlyValues<-merge(testing, YearlyAverage6, by=c("PWSID", "SixYearlyDate")); dim(SixYearlyValues) #[1] 37098


### Reformat yearly averages into wide format ##
## Corrected (yearly)
YearlyValuestestcorrected<-corrected
myvars <- c("PWSID", "YearlyDate","YearlyAverage")
YearlyValuestestcorrected <- YearlyValuestestcorrected[myvars]

wideyearlycorrected<-reshape(YearlyValuestestcorrected, idvar = "PWSID", timevar = "YearlyDate", direction = "wide")
describe(wideyearlycorrected$PWSID) #37,098 PWSIDs with yearly average values


#Reorder variables so the years are in the normal order
wideyearlycorrected <- wideyearlycorrected[c("PWSID", "YearlyAverage.2006",
                                             "YearlyAverage.2007", "YearlyAverage.2008","YearlyAverage.2009","YearlyAverage.2010","YearlyAverage.2011"
)]

write.csv(wideyearlycorrected, file="~/Google Drive/Research/EPAAsTrajectories/March2020/wideyearlycorrected.csv")

describe(My6YR$PWSID)
myvars<-c("PWSID", "System.Name", "State.Code", "System.Type","Retail.Population.Served", "Adjusted.Total.Population.Served", "Source.Water.Type")
new6yr<-My6YR[myvars]
new6yra<-new6yr[!duplicated(new6yr[1]),]
describe(new6yra$PWSID) #37,098

## Three-year averages:
ThreeYearlyValuestestcorrected<-corrected3
myvars <- c("PWSID", "ThreeYearlyDate","ThreeYearAverage")
ThreeYearlyValuestestcorrected <- ThreeYearlyValuestestcorrected[myvars]

widethreeyearlycorrected<-reshape(ThreeYearlyValuestestcorrected, idvar = "PWSID", timevar = "ThreeYearlyDate", direction = "wide")
describe(widethreeyearlycorrected$PWSID) #37,098 PWSIDs with three-year average values

#Reorder variables so the years are in the normal order
widethreeyearlycorrected <- widethreeyearlycorrected[c("PWSID", "ThreeYearAverage.1", "ThreeYearAverage.2" )]
write.csv(widethreeyearlycorrected, file="~/Google Drive/Research/EPAAsTrajectories/March2020/widethreeyearlycorrected.csv")

## Six-year averages:
SixYearlyValuestestcorrected<-SixYearlyValues
myvars <- c("PWSID", "SixYearlyDate","SixYearAverage")
SixYearlyValuestestcorrected <- SixYearlyValuestestcorrected[myvars]

widesixyearlycorrected<-reshape(SixYearlyValuestestcorrected, idvar = "PWSID", timevar = "SixYearlyDate", direction = "wide")
describe(widesixyearlycorrected$PWSID) #37,098 PWSIDs with six-year average values

#REorder variables so the years are in the normal order
widesixyearlycorrected <- widesixyearlycorrected[c("PWSID", "SixYearAverage.1" )]
write.csv(widesixyearlycorrected, file="~/Google Drive/Research/EPAAsTrajectories/March2020/widesixyearlycorrected.csv")

# Need to read these back in?
#wideyearlycorrected<-read.csv("~/Google Drive/Research/EPAAsTrajectories/March2020/wideyearlycorrected.csv")
#widethreeyearlycorrected<-read.csv("~/Google Drive/Research/EPAAsTrajectories/March2020/widethreeyearlycorrected.csv")
#widesixyearlycorrected<-read.csv("~/Google Drive/Research/EPAAsTrajectories/March2020/widesixyearlycorrected.csv")


### end

#######   3.B Sensitivity analysis: ignore finished vs raw averages ######
# Create subset
t6yr<-My6YR[c("PWSID","Sample.Collection.Date","ugLValue","System.Name","State.Code","System.Type","Retail.Population.Served", "Adjusted.Total.Population.Served",
              "Source.Water.Type", "Sample.ID")]
testing<-t6yr[!duplicated(t6yr[1:2]),] #One row for each sampling day within each PWSID; 150,176

#Create yearly date -- 
My6YR$YearlyDate<-format(My6YR$Sample.Collection.Date, format="%Y")
class(My6YR$YearlyDate) #remove last 6 characters
My6YR$YearlyDate <- substr(My6YR$YearlyDate, 0, 4) #only keep first 10characters in character vector (keep date, not time)
describe(My6YR$YearlyDate)
table(My6YR$YearlyDate)

#Create three-year date
My6YR$ThreeYearlyDate<-NA
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2006)]<-1
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2007)]<-1
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2008)]<-1
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2009)]<-2
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2010)]<-2
My6YR$ThreeYearlyDate[(My6YR$YearlyDate==2011)]<-2
table(My6YR$ThreeYearlyDate)
describe(My6YR$ThreeYearlyDate)

#Create six-year date
My6YR$SixYearlyDate<-1

#Create yearly average - these are biased by sampling protocol / reporting 
My6YR$Yearly <- NA
YearlyAverage <- aggregate(x = My6YR["ugLValue"],
                           by = My6YR[c("PWSID", "YearlyDate")],
                           FUN = "mean")
YearlyAverage
colnames(YearlyAverage)[colnames(YearlyAverage)=="ugLValue"] <- "YearlyAverage"

#Create 3 year average - these are what we believe are most reliable
My6YR$ThreeYearly <- NA
YearlyAverage3 <- aggregate(x = My6YR["ugLValue"],
                            by = My6YR[c("PWSID", "ThreeYearlyDate")],
                            FUN = "mean")
YearlyAverage3
colnames(YearlyAverage3)[colnames(YearlyAverage3)=="ugLValue"] <- "ThreeYearAverage"

##** CHANGE ME: replace ugLValue with yearly averages
#Create 6-year average using yearly values
YearlyAverage$SixYearly <- NA
YearlyAverage$SixYearlyDate<-1
YearlyAverage$newyearlyav<-YearlyAverage$YearlyAverage
YearlyAverage6 <- aggregate(x = YearlyAverage["newyearlyav"],
                            by = YearlyAverage[c("PWSID", "SixYearlyDate")],
                            FUN = "mean")
YearlyAverage6
colnames(YearlyAverage6)[colnames(YearlyAverage6)=="newyearlyav"] <- "SixYearAverage.1"
YearlyAverage$SixYearly<-NULL
YearlyAverage$SixYearlyDate<-NULL
YearlyAverage$newyearlyav<-NULL

## Create Yearly database
t6yr<-My6YR[c("PWSID","YearlyDate", "Sample.Collection.Date","ugLValue","System.Name","State.Code","System.Type","Retail.Population.Served","Adjusted.Total.Population.Served",
              "Source.Water.Type", "Sample.ID")]
testing<-t6yr[!duplicated(t6yr[1:2]),] #One row for each sampling year within each PWSID; N= 90,290
# Save yearly averages in "YearlyValues"
YearlyValues<-merge(testing, YearlyAverage, by=c("PWSID", "YearlyDate")); dim(YearlyValues) #[1] 90290    12

## Create Three-year avearge database
t6yr<-My6YR[c("PWSID","ThreeYearlyDate", "Sample.Collection.Date","ugLValue","System.Name","State.Code","System.Type","Retail.Population.Served","Adjusted.Total.Population.Served",
              "Source.Water.Type", "Sample.ID")]
testing<-t6yr[!duplicated(t6yr[1:2]),] #One row for each 3-year sampling time frame within each PWSID; N= 64,425
# Save three year averages in "ThreeYearlyValues"
ThreeYearlyValues<-merge(testing, YearlyAverage3, by=c("PWSID", "ThreeYearlyDate")); dim(ThreeYearlyValues) #[1] 64425    12
# ThreeYearlyValues is the CWS-level database with 3-year averages at the CWS level

## Create Six-year avearge database
t6yr<-My6YR[c("PWSID","SixYearlyDate", "Sample.Collection.Date","ugLValue","System.Name","State.Code","System.Type","Retail.Population.Served","Adjusted.Total.Population.Served",
              "Source.Water.Type", "Sample.ID")]
testing<-t6yr[!duplicated(t6yr[1:2]),] #One row for each 3-year sampling time frame within each PWSID; N= 64,425
# Save six year averages in "SixYearlyValues"
SixYearlyValues<-merge(testing, YearlyAverage6, by=c("PWSID", "SixYearlyDate")); dim(SixYearlyValues) # 
# SixYearlyValues is the CWS-level database with 6-year averages at the CWS level


### Reformat yearly avearges into wide format
## YearlyValues
YearlyValuestest<-YearlyValues
myvars <- c("PWSID", "YearlyDate","YearlyAverage")
YearlyValuestest <- YearlyValuestest[myvars]

wideyearly<-reshape(YearlyValuestest, idvar = "PWSID", timevar = "YearlyDate", direction = "wide")
describe(wideyearly$PWSID) # PWSIDs with yearly average values

#Reorder variables so the years are in the normal order
wideyearly <- wideyearly[c("PWSID", "YearlyAverage.2006",
                           "YearlyAverage.2007", "YearlyAverage.2008","YearlyAverage.2009","YearlyAverage.2010","YearlyAverage.2011"    
)]


describe(My6YR$PWSID)
myvars<-c("PWSID", "System.Name", "State.Code", "System.Type","Retail.Population.Served", "Adjusted.Total.Population.Served", "Source.Water.Type")
new6yr<-My6YR[myvars]
new6yra<-new6yr[!duplicated(new6yr[1]),]
describe(new6yra$PWSID) #37,098

## Three-year averages:
ThreeYearlyValuestest<-ThreeYearlyValues
myvars <- c("PWSID", "ThreeYearlyDate","ThreeYearAverage")
ThreeYearlyValuestest <- ThreeYearlyValuestest[myvars]

widethreeyearly<-reshape(ThreeYearlyValuestest, idvar = "PWSID", timevar = "ThreeYearlyDate", direction = "wide")
describe(widethreeyearly$PWSID) #37,098 PWSIDs with three-year average values

#REorder variables so the years are in the normal order
widethreeyearly <- widethreeyearly[c("PWSID", "ThreeYearAverage.1", "ThreeYearAverage.2" )]


## Six-year aveages (not really needed since 1 value per PWSID):
SixYearlyValuestest<-SixYearlyValues
myvars <- c("PWSID", "SixYearlyDate","SixYearAverage.1")
SixYearlyValuestest <- SixYearlyValuestest[myvars]

widesixyearly<-reshape(SixYearlyValuestest, idvar = "PWSID", timevar = "SixYearlyDate", direction = "wide")
describe(widesixyearly$PWSID) #37,098 PWSIDs

#REorder variables so the years are in the normal order
widesixyearly <- widesixyearly[c("PWSID", "SixYearAverage.1.1")]
colnames(widesixyearly)[colnames(widesixyearly)=="SixYearAverage.1.1"] <- "SixYearAverage.1"

# Save all three databases
write.csv(wideyearly, file="wideyearly.csv");write.csv(wideyearly, file="~/Google Drive/Research/EPAAsTrajectories/March2020/wideyearly.csv")
write.csv(widethreeyearly, file="widethreeyearly.csv");write.csv(widethreeyearly, file="~/Google Drive/Research/EPAAsTrajectories/March2020/widethreeyearly.csv")
write.csv(widesixyearly, file="widesixyearly.csv");write.csv(widesixyearly, file="~/Google Drive/Research/EPAAsTrajectories/March2020/widesixyearly.csv")

## Corrected for raw vs. finished samples: widesixyearlycorrected, widethreeyearlycorrected, and wideyearlycorrected
## Original estimates, not corrected for raw vs. finished samples: widesixyearly, widethreeyearly, and wideyearly

### end





####### 5. Merge SYR3 with SDWIS and assign county-served to each CWS  ########
## Keep corrected means (finished instead of gross if appropriate):
wideyearly<-wideyearlycorrected
widethreeyearly<-widethreeyearlycorrected
widesixyearly<-widesixyearlycorrected

# Load SDWIS, merge county-served with classes dataset
load("MySDWIS"); dim(MySDWIS) #92455    20
# June 22 try my new SDWIS
colnames(MySDWIS)[colnames(MySDWIS)=="WATER_SYSTEM.PWSID"] <- "PWSID"
as.character(My6YR$PWSID) 
as.character(MySDWIS$PWSID)

## Automatched records with SDWIS     ##
#Merge "wideyearly" dataframe with MySDWIS to assign county-assignments
water <- merge(wideyearly, MySDWIS, by="PWSID"); dim(water) # [1] 36372    26
# "wideyearly" had 37,098 PWSIDs; water now has 36,372; missing 726 PWSs that were not automatcehd by PWSID from MySDWIS
describe(water$WATER_SYSTEM.COUNTIES_SERVED) #none missing from 'water'
describe(water$PWSID)

## Manually find PWSIDs not in SDWIS (726 missing) ##
unknowns<- data.frame(wideyearly$PWSID[!(wideyearly$PWSID %in% MySDWIS$PWSID)])
colnames(unknowns)[1] <- "PWSID"
write.csv(unknowns, file="unknowns.csv")
#We manually extracted SDWIS records for these 726 PWSIDs; we manually searched in SDWIS by geography or PWSID
#of these, we kept only 480 - the others were not CWSs or were inactive (see Manual PWS identification.xls)
# From SDWIS, we extracted population served, source water type, zip code, and state and saved in "ManualPWSIDs480a.csv"
manual<-read.csv("ManualPWSIDs480a.csv",stringsAsFactors = FALSE) 

#Fix two zip codes who lost the leading zero upon import
manual$Zip.code[which(manual$PWSID==10106001)]<-"06339"
manual$Zip.code[which(manual$PWSID==10307001)]<-"02535"
#Merge these manual PWSIDs (N=480) in with 'water' for final database of county-level assignmnets
#First, merge info from wideyearly with PWSIDs from manual
final<-merge(manual,wideyearly,by="PWSID",all.x=T)
describe(final$PWS.NAME)

# Prior to merge, ensure column names are similar when possible and Fix column names that were duplicated in merge:
colnames(water)[colnames(water)=="WATER_SYSTEM.PWS_NAME"] <- "PWS.NAME"
colnames(water)[colnames(water)=="WATER_SYSTEM.ZIP_CODE"] <- "Zip.code"
colnames(water)[colnames(water)=="WATER_SYSTEM.POPULATION_SERVED_COUNT"] <- "Population.served"
final$State.Code<-final$State
water$State.Code<-water$WATER_SYSTEM.PRIMACY_AGENCY_CODE
describe(final$Primary.source.water)
final$Source.Water.Type<-final$Primary.source.water
describe(final$Source.Water.Type)
final$Source.Water.Type[which(final$Source.Water.Type=="GW purchased")]<-"GWP"
final$Source.Water.Type[which(final$Source.Water.Type=="SW purchased")]<-"SWP"
final$Source.Water.Type[which(final$Source.Water.Type=="GWuSW")]<-"GU"
describe(water$WATER_SYSTEM.GW_SW_CODE)
water$Source.Water.Type<-water$WATER_SYSTEM.GW_SW_CODE
final$CountyServed<-NA
water$CountyServed<- water$WATER_SYSTEM.COUNTIES_SERVED

# Select final variables
myvars<-c("PWSID", "PWS.NAME", "State.Code", "Zip.code", "Population.served", "CountyServed",
          "Source.Water.Type")

finala<-final[myvars]
watera<-water[myvars]

whole<-rbind(finala,watera);dim(whole) #36852 = 480+36372
describe(whole$PWSID) #36,852
describe(whole$PWS.NAME)
describe(whole$State.Code)
describe(whole$Zip.code) #86 missing zip codes (these have county served though)
describe(whole$CountyServed) #480 missing
describe(finala$Zip.code);describe(watera$Zip.code)

## Assign PWSIDs missing county-served (N= 480) within "finala" based on Zip Code from HUD USPS crosswalk files
# For these PWSIDs, we only assign to each county (within finala, then we re-merge to create 'whole')
# Next we will assign each of the 480 PWSIDS with a zip code a county using HUD and census data:
#HUD USPS ZIP CODE CROSSWALK FILES: https://www.huduser.gov/portal/datasets/usps_crosswalk.html#codebook
#We are allocating zipcodes to counties; we used March 2010 file
hud<-read.csv("ZIP_COUNTY_032010.csv", colClasses=c(COUNTY="character", ZIP="character"))
#Keep only relevant data:
myvars<-c("COUNTY", "ZIP")
hud<-hud[myvars]
colnames(hud)[2] <- "Zip.code" #Rename to match finala
colnames(hud)[1] <- "CountyFIPS" #Rename to match finala
#Note: in "hud" there are zip codes with multiple counties serving them. 
# We will assign each zip code multiple counties when relevant
describe(finala$Zip.code) #192 distinct; 83 of total 563 rows are for non-unique PWSIDs
describe(hud$CountyFIPS)

## Now from the "finala" dataframe, we use the zipcode to assign a/multiple county(ies) to each PWSID
finala<-merge(finala,hud,by="Zip.code",all.x=T); dim(finala) #[1] 563  8
#83 of 563 rows are duplicate counties served for one PWSID
describe(finala$PWSID)
describe(finala$Zip.code)
describe(finala$CountyFIPS)#23 still missing county assignment
finala$FLAG<-1 # create FLAG variable for sensitivity analysis excluding these rows
whymissing<-finala[which(is.na(finala$CountyFIPS)),]
y<-hud[which(hud$Zip.code==28053)]
# Of these 23, 10 are in AS (American Samoa)- remove these: 
# 6 in CA; 2 AZ; 3 WY;1 FL; 1 NC
finala<-finala[!(finala$State.Code=="AS"),] # remove AS records (American Samoa)
describe(finala$PWSID)
describe(finala$CountyFIPS)

# Next we get remaining 13 that are not in the 2011 HUD file
hud2011<-read.csv("ZIP_COUNTY_122011.csv", colClasses=c(COUNTY="character", ZIP="character"))
# Zip: 28053	FIPS: 37071
# Zip: 33475; FIPS 12085
# Zip: 52514 ***NOTE THIS IS A TYPO IN SDWIS DATA*** THE correct zip code is 82514
# Zip: 85269; F 04013
# Zip: 85366; F 04027
# Zip: 92581; F 06065
# Zip: 92593; F 06065
# Zip: 93258; F 06107
# Zip: 93519; F 06029 #Not in HUD file;found via GoogleMaps
# Zip: 93912; F 06053
finala$CountyFIPS[which(finala$Zip.code==28053)]<-37071
finala$CountyFIPS[which(finala$Zip.code==33475)]<-12085
finala$CountyFIPS[which(finala$Zip.code==85269)]<-04013
finala$CountyFIPS[which(finala$Zip.code==85366)]<-04027
finala$CountyFIPS[which(finala$Zip.code==92581)]<-06065
finala$CountyFIPS[which(finala$Zip.code==92593)]<-06065
finala$CountyFIPS[which(finala$Zip.code==93258)]<-06107
finala$CountyFIPS[which(finala$Zip.code==93519)]<-06029
finala$CountyFIPS[which(finala$Zip.code==93912)]<-06053
# Fix incorrect zip code for Wind River, WY area (found via GoogleMaps)
finala$Zip.code[which(finala$Zip.code==52514)]<-82514
finala$CountyFIPS[which(finala$Zip.code==82514)]<-56013

describe(finala$CountyFIPS) #Each PWSID now has 1+ FIPS assigned
describe(watera$CountyServed)

#Next, assign CountyFIPS to "watera" before re-rbinding with "finala"
# To do this we must create duplicate rows for PWSIDs serving multiple counties
# County is 5 digit unique 2000 or 2010 Census county GEOID consisting of state FIPS + county FIPS.
# County FIPS data was downloadedfrom:
#https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt
#https://www.census.gov/geo/reference/codes/cou.html
fips<-read.csv("FIPScodes2010.csv", stringsAsFactors = FALSE, colClasses=c(STATE="character",
                                                                           STATEFIPS="character",COUNTYFIPS="character", COUNTYNAME="character"))
fips$X<-NULL
# Need to specify that numbers should be read as characters before import or will lose zeroes 
fips$fullfips<- paste(fips$STATEFIPS,fips$COUNTYFIPS)
#Remove space between both
fips$fullfips <- gsub(" ", "", fips$fullfips) #Removes space from PWSID variable
colnames(fips)[colnames(fips)=="COUNTYNAME"] <- "CountyServed"
colnames(fips)[colnames(fips)=="STATE"] <- "State.Code"

#Now assign watera a "CountyFIPS" based on the state and county name from fips
#Now from the 'watera' databse, which had "counties served" - we extract county and state and try to match with FIPS county code
#We merge county fips$fullfips with 'water' by "State" and name of county

#If a PWSID in "watera" has multiple counties served (contains a ,), we duplicate that row and make
#sure there is one row for each county served
#First, create column "todup" that takes the value of 1 when there is a , and 0 if not

#See if there are PWSIDs in water that serve multiple counties
problemcounties<-data.frame(water$PWSID,water$WATER_SYSTEM.COUNTIES_SERVED)
probcounties<-data.frame(problemcounties[!duplicated(problemcounties[2]),]) #one unique record for each countyserved
write.csv(probcounties,file="problemcountiesApril.csv")

## We have 77 "problematic" records, where PWSIDs serve multiple counties; for these, we will create duplicate rows for each county
#max # of counties served is 10;  These were corrected manually and saved as correctedcounties.csv
corrected<-read.csv("correctedcounties.csv")
colnames(corrected)[colnames(corrected)=="water.WATER_SYSTEM.COUNTIES_SERVED"] <- "CountyServed"
colnames(corrected)[colnames(corrected)=="water.PWSID"] <- "PWSID"

table(corrected$Duplicate) #214 records that are duplicated
dups<-corrected[which(corrected$Duplicate==1),]
describe(dups$PWSID)

# Create new database merging PWSIDs from "dup" with the data for them water "water"
newdups<-merge(dups, water,by="PWSID",all.x=T)
describe(newdups$PWSID) #These get rbinded back to water after the single rows are deleted from water

#If PWSID serves mulitple counties (has , in CountyServed), remove that row from water
NAMES_list<-newdups$PWSID
water2 <- water[ ! water$PWSID %in% NAMES_list, ];dim(water2)#[1] 36295    29
#77rows removed

#Add PWSIDs serving multiple counites back to "water2" 
#First make all column names same and remove unnecessary columns
# Select final variables
myvars<-c("PWSID", "PWS.NAME", "State.Code", "Zip.code", "Population.served", "CountyServed",
          "Source.Water.Type")

water2<-water2[myvars] 
colnames(newdups)[colnames(newdups)=="CountyServed.x"] <- "CountyServed"

newdups$CountyServed<-as.character(newdups$CountyServed)
newdups2<-newdups[myvars];dim(newdups2) #[1] 214  7

watera<-rbind(water2,newdups2); dim(watera) #[1] 36509    7
#This has one row for each county served by a PWSID (there will be 137 duplicated PWSIDs)
describe(watera$PWSID)
waterx<-watera

#Assign county FIPS code to waterx before binding with finala 
#Merge 'waterx' with fips to assign a County FIP to each county based on name and state ID
## First remove "County" from fips$CountyServed names
fips$CountyServed <- gsub("County", "", fips$CountyServed) #Removes space from PWSID variable
fips$CountyServed #need to remove " " from the character
fips$CountyServed <- gsub(" ", "", fips$CountyServed) #Removes space from PWSID variable
#Repeaat for waterx so county names with two words are the same
waterx$CountyServed <- gsub(" ", "", waterx$CountyServed) #Removes space from PWSID variable

fips$FullName<-paste0(as.character(fips$State.Code),", ", as.character(fips$CountyServed))
waterx$FullName<-paste0(as.character(waterx$State.Code),", ", as.character(waterx$CountyServed))
describe(fips$FullName)
describe(waterx$FullName)
waterx$CountyServed
fips$CountyServed
testinga<-merge(waterx, fips, by=c("CountyServed", "State.Code"),all.x=T)
finaldata<-testinga
describe(testinga$CountyServed)
describe(testinga$FullName.y) #92 missing 
describe(testinga$FullName.x) #0 missing
describe(testinga$PWSID)# none missing, 36,509 total
describe(testinga$fullfips) #92 missing  
whoismissing<-testinga[which(is.na(testinga$fullfips)),]
describe(whoismissing$CountyServed) #9 counties which need a fullfips assigned manually
describe(whoismissing$PWSID)

#Create multiple rows for rows that have multiple counties served
whoismissing$freq<-1
whoismissing$freq[whoismissing$PWSID=="AR0000706"]<-2
whoismissing$freq[whoismissing$PWSID=="SD4602314"]<-2
whoismissing$freq[whoismissing$PWSID=="SD4602277"]<-4
whoismissing$freq[whoismissing$PWSID=="MO5031606"]<-2
whoismissing$freq[whoismissing$PWSID=="AR0000527"]<-2
whoismissing$freq[whoismissing$PWSID=="AR0000672"]<-2
whoismissing$freq[whoismissing$PWSID=="AR0000759"]<-2
library(splitstackshape)
whoismissing<-expandRows(whoismissing, "freq")
# Fill in missing county name
whoismissing$CountyServed[7]<-"Boone"
whoismissing$CountyServed[8]<-"Newton"
whoismissing$CountyServed[9]<-"Butte"
whoismissing$CountyServed[10]<-"Meade"
whoismissing$CountyServed[11]<-"Clark"
whoismissing$CountyServed[12]<-"Codington"
whoismissing$CountyServed[13]<-"Day"
whoismissing$CountyServed[14]<-"Hamlin"
whoismissing$CountyServed[15]<-"Dallas"
whoismissing$CountyServed[16]<-"Polk"
whoismissing$CountyServed[17]<-"Fulton"
whoismissing$CountyServed[18]<-"Sharp"
whoismissing$CountyServed[19]<-"Fulton"
whoismissing$CountyServed[20]<-"Sharp"
whoismissing$CountyServed[21]<-"Independence"
whoismissing$CountyServed[22]<-"Sharp"
whoismissing$CountyServed[23]<-"Maries"
whoismissing$CountyServed[24]<-"Osage"
whoismissing$CountyServed[whoismissing$CountyServed=="b"]<-"NotReported"
whoismissing$FullName.x<-NULL
whoismissing$FullName.y<-NULL
whoismissing$COUNTYFIPS<-NULL
whoismissing$STATEFIPS<-NULL
whoismissing$fullfips<-NULL
whoismissing$FullName<-paste0(as.character(whoismissing$State.Code),", ", as.character(whoismissing$CountyServed))
whoismissing<-merge(whoismissing, fips, by="FullName",all.x=T)
fixed<-whoismissing[!is.na(whoismissing$fullfips),]#17
fixed$CountyServed.x<-NULL
colnames(fixed)[colnames(fixed)=="CountyServed.y"] <- "CountyServed"
colnames(fixed)[colnames(fixed)=="State.Code.x"] <- "State.Code"

describe(fixed$State.Code)
fixed$State.Code.y<-NULL
describe(testinga$FullName.x)
testinga$FullName.y<-NULL
colnames(testinga)[colnames(testinga)=="FullName.x"] <- "FullName"

#First remove those who are missing CountyFIPS entirely then add them back in from "fixed"
testinga<-testinga[!is.na(testinga$fullfips),]
testinga<-rbind(testinga,fixed); dim(testinga)#[1] 36434    11

stillmissing<-whoismissing[is.na(whoismissing$fullfips),] #84 still missing, will assign using Zipcode
stillmissing$fullfips<-NULL
stillmissing$CountyServed.y<-NULL
stillmissing$COUNTYFIPS<-NULL
stillmissing$STATEFIPS<-NULL
stillmissing$State.Code.y<-NULL
stillmissing$Zip.code[stillmissing$Zip.code=="23705-0490"]<-23705
stillmissing$Zip.code[stillmissing$Zip.code=="24203-8100"]<-24203
stillmissing<-merge(stillmissing,hud,by="Zip.code",all.x=T); dim(stillmissing) #[1] 123  9

# 16 rows still without zip code or countyFIPS --> these we exclude
excluded<-stillmissing[is.na(stillmissing$CountyFIPS),]
describe(excluded$PWSID)#18 distinct PWSIDs that we cannot match with zipcode or with "countyserved" from SDWIS
stillmissing<-stillmissing[!is.na(stillmissing$CountyFIPS),] #105
#Now rbind back into testinga
colnames(stillmissing)[colnames(stillmissing)=="CountyServed.x"] <- "CountyServed"
colnames(stillmissing)[colnames(stillmissing)=="State.Code.x"] <- "State.Code"
stillmissing$STATEFIPS<-"NA"
colnames(stillmissing)[colnames(stillmissing)=="CountyFIPS"] <- "fullfips"

stillmissing$COUNTYFIPS<-"NA"

testinga<-rbind(testinga,stillmissing);dim(testinga)#[1] 36539    11
describe(testinga$fullfips) # none missing!
describe(testinga$PWSID) #36539 unique
describe(waterx$PWSID) #36509  unique
describe(watera$PWSID) #36509
#final step: merge back in "finala" which has the 480 PWSIDs we pulled manually from SDWIS
colnames(testinga)[colnames(testinga)=="fullfips"] <- "CountyFIPS"

testinga$STATEFIPS<-NULL
testinga$COUNTYFIPS<-NULL
finala$FullName<-"Manual"
# This identifies county-served allocations we made manually (not done 
# automatically by matching to SDWIS)

testinga$FLAG<-0
Dataset1<-rbind(testinga,finala)  
#Before merging wideythreeyearly arsenic averages back in, need to add the leading zero
# for PWSIDs from Dataset1
library(stringr)
Dataset1$PWSID<-str_pad(Dataset1$PWSID, width=9, pad="0")
Dataset1$CountyFIPS<-str_pad(Dataset1$CountyFIPS, width=5, pad="0")
Dataset1ThreeYear<-merge(Dataset1, widethreeyearly,by="PWSID",all.x=T);dim(Dataset1ThreeYear) #[1] 37092    12
Dataset1<-merge(Dataset1, wideyearly, by="PWSID",all.x=T); dim(Dataset1) #[1] 37092    16

describe(Dataset1$PWSID)# 36824 distinct PWSs, 37,092 total, because some PWSs serve multiple counties
describe(Dataset1$CountyFIPS)#2,750 distinct counties, because most counties served by multiple PWSs
describe(Dataset1ThreeYear$PWSID) #same!
describe(Dataset1ThreeYear$CountyFIPS)# Same!

# Add back Adjusted pop served from SYR 
myvars<-c("PWSID","Adjusted.Total.Population.Served")
d<-As6YR3[myvars]
describe(d$PWSID)
#Keep one unique row for each pwisd
dd<-d[!duplicated(d$PWSID),]
Dataset1<-merge(Dataset1,dd,by="PWSID",all.x=T)
Dataset1ThreeYear<-merge(Dataset1ThreeYear,dd,by="PWSID",all.x=T)
write.csv(Dataset1, "Dataset1.csv")
write.csv(Dataset1ThreeYear, "Dataset1ThreeYear.csv")

## REMINDER: Dataset1ThreeYear.csv retains 1 record for each county-served for each PWS;
## finaldata (created below) retains only 1 record for each PWS, total.

#Create new database that retains only 1 record for each PWS
# This record was used to identify trajectory classes in MPlus; therefore all missing 
# records must be "." instead of NA; we did not publish this analysis in the end
finaldata<-Dataset1[!duplicated(Dataset1[1]),];dim(finaldata)#[1] 36824    17
finaldataThree<-Dataset1ThreeYear[!duplicated(Dataset1ThreeYear[1]),];dim(finaldata)#[1] 36824    17

#For simplicity we change the column names 
colnames(finaldata)[colnames(finaldata)=="YearlyAverage.2006"] <- "Y2006"
colnames(finaldata)[colnames(finaldata)=="YearlyAverage.2007"] <- "Y2007"
colnames(finaldata)[colnames(finaldata)=="YearlyAverage.2008"] <- "Y2008"
colnames(finaldata)[colnames(finaldata)=="YearlyAverage.2009"] <- "Y2009"
colnames(finaldata)[colnames(finaldata)=="YearlyAverage.2010"] <- "Y2010"
colnames(finaldata)[colnames(finaldata)=="YearlyAverage.2011"] <- "Y2011"

finaldata <- finaldata[order(finaldata$PWSID),] 
finaldataThree <- finaldataThree[order(finaldataThree$PWSID),] 
finaldata$Y2006<-as.character(finaldata$Y2006)
finaldata$Y2007<-as.character(finaldata$Y2007)
finaldata$Y2008<-as.character(finaldata$Y2008)
finaldata$Y2009<-as.character(finaldata$Y2009)
finaldata$Y2010<-as.character(finaldata$Y2010)
finaldata$Y2011<-as.character(finaldata$Y2011)

myvars<-c("PWSID","Y2006","Y2007","Y2008","Y2009","Y2010","Y2011")
finaldata1<-finaldata[myvars]
myvars<-c("PWSID","ThreeYearAverage.1","ThreeYearAverage.2")
finaldataThree1<-finaldataThree[myvars]
# Check to see if finaldata has only PWSIDs with multiple yearly averages
# Holdover from prior analysis requiring NA values as "."
finaldata$Y2006[which(finaldata$Y2006==".")]<-NA
finaldata$Y2007[which(finaldata$Y2007==".")]<-NA
finaldata$Y2008[which(finaldata$Y2008==".")]<-NA
finaldata$Y2009[which(finaldata$Y2009==".")]<-NA
finaldata$Y2010[which(finaldata$Y2010==".")]<-NA
finaldata$Y2011[which(finaldata$Y2011==".")]<-NA
# Holdover from prior analysis requiring NA values as "."
finaldataThree$ThreeYearAverage.1[which(finaldataThree$ThreeYearAverage.1==".")]<-NA
finaldataThree$ThreeYearAverage.2[which(finaldataThree$ThreeYearAverage.2==".")]<-NA

# Find PWSIDs by yearly average missing pattern:
missingall<-finaldata[is.na(finaldata$Y2006)&is.na(finaldata$Y2007)&is.na(finaldata$Y2008)&is.na(finaldata$Y2009)&is.na(finaldata$Y2010)&is.na(finaldata$Y2011),]
missingallthree<-finaldataThree[is.na(finaldataThree$ThreeYearAverage.1)&is.na(finaldataThree$ThreeYearlyAverage.2),]
# none are missing all in either dataset

dim(missingall) #none missing every year
dim(finaldata) # 36824    17

finaldata$Y2006<-as.character(finaldata$Y2006)
finaldata$Y2007<-as.character(finaldata$Y2007)
finaldata$Y2008<-as.character(finaldata$Y2008)
finaldata$Y2009<-as.character(finaldata$Y2009)
finaldata$Y2010<-as.character(finaldata$Y2010)
finaldata$Y2011<-as.character(finaldata$Y2011)

describe(finaldataThree$CountyFIPS)
finaldata<-finaldata;dim(finaldata)#[1] 36824    17
finaldatathreeyears<-finaldataThree;dim(finaldatathreeyears)#[1] 36824    13

#Fix missing leading zeroes
finaldata$CountyFIPS[which(finaldata$CountyFIPS=="6029")]<-"06029"
finaldata$CountyFIPS[which(finaldata$CountyFIPS=="6065")]<-"06065"
finaldata$CountyFIPS[which(finaldata$CountyFIPS=="6107")]<-"06107"
finaldata$CountyFIPS<-str_pad(finaldata$CountyFIPS, width=5, pad="0") #make sure all leading zeroes are kept

finaldatathreeyears$CountyFIPS[which(finaldatathreeyears$CountyFIPS=="6029")]<-"06029"
finaldatathreeyears$CountyFIPS[which(finaldatathreeyears$CountyFIPS=="6065")]<-"06065"
finaldatathreeyears$CountyFIPS[which(finaldatathreeyears$CountyFIPS=="6107")]<-"06107"
finaldatathreeyears$CountyFIPS<-str_pad(finaldatathreeyears$CountyFIPS, width=5, pad="0") #make sure all leading zeroes are kept

# Final exclusion: remove systems reporting serving 0 people 
table(finaldata$Population.served);dim(finaldata)#[1] 36824    17
g<-finaldata[which(finaldata$Population.served==0),] #418
finaldata<-finaldata[which(finaldata$Population.served!=0),];dim(finaldata) #[1] 36406    17
#new feb 2
table(Dataset1$Population.served);dim(Dataset1)#[1] 37092    17
g<-Dataset1[which(Dataset1$Population.served==0),] #418
Dataset1<-Dataset1[which(Dataset1$Population.served!=0),];dim(Dataset1) #[1] 36674    17

# Final exclusion: remove systems reporting serving 0 people
table(finaldatathreeyears$Population.served);dim(finaldatathreeyears)#[1] 36824    13
g<-finaldatathreeyears[which(finaldatathreeyears$Population.served==0),]
finaldatathreeyears<-finaldatathreeyears[which(finaldatathreeyears$Population.served!=0),];dim(finaldatathreeyears) #[1] 36406    13
# Lost 418 systems; Population.served is the Water system’s estimate of the number of people served by the system.
table(Dataset1ThreeYear$Population.served);dim(Dataset1ThreeYear)#[1] 37092    17
g<-Dataset1ThreeYear[which(Dataset1ThreeYear$Population.served==0),] #418
Dataset1ThreeYear<-Dataset1ThreeYear[which(Dataset1ThreeYear$Population.served!=0),];dim(Dataset1ThreeYear) #[1] 36674    17

save("finaldata",file="finaldata_inc8000");dim(finaldata) #[1] 36406    17
save("finaldatathreeyears",file="finaldatathreeyears_inc8000");dim(finaldatathreeyears) #[1] 36406    13

# These databases we save for later use (section 8) to have one row/record for each CWS:
finaldatacws<-finaldata
finaldatacwsthreeyears<-finaldatathreeyears

### end


####### 6. Create county-level weighted As averages #######
# Create population-weighted averages for each county, for each year 
# Stratify the datasets by year to be merged together later
# First, create population weight for each PWSID serving each county
# We create a loop, that says, for each unique countyFIPS, multiply the water As by the poplation weight of that PWSID

### Part 1: Determine population denominator and the estimated public water reliant pop count

### Identify counties where the major system is missing data
# If counties do not have 50% or more public water reliant population represented by PWSs, exclude them
#Load county-population count from Census 2010 (reference: Ruggles S, Flood S, Goeken R, Grover J, Meyer E, Pacas J, et al. 2019. Ipums USA: Version 9.0. Minneapolis, MN)
censuspop<-read.csv("co-est00int-tot.csv")
# First pad state and county FIPS with zeroes, then merge to get CountyFIPS
censuspop$STATE<-str_pad(censuspop$STATE, width=2, pad="0")
censuspop$COUNTY<-str_pad(censuspop$COUNTY, width=3, pad="0")
censuspop$CountyFIPS<- paste(censuspop$STATE,censuspop$COUNTY)
censuspop$CountyFIPS <- gsub(" ", "", censuspop$CountyFIPS) #Removes excess space
# Var CENSUS2010POP is the 2010 census pop

# Next, load 1990 US Census data estimating the % in each county reliant on public water, drilled well,
# dug well, or other source of drinking water
# Reference: (reference: Ruggles S, Flood S, Goeken R, Grover J, Meyer E, Pacas J, et al. 2019. Ipums USA: Version 9.0. Minneapolis, MN)
sourcewater<-read.csv("nhgis0001_ds123_1990_county.csv") 
sourcewater$COUNTYA<-str_pad(sourcewater$COUNTYA, width=3, pad="0")
sourcewater$STATEA<-str_pad(sourcewater$STATEA, width=2, pad="0")
sourcewater$CountyFIPS<- paste(sourcewater$STATEA,sourcewater$COUNTYA)
sourcewater$CountyFIPS <- gsub(" ", "", sourcewater$CountyFIPS)
myvars<-c("CountyFIPS","EX5001","EX5002","EX5003","EX5004")
sw1<-sourcewater[myvars]
colnames(sw1)[colnames(sw1)=="EX5001"] <- "pubpop"
colnames(sw1)[colnames(sw1)=="EX5002"] <- "drillwell"
colnames(sw1)[colnames(sw1)=="EX5003"] <- "dugwell"
colnames(sw1)[colnames(sw1)=="EX5004"] <- "other"
sw1$total<-(sw1$pubpop)+(sw1$drillwell)+(sw1$dugwell)+(sw1$other) #total estimate

myvars<-c("CountyFIPS","CENSUS2010POP")
censuspop<-censuspop[myvars]
Dataset1$CountyFIPS<-str_pad(Dataset1$CountyFIPS, width=5, pad="0")
Dataset1<-merge(Dataset1,censuspop, by="CountyFIPS",all.x=T)
Dataset1<-merge(Dataset1,sw1, by="CountyFIPS",all.x=T)

## Anybody missing pubpop?  Yes, 3 counties: 02282 (AK, Yakutat City and Borough); 02232 (AK, Skagway-Hoonah-Angoon) and 12086 (Miami-Dade, FL)
# We'll determine manually if these should be excluded or not
miss<-Dataset1[which(is.na(Dataset1$pubpop)),]
miss$CountyFIPS

# For Miami-Dade FL (12086), total population Census 1990: 1.943 million; assign 1943000 as pub pop assuming urban county; main PWS serving 2.8 mil has data in both time periods
# For Yakutat AK (02282), total population Census 1990: 727 people - this is okay, one PWSID serves 740 ppl; assing pubpop 727
# For Skagway-Hoonah-Angoon AK, total population Census 1990: 3,689 people;  assing 3689 to pub pop

Dataset1$pubpop[which(Dataset1$CountyFIPS=="12086")]<-1943000
Dataset1$pubpop[which(Dataset1$CountyFIPS=="02282")]<-727
Dataset1$pubpop[which(Dataset1$CountyFIPS=="02232")]<-3689
describe(Dataset1$pubpop)

Dataset1ThreeYear$CountyFIPS<-str_pad(Dataset1ThreeYear$CountyFIPS, width=5, pad="0")
Dataset1ThreeYear<-merge(Dataset1ThreeYear,censuspop, by="CountyFIPS",all.x=T)
Dataset1ThreeYear<-merge(Dataset1ThreeYear,sw1, by="CountyFIPS",all.x=T)
describe(Dataset1ThreeYear$pubpop)
Dataset1ThreeYear$pubpop[which(Dataset1ThreeYear$CountyFIPS=="12086")]<-1943000
Dataset1ThreeYear$pubpop[which(Dataset1ThreeYear$CountyFIPS=="02282")]<-727
Dataset1ThreeYear$pubpop[which(Dataset1ThreeYear$CountyFIPS=="02232")]<-3689
describe(Dataset1ThreeYear$pubpop)

# Create var identifying 50% of county population, as in 1990 Census
Dataset1$pop50 <- Dataset1$pubpop*0.5 #50%
Dataset1$pop70 <- Dataset1$pubpop*0.7 #70%
Dataset1$pop80 <- Dataset1$pubpop*0.8 #80%

Dataset1ThreeYear$pop50 <- Dataset1ThreeYear$pubpop*0.5 #50%
Dataset1ThreeYear$pop70 <- Dataset1ThreeYear$pubpop*0.7 #70%
Dataset1ThreeYear$pop80 <- Dataset1ThreeYear$pubpop*0.8 #80%

#Fix the "." back to NA again
Dataset1$YearlyAverage.2006[which(Dataset1$YearlyAverage.2006==".")]<-NA
Dataset1$YearlyAverage.2007[which(Dataset1$YearlyAverage.2007==".")]<-NA
Dataset1$YearlyAverage.2008[which(Dataset1$YearlyAverage.2008==".")]<-NA
Dataset1$YearlyAverage.2009[which(Dataset1$YearlyAverage.2009==".")]<-NA
Dataset1$YearlyAverage.2010[which(Dataset1$YearlyAverage.2010==".")]<-NA
Dataset1$YearlyAverage.2011[which(Dataset1$YearlyAverage.2011==".")]<-NA

Dataset1ThreeYear$ThreeYearAverage.1[which(Dataset1ThreeYear$ThreeYearAverage.1==".")]<-NA
Dataset1ThreeYear$ThreeYearAverage.2[which(Dataset1ThreeYear$ThreeYearAverage.2==".")]<-NA

## Here we calculate the populations by year and county
# First, for yearly averages using Dataset1
#2006
myvars<-c("CountyFIPS","PWSID", "State.Code","PWS.NAME","Zip.code",
          "Population.served", "Adjusted.Total.Population.Served", "FullName","YearlyAverage.2006","pubpop","pop50")
Dataset106<-Dataset1[myvars]
# Remove missing rows, which are PWSs missing data for that year
Dataset106<-Dataset106[which(!is.na(Dataset106$YearlyAverage.2006)),]

#For each county, create total popserved 
vector.id <- c( unique(Dataset106$CountyFIPS)) 
count06 <- as.data.frame(vector.id)
count06$tpopser06 <- NA

for ( i in unique(Dataset106$CountyFIPS)){
  sum <- cbind ( sum(Dataset106$Adjusted.Total.Population.Served[Dataset106$CountyFIPS==i]) ) 
  count06$tpopser06[count06$vector.id==i] <- sum
}
colnames(count06)[colnames(count06)=="vector.id"] <- "CountyFIPS"

# checked with FIPS 01001 and 01003 - okay!

#2007
myvars<-c("CountyFIPS","PWSID","CountyServed", "State.Code","PWS.NAME","Zip.code",
          "Population.served", "Adjusted.Total.Population.Served","FullName","YearlyAverage.2007","pubpop","pop50")
Dataset107<-Dataset1[myvars]
# Remove missing rows, which are PWSs missing data for that year
Dataset107<-Dataset107[which(!is.na(Dataset107$YearlyAverage.2007)),]

#For each county, create total popserved 
vector.id <- c( unique(Dataset107$CountyFIPS)) 
count07 <- as.data.frame(vector.id)
count07$tpopser07 <- NA

for ( i in unique(Dataset107$CountyFIPS)){
  sum <- cbind ( sum(Dataset107$Adjusted.Total.Population.Served[Dataset107$CountyFIPS==i]) ) 
  count07$tpopser07[count07$vector.id==i] <- sum
}
colnames(count07)[colnames(count07)=="vector.id"] <- "CountyFIPS"

#2008
myvars<-c("CountyFIPS","PWSID","CountyServed", "State.Code","PWS.NAME","Zip.code",
          "Population.served", "Adjusted.Total.Population.Served","FullName","YearlyAverage.2008","pubpop","pop50")
Dataset108<-Dataset1[myvars]
# Remove missing rows, which are PWSs missing data for that year
Dataset108<-Dataset108[which(!is.na(Dataset108$YearlyAverage.2008)),]

#For each county, create total popserved 
vector.id <- c( unique(Dataset108$CountyFIPS)) 
count08 <- as.data.frame(vector.id)
count08$tpopser08 <- NA

for ( i in unique(Dataset108$CountyFIPS)){
  sum <- cbind ( sum(Dataset108$Adjusted.Total.Population.Served[Dataset108$CountyFIPS==i]) ) 
  count08$tpopser08[count08$vector.id==i] <- sum
}
colnames(count08)[colnames(count08)=="vector.id"] <- "CountyFIPS"

#2009
myvars<-c("CountyFIPS","PWSID","CountyServed", "State.Code","PWS.NAME","Zip.code",
          "Population.served", "Adjusted.Total.Population.Served","FullName","YearlyAverage.2009","pubpop","pop50")
Dataset109<-Dataset1[myvars]
# Remove missing rows, which are PWSs missing data for that year
Dataset109<-Dataset109[which(!is.na(Dataset109$YearlyAverage.2009)),]

#For each county, create total popserved 
vector.id <- c( unique(Dataset109$CountyFIPS)) 
count09 <- as.data.frame(vector.id)
count09$tpopser09 <- NA

for ( i in unique(Dataset109$CountyFIPS)){
  sum <- cbind ( sum(Dataset109$Adjusted.Total.Population.Served[Dataset109$CountyFIPS==i]) ) 
  count09$tpopser09[count09$vector.id==i] <- sum
}
colnames(count09)[colnames(count09)=="vector.id"] <- "CountyFIPS"


#2010
myvars<-c("CountyFIPS","PWSID","CountyServed", "State.Code","PWS.NAME","Zip.code",
          "Population.served", "Adjusted.Total.Population.Served","FullName","YearlyAverage.2010","pubpop","pop50")
Dataset110<-Dataset1[myvars]
# Remove missing rows, which are PWSs missing data for that year
Dataset110<-Dataset110[which(!is.na(Dataset110$YearlyAverage.2010)),]

#For each county, create total popserved 
vector.id <- c( unique(Dataset110$CountyFIPS)) 
count10 <- as.data.frame(vector.id)
count10$tpopser10 <- NA

for ( i in unique(Dataset110$CountyFIPS)){
  sum <- cbind ( sum(Dataset110$Adjusted.Total.Population.Served[Dataset110$CountyFIPS==i]) ) 
  count10$tpopser10[count10$vector.id==i] <- sum
}
colnames(count10)[colnames(count10)=="vector.id"] <- "CountyFIPS"

#2011
myvars<-c("CountyFIPS","PWSID","CountyServed", "State.Code","PWS.NAME","Zip.code",
          "Population.served", "Adjusted.Total.Population.Served","FullName","YearlyAverage.2011","pubpop","pop50")
Dataset111<-Dataset1[myvars]
# Remove missing rows, which are PWSs missing data for that year
Dataset111<-Dataset111[which(!is.na(Dataset111$YearlyAverage.2011)),]

#For each county, create total popserved 
vector.id <- c( unique(Dataset111$CountyFIPS)) 
count11 <- as.data.frame(vector.id)
count11$tpopser11 <- NA

for ( i in unique(Dataset111$CountyFIPS)){
  sum <- cbind ( sum(Dataset111$Adjusted.Total.Population.Served[Dataset111$CountyFIPS==i]) ) 
  count11$tpopser11[count11$vector.id==i] <- sum
}
colnames(count11)[colnames(count11)=="vector.id"] <- "CountyFIPS"

count<-merge(count06,count07,by="CountyFIPS", all=T)
count<-merge(count,count08,by="CountyFIPS", all=T)
count<-merge(count,count09,by="CountyFIPS", all=T)
count<-merge(count,count10,by="CountyFIPS", all=T)
count<-merge(count,count11,by="CountyFIPS", all=T)

# Jan 31 changed from "finaldata" to Dataset1
# Merge the population served for each year back into finaldata
finaldata<-merge(Dataset1,count,by="CountyFIPS",all=T)
finaldata<-finaldata[which(!is.na(finaldata$PWSID)),]
# tpopser06 = total pop served by PWSs in our data set in 2011
# in sens50data, if tpopser <50, 70, or 80, we will remove from our analysis
#change names for ease
colnames(finaldata)[which(names(finaldata) == "YearlyAverage.2006")] <- "Y2006"
colnames(finaldata)[which(names(finaldata) == "YearlyAverage.2007")] <- "Y2007"
colnames(finaldata)[which(names(finaldata) == "YearlyAverage.2008")] <- "Y2008"
colnames(finaldata)[which(names(finaldata) == "YearlyAverage.2009")] <- "Y2009"
colnames(finaldata)[which(names(finaldata) == "YearlyAverage.2010")] <- "Y2010"
colnames(finaldata)[which(names(finaldata) == "YearlyAverage.2011")] <- "Y2011"

## Next, for three-year averages, using Dataset1ThreeYear
#2006-2008
myvars<-c("CountyFIPS","PWSID", "State.Code","PWS.NAME","Zip.code",
          "Population.served", "Adjusted.Total.Population.Served", "FullName","ThreeYearAverage.1","pubpop","pop50")
Dataset106<-Dataset1ThreeYear[myvars]
# Remove missing rows, which are PWSs missing data for that year
Dataset106<-Dataset106[which(!is.na(Dataset106$ThreeYearAverage.1)),] #none will do this with the 3-yr average

#For each county, create total popserved 
vector.id <- c( unique(Dataset106$CountyFIPS)) 
count06 <- as.data.frame(vector.id)
count06$tpopser06 <- NA

for ( i in unique(Dataset106$CountyFIPS)){
  sum <- cbind ( sum(Dataset106$Adjusted.Total.Population.Served[Dataset106$CountyFIPS==i]) ) 
  count06$tpopser06[count06$vector.id==i] <- sum
}
colnames(count06)[colnames(count06)=="vector.id"] <- "CountyFIPS"

# checked with FIPS 01001 - okay!

#2009-2011
myvars<-c("CountyFIPS","PWSID", "State.Code","PWS.NAME","Zip.code",
          "Population.served", "Adjusted.Total.Population.Served", "FullName","ThreeYearAverage.2","pubpop","pop50")
Dataset107<-Dataset1ThreeYear[myvars]
# Remove missing rows, which are PWSs missing data for that year
Dataset107<-Dataset107[which(!is.na(Dataset107$ThreeYearAverage.2)),] #none will do this with the 3-yr average

#For each county, create total popserved 
vector.id <- c( unique(Dataset107$CountyFIPS)) 
count07 <- as.data.frame(vector.id)
count07$tpopser07 <- NA

for ( i in unique(Dataset107$CountyFIPS)){
  sum <- cbind ( sum(Dataset107$Adjusted.Total.Population.Served[Dataset107$CountyFIPS==i]) ) 
  count07$tpopser07[count07$vector.id==i] <- sum
}
colnames(count07)[colnames(count07)=="vector.id"] <- "CountyFIPS"

# checked with FIPS 01001 - okay!
countthree<-merge(count06,count07,by="CountyFIPS", all=T)

# Merge the population served for each year back into finaldata
# Changed jan 31 from mergining in finaldatathreeyears into Dataset1ThreeYear
finaldatathreeyears<-merge(Dataset1ThreeYear,countthree,by="CountyFIPS",all=T)
finaldatathreeyears<-finaldatathreeyears[which(!is.na(finaldatathreeyears$PWSID)),]
# tpopser06 = total pop served by PWSs in our data set in 2011
# in sens50data, if tpopser <50, 70, or 80, remove from our analysis
describe(finaldatathreeyears$CountyFIPS) # 2,750 individual counties


### Part 2: assign weights and conduct weighted averaging 
##First, for each year separately (finaldata)
## 2006 ##
myvars<-c("CountyFIPS","PWSID","Adjusted.Total.Population.Served","Y2006")
fd06<-finaldata[myvars]

fd06<-fd06[which(!is.na(fd06$Y2006)),]
min(fd06$Y2006)

#Create totalpop for each county
vector.id <- c( unique(fd06$CountyFIPS)) 
counties <- as.data.frame(vector.id)
counties$totalpop <- NA

for ( i in unique(fd06$CountyFIPS)){
  sum <- cbind ( sum(fd06$Adjusted.Total.Population.Served[fd06$CountyFIPS==i]) ) 
  counties$totalpop[counties$vector.id==i] <- sum
}
colnames(counties)[colnames(counties)=="vector.id"] <- "CountyFIPS"


fd06<-merge(fd06,counties,by="CountyFIPS",all.x=T)

#Create within-county population-weight for each PWSID
fd06$pwsweight <-0
fd06$pwsweight <- fd06$Adjusted.Total.Population.Served / fd06$totalpop
describe(fd06$pwsweight)
fd06$Y2006<-as.numeric(fd06$Y2006)
fd06$ind06 <-0
fd06$ind06 <- fd06$Y2006 * fd06$pwsweight
# If indo06 is "NA" we make it zero so we can still sum and not get NAs
fd06$ind06[is.na(fd06$ind06)] <- 0

#Now sum weighted arsenics for each county
vector.id <- c( unique(fd06$CountyFIPS)) 
counties06 <- as.data.frame(vector.id)
counties06$WeightedAs2006 <- NA

for ( i in unique(fd06$CountyFIPS)){
  sum <- cbind ( sum(fd06$ind06[fd06$CountyFIPS==i]) ) 
  counties06$WeightedAs2006[counties06$vector.id==i] <- sum
}
colnames(counties06)[colnames(counties06)=="vector.id"] <- "CountyFIPS"

# Checked that this is correct manually with CountyFIPS 02013
describe(counties06$WeightedAs2006)

## 2007 ##
myvars<-c("CountyFIPS","PWSID","Adjusted.Total.Population.Served","Y2007")
fd07<-finaldata[myvars]

#Remove rows missing data 
fd07<-fd07[which(!is.na(fd07$Y2007)),]

#Create totalpop for each county
vector.id <- c( unique(fd07$CountyFIPS)) 
counties <- as.data.frame(vector.id)
counties$totalpop <- NA

for ( i in unique(fd07$CountyFIPS)){
  sum <- cbind ( sum(fd07$Adjusted.Total.Population.Served[fd07$CountyFIPS==i]) ) 
  counties$totalpop[counties$vector.id==i] <- sum
}
colnames(counties)[colnames(counties)=="vector.id"] <- "CountyFIPS"


fd07<-merge(fd07,counties,by="CountyFIPS",all.x=T)

#Create within-county population-weight for each PWSID
fd07$pwsweight <-0
fd07$pwsweight <- fd07$Adjusted.Total.Population.Served / fd07$totalpop
fd07$Y2007<-as.numeric(fd07$Y2007)
fd07$ind06 <-0
fd07$ind06 <- fd07$Y2007 * fd07$pwsweight
# If indo06 is "NA" we make it zero so we can still sum and not get NAs
fd07$ind06[is.na(fd07$ind06)] <- 0

#Now sum weighted arsenics for each county
vector.id <- c( unique(fd07$CountyFIPS)) 
counties07 <- as.data.frame(vector.id)
counties07$WeightedAs2007 <- NA

for ( i in unique(fd07$CountyFIPS)){
  sum <- cbind ( sum(fd07$ind06[fd07$CountyFIPS==i]) ) 
  counties07$WeightedAs2007[counties07$vector.id==i] <- sum
}
colnames(counties07)[colnames(counties07)=="vector.id"] <- "CountyFIPS"

# Checked that this is correct manually with CountyFIPS 02013


## 2008 ##
myvars<-c("CountyFIPS","PWSID","Adjusted.Total.Population.Served","Y2008")
fd08<-finaldata[myvars]

#Remove rows missing data 
fd08<-fd08[which(!is.na(fd08$Y2008)),]

#Create totalpop for each county
vector.id <- c( unique(fd08$CountyFIPS)) 
counties <- as.data.frame(vector.id)
counties$totalpop <- NA

for ( i in unique(fd08$CountyFIPS)){
  sum <- cbind ( sum(fd08$Adjusted.Total.Population.Served[fd08$CountyFIPS==i]) ) 
  counties$totalpop[counties$vector.id==i] <- sum
}
colnames(counties)[colnames(counties)=="vector.id"] <- "CountyFIPS"


fd08<-merge(fd08,counties,by="CountyFIPS",all.x=T)

#Create within-county population-weight for each PWSID
fd08$pwsweight <-0
fd08$pwsweight <- fd08$Adjusted.Total.Population.Served / fd08$totalpop
fd08$Y2008<-as.numeric(fd08$Y2008)
fd08$ind06 <-0
fd08$ind06 <- fd08$Y2008 * fd08$pwsweight
# If indo06 is "NA" we make it zero so we can still sum and not get NAs
fd08$ind06[is.na(fd08$ind06)] <- 0

#Now sum weighted arsenics for each county
vector.id <- c( unique(fd08$CountyFIPS)) 
counties08 <- as.data.frame(vector.id)
counties08$WeightedAs2008 <- NA

for ( i in unique(fd08$CountyFIPS)){
  sum <- cbind ( sum(fd08$ind06[fd08$CountyFIPS==i]) ) 
  counties08$WeightedAs2008[counties08$vector.id==i] <- sum
}
colnames(counties08)[colnames(counties08)=="vector.id"] <- "CountyFIPS"

# Checked that this is correct manually with CountyFIPS 02013


## 2009 ##
myvars<-c("CountyFIPS","PWSID","Adjusted.Total.Population.Served","Y2009")
fd09<-finaldata[myvars]

#Remove rows missing data 
fd09<-fd09[which(!is.na(fd09$Y2009)),]

#Create totalpop for each county
vector.id <- c( unique(fd09$CountyFIPS)) 
counties <- as.data.frame(vector.id)
counties$totalpop <- NA

for ( i in unique(fd09$CountyFIPS)){
  sum <- cbind ( sum(fd09$Adjusted.Total.Population.Served[fd09$CountyFIPS==i]) ) 
  counties$totalpop[counties$vector.id==i] <- sum
}
colnames(counties)[colnames(counties)=="vector.id"] <- "CountyFIPS"


fd09<-merge(fd09,counties,by="CountyFIPS",all.x=T)

#Create within-county population-weight for each PWSID
fd09$pwsweight <-0
fd09$pwsweight <- fd09$Adjusted.Total.Population.Served / fd09$totalpop
fd09$Y2009<-as.numeric(fd09$Y2009)
fd09$ind06 <-0
fd09$ind06 <- fd09$Y2009 * fd09$pwsweight
# If indo06 is "NA" we make it zero so we can still sum and not get NAs
fd09$ind06[is.na(fd09$ind06)] <- 0

#Now sum weighted arsenics for each county
vector.id <- c( unique(fd09$CountyFIPS)) 
counties09 <- as.data.frame(vector.id)
counties09$WeightedAs2009 <- NA

for ( i in unique(fd09$CountyFIPS)){
  sum <- cbind ( sum(fd09$ind06[fd09$CountyFIPS==i]) ) 
  counties09$WeightedAs2009[counties09$vector.id==i] <- sum
}
colnames(counties09)[colnames(counties09)=="vector.id"] <- "CountyFIPS"

# Checked that this is correct manually with CountyFIPS 02013


## 2010 ##
myvars<-c("CountyFIPS","PWSID","Adjusted.Total.Population.Served","Y2010")
fd10<-finaldata[myvars]

#Remove rows missing data 
fd10<-fd10[which(!is.na(fd10$Y2010)),]

#Create totalpop for each county
vector.id <- c( unique(fd10$CountyFIPS)) 
counties <- as.data.frame(vector.id)
counties$totalpop <- NA

for ( i in unique(fd10$CountyFIPS)){
  sum <- cbind ( sum(fd10$Adjusted.Total.Population.Served[fd10$CountyFIPS==i]) ) 
  counties$totalpop[counties$vector.id==i] <- sum
}
colnames(counties)[colnames(counties)=="vector.id"] <- "CountyFIPS"


fd10<-merge(fd10,counties,by="CountyFIPS",all.x=T)

#Create within-county population-weight for each PWSID
fd10$pwsweight <-0
fd10$pwsweight <- fd10$Adjusted.Total.Population.Served / fd10$totalpop
fd10$Y2010<-as.numeric(fd10$Y2010)
fd10$ind06 <-0
fd10$ind06 <- fd10$Y2010 * fd10$pwsweight
# If indo06 is "NA" we make it zero so we can still sum and not get NAs
fd10$ind06[is.na(fd10$ind06)] <- 0

#Now sum weighted arsenics for each county
vector.id <- c( unique(fd10$CountyFIPS)) 
counties10 <- as.data.frame(vector.id)
counties10$WeightedAs2010 <- NA

for ( i in unique(fd10$CountyFIPS)){
  sum <- cbind ( sum(fd10$ind06[fd10$CountyFIPS==i]) ) 
  counties10$WeightedAs2010[counties10$vector.id==i] <- sum
}
colnames(counties10)[colnames(counties10)=="vector.id"] <- "CountyFIPS"

# Checked that this is correct manually with CountyFIPS 02013


## 2011 ##
myvars<-c("CountyFIPS","PWSID","Adjusted.Total.Population.Served","Y2011")
fd11<-finaldata[myvars]

#Remove rows missing data 
fd11<-fd11[which(!is.na(fd11$Y2011)),]

#Create totalpop for each county
vector.id <- c( unique(fd11$CountyFIPS)) 
counties <- as.data.frame(vector.id)
counties$totalpop <- NA

for ( i in unique(fd11$CountyFIPS)){
  sum <- cbind ( sum(fd11$Adjusted.Total.Population.Served[fd11$CountyFIPS==i]) ) 
  counties$totalpop[counties$vector.id==i] <- sum
}
colnames(counties)[colnames(counties)=="vector.id"] <- "CountyFIPS"


fd11<-merge(fd11,counties,by="CountyFIPS",all.x=T)

#Create within-county population-weight for each PWSID
fd11$pwsweight <-0
fd11$pwsweight <- fd11$Adjusted.Total.Population.Served / fd11$totalpop
fd11$Y2011<-as.numeric(fd11$Y2011)
fd11$ind06 <-0
fd11$ind06 <- fd11$Y2011 * fd11$pwsweight
# If indo06 is "NA" we make it zero so we can still sum and not get NAs
fd11$ind06[is.na(fd11$ind06)] <- 0

#Now sum weighted arsenics for each county
vector.id <- c( unique(fd11$CountyFIPS)) 
counties11 <- as.data.frame(vector.id)
counties11$WeightedAs2011 <- NA

for ( i in unique(fd11$CountyFIPS)){
  sum <- cbind ( sum(fd11$ind06[fd11$CountyFIPS==i]) ) 
  counties11$WeightedAs2011[counties11$vector.id==i] <- sum
}
colnames(counties11)[colnames(counties11)=="vector.id"] <- "CountyFIPS"

# Checked that this is correct manually with CountyFIPS 02013

#Merge all years back together
allyears<-merge(counties06,counties07,by="CountyFIPS",all=T)
allyears<-merge(allyears,counties08,by="CountyFIPS",all=T)
allyears<-merge(allyears,counties09,by="CountyFIPS",all=T)
allyears<-merge(allyears,counties10,by="CountyFIPS",all=T)
allyears<-merge(allyears,counties11,by="CountyFIPS",all=T)

## save
write.csv(allyears, file="allyearscounties_incl8000.csv")
write.csv(allyears, file="~/Google Drive/Research/EPAAsTrajectories/March2020/allyearscounties_incl8000.csv")


### Next, for three-year averages (2006-2008 vs 2009-2011):
## 2006-2008 ##
myvars<-c("CountyFIPS","PWSID","Adjusted.Total.Population.Served","ThreeYearAverage.1")
fd10<-finaldatathreeyears[myvars]

#Remove rows missing data 
fd10<-fd10[which(!is.na(fd10$ThreeYearAverage.1)),]

#Create totalpop for each county
vector.id <- c( unique(fd10$CountyFIPS)) 
counties <- as.data.frame(vector.id)
counties$totalpop <- NA

for ( i in unique(fd10$CountyFIPS)){
  sum <- cbind ( sum(fd10$Adjusted.Total.Population.Served[fd10$CountyFIPS==i]) ) 
  counties$totalpop[counties$vector.id==i] <- sum
}
colnames(counties)[colnames(counties)=="vector.id"] <- "CountyFIPS"

fd10<-merge(fd10,counties,by="CountyFIPS",all.x=T)

#Create within-county population-weight for each PWSID
fd10$pwsweight <-0
fd10$pwsweight <- fd10$Adjusted.Total.Population.Served / fd10$totalpop
fd10$Y2010<-as.numeric(fd10$ThreeYearAverage.1)
fd10$ind06 <-0
fd10$ind06 <- fd10$Y2010 * fd10$pwsweight
# If indo06 is "NA" we make it zero so we can still sum and not get NAs
fd10$ind06[is.na(fd10$ind06)] <- 0

#Now sum weighted arsenics for each county
vector.id <- c( unique(fd10$CountyFIPS)) 
counties10 <- as.data.frame(vector.id)
counties10$WeightedAs20062008 <- NA

for ( i in unique(fd10$CountyFIPS)){
  sum <- cbind ( sum(fd10$ind06[fd10$CountyFIPS==i]) ) 
  counties10$WeightedAs20062008[counties10$vector.id==i] <- sum
}
colnames(counties10)[colnames(counties10)=="vector.id"] <- "CountyFIPS"
# Checked that this is correct manually 


## 2009-2011 ##
myvars<-c("CountyFIPS","PWSID","Adjusted.Total.Population.Served","ThreeYearAverage.2")
fd11<-finaldatathreeyears[myvars]

#Remove rows missing data 
fd11<-fd11[which(!is.na(fd11$ThreeYearAverage.2)),]

#Create totalpop for each county
vector.id <- c( unique(fd11$CountyFIPS)) 
counties <- as.data.frame(vector.id)
counties$totalpop <- NA

for ( i in unique(fd11$CountyFIPS)){
  sum <- cbind ( sum(fd11$Adjusted.Total.Population.Served[fd11$CountyFIPS==i]) ) 
  counties$totalpop[counties$vector.id==i] <- sum
}
colnames(counties)[colnames(counties)=="vector.id"] <- "CountyFIPS"


fd11<-merge(fd11,counties,by="CountyFIPS",all.x=T)

#Create within-county population-weight for each PWSID
fd11$pwsweight <-0
fd11$pwsweight <- fd11$Adjusted.Total.Population.Served / fd11$totalpop
fd11$Y2011<-as.numeric(fd11$ThreeYearAverage.2)
fd11$ind06 <-0
fd11$ind06 <- fd11$Y2011 * fd11$pwsweight
# If indo06 is "NA" we make it zero so we can still sum and not get NAs
fd11$ind06[is.na(fd11$ind06)] <- 0

#Now sum weighted arsenics for each county
vector.id <- c( unique(fd11$CountyFIPS)) 
counties11 <- as.data.frame(vector.id)
counties11$WeightedAs20092011 <- NA

for ( i in unique(fd11$CountyFIPS)){
  sum <- cbind ( sum(fd11$ind06[fd11$CountyFIPS==i]) ) 
  counties11$WeightedAs20092011[counties11$vector.id==i] <- sum
}
colnames(counties11)[colnames(counties11)=="vector.id"] <- "CountyFIPS"

# Checked that this is correct manually
#Merge all years back together
allyearsthree<-merge(counties10,counties11,by="CountyFIPS",all=T)
write.csv(allyearsthree, file="allyearsthreecounties_incl8000.csv")
write.csv(allyearsthree, file="~/Google Drive/Research/EPAAsTrajectories/March2020/allyearsthreecounties_incl8000.csv")

# Now, allyears and allyearsthree contain the yearly or 3-year average county-weighted As values,
# weighted by pop served. These do not yet replace values with NA for counties served by systems
# representing <50% of public water pop. These population served estimates are in finaldata (yearly averages) and 
# finaldatathreeyears (three-year averages)


# The two approaches below are probably a) averaging all values within the 6-year period, or b) 
# averaging the yearly averages to the 6-year period


# We create the 6-year averages below in the next section after assigning county-served
# Two examples of sensitivity analyses are below considering alternative approaches,
# which first aggregate 6-year averages the CWS level before weighting within a county

## Finally, the average of both three-year periods for the Map:
# Two examples of sensitivity analyses are below considering alternative approaches,
# which first aggregate 6-year averages the CWS level before weighting within a county

## not run 
#below we merge this TotalWeightedAs back with total county database for mapping purposes later
#fig1data<-merge(allyears,f1,by="CountyFIPS",all.x=T)
#write.csv(fig1data, file="fig1data_M2020.csv")
#this needs to really be merged into 'notlowcounties50subset' for mapping ease (figure1)

#myvars<-c("CountyFIPS","tpopserf1","pop50")
#tomerge<-fig1l[myvars]
#tomerge<-tomerge[!duplicated(tomerge$CountyFIPS),]
#below we merge 'tomerge' back in

# double check with Miami-Dade county, FL (12086)
#flmi<-fig1data[which(fig1data$CountyFIPS=="12086"),]
#flmi2<-fig1l[which(fig1l$CountyFIPS=="12086"),]
#sum(flmi2$indas) #0.5905379 yes correct


### Other options we explored for calculating 6-year averages:
### 1. Using SixYearAverage.1 from widesixyearlycorrected - 
# this approach takes the 6 yearly averages and averages them up 

# #View(widesixyearlycorrected)
# # ^This has the correct six-year averages for each PWSID
# myvars<-c("PWSID","Adjusted.Total.Population.Served")
# tm<-corrected3[myvars]
# f1dat<-merge(widesixyearlycorrected,tm,by="PWSID",all.x=T)
# f1dat<-f1dat[!duplicated(f1dat$PWSID),]
# 
# myvars<-c("PWSID","CountyFIPS","CountyServed","State.Code","PWS.NAME","Zip.code","FullName","pop50")
# fdtm<-Dataset1[myvars]
# describe(fdtm$CountyFIPS) # correct number of coutnies (2740)
# describe(fdtm$PWSID) # duplicate PWSIDs for those that serve multipel counties
# f1dat<-merge(f1dat,fdtm,by="PWSID",all.x=T)
# describe(f1dat$PWSID) # 
# describe(f1dat$SixYearAverage.1) # none missing
# describe(f1dat$CountyFIPS) # correct 2740; 692 missing CountyFIPS; these were excluded earlier b/c county not assigned; we delete them 
# 
# f1dat<-f1dat[which(!is.na(f1dat$CountyFIPS)),]
# describe(f1dat$PWSID)
# describe(f1dat$CountyFIPS)
# # We use "f1dat" for this calculation: 
# 
# ### First, create totalpop for each county
# vector.id <- c( unique(f1dat$CountyFIPS)) 
# f1count <- as.data.frame(vector.id)
# f1count$tpopserf1 <- NA
# 
# for ( i in unique(f1dat$CountyFIPS)){
#   sum <- cbind ( sum(f1dat$Adjusted.Total.Population.Served[f1dat$CountyFIPS==i]) ) 
#   f1count$tpopserf1[f1count$vector.id==i] <- sum
# }
# colnames(f1count)[colnames(f1count)=="vector.id"] <- "CountyFIPS"
# 
# #check if correct
# x<-f1dat[which(f1dat$CountyFIPS=="01001"),]
# sum(x$Adjusted.Total.Population.Served)#230259
# #yes matches with f1count
# #matches; this is okay
# 
# fig1l<-merge(f1dat,f1count,by="CountyFIPS",all.x=T)
# 
# ### Second, create within-county population-weight for each PWSID
# fig1l$pwsweight <-0
# fig1l$pwsweight <- (fig1l$Adjusted.Total.Population.Served) / (fig1l$tpopserf1)
# describe(fig1l$pwsweight)
# is.numeric(fig1l$SixYearAverage.1)
# fig1l$indas <-0
# fig1l$indas <- fig1l$SixYearAverage.1 * fig1l$pwsweight
# # If indoas is "NA" we make it zero so we can still sum and not get NAs
# fig1l$indas[is.na(fig1l$indas)] <- 0
# 
# #Now sum weighted arsenics for each county
# vector.id <- c( unique(fig1l$CountyFIPS)) 
# f1 <- as.data.frame(vector.id)
# f1$WeightedAs <- NA
# 
# for ( i in unique(fig1l$CountyFIPS)){
#   sum <- cbind ( sum(fig1l$indas[fig1l$CountyFIPS==i]) ) 
#   f1$WeightedAs[f1$vector.id==i] <- sum
# }
# colnames(f1)[colnames(f1)=="vector.id"] <- "CountyFIPS"
# describe(f1$WeightedAs)
# colnames(f1)[colnames(f1)=="WeightedAs"] <- "TotalWeightedAs"
# 
# 
# 
# ### Examples of how we are modeling our averages.
# ## At the CWS-level, we have averaged all records within a 3-year period
# 
# # Let's look at Elko, NV (32007) and Mono, CA (06051)
# # which have 6-yr averages greater than either 3-year period
# f1$TotalWeightedAs[which(f1$CountyFIPS=="32007")] #6.970015
# allyearsthree$WeightedAs20062008[which(allyearsthree$CountyFIPS=="32007")] #6.018259
# allyearsthree$WeightedAs20092011[which(allyearsthree$CountyFIPS=="32007")] #6.572379
# f1$TotalWeightedAs[which(f1$CountyFIPS=="06051")] #10.86411
# 
# # Re-create yearly, three-year, and six-year averages to double check
# wideyearlycorrected$YearlyAverage.2006[which(wideyearlycorrected$PWSID=="NV0000351")]#yes correct
# ch<-Dataset1[which(Dataset1$CountyFIPS=="32007"),]
# sum(ch$Adjusted.Total.Population.Served[which(!is.na(ch$YearlyAverage.2006))]) #18029
# mean(ch$YearlyAverage.2006,na.rm=T) #4.636806
# 
# # manual yearly averager for 2006:
# (7* 0.003327972) + (0.35 *0.008319929 ) + (0.35 *0.001663986 ) + (27.55556 * 0.2021743) + (4.71250* 0.3720672) +(0.35* 0.06877808) + (0.35* 0.01109324)+ (5* 0.0746575) +(0.35* 0.02495979) +(0.35* 0.232958)
# # 2006 yearly average manual: 7.842697
# allyears$WeightedAs2006[which(allyears$CountyFIPS=="32007")] #7.842695 matches
# # simple average of yearly averages: 8.145894
# 
# ThreeYearlyValues$ThreeYearAverage[which(ThreeYearlyValues$PWSID=="093200366")] #9.571429
# widethreeyearlycorrected$ThreeYearAverage.2[which(widethreeyearlycorrected$PWSID=="093200366")] #9.571429
# wideyearlycorrected[which(wideyearlycorrected$PWSID=="093200366"),] #( 11+13+8.6 )/3 = 10.86667
# widesixyearlycorrected$SixYearAverage.1[which(widesixyearlycorrected$PWSID=="093200366")] # 10.86667
# ### Our widesixyearlycorrected$SixYearAverage.1 values were computed differently; these are not the average of all records in a 6-year period;
# #    These are the average of each yearly average, which does not match how we did 3-year averages
# 
# # We should either:
# # A. Take average of all records in six year period, replacing with 6-year finished averages if appropriate
# # B. Average the two 3-year averages as they are
# 
# 
# # YearlyAverage3 for time period 2 for 093200366 is correct at 9.571429 
# # This is because:
# My6YR[which(My6YR$PWSID=="093200366"),] # average of these 7 values over 3 years (second period) is 9.57
# # If we first average up to the year, then take the 3 year average, we get:
# Dataset1[which(Dataset1$PWSID=="093200366"),] # an average of 10.87 (11+13+8.6)/3
# 
# # Which way did we do 3-year averages? see finaldatathreeyears , which is the basis of 3-year averages: 9.571429
# # Our three-year averages simply average all records within the three year period. 

### 2. Below here is old code where we averaged the 6 yearly averages; this was a sensitivity analysis
# ## Figure 1 ##
# #create long dataset so we can average within a county
# library(tidyr)
# fig1long<-gather(finaldata, year, arsenic, Y2006:Y2011, factor_key=TRUE);dim(fig1long) #220044
# 
# #Remove rows missing data 
# fig1long<-fig1long[which(!is.na(fig1long$arsenic)),];dim(fig1long)#89394
# 
# #Create totalpop for each county
# vector.id <- c( unique(fig1long$CountyFIPS)) 
# f1count <- as.data.frame(vector.id)
# f1count$tpopserf1 <- NA
# 
# for ( i in unique(fig1long$CountyFIPS)){
#   sum <- cbind ( sum(fig1long$Adjusted.Total.Population.Served[fig1long$CountyFIPS==i]) ) 
#   f1count$tpopserf1[f1count$vector.id==i] <- sum
# }
# colnames(f1count)[colnames(f1count)=="vector.id"] <- "CountyFIPS"
# 
# #check if correct
# x<-fig1long[which(fig1long$CountyFIPS=="01001"),]
# sum(x$Adjusted.Total.Population.Served)#230259
# #yes matches with f1count
# x$tpop0611<-(x$tpopser06+x$tpopser07+x$tpopser08+x$tpopser09+x$tpopser10+x$tpopser11)
# #matches; this is okay
# 
# fig1long<-merge(fig1long,f1count,by="CountyFIPS",all.x=T)
# 
# #Create within-county population-weight for each PWSID
# fig1long$pwsweight <-0
# fig1long$pwsweight <- (fig1long$Adjusted.Total.Population.Served) / (fig1long$tpopserf1)
# describe(fig1long$pwsweight)
# is.numeric(fig1long$arsenic)
# fig1long$arsenic<-as.numeric(fig1long$arsenic)
# fig1long$indas <-0
# fig1long$indas <- fig1long$arsenic * fig1long$pwsweight
# # If indoas is "NA" we make it zero so we can still sum and not get NAs
# fig1long$indas[is.na(fig1long$indas)] <- 0
# 
# #Now sum weighted arsenics for each county
# vector.id <- c( unique(fig1long$CountyFIPS)) 
# f1 <- as.data.frame(vector.id)
# f1$WeightedAs <- NA
# 
# for ( i in unique(fig1long$CountyFIPS)){
#   sum <- cbind ( sum(fig1long$indas[fig1long$CountyFIPS==i]) ) 
#   f1$WeightedAs[f1$vector.id==i] <- sum
# }
# colnames(f1)[colnames(f1)=="vector.id"] <- "CountyFIPS"
# describe(f1$WeightedAs)
# colnames(f1)[colnames(f1)=="WeightedAs"] <- "TotalWeightedAs"
# 
# #below we merge this TotalWeightedAs back with total county database for mapping purposes later
# fig1data<-merge(allyears,f1,by="CountyFIPS",all.x=T)
# write.csv(fig1data, file="fig1data_incl8000.csv")
# #this needs to really be merged into 'notlowcounties50subset' for mapping ease (figure1)
# 
# myvars<-c("CountyFIPS","tpopserf1","pop50")
# tomerge<-fig1long[myvars]
# tomerge<-tomerge[!duplicated(tomerge$CountyFIPS),]
# #below we merge 'tomerge' back in
# 
# # double check with Miami-Dade county, FL (12086)
# flmi<-fig1data[which(fig1data$CountyFIPS=="12086"),]
# flmi2<-fig1long[which(fig1long$CountyFIPS=="12086"),]
# sum(flmi2$indas) #0.5905379 yes correct

# ### end 

####### 7. Keep only counties with 50% or more total pop served; create 6-year average  #######
# those with <50% served get replaced with NA

### First for yearly averages
myvars<-c("CountyFIPS", "pubpop","pop50","pop70","pop80", "tpopser06","tpopser07",
          "tpopser08","tpopser09","tpopser10","tpopser11")
d<-finaldata[myvars]
dd<-d[!duplicated(d$CountyFIPS),]

s50<-merge(allyears,dd,by="CountyFIPS",all.x=T)
s50$Flag<-0
s50$Flag[which(s50$tpopser06<s50$pop50)]<-1
table(s50$Flag) #N=406 with Flag =1

#If tpopser<50%, replace with "."
s50$WeightedAs2006[which(s50$tpopser06<s50$pop50)]<-NA
s50$WeightedAs2007[which(s50$tpopser07<s50$pop50)]<-NA
s50$WeightedAs2008[which(s50$tpopser08<s50$pop50)]<-NA
s50$WeightedAs2009[which(s50$tpopser09<s50$pop50)]<-NA
s50$WeightedAs2010[which(s50$tpopser10<s50$pop50)]<-NA
s50$WeightedAs2011[which(s50$tpopser11<s50$pop50)]<-NA
write.csv(s50, file="s50_incl8000.csv")

#70% cut off
s70<-merge(allyears,dd,by="CountyFIPS",all.x=T)
s70$WeightedAs2006[which(s70$tpopser06<s70$pop70)]<-NA
s70$WeightedAs2007[which(s70$tpopser07<s70$pop70)]<-NA
s70$WeightedAs2008[which(s70$tpopser08<s70$pop70)]<-NA
s70$WeightedAs2009[which(s70$tpopser09<s70$pop70)]<-NA
s70$WeightedAs2010[which(s70$tpopser10<s70$pop70)]<-NA
s70$WeightedAs2011[which(s70$tpopser11<s70$pop70)]<-NA
write.csv(s70, file="s70_incl8000.csv")

#80% cutoff
s80<-merge(allyears,dd,by="CountyFIPS",all.x=T)
s80$WeightedAs2006[which(s80$tpopser06<s80$pop80)]<-NA
s80$WeightedAs2007[which(s80$tpopser07<s80$pop80)]<-NA
s80$WeightedAs2008[which(s80$tpopser08<s80$pop80)]<-NA
s80$WeightedAs2009[which(s80$tpopser09<s80$pop80)]<-NA
s80$WeightedAs2010[which(s80$tpopser10<s80$pop80)]<-NA
s80$WeightedAs2011[which(s80$tpopser11<s80$pop80)]<-NA
write.csv(s80, file="s80_incl8000.csv")


# ### Next for overall arsenic 2006-2011 (Figure 1)
# #we merge back in tomerge (which has pop counts and cut-offs)
# tomerge$pop50f0611<-(tomerge$pop50)*6
# #now, we multiply pop50 by 6 - this tells us 50% of the # of public-well water reliant population
# # in total from 2006-2011 for that county
# # if tpopserf1 is <pop506, we treat that county as missing
# fig150<-merge(f1,tomerge,by="CountyFIPS") #add TotalWeightedAs back to dd
# describe(fig150$CountyFIPS)
# 
# fig150$Flag<-0
# fig150$Flag[which(fig150$tpopserf1<fig150$pop506)]<-1
# table(fig150$Flag)
# #   0 
# # 2740
# # None of these counties need to be excluded for being represented by <50% of PW reliant population
# write.csv(fig150, file="fig150_incl8000.csv")

### Finally, repeat for three-year averages 
myvars<-c("CountyFIPS", "pubpop","pop50","pop70","pop80", "tpopser06","tpopser07")
d<-finaldatathreeyears[myvars]
dd<-d[!duplicated(d$CountyFIPS),]

s50three<-merge(allyearsthree,dd,by="CountyFIPS",all.x=T)
s50three$Flagy1<-0
s50three$Flagy1[which(s50three$tpopser06<s50three$pop50)]<-1
s50three$Flagy2<-0
s50three$Flagy2[which(s50three$tpopser07<s50three$pop50)]<-1
table(s50three$Flagy1) #206 FLags for time 1
table(s50three$Flagy2) #117 flags for time 2
flagsformap<-s50three

#If popser<50%, replace with "." :
s50three$WeightedAs20062008[which(s50three$tpopser06<s50three$pop50)]<-"."
s50three$WeightedAs20092011[which(s50three$tpopser07<s50three$pop50)]<-"."
write.csv(s50three, file="s50three_incl8000.csv")

#how mnay counties have any As value >1 ?
notlows50three<-s50three[which(s50three$WeightedAs20062008>1 | s50three$WeightedAs20092011> 1),];dim(notlows50three)
#[1] 962  11

## See if any missing all years (replacing with NA could have knocked out)
myvars<-c("CountyFIPS","WeightedAs20062008","WeightedAs20092011")
check<-notlows50three[myvars]
check[is.na(check)] <- "."
check$WeightedAs20062008[which(check$WeightedAs20062008==".")]<-NA
check$WeightedAs20092011[which(check$WeightedAs20092011==".")]<-NA
missingallthree<-check[is.na(check$WeightedAs20062008)&is.na(check$WeightedAs20092011),]
#none missing both
# how many are missing the first three years?
describe(check$WeightedAs20062008) #125 missing
describe(check$WeightedAs20092011) #19 missing


### Finally, construct the average of both three-year periods for the Map:
# For our descriptive mapping purposes we will use the average of the two three-year periods
## 2006-2011 ##
myvars<-c("CountyFIPS","WeightedAs20062008","WeightedAs20092011")
toavg<-s50three[myvars]
class(toavg$WeightedAs20062008)
toavg$WeightedAs20062008<-as.numeric(toavg$WeightedAs20062008)
toavg$WeightedAs20092011<-as.numeric(toavg$WeightedAs20092011)

toavg <- transform(toavg, WeightedAs20062011 = rowMeans(toavg[,-1], na.rm = TRUE))
toavg$WeightedAs20062011[which(toavg$WeightedAs20062011=="NaN")]<-NA # Make sure NAs are read as NAs not NaN


# Merge WeightedAs20062011 back into s50three
myvars<-c("CountyFIPS","WeightedAs20062011")
tomerge<-toavg[myvars]
s50three<-merge(s50three,tomerge,by="CountyFIPS")

# Save all three averages at county-level to fig1data for plotting
fig1data<-toavg

### end 


####### 8. Create additional variables needed for analysis ######
# Rename our dataframes for clarity
clusters<-read.csv("clustershare.csv")
#Fix FIPS codes for counties, these need the proper number of decimals
library(stringr)
clusters$CountyFIPS<-str_pad(clusters$FIPS, width=5, pad="0")


finaldatadups<-finaldata #one record for each CWS serving each county, yearly averages (rarely used)
finaldatadups3<-finaldatathreeyears #one record for each CWS serving each county, 3-yr averages (rarely used)
finaldatadups3<-merge(finaldatadups3,clusters,by="CountyFIPS",all.x=T);dim(finaldatadups3)
finaldata<-finaldatacws #- one record for each CWS, yearly averages
finaldatathreeyears<-finaldatacwsthreeyears #- one record for each CWS, 3-yr averages (Main database)

# A reminder of dataframes working with:
describe(finaldata$CountyFIPS);describe(finaldata$PWSID) # finaldatacws - one record for each CWS, yearly averages
describe(finaldatathreeyears$CountyFIPS);describe(finaldatathreeyears$PWSID) # finaldatacwsthreeyears - one record for each CWS, 3-yr averages (Main database)
describe(s50$CountyFIPS) # one record each county, replaced with NA when pop50< 50% served, yearly averages
describe(s50three$CountyFIPS) # one record each county, replaced with NA when pop50< 50% served, 3-yr averages (Main database)

describe(finaldata$Population.served)
describe(finaldata$Source.Water.Type)
describe(finaldata$State.Code)
table(finaldata$State.Code)

#Make new variable that identifies tribal PWSs - these either start with NN or do not have 
#alphabetic characters before letters; Records 1-241 and NN records
#organize by PWSID first
finaldata <- finaldata[order(finaldata$PWSID),] 
finaldata$istribal <- grepl("NN", finaldata$PWSID)
finaldata$istribal[1:314]<-"TRUE"
table(finaldata$istribal)
# FALSE  TRUE 
# 35976   430 

finaldatathreeyears <- finaldatathreeyears[order(finaldatathreeyears$PWSID),] 
finaldatathreeyears$istribal <- grepl("NN", finaldatathreeyears$PWSID)
finaldatathreeyears$istribal[1:314]<-"TRUE"
table(finaldatathreeyears$istribal)

#Make new variable for region
# Based on Ayotte et al 2017 (Environ Sci Technology), in part
# Also based on US Census Bureau Regions and Divisions
finaldata$Region<-"None"
finaldata$Region[which(finaldata$State.Code=="WA"|finaldata$State.Code=="OR"|finaldata$State.Code=="ID"|
                         finaldata$State.Code=="MT"|finaldata$State.Code=="WY")]<-"PNW" #PacificNW/Moutnain

finaldata$Region[which(finaldata$State.Code=="CA"|finaldata$State.Code=="NV"|finaldata$State.Code=="UT"|
                         finaldata$State.Code=="CO"|finaldata$State.Code=="AZ"|finaldata$State.Code=="NM"|
                         finaldata$State.Code=="TX")]<-"SW"#Southwest/Moutnain

finaldata$Region[which(finaldata$State.Code=="ND"|finaldata$State.Code=="SD"|finaldata$State.Code=="NE"|
                         finaldata$State.Code=="KS"|finaldata$State.Code=="MO")]<-"CMW" #CentralMW


finaldata$Region[which(finaldata$State.Code=="WI"|finaldata$State.Code=="IL"|finaldata$State.Code=="IN"|
                         finaldata$State.Code=="MI"|finaldata$State.Code=="OH"|finaldata$State.Code=="MN"|
                         finaldata$State.Code=="IA")]<-"EMW" #EasternMW

finaldata$Region[which(finaldata$State.Code=="OK"|finaldata$State.Code=="AR"|finaldata$State.Code=="LA"|
                         finaldata$State.Code=="MS"|finaldata$State.Code=="AL"|finaldata$State.Code=="FL"|
                         finaldata$State.Code=="GA"|finaldata$State.Code=="TN"|finaldata$State.Code=="KY"|
                         finaldata$State.Code=="SC"|finaldata$State.Code=="NC"|finaldata$State.Code=="VA"|
                         finaldata$State.Code=="WV")]<-"SE" #Southeast

finaldata$Region[which(finaldata$State.Code=="PA"|finaldata$State.Code=="MD"|finaldata$State.Code=="DC"
                       |finaldata$State.Code=="DE"|finaldata$State.Code=="NY"|finaldata$State.Code=="NJ"|
                         finaldata$State.Code=="CT"|finaldata$State.Code=="RI")]<-"MA" #MidAtlantic

finaldata$Region[which(finaldata$State.Code=="MA"|finaldata$State.Code=="VT"|finaldata$State.Code=="NH"|
                         finaldata$State.Code=="ME")]<-"NE"#New England

finaldata$Region[which(finaldata$State.Code=="HI"|finaldata$State.Code=="AK")]<-"AKHI" #AK and HI

table(finaldata$Region)
# AKHI  CMW  EMW   MA   NE  PNW   SE   SW 
# 482 2648 6063 4879 1729 4428 7795 8382

finaldatathreeyears$Region<-"None"
finaldatathreeyears$Region[which(finaldatathreeyears$State.Code=="WA"|finaldatathreeyears$State.Code=="OR"|finaldatathreeyears$State.Code=="ID"|
                                   finaldatathreeyears$State.Code=="MT"|finaldatathreeyears$State.Code=="WY")]<-"PNW" #PacificNW/Moutnain

finaldatathreeyears$Region[which(finaldatathreeyears$State.Code=="CA"|finaldatathreeyears$State.Code=="NV"|finaldatathreeyears$State.Code=="UT"|
                                   finaldatathreeyears$State.Code=="CO"|finaldatathreeyears$State.Code=="AZ"|finaldatathreeyears$State.Code=="NM"|
                                   finaldatathreeyears$State.Code=="TX")]<-"SW"#Southwest/Moutnain

finaldatathreeyears$Region[which(finaldatathreeyears$State.Code=="ND"|finaldatathreeyears$State.Code=="SD"|finaldatathreeyears$State.Code=="NE"|
                                   finaldatathreeyears$State.Code=="KS"|finaldatathreeyears$State.Code=="MO")]<-"CMW" #CentralMW


finaldatathreeyears$Region[which(finaldatathreeyears$State.Code=="WI"|finaldatathreeyears$State.Code=="IL"|finaldatathreeyears$State.Code=="IN"|
                                   finaldatathreeyears$State.Code=="MI"|finaldatathreeyears$State.Code=="OH"|finaldatathreeyears$State.Code=="MN"|
                                   finaldatathreeyears$State.Code=="IA")]<-"EMW" #EasternMW

finaldatathreeyears$Region[which(finaldatathreeyears$State.Code=="OK"|finaldatathreeyears$State.Code=="AR"|finaldatathreeyears$State.Code=="LA"|
                                   finaldatathreeyears$State.Code=="MS"|finaldatathreeyears$State.Code=="AL"|finaldatathreeyears$State.Code=="FL"|
                                   finaldatathreeyears$State.Code=="GA"|finaldatathreeyears$State.Code=="TN"|finaldatathreeyears$State.Code=="KY"|
                                   finaldatathreeyears$State.Code=="SC"|finaldatathreeyears$State.Code=="NC"|finaldatathreeyears$State.Code=="VA"|
                                   finaldatathreeyears$State.Code=="WV")]<-"SE" #Southeast

finaldatathreeyears$Region[which(finaldatathreeyears$State.Code=="PA"|finaldatathreeyears$State.Code=="MD"|finaldatathreeyears$State.Code=="DC"
                                 |finaldatathreeyears$State.Code=="DE"|finaldatathreeyears$State.Code=="NY"|finaldatathreeyears$State.Code=="NJ"|
                                   finaldatathreeyears$State.Code=="CT"|finaldatathreeyears$State.Code=="RI")]<-"MA" #MidAtlantic

finaldatathreeyears$Region[which(finaldatathreeyears$State.Code=="MA"|finaldatathreeyears$State.Code=="VT"|finaldatathreeyears$State.Code=="NH"|
                                   finaldatathreeyears$State.Code=="ME")]<-"NE"#New England

finaldatathreeyears$Region[which(finaldatathreeyears$State.Code=="HI"|finaldatathreeyears$State.Code=="AK")]<-"AKHI" #AK and HI

table(finaldatathreeyears$Region)



# Re-code the source water type
table(finaldata$Source.Water.Type)
#  
#         GU    GW   GWP    SW   SWP 
#    2     3 31116     4  5263    18 
# Where, GU: GW under influence of SW; SWP surface water purchased
finaldata$Source.Water.Type <- gsub(" ", "", finaldata$Source.Water.Type) #Removes space 
table(finaldata$Source.Water.Type)
# 
finaldata$Source.Water.Type[which(finaldata$Source.Water.Type=="GWP")]<-"GW"
finaldata$Source.Water.Type[which(finaldata$Source.Water.Type=="SWP")]<-"SW"
table(finaldata$Source.Water.Type)
#      GU    GW    SW 
# 2     3 31120  5281 

finaldatathreeyears$Source.Water.Type <- gsub(" ", "", finaldatathreeyears$Source.Water.Type) #Removes space 
table(finaldatathreeyears$Source.Water.Type)
# 
finaldatathreeyears$Source.Water.Type[which(finaldatathreeyears$Source.Water.Type=="GWP")]<-"GW"
finaldatathreeyears$Source.Water.Type[which(finaldatathreeyears$Source.Water.Type=="SWP")]<-"SW"
table(finaldatathreeyears$Source.Water.Type)


# Merge in sociodemographic county clusters
###-------- A. CWS-level -----------###
#Load cluster data from Wallace et al. 2019 and merge uniqueFIPS with cluster ID
clusters<-read.csv("clustershare.csv")
#Fix FIPS codes for counties, these need the proper number of decimals
library(stringr)
clusters$CountyFIPS<-str_pad(clusters$FIPS, width=5, pad="0")
table(clusters$CountyFIPS)
describe(clusters$CountyFIPS)
tb<-merge(finaldata,clusters,by="CountyFIPS",all.x=T);dim(tb)#[1] 36406    29
describe(tb$cluster.number)
tbthree<-merge(finaldatathreeyears,clusters,by="CountyFIPS",all.x=T);dim(tbthree)#[1] 36406    25


###-------- A. County level -----------###
#Merge County names back to help us ID counties 
myvars<-c("fullfips", "FullName", "State.Code")
fips2<-fips[myvars]
colnames(fips2)[colnames(fips2)=="fullfips"] <- "CountyFIPS"
class(fips2$CountyFIPS)
class(s50$CountyFIPS)
s50$CountyFIPS<-as.character(s50$CountyFIPS)

notlowcounties50subset<-merge(s50,fips2,by="CountyFIPS", all.x=T);dim(s50) #2740; merging back in the county-lvel data replaced with
# NA when the pop50< 50% 
notlowcounties50subset3<-merge(s50three,fips2,by="CountyFIPS", all.x=T);dim(s50three);dim(notlowcounties50subset3) #2740

notlowcounties50subset$WeightedAs2006<-as.numeric(notlowcounties50subset$WeightedAs2006)
notlowcounties50subset$WeightedAs2007<-as.numeric(notlowcounties50subset$WeightedAs2007)
notlowcounties50subset$WeightedAs2008<-as.numeric(notlowcounties50subset$WeightedAs2008)
notlowcounties50subset$WeightedAs2009<-as.numeric(notlowcounties50subset$WeightedAs2009)
notlowcounties50subset$WeightedAs2010<-as.numeric(notlowcounties50subset$WeightedAs2010)
notlowcounties50subset$WeightedAs2011<-as.numeric(notlowcounties50subset$WeightedAs2011)

notlowcounties50subset3$WeightedAs20062008<-as.numeric(notlowcounties50subset3$WeightedAs20062008)
notlowcounties50subset3$WeightedAs20092011<-as.numeric(notlowcounties50subset3$WeightedAs20092011)

describe(notlowcounties50subset3$FullName)
who<-notlowcounties50subset3[which(is.na(notlowcounties50subset3$FullName)),]
#These three counties/county-equivalents have changed over the time and are not represented in the "fips" dataset we used

describe(notlowcounties50subset3$State.Code) # missing 3 from Alaska; these are:
#county FIPS 02201: "Prince of Wales-Outer Ketchikan"
# 02232: "Skagway-Hoonah-Angoon"
# 02280: "Wrangell-Petersburg"
notlowcounties50subset3$State.Code[notlowcounties50subset3$CountyFIPS=="02201"]<-"AK"
notlowcounties50subset3$State.Code[notlowcounties50subset3$CountyFIPS=="02232"]<-"AK"
notlowcounties50subset3$State.Code[notlowcounties50subset3$CountyFIPS=="02280"]<-"AK"
describe(notlowcounties50subset3$State.Code) # missing 0

max(notlowcounties50subset$WeightedAs2006,na.rm=T) #44.28
max(notlowcounties50subset$WeightedAs2007,na.rm=T)  #27.6
max(notlowcounties50subset$WeightedAs2008,na.rm=T) #49
max(notlowcounties50subset$WeightedAs2009,na.rm=T) #240.2
max(notlowcounties50subset$WeightedAs2010,na.rm=T) #41.6
max(notlowcounties50subset$WeightedAs2011,na.rm=T) #55.45
notlowcounties50subset3$Region<-"None"
notlowcounties50subset3$Region[which(notlowcounties50subset3$State.Code=="WA"|notlowcounties50subset3$State.Code=="OR"|notlowcounties50subset3$State.Code=="ID"|
                                       notlowcounties50subset3$State.Code=="MT"|notlowcounties50subset3$State.Code=="WY")]<-"PNW" #PacificNW/Moutnain

notlowcounties50subset3$Region[which(notlowcounties50subset3$State.Code=="CA"|notlowcounties50subset3$State.Code=="NV"|notlowcounties50subset3$State.Code=="UT"|
                                       notlowcounties50subset3$State.Code=="CO"|notlowcounties50subset3$State.Code=="AZ"|notlowcounties50subset3$State.Code=="NM"|
                                       notlowcounties50subset3$State.Code=="TX")]<-"SW"#Southwest/Moutnain

notlowcounties50subset3$Region[which(notlowcounties50subset3$State.Code=="ND"|notlowcounties50subset3$State.Code=="SD"|notlowcounties50subset3$State.Code=="NE"|
                                       notlowcounties50subset3$State.Code=="KS"|notlowcounties50subset3$State.Code=="MO")]<-"CMW" #CentralMW


notlowcounties50subset3$Region[which(notlowcounties50subset3$State.Code=="WI"|notlowcounties50subset3$State.Code=="IL"|notlowcounties50subset3$State.Code=="IN"|
                                       notlowcounties50subset3$State.Code=="MI"|notlowcounties50subset3$State.Code=="OH"|notlowcounties50subset3$State.Code=="MN"|
                                       notlowcounties50subset3$State.Code=="IA")]<-"EMW" #EasternMW

notlowcounties50subset3$Region[which(notlowcounties50subset3$State.Code=="OK"|notlowcounties50subset3$State.Code=="AR"|notlowcounties50subset3$State.Code=="LA"|
                                       notlowcounties50subset3$State.Code=="MS"|notlowcounties50subset3$State.Code=="AL"|notlowcounties50subset3$State.Code=="FL"|
                                       notlowcounties50subset3$State.Code=="GA"|notlowcounties50subset3$State.Code=="TN"|notlowcounties50subset3$State.Code=="KY"|
                                       notlowcounties50subset3$State.Code=="SC"|notlowcounties50subset3$State.Code=="NC"|notlowcounties50subset3$State.Code=="VA"|
                                       notlowcounties50subset3$State.Code=="WV")]<-"SE" #Southeast

notlowcounties50subset3$Region[which(notlowcounties50subset3$State.Code=="PA"|notlowcounties50subset3$State.Code=="MD"|notlowcounties50subset3$State.Code=="DC"
                                     |notlowcounties50subset3$State.Code=="DE"|notlowcounties50subset3$State.Code=="NY"|notlowcounties50subset3$State.Code=="NJ"|
                                       notlowcounties50subset3$State.Code=="CT"|notlowcounties50subset3$State.Code=="RI")]<-"MA" #MidAtlantic

notlowcounties50subset3$Region[which(notlowcounties50subset3$State.Code=="MA"|notlowcounties50subset3$State.Code=="VT"|notlowcounties50subset3$State.Code=="NH"|
                                       notlowcounties50subset3$State.Code=="ME")]<-"NE"#New England

notlowcounties50subset3$Region[which(notlowcounties50subset3$State.Code=="HI"|notlowcounties50subset3$State.Code=="AK")]<-"AKHI" #AK and HI

table(notlowcounties50subset3$Region)


### end

####### 9. Simplify and create final databases ###### 
# Create a public database and re-name some vars for public use
setwd("~/Google Drive/Research/EPAAsTrajectories/Data")

# Pull additional cws-levels vars from finaldatadups
myvars<-c("PWSID","tpopser06","tpopser07")
needed<-finaldatadups3[myvars]
needed <-needed[!duplicated(needed$PWSID), ]
dim(finaldatathreeyears)
finaldatathreeyears<-merge(finaldatathreeyears,needed,by.x="PWSID",all.x=T);dim(finaldatathreeyears)

colnames(finaldatathreeyears)[colnames(finaldatathreeyears)=="tpopser06"] <- "CountyPopServed20062008" #none
colnames(finaldatathreeyears)[colnames(finaldatathreeyears)=="tpopser07"] <- "CountyPopServed20092011" #none
colnames(finaldatathreeyears)[colnames(finaldatathreeyears)=="ThreeYearAverage.1"] <- "ThreeYearAs20062008"
colnames(finaldatathreeyears)[colnames(finaldatathreeyears)=="ThreeYearAverage.2"] <- "ThreeYearAs20092011"
colnames(finaldatathreeyears)[colnames(finaldatathreeyears)=="FLAG"] <- "Flag"
colnames(finaldatathreeyears)[colnames(finaldatathreeyears)=="PWS.NAME"] <- "PWS.Name" #none
colnames(finaldatathreeyears)[colnames(finaldatathreeyears)=="Population.served"] <- "Population.Served"
colnames(finaldatathreeyears)[colnames(finaldatathreeyears)=="Zip.code"] <- "Zip.Code"
colnames(finaldatathreeyears)[colnames(finaldatathreeyears)=="FullName"] <- "Full.Name"
colnames(finaldatathreeyears)[colnames(finaldatathreeyears)=="istribal"] <- "Tribal"
finaldatathreeyears$CountyServed<-NULL
finaldatathreeyears$Population.Served<-NULL

#Re-order in preferred order
finaldatathreeyears <- finaldatathreeyears[c("PWSID", "CountyFIPS",
                                                     "State.Code", "PWS.Name","Zip.Code","Source.Water.Type","Full.Name",
                                                     "Flag","ThreeYearAs20062008","ThreeYearAs20092011","Adjusted.Total.Population.Served",
                                                     "CountyPopServed20062008","CountyPopServed20092011","Tribal", "Region"
)]

colnames(notlowcounties50subset3)[colnames(notlowcounties50subset3)=="tpopser06"] <- "CountyPopServed20062008"
colnames(notlowcounties50subset3)[colnames(notlowcounties50subset3)=="tpopser07"] <- "CountyPopServed20092011"
colnames(notlowcounties50subset3)[colnames(notlowcounties50subset3)=="pubpop"] <- "PubWaterPop"
colnames(notlowcounties50subset3)[colnames(notlowcounties50subset3)=="pop50"] <- "PubWaterPop50"
colnames(notlowcounties50subset3)[colnames(notlowcounties50subset3)=="pop70"] <- "PubWaterPop70"
colnames(notlowcounties50subset3)[colnames(notlowcounties50subset3)=="pop80"] <- "PubWaterPop80"
colnames(notlowcounties50subset3)[colnames(notlowcounties50subset3)=="Flagy1"] <- "Flag20062008" #not 50% represented
colnames(notlowcounties50subset3)[colnames(notlowcounties50subset3)=="Flagy2"] <- "Flag20092011" #not 50% represented
colnames(notlowcounties50subset3)[colnames(notlowcounties50subset3)=="FullName"] <- "Full.Name" #not 50% represented


#Re-order in preferred order
notlowcounties50subset3 <- notlowcounties50subset3[c("CountyFIPS", "WeightedAs20062008",
                           "WeightedAs20092011", "WeightedAs20062011", "PubWaterPop","PubWaterPop50","PubWaterPop70","PubWaterPop80",
                           "CountyPopServed20062008","CountyPopServed20092011","Flag20062008","Flag20092011",
                           "Full.Name","State.Code","Region"
)]

#Round all arsenic concentration values to 2 digits
cws<-finaldatathreeyears
county<-notlowcounties50subset3
cws$ThreeYearAs20062008<-as.numeric(cws$ThreeYearAs20062008)
cws$ThreeYearAs20062008 <-round(cws$ThreeYearAs20062008, digits=2)
cws$ThreeYearAs20092011<-as.numeric(cws$ThreeYearAs20092011)
cws$ThreeYearAs20092011 <-round(cws$ThreeYearAs20092011, digits=2)
county$WeightedAs20062008<-as.numeric(county$WeightedAs20062008)
county$WeightedAs20092011<-as.numeric(county$WeightedAs20092011)
county$WeightedAs20062011<-as.numeric(county$WeightedAs20062011)
county$WeightedAs20062008<-round(county$WeightedAs20062008,digits=2)
county$WeightedAs20092011<-round(county$WeightedAs20092011,digits=2)
county$WeightedAs20062011<-round(county$WeightedAs20062011,digits=2)
class(county$CountyFIPS) #factor
county$CountyFIPS<-as.character(county$CountyFIPS)
cws$CountyFIPS<-as.character(cws$CountyFIPS)

county$Full.Name[which(county$CountyFIPS=="02201")]<-"AK,PrinceofWalesOuterKetchikan"
county$Full.Name[which(county$CountyFIPS=="02232")]<-"AK,SkagwayHoonahAngoon"
county$Full.Name[which(county$CountyFIPS=="02280")]<-"AK,WrangellPetersburg"

# Keep leading countyfips zeroes and pwsid zeroes by saving as .xlsx
library(openxlsx)
write.xlsx(cws, file="CWSAsSYR3.xlsx") ;write.xlsx(cws, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/CWSAsSYR3.xlsx")
write.xlsx(county, file="CountyAsSYR3.xlsx") ;write.xlsx(county, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/CountyAsSYR3.xlsx")
 ;write.xlsx(cws, file="~/Google Drive/Research/EPAAsTrajectories/March2020/CWSAsSYR3.xlsx")
write.xlsx(county, file="~/Google Drive/Research/EPAAsTrajectories/March2020/CountyAsSYR3.xlsx")


write.csv(cws, file="CWSAsSYR3.csv") 
write.csv(county, file="CountyAsSYR3.csv") 


### end




####------------------  TABLES  ---------------------####
cws<-read.xlsx("~/Google Drive/Research/EPAAsTrajectories/March2020/CWSAsSYR3.xlsx")
county<-read.xlsx("~/Google Drive/Research/EPAAsTrajectories/March2020/CountyAsSYR3.xlsx")

###end

####### 1. Table 1 and Table 2. Mean 3-year CWS averages, stratified by pop served, water type, region, & cluster #########
cws$ThreeYearAs20062008<-as.numeric(cws$ThreeYearAs20062008)
cws$ThreeYearAs20092011<-as.numeric(cws$ThreeYearAs20092011)

library(tidyr)
is.factor(cws$PWSID)
cws$PWSID<-as.factor(cws$PWSID)
finallong <- gather(cws, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE)


### Row group 0: overall, all CWSs:
# using "cws"
tdelete<-cws[which(cws$ThreeYearAs20062008>=10),];nrow(tdelete)#995 /30820

# 2006-2008
tn<-nrow(cws)
nav<-nrow(cws[which(!is.na(cws$ThreeYearAs20062008)),])
nmcl1<-nrow(cws[which(cws$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(cws$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=finallong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                          summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                          summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "All CWSs 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL"; rownames(Time1)[3] <- "Arithmetic mean"

# 2009-2011
tn<-nrow(cws)
nav<-nrow(cws[which(!is.na(cws$ThreeYearAs20092011)),])
nmcl1<-nrow(cws[which(cws$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(cws$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=finallong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "All CWSs 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL"; rownames(Time2)[3] <- "Arithmetic mean"


# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=finallong)
summary(fit1)

T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=finallong)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                          (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                          (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)
mds<-cbind(T1,T2)
colnames(mds)[1]<-"All CWSs 2006-2008";colnames(mds)[2]<-"All CWSs 2009-2011"
colnames(perch)[1]<-"All CWSs 2006-2008";colnames(perch)[2]<-"All CWSs 2009-2011"

allcws<-cbind(Time1,Time2)
#allcws <-round(allcws[,],2)
allcws<-rbind(allcws,mds,perch)
rownames(allcws)[4]<-"Mean difference (95% CI)"
rownames(allcws)[5]<-"Corresponding percent difference (95% CI)"



### Row group 1: by source water type:
## Groundwater
gw<-cws[which(cws$Source.Water.Type=="GW"),];dim(gw)
#2006-2008
mean(gw$ThreeYearAs20062008, na.rm=T)
mean(gw$ThreeYearAs20092011, na.rm=T)
gwlong<-finallong[which(finallong$Source.Water.Type=="GW"),];dim(gwlong)

# Distributions by 3-year period
# 2006-2008
tn<-nrow(gw)
nav<-nrow(gw[which(!is.na(gw$ThreeYearAs20062008)),])
nmcl1<-nrow(gw[which(gw$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(gw$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=gwlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "GW 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(gw)
nav<-nrow(gw[which(!is.na(gw$ThreeYearAs20092011)),])
nmcl1<-nrow(gw[which(gw$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(gw$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=gwlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "GW 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=gwlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=gwlong)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"GW 2006-2008";colnames(mds)[2]<-"GW 2009-2011"
colnames(perch)[1]<-"GW 2006-2008";colnames(perch)[2]<-"GW 2009-2011"

gwcws<-cbind(Time1,Time2)
#gwcws <-round(gwcws[,],2)
gwcws<-rbind(gwcws,mds,perch)
rownames(gwcws)[4]<-"Mean difference (95% CI)"
rownames(gwcws)[5]<-"Corresponding percent difference (95% CI)"


## Surfacewater
sw<-cws[which(cws$Source.Water.Type=="SW"),];dim(sw)
mean(sw$ThreeYearAs20062008, na.rm=T)
mean(sw$ThreeYearAs20092011, na.rm=T)
swlong<-finallong[which(finallong$Source.Water.Type=="SW"),];dim(swlong)

# Distributions by 3-year period
# 2006-2008
tn<-nrow(sw)
nav<-nrow(sw[which(!is.na(sw$ThreeYearAs20062008)),])
nmcl1<-nrow(sw[which(sw$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(sw$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=swlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "SW 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(sw)
nav<-nrow(sw[which(!is.na(sw$ThreeYearAs20092011)),])
nmcl1<-nrow(sw[which(sw$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(sw$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=swlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "SW 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=swlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=swlong)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"SW 2006-2008";colnames(mds)[2]<-"SW 2009-2011"
colnames(perch)[1]<-"SW 2006-2008";colnames(perch)[2]<-"SW 2009-2011"

swcws<-cbind(Time1,Time2)
#swcws <-round(swcws[,],2)
swcws<-rbind(swcws,mds,perch)
rownames(swcws)[4]<-"Mean difference (95% CI)"
rownames(swcws)[5]<-"Corresponding percent difference (95% CI)"



### Row group 2: by groups of population served:
## Quantiles/Categories of pop served
quantile(finaldata$Adjusted.Total.Population.Served,  probs = c(0.25, 0.5, 0.75), na.rm=TRUE)
#    25%     50%     75% 
#  98.00  325.00 1844.75 
cws$popcat<-0
cws$popcat[cws$Adjusted.Total.Population.Served<501]<-1
cws$popcat[501<=cws$Adjusted.Total.Population.Served&cws$Adjusted.Total.Population.Served<3301]<-2
cws$popcat[3301<=cws$Adjusted.Total.Population.Served&cws$Adjusted.Total.Population.Served<10001]<-3
cws$popcat[10001<=cws$Adjusted.Total.Population.Served&cws$Adjusted.Total.Population.Served<100001]<-4
cws$popcat[100001<=cws$Adjusted.Total.Population.Served]<-5
table(cws$popcat)

# Q1
q1<-cws[which(cws$popcat==1),];dim(q1)
mean(q1$ThreeYearAs20062008, na.rm=T)
mean(q1$ThreeYearAs20092011, na.rm=T)
q1long <- gather(q1, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(q1long)

# Distributions by 3-year period
# 2006-2008
tn<-nrow(q1)
nav<-nrow(q1[which(!is.na(q1$ThreeYearAs20062008)),])
nmcl1<-nrow(q1[which(q1$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(q1$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=q1long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "Q1 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(q1)
nav<-nrow(q1[which(!is.na(q1$ThreeYearAs20092011)),])
nmcl1<-nrow(q1[which(q1$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(q1$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=q1long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))

colnames(Time2)[1] <- "Q1 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=q1long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=q1long)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"Q1 2006-2008";colnames(mds)[2]<-"Q1 2009-2011"
colnames(perch)[1]<-"Q1 2006-2008";colnames(perch)[2]<-"Q1 2009-2011"

q1cws<-cbind(Time1,Time2)
#q1cws <-round(q1cws[,],2)
q1cws<-rbind(q1cws,mds,perch)
rownames(q1cws)[4]<-"Mean difference (95% CI)"
rownames(q1cws)[5]<-"Corresponding percent difference (95% CI)"

# Q2
q2<-cws[which(cws$popcat==2),];dim(q2)
q2long <- gather(q2, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(q2long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(q2)
nav<-nrow(q2[which(!is.na(q2$ThreeYearAs20062008)),])
nmcl1<-nrow(q2[which(q2$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(q2$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=q2long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "Q2 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(q2)
nav<-nrow(q2[which(!is.na(q2$ThreeYearAs20092011)),])
nmcl1<-nrow(q2[which(q2$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(q2$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=q2long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "Q2 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=q2long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=q2long)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"Q2 2006-2008";colnames(mds)[2]<-"Q2 2009-2011"
colnames(perch)[1]<-"Q2 2006-2008";colnames(perch)[2]<-"Q2 2009-2011"

q2cws<-cbind(Time1,Time2)
#q2cws <-round(q2cws[,],2)
q2cws<-rbind(q2cws,mds,perch)
rownames(q2cws)[4]<-"Mean difference (95% CI)"
rownames(q2cws)[5]<-"Corresponding percent difference (95% CI)"

# Q3
q3<-cws[which(cws$popcat==3),];dim(q3)
q3long <- gather(q3, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(q3long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(q3)
nav<-nrow(q3[which(!is.na(q3$ThreeYearAs20062008)),])
nmcl1<-nrow(q3[which(q3$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(q3$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=q3long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "Q3 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(q3)
nav<-nrow(q3[which(!is.na(q3$ThreeYearAs20092011)),])
nmcl1<-nrow(q3[which(q3$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(q3$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=q3long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "Q3 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=q3long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=q3long)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"Q3 2006-2008";colnames(mds)[2]<-"Q3 2009-2011"
colnames(perch)[1]<-"Q3 2006-2008";colnames(perch)[2]<-"Q3 2009-2011"

q3cws<-cbind(Time1,Time2)
#q3cws <-round(q3cws[,],2)
q3cws<-rbind(q3cws,mds,perch)
rownames(q3cws)[4]<-"Mean difference (95% CI)"
rownames(q3cws)[5]<-"Corresponding percent difference (95% CI)"

# Q4
q4<-cws[which(cws$popcat==4),];dim(q4)
q4long <- gather(q4, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(q4long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(q4)
nav<-nrow(q4[which(!is.na(q4$ThreeYearAs20062008)),])
nmcl1<-nrow(q4[which(q4$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(q4$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=q4long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "Q4 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(q4)
nav<-nrow(q4[which(!is.na(q4$ThreeYearAs20092011)),])
nmcl1<-nrow(q4[which(q4$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(q4$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=q4long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "Q4 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=q4long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=q4long)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"Q4 2006-2008";colnames(mds)[2]<-"Q4 2009-2011"
colnames(perch)[1]<-"Q4 2006-2008";colnames(perch)[2]<-"Q4 2009-2011"

q4cws<-cbind(Time1,Time2)
#q4cws <-round(q4cws[,],2)
q4cws<-rbind(q4cws,mds,perch)
rownames(q4cws)[4]<-"Mean difference (95% CI)"
rownames(q4cws)[5]<-"Corresponding percent difference (95% CI)"

# Q5
q5<-cws[which(cws$popcat==5),];dim(q5)
q5long <- gather(q5, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(q5long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(q5)
nav<-nrow(q5[which(!is.na(q5$ThreeYearAs20062008)),])
nmcl1<-nrow(q5[which(q5$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(q5$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=q5long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "Q5 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(q5)
nav<-nrow(q5[which(!is.na(q5$ThreeYearAs20092011)),])
nmcl1<-nrow(q5[which(q5$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(q5$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=q5long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "Q5 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=q5long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=q5long)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"Q5 2006-2008";colnames(mds)[2]<-"Q5 2009-2011"
colnames(perch)[1]<-"Q5 2006-2008";colnames(perch)[2]<-"Q5 2009-2011"

q5cws<-cbind(Time1,Time2)
#q5cws <-round(q5cws[,],2)
q5cws<-rbind(q5cws,mds,perch)
rownames(q5cws)[4]<-"Mean difference (95% CI)"
rownames(q5cws)[5]<-"Corresponding percent difference (95% CI)"


### Row group 3: by region:
# AKHI
AKHI<-cws[which(cws$Region=="AKHI"),];dim(AKHI)
AKHIlong <- gather(AKHI, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(AKHIlong)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(AKHI)
nav<-nrow(AKHI[which(!is.na(AKHI$ThreeYearAs20062008)),])
nmcl1<-nrow(AKHI[which(AKHI$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(AKHI$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=AKHIlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "AKHI 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(AKHI)
nav<-nrow(AKHI[which(!is.na(AKHI$ThreeYearAs20092011)),])
nmcl1<-nrow(AKHI[which(AKHI$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(AKHI$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=AKHIlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "AKHI 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=AKHIlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=AKHIlong)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"AKHI 2006-2008";colnames(mds)[2]<-"AKHI 2009-2011"
colnames(perch)[1]<-"AKHI 2006-2008";colnames(perch)[2]<-"AKHI 2009-2011"

AKHIcws<-cbind(Time1,Time2)
#AKHIcws <-round(AKHIcws[,],2)
AKHIcws<-rbind(AKHIcws,mds,perch)
rownames(AKHIcws)[4]<-"Mean difference (95% CI)"
rownames(AKHIcws)[5]<-"Corresponding percent difference (95% CI)"


# CMW
CMW<-cws[which(cws$Region=="CMW"),];dim(CMW)
CMWlong <- gather(CMW, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(CMWlong)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(CMW)
nav<-nrow(CMW[which(!is.na(CMW$ThreeYearAs20062008)),])
nmcl1<-nrow(CMW[which(CMW$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(CMW$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=CMWlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "CMW 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(CMW)
nav<-nrow(CMW[which(!is.na(CMW$ThreeYearAs20092011)),])
nmcl1<-nrow(CMW[which(CMW$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(CMW$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=CMWlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "CMW 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=CMWlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=CWSlong)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CMW 2006-2008";colnames(mds)[2]<-"CMW 2009-2011"
colnames(perch)[1]<-"CMW 2006-2008";colnames(perch)[2]<-"CMW 2009-2011"

CMWcws<-cbind(Time1,Time2)
#CMWcws <-round(CMWcws[,],2)
CMWcws<-rbind(CMWcws,mds,perch)
rownames(CMWcws)[4]<-"Mean difference (95% CI)"
rownames(CMWcws)[5]<-"Corresponding percent difference (95% CI)"


# EMW
EMW<-cws[which(cws$Region=="EMW"),];dim(EMW)
EMWlong <- gather(EMW, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(EMWlong)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(EMW)
nav<-nrow(EMW[which(!is.na(EMW$ThreeYearAs20062008)),])
nmcl1<-nrow(EMW[which(EMW$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(EMW$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=EMWlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "EMW 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(EMW)
nav<-nrow(EMW[which(!is.na(EMW$ThreeYearAs20092011)),])
nmcl1<-nrow(EMW[which(EMW$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(EMW$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=EMWlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "EMW 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=EMWlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=EMWlong)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"EMW 2006-2008";colnames(mds)[2]<-"EMW 2009-2011"
colnames(perch)[1]<-"EMW 2006-2008";colnames(perch)[2]<-"EMW 2009-2011"

EMWcws<-cbind(Time1,Time2)
#EMWcws <-round(EMWcws[,],2)
EMWcws<-rbind(EMWcws,mds,perch)
rownames(EMWcws)[4]<-"Mean difference (95% CI)"
rownames(EMWcws)[5]<-"Corresponding percent difference (95% CI)"

# MA
MA<-cws[which(cws$Region=="MA"),];dim(MA)
MAlong <- gather(MA, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(MAlong)
# Distributions by 3-year period
# 2006-2008
nav<-nrow(MA)
nav<-nrow(MA[which(!is.na(MA$ThreeYearAs20062008)),])
nmcl1<-nrow(MA[which(MA$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(MA$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=MAlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "MA 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(MA)
nav<-nrow(MA[which(!is.na(MA$ThreeYearAs20092011)),])
nmcl1<-nrow(MA[which(MA$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(MA$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=MAlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "MA 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=MAlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=MAlong)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"MA 2006-2008";colnames(mds)[2]<-"MA 2009-2011"
colnames(perch)[1]<-"MA 2006-2008";colnames(perch)[2]<-"MA 2009-2011"

MAcws<-cbind(Time1,Time2)
#MAcws <-round(MAcws[,],2)
MAcws<-rbind(MAcws,mds,perch)
rownames(MAcws)[4]<-"Mean difference (95% CI)"
rownames(MAcws)[5]<-"Corresponding percent difference (95% CI)"


# NE
NE<-cws[which(cws$Region=="NE"),];dim(NE)
NElong <- gather(NE, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(NElong)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(NE)
nav<-nrow(NE[which(!is.na(NE$ThreeYearAs20062008)),])
nmcl1<-nrow(NE[which(NE$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(NE$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=NElong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "NE 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(NE)
nav<-nrow(NE[which(!is.na(NE$ThreeYearAs20092011)),])
nmcl1<-nrow(NE[which(NE$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(NE$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=NElong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "NE 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=NElong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=NElong)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"NE 2006-2008";colnames(mds)[2]<-"NE 2009-2011"
colnames(perch)[1]<-"NE 2006-2008";colnames(perch)[2]<-"NE 2009-2011"

NEcws<-cbind(Time1,Time2)
#NEcws <-round(NEcws[,],2)
NEcws<-rbind(NEcws,mds,perch)
rownames(NEcws)[4]<-"Mean difference (95% CI)"
rownames(NEcws)[5]<-"Corresponding percent difference (95% CI)"


# PNW
PNW<-cws[which(cws$Region=="PNW"),];dim(PNW)
PNWlong <- gather(PNW, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(PNWlong)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(PNW)
nav<-nrow(PNW[which(!is.na(PNW$ThreeYearAs20062008)),])
nmcl1<-nrow(PNW[which(PNW$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(PNW$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=PNWlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "PNW 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(PNW)
nav<-nrow(PNW[which(!is.na(PNW$ThreeYearAs20092011)),])
nmcl1<-nrow(PNW[which(PNW$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(PNW$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=PNWlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "PNW 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=PNWlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=PNWlong)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"PNW 2006-2008";colnames(mds)[2]<-"PNW 2009-2011"
colnames(perch)[1]<-"PNW 2006-2008";colnames(perch)[2]<-"PNW 2009-2011"

PNWcws<-cbind(Time1,Time2)
#PNWcws <-round(PNWcws[,],2)
PNWcws<-rbind(PNWcws,mds,perch)
rownames(PNWcws)[4]<-"Mean difference (95% CI)"
rownames(PNWcws)[5]<-"Corresponding percent difference (95% CI)"

# SE
SE<-cws[which(cws$Region=="SE"),];dim(SE)
SElong <- gather(SE, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(SElong)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(SE)
nav<-nrow(SE[which(!is.na(SE$ThreeYearAs20062008)),])
nmcl1<-nrow(SE[which(SE$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(SE$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=SElong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "SE 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(SE)
nav<-nrow(SE[which(!is.na(SE$ThreeYearAs20092011)),])
nmcl1<-nrow(SE[which(SE$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(SE$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=SElong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "SE 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=SElong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=SElong)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"SE 2006-2008";colnames(mds)[2]<-"SE 2009-2011"
colnames(perch)[1]<-"SE 2006-2008";colnames(perch)[2]<-"SE 2009-2011"

SEcws<-cbind(Time1,Time2)
#SEcws <-round(SEcws[,],2)
SEcws<-rbind(SEcws,mds,perch)
rownames(SEcws)[4]<-"Mean difference (95% CI)"
rownames(SEcws)[5]<-"Corresponding percent difference (95% CI)"

# SW
SW<-cws[which(cws$Region=="SW"),];dim(SW)
SWlong <- gather(SW, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(SWlong)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(SW)
nav<-nrow(SW[which(!is.na(SW$ThreeYearAs20062008)),])
nmcl1<-nrow(SW[which(SW$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(SW$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=SWlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "SW 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(SW)
nav<-nrow(SW[which(!is.na(SW$ThreeYearAs20092011)),])
nmcl1<-nrow(SW[which(SW$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(SW$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=SWlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "SW 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=SWlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=SWlong)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"SW 2006-2008";colnames(mds)[2]<-"SW 2009-2011"
colnames(perch)[1]<-"SW 2006-2008";colnames(perch)[2]<-"SW 2009-2011"

SWcws<-cbind(Time1,Time2)
#SWcws <-round(SWcws[,],2)
SWcws<-rbind(SWcws,mds,perch)
rownames(SWcws)[4]<-"Mean difference (95% CI)"
rownames(SWcws)[5]<-"Corresponding percent difference (95% CI)"


### Row group 3: by sociodemographic cluster group:
## For this group we use: finaldatadups3
# First we need to assign each CWS to a county-cluster; lets pull thoe CWSs from _data_ which serve multiple counties
# and see if these are the same counties:
describe(finaldatadups3$PWSID) ##one record for each CWS serving each county, 3-yr averages 
describe(finaldatadups3$CountyFIPS)

# fix arsenic var names
colnames(finaldatadups3)[11] <- "ThreeYearAs20062008"
colnames(finaldatadups3)[12] <- "ThreeYearAs20092011"
library(tidyr)
is.factor(finaldatadups3$PWSID)
finaldatadups3$PWSID<-as.factor(finaldatadups3$PWSID)
fdups3long <- gather(finaldatadups3, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE)

# c1 cluster 
c1<-finaldatadups3[which(finaldatadups3$cluster.number==1),];dim(c1)
c1long <- gather(c1, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(c1long)
# Distributions by 3-year period
# 2006-2008
nav<-nrow(c1)
tn<-nrow(c1[which(!is.na(c1$ThreeYearAs20062008)),])
nmcl1<-nrow(c1[which(c1$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(c1$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c1long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "c1 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(c1)
tn<-nrow(c1[which(!is.na(c1$ThreeYearAs20092011)),])
nmcl1<-nrow(c1[which(c1$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(c1$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c1long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "c1 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c1long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c1long)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"c1 2006-2008";colnames(mds)[2]<-"c1 2009-2011"
colnames(perch)[1]<-"c1 2006-2008";colnames(perch)[2]<-"c1 2009-2011"

c1cws<-cbind(Time1,Time2)
#c1cws <-round(c1cws[,],2)
c1cws<-rbind(c1cws,mds,perch)
rownames(c1cws)[4]<-"Mean difference (95% CI)"
rownames(c1cws)[5]<-"Corresponding percent difference (95% CI)"


# c2 cluster 
c2<-finaldatadups3[which(finaldatadups3$cluster.number==2),];dim(c2)
c2long <- gather(c2, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(c2long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(c2)
nav<-nrow(c2[which(!is.na(c2$ThreeYearAs20062008)),])
nmcl1<-nrow(c2[which(c2$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(c2$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c2long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "c2 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(c2)
nav<-nrow(c2[which(!is.na(c2$ThreeYearAs20092011)),])
nmcl1<-nrow(c2[which(c2$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(c2$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c2long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "c2 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c2long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c2long)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"c2 2006-2008";colnames(mds)[2]<-"c2 2009-2011"
colnames(perch)[1]<-"c2 2006-2008";colnames(perch)[2]<-"c2 2009-2011"

c2cws<-cbind(Time1,Time2)
#c2cws <-round(c2cws[,],2)
c2cws<-rbind(c2cws,mds,perch)
rownames(c2cws)[4]<-"Mean difference (95% CI)"
rownames(c1cws)[5]<-"Corresponding percent difference (95% CI)"


# c3 cluster 
c3<-finaldatadups3[which(finaldatadups3$cluster.number==3),];dim(c3)
c3long <- gather(c3, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(c3long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(c3)
nav<-nrow(c3[which(!is.na(c3$ThreeYearAs20062008)),])
nmcl1<-nrow(c3[which(c3$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(c3$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c3long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "c3 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(c3)
nav<-nrow(c3[which(!is.na(c3$ThreeYearAs20092011)),])
nmcl1<-nrow(c3[which(c3$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(c3$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c3long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "c3 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c3long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c3long)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"c3 2006-2008";colnames(mds)[2]<-"c3 2009-2011"
colnames(perch)[1]<-"c3 2006-2008";colnames(perch)[2]<-"c3 2009-2011"

c3cws<-cbind(Time1,Time2)
#c3cws <-round(c3cws[,],2)
c3cws<-rbind(c3cws,mds,perch)
rownames(c3cws)[4]<-"Mean difference (95% CI)"
rownames(c3cws)[5]<-"Corresponding percent difference (95% CI)"


# c4 cluster 
c4<-finaldatadups3[which(finaldatadups3$cluster.number==4),];dim(c4)
c4long <- gather(c4, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(c4long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(c4)
nav<-nrow(c4[which(!is.na(c4$ThreeYearAs20062008)),])
nmcl1<-nrow(c4[which(c4$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(c4$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c4long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "c4 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(c4)
nav<-nrow(c4[which(!is.na(c4$ThreeYearAs20092011)),])
nmcl1<-nrow(c4[which(c4$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(c4$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c4long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "c4 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c4long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c4long)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"c4 2006-2008";colnames(mds)[2]<-"c4 2009-2011"
colnames(perch)[1]<-"c4 2006-2008";colnames(perch)[2]<-"c4 2009-2011"

c4cws<-cbind(Time1,Time2)
#c4cws <-round(c4cws[,],2)
c4cws<-rbind(c4cws,mds,perch)
rownames(c4cws)[4]<-"Mean difference (95% CI)"
rownames(c4cws)[5]<-"Corresponding percent difference (95% CI)"


# c5 cluster 
c5<-finaldatadups3[which(finaldatadups3$cluster.number==5),];dim(c5)
c5long <- gather(c5, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(c5long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(c5)
nav<-nrow(c5[which(!is.na(c5$ThreeYearAs20062008)),])
nmcl1<-nrow(c5[which(c5$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(c5$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c5long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "c5 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(c5)
nav<-nrow(c5[which(!is.na(c5$ThreeYearAs20092011)),])
nmcl1<-nrow(c5[which(c5$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(c5$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c5long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "c5 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c5long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c5long)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"c5 2006-2008";colnames(mds)[2]<-"c5 2009-2011"
colnames(perch)[1]<-"c5 2006-2008";colnames(perch)[2]<-"c5 2009-2011"

c5cws<-cbind(Time1,Time2)
#c5cws <-round(c5cws[,],2)
c5cws<-rbind(c5cws,mds,perch)
rownames(c5cws)[4]<-"Mean difference (95% CI)"
rownames(c5cws)[5]<-"Corresponding percent difference (95% CI)"


# c6 cluster 
c6<-finaldatadups3[which(finaldatadups3$cluster.number==6),];dim(c6)
c6long <- gather(c6, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(c6long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(c6)
nav<-nrow(c6[which(!is.na(c6$ThreeYearAs20062008)),])
nmcl1<-nrow(c6[which(c6$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(c6$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c6long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "c6 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(c6)
nav<-nrow(c6[which(!is.na(c6$ThreeYearAs20092011)),])
nmcl1<-nrow(c6[which(c6$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(c6$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c6long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "c6 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c6long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c6long)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"c6 2006-2008";colnames(mds)[2]<-"c6 2009-2011"
colnames(perch)[1]<-"c6 2006-2008";colnames(perch)[2]<-"c6 2009-2011"

c6cws<-cbind(Time1,Time2)
#c6cws <-round(c6cws[,],2)
c6cws<-rbind(c6cws,mds,perch)
rownames(c6cws)[4]<-"Mean difference (95% CI)"
rownames(c6cws)[5]<-"Corresponding percent difference (95% CI)"


# c7 cluster 
c7<-finaldatadups3[which(finaldatadups3$cluster.number==7),];dim(c7)
c7long <- gather(c7, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(c7long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(c7)
nav<-nrow(c7[which(!is.na(c7$ThreeYearAs20062008)),])
nmcl1<-nrow(c7[which(c7$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(c7$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c7long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "c7 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(c7)
nav<-nrow(c7[which(!is.na(c7$ThreeYearAs20092011)),])
nmcl1<-nrow(c7[which(c7$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(c7$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c7long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "c7 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c7long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c7long)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"c7 2006-2008";colnames(mds)[2]<-"c7 2009-2011"
colnames(perch)[1]<-"c7 2006-2008";colnames(perch)[2]<-"c7 2009-2011"

c7cws<-cbind(Time1,Time2)
#c7cws <-round(c7cws[,],2)
c7cws<-rbind(c7cws,mds,perch)
rownames(c7cws)[4]<-"Mean difference (95% CI)"
rownames(c7cws)[5]<-"Corresponding percent difference (95% CI)"

# c8 cluster 
c8<-finaldatadups3[which(finaldatadups3$cluster.number==8),];dim(c8)
c8long <- gather(c8, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(c8long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(c8)
nav<-nrow(c8[which(!is.na(c8$ThreeYearAs20062008)),])
nmcl1<-nrow(c8[which(c8$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(c8$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c8long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

Time1<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time1)[1] <- "c8 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(c8)
nav<-nrow(c8[which(!is.na(c8$ThreeYearAs20092011)),])
nmcl1<-nrow(c8[which(c8$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mclcol<-to.n.prop(c(nmcl1,nmcl))
#mean<-mean(c8$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c8long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

Time2<-as.data.frame(rbind(nav,mclcol,mean))
colnames(Time2)[1] <- "c8 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c8long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# percent change
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c8long)
summary(fit1)
pch<-convert.to.ci(round(c((summary(fit1)$coeff[2,1] / summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100,
                           (summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]) / (summary(fit1)$coeff[1,1]) *100),1))
perch<-cbind(T1,pch)

mds<-cbind(T1,T2)
colnames(mds)[1]<-"c8 2006-2008";colnames(mds)[2]<-"c8 2009-2011"
colnames(perch)[1]<-"c8 2006-2008";colnames(perch)[2]<-"c8 2009-2011"

c8cws<-cbind(Time1,Time2)
#c8cws <-round(c8cws[,],2)
c8cws<-rbind(c8cws,mds,perch)
rownames(c8cws)[4]<-"Mean difference (95% CI)"
rownames(c8cws)[5]<-"Corresponding percent difference (95% CI)"


## Save to Table

Table1A<-cbind(allcws,gwcws,swcws,q1cws,q2cws,q3cws,q4cws,q5cws,AKHIcws,CMWcws,EMWcws,MAcws,NEcws,PNWcws,SEcws,SWcws,c1cws,c2cws,c3cws,c4cws,c5cws,c6cws,c7cws,c8cws)

# Transpose into better format
Table1 <- data.frame(t(Table1A[]))
colnames(Table1) <- Table1A[, 1]
colnames(Table1)[1]<-"N";colnames(Table1)[2]<-"% at/above MCL";colnames(Table1)[3]<-"Arithmetic mean";colnames(Table1)[4]<-"Mean difference";colnames(Table1)[5]<-"Percent difference"



write.xlsx(Table1,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/Table1 .xlsx")

# Create wide version of Table 1
# Change row names to column
Table1$Subgroup <- rownames(Table1)
# Remove year from subgroup name to get ID
Table1$Subgroup<-as.character(Table1$Subgroup)
Table1$Subgroup = substr(Table1$Subgroup,1,nchar(Table1$Subgroup)-10)
table(Table1$Subgroup)
# Create Time var
Table1$Time<-"20092011"
Table1$Time[which(Table1$`Mean difference`=="0 (reference)")]<-"20062008"
table(Table1$Time)

T1wide<-reshape(Table1, idvar = "Subgroup", timevar = "Time", direction = "wide")
rownames(T1wide) <- c()
write.xlsx(T1wide,row.names=F, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/Table 1 Wide Format .xlsx")

### end 



#######       1B. Sensitivity analysis: adjust stratified models by source water type  ######

cws$SWT<-0
cws$SWT[which(cws$Source.Water.Type!="SW")]<-1
table(cws$SWT) # surface water is reference group
finaldatadups3$SWT<-0
finaldatadups3$SWT[which(finaldatadups3$Source.Water.Type!="SW")]<-1
table(finaldatadups3$SWT) # surface water is reference group
table(cws$SWT,cws$Region)

cws$ThreeYearAs20062008<-as.numeric(cws$ThreeYearAs20062008)
cws$ThreeYearAs20092011<-as.numeric(cws$ThreeYearAs20092011)

library(tidyr)
is.factor(cws$PWSID)
cws$PWSID<-as.factor(cws$PWSID)
finallong <- gather(cws, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE)


### Row group 1: overall, all CWSs:
# using "cws"
tdelete<-cws[which(cws$ThreeYearAs20062008>=10),];nrow(tdelete)#995 /30820

# 2006-2008
tn<-nrow(cws)
nav<-nrow(cws[which(!is.na(cws$ThreeYearAs20062008)),])
nmcl1<-nrow(cws[which(cws$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(cws$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "All CWSs 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL"; rownames(Time1)[3] <- "Arithmetic mean"

# 2009-2011
tn<-nrow(cws)
nav<-nrow(cws[which(!is.na(cws$ThreeYearAs20092011)),])
nmcl1<-nrow(cws[which(cws$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(cws$ThreeYearAs20092011,na.rm=T)
Time2<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time2)[1] <- "All CWSs 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL"; rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=finallong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"All CWSs 2006-2008";colnames(mds)[2]<-"All CWSs 2009-2011"
allcws<-cbind(Time1,Time2)
allcws <-round(allcws[,],2)
allcws<-rbind(allcws,mds)
rownames(allcws)[4]<-"Mean difference (95% CI)"
allcws <- rbind(allcws, Type= c("All CWSs","All CWSs"))



### Row group 2: by groups of population served:
## Quantiles/Categories of pop served
quantile(finaldata$Adjusted.Total.Population.Served,  probs = c(0.25, 0.5, 0.75), na.rm=TRUE)
#    25%     50%     75% 
#  98.00  325.00 1844.75 
cws$popcat<-0
cws$popcat[cws$Adjusted.Total.Population.Served<501]<-1
cws$popcat[501<=cws$Adjusted.Total.Population.Served&cws$Adjusted.Total.Population.Served<3301]<-2
cws$popcat[3301<=cws$Adjusted.Total.Population.Served&cws$Adjusted.Total.Population.Served<10001]<-3
cws$popcat[10001<=cws$Adjusted.Total.Population.Served&cws$Adjusted.Total.Population.Served<100001]<-4
cws$popcat[100001<=cws$Adjusted.Total.Population.Served]<-5
table(cws$popcat)

# Q1
q1<-cws[which(cws$popcat==1),];dim(q1)
mean(q1$ThreeYearAs20062008, na.rm=T)
mean(q1$ThreeYearAs20092011, na.rm=T)
q1long <- gather(q1, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(q1long)

# Distributions by 3-year period
# 2006-2008
tn<-nrow(q1)
nav<-nrow(q1[which(!is.na(q1$ThreeYearAs20062008)),])
nmcl1<-nrow(q1[which(q1$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(q1$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "Q1 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(q1)
nav<-nrow(q1[which(!is.na(q1$ThreeYearAs20092011)),])
nmcl1<-nrow(q1[which(q1$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(q1$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "Q1 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=q1long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"Q1 2006-2008";colnames(mds)[2]<-"Q1 2009-2011"
q1cws<-cbind(Time1,Time2)
q1cws <-round(q1cws[,],2)
q1cws<-rbind(q1cws,mds)
rownames(q1cws)[4]<-"Mean difference (95% CI)"
q1cws <- rbind(q1cws, Type= c("Q1","Q1"))

# Q2
q2<-cws[which(cws$popcat==2),];dim(q2)
q2long <- gather(q2, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(q2long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(q2)
nav<-nrow(q2[which(!is.na(q2$ThreeYearAs20062008)),])
nmcl1<-nrow(q2[which(q2$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(q2$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "Q2 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(q2)
nav<-nrow(q2[which(!is.na(q2$ThreeYearAs20092011)),])
nmcl1<-nrow(q2[which(q2$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(q2$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "Q2 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=q2long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"Q2 2006-2008";colnames(mds)[2]<-"Q2 2009-2011"
q2cws<-cbind(Time1,Time2)
q2cws <-round(q2cws[,],2)
q2cws<-rbind(q2cws,mds)
rownames(q2cws)[4]<-"Mean difference (95% CI)"
q2cws <- rbind(q2cws, Type= c("Q2","Q2"))

# Q3
q3<-cws[which(cws$popcat==3),];dim(q3)
q3long <- gather(q3, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(q3long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(q3)
nav<-nrow(q3[which(!is.na(q3$ThreeYearAs20062008)),])
nmcl1<-nrow(q3[which(q3$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(q3$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "Q3 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(q3)
nav<-nrow(q3[which(!is.na(q3$ThreeYearAs20092011)),])
nmcl1<-nrow(q3[which(q3$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(q3$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "Q3 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=q3long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"Q3 2006-2008";colnames(mds)[2]<-"Q3 2009-2011"
q3cws<-cbind(Time1,Time2)
q3cws <-round(q3cws[,],2)
q3cws<-rbind(q3cws,mds)
rownames(q3cws)[4]<-"Mean difference (95% CI)"
q3cws <- rbind(q3cws, Type= c("Q3","Q3"))

# Q4
q4<-cws[which(cws$popcat==4),];dim(q4)
q4long <- gather(q4, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(q4long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(q4)
nav<-nrow(q4[which(!is.na(q4$ThreeYearAs20062008)),])
nmcl1<-nrow(q4[which(q4$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(q4$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "Q4 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(q4)
nav<-nrow(q4[which(!is.na(q4$ThreeYearAs20092011)),])
nmcl1<-nrow(q4[which(q4$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(q4$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "Q4 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=q4long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"Q4 2006-2008";colnames(mds)[2]<-"Q4 2009-2011"
q4cws<-cbind(Time1,Time2)
q4cws <-round(q4cws[,],2)
q4cws<-rbind(q4cws,mds)
rownames(q4cws)[4]<-"Mean difference (95% CI)"
q4cws <- rbind(q4cws, Type= c("Q4","Q4"))

# Q5
q5<-cws[which(cws$popcat==5),];dim(q5)
q5long <- gather(q5, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(q5long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(q5)
nav<-nrow(q5[which(!is.na(q5$ThreeYearAs20062008)),])
nmcl1<-nrow(q5[which(q5$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(q5$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "Q5 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(q5)
nav<-nrow(q5[which(!is.na(q5$ThreeYearAs20092011)),])
nmcl1<-nrow(q5[which(q5$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(q5$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "Q5 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=q5long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"Q5 2006-2008";colnames(mds)[2]<-"Q5 2009-2011"
q5cws<-cbind(Time1,Time2)
q5cws <-round(q5cws[,],2)
q5cws<-rbind(q5cws,mds)
rownames(q5cws)[4]<-"Mean difference (95% CI)"
q5cws <- rbind(q5cws, Type= c("Q5","Q5"))


### Row group 3: by region:
# AKHI
AKHI<-cws[which(cws$Region=="AKHI"),];dim(AKHI)
AKHIlong <- gather(AKHI, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(AKHIlong)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(AKHI)
nav<-nrow(AKHI[which(!is.na(AKHI$ThreeYearAs20062008)),])
nmcl1<-nrow(AKHI[which(AKHI$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(AKHI$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "AKHI 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(AKHI)
nav<-nrow(AKHI[which(!is.na(AKHI$ThreeYearAs20092011)),])
nmcl1<-nrow(AKHI[which(AKHI$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(AKHI$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "AKHI 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=AKHIlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"AKHI 2006-2008";colnames(mds)[2]<-"AKHI 2009-2011"
AKHIcws<-cbind(Time1,Time2)
AKHIcws <-round(AKHIcws[,],2)
AKHIcws<-rbind(AKHIcws,mds)
rownames(AKHIcws)[4]<-"Mean difference (95% CI)"
AKHIcws <- rbind(AKHIcws, Type= c("AKHI","AKHI"))


# CMW
CMW<-cws[which(cws$Region=="CMW"),];dim(CMW)
CMWlong <- gather(CMW, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(CMWlong)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(CMW)
nav<-nrow(CMW[which(!is.na(CMW$ThreeYearAs20062008)),])
nmcl1<-nrow(CMW[which(CMW$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(CMW$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "CMW 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(CMW)
nav<-nrow(CMW[which(!is.na(CMW$ThreeYearAs20092011)),])
nmcl1<-nrow(CMW[which(CMW$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(CMW$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "CMW 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=CMWlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
mds<-cbind(T1,T2)
colnames(mds)[1]<-"CMW 2006-2008";colnames(mds)[2]<-"CMW 2009-2011"
CMWcws<-cbind(Time1,Time2)
CMWcws <-round(CMWcws[,],2)
CMWcws<-rbind(CMWcws,mds)
rownames(CMWcws)[4]<-"Mean difference (95% CI)"
CMWcws <- rbind(CMWcws, Type= c("CMW","CMW"))


# EMW
EMW<-cws[which(cws$Region=="EMW"),];dim(EMW)
EMWlong <- gather(EMW, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(EMWlong)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(EMW)
nav<-nrow(EMW[which(!is.na(EMW$ThreeYearAs20062008)),])
nmcl1<-nrow(EMW[which(EMW$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(EMW$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "EMW 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(EMW)
nav<-nrow(EMW[which(!is.na(EMW$ThreeYearAs20092011)),])
nmcl1<-nrow(EMW[which(EMW$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(EMW$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "EMW 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=EMWlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"EMW 2006-2008";colnames(mds)[2]<-"EMW 2009-2011"
EMWcws<-cbind(Time1,Time2)
EMWcws <-round(EMWcws[,],2)
EMWcws<-rbind(EMWcws,mds)
rownames(EMWcws)[4]<-"Mean difference (95% CI)"
EMWcws <- rbind(EMWcws, Type= c("EMW","EMW"))

# MA
MA<-cws[which(cws$Region=="MA"),];dim(MA)
MAlong <- gather(MA, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(MAlong)
# Distributions by 3-year period
# 2006-2008
nav<-nrow(MA)
nav<-nrow(MA[which(!is.na(MA$ThreeYearAs20062008)),])
nmcl1<-nrow(MA[which(MA$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(MA$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "MA 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(MA)
nav<-nrow(MA[which(!is.na(MA$ThreeYearAs20092011)),])
nmcl1<-nrow(MA[which(MA$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(MA$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "MA 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=MAlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"MA 2006-2008";colnames(mds)[2]<-"MA 2009-2011"
MAcws<-cbind(Time1,Time2)
MAcws <-round(MAcws[,],2)
MAcws<-rbind(MAcws,mds)
rownames(MAcws)[4]<-"Mean difference (95% CI)"
MAcws <- rbind(MAcws, Type= c("MA","MA"))


# NE
NE<-cws[which(cws$Region=="NE"),];dim(NE)
NElong <- gather(NE, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(NElong)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(NE)
nav<-nrow(NE[which(!is.na(NE$ThreeYearAs20062008)),])
nmcl1<-nrow(NE[which(NE$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(NE$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "NE 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(NE)
nav<-nrow(NE[which(!is.na(NE$ThreeYearAs20092011)),])
nmcl1<-nrow(NE[which(NE$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(NE$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "NE 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=NElong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"NE 2006-2008";colnames(mds)[2]<-"NE 2009-2011"
NEcws<-cbind(Time1,Time2)
NEcws <-round(NEcws[,],2)
NEcws<-rbind(NEcws,mds)
rownames(NEcws)[4]<-"Mean difference (95% CI)"
NEcws <- rbind(NEcws, Type= c("NE","NE"))


# PNW
PNW<-cws[which(cws$Region=="PNW"),];dim(PNW)
PNWlong <- gather(PNW, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(PNWlong)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(PNW)
nav<-nrow(PNW[which(!is.na(PNW$ThreeYearAs20062008)),])
nmcl1<-nrow(PNW[which(PNW$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(PNW$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "PNW 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(PNW)
nav<-nrow(PNW[which(!is.na(PNW$ThreeYearAs20092011)),])
nmcl1<-nrow(PNW[which(PNW$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(PNW$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "PNW 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=PNWlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"PNW 2006-2008";colnames(mds)[2]<-"PNW 2009-2011"
PNWcws<-cbind(Time1,Time2)
PNWcws <-round(PNWcws[,],2)
PNWcws<-rbind(PNWcws,mds)
rownames(PNWcws)[4]<-"Mean difference (95% CI)"
PNWcws <- rbind(PNWcws, Type= c("PNW","PNW"))

# SE
SE<-cws[which(cws$Region=="SE"),];dim(SE)
SElong <- gather(SE, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(SElong)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(SE)
nav<-nrow(SE[which(!is.na(SE$ThreeYearAs20062008)),])
nmcl1<-nrow(SE[which(SE$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(SE$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "SE 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(SE)
nav<-nrow(SE[which(!is.na(SE$ThreeYearAs20092011)),])
nmcl1<-nrow(SE[which(SE$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(SE$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "SE 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=SElong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"SE 2006-2008";colnames(mds)[2]<-"SE 2009-2011"
SEcws<-cbind(Time1,Time2)
SEcws <-round(SEcws[,],2)
SEcws<-rbind(SEcws,mds)
rownames(SEcws)[4]<-"Mean difference (95% CI)"
SEcws <- rbind(SEcws, Type= c("SE","SE"))

# SW
SW<-cws[which(cws$Region=="SW"),];dim(SW)
SWlong <- gather(SW, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(SWlong)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(SW)
nav<-nrow(SW[which(!is.na(SW$ThreeYearAs20062008)),])
nmcl1<-nrow(SW[which(SW$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(SW$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "SW 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(SW)
nav<-nrow(SW[which(!is.na(SW$ThreeYearAs20092011)),])
nmcl1<-nrow(SW[which(SW$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(SW$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "SW 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=SWlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"SW 2006-2008";colnames(mds)[2]<-"SW 2009-2011"
SWcws<-cbind(Time1,Time2)
SWcws <-round(SWcws[,],2)
SWcws<-rbind(SWcws,mds)
rownames(SWcws)[4]<-"Mean difference (95% CI)"
SWcws <- rbind(SWcws, Type= c("SW","SW"))


### Row group 3: by sociodemographic cluster group:
## For this group we use: finaldatadups3
# First we need to assign each CWS to a county-cluster; lets pull thoe CWSs from _data_ which serve multiple counties
# and see if these are the same counties:
describe(finaldatadups3$PWSID) ##one record for each CWS serving each county, 3-yr averages 
describe(finaldatadups3$CountyFIPS)

# fix arsenic var names
colnames(finaldatadups3)[11] <- "ThreeYearAs20062008"
colnames(finaldatadups3)[12] <- "ThreeYearAs20092011"
library(tidyr)
is.factor(finaldatadups3$PWSID)
finaldatadups3$PWSID<-as.factor(finaldatadups3$PWSID)
fdups3long <- gather(finaldatadups3, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE)

# c1 cluster 
c1<-finaldatadups3[which(finaldatadups3$cluster.number==1),];dim(c1)
c1long <- gather(c1, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(c1long)
# Distributions by 3-year period
# 2006-2008
nav<-nrow(c1)
tn<-nrow(c1[which(!is.na(c1$ThreeYearAs20062008)),])
nmcl1<-nrow(c1[which(c1$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(c1$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "c1 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(c1)
tn<-nrow(c1[which(!is.na(c1$ThreeYearAs20092011)),])
nmcl1<-nrow(c1[which(c1$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(c1$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "c1 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=c1long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"c1 2006-2008";colnames(mds)[2]<-"c1 2009-2011"
c1cws<-cbind(Time1,Time2)
c1cws <-round(c1cws[,],2)
c1cws<-rbind(c1cws,mds)
rownames(c1cws)[4]<-"Mean difference (95% CI)"
c1cws <- rbind(c1cws, Type= c("c1","c1"))


# c2 cluster 
c2<-finaldatadups3[which(finaldatadups3$cluster.number==2),];dim(c2)
c2long <- gather(c2, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(c2long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(c2)
nav<-nrow(c2[which(!is.na(c2$ThreeYearAs20062008)),])
nmcl1<-nrow(c2[which(c2$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(c2$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "c2 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(c2)
nav<-nrow(c2[which(!is.na(c2$ThreeYearAs20092011)),])
nmcl1<-nrow(c2[which(c2$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(c2$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "c2 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=c2long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"c2 2006-2008";colnames(mds)[2]<-"c2 2009-2011"
c2cws<-cbind(Time1,Time2)
c2cws <-round(c2cws[,],2)
c2cws<-rbind(c2cws,mds)
rownames(c2cws)[4]<-"Mean difference (95% CI)"
c2cws <- rbind(c2cws, Type= c("c2","c2"))


# c3 cluster 
c3<-finaldatadups3[which(finaldatadups3$cluster.number==3),];dim(c3)
c3long <- gather(c3, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(c3long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(c3)
nav<-nrow(c3[which(!is.na(c3$ThreeYearAs20062008)),])
nmcl1<-nrow(c3[which(c3$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(c3$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "c3 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(c3)
nav<-nrow(c3[which(!is.na(c3$ThreeYearAs20092011)),])
nmcl1<-nrow(c3[which(c3$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(c3$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "c3 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=c3long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"c3 2006-2008";colnames(mds)[2]<-"c3 2009-2011"
c3cws<-cbind(Time1,Time2)
c3cws <-round(c3cws[,],2)
c3cws<-rbind(c3cws,mds)
rownames(c3cws)[4]<-"Mean difference (95% CI)"
c3cws <- rbind(c3cws, Type= c("c3","c3"))


# c4 cluster 
c4<-finaldatadups3[which(finaldatadups3$cluster.number==4),];dim(c4)
c4long <- gather(c4, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(c4long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(c4)
nav<-nrow(c4[which(!is.na(c4$ThreeYearAs20062008)),])
nmcl1<-nrow(c4[which(c4$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(c4$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "c4 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(c4)
nav<-nrow(c4[which(!is.na(c4$ThreeYearAs20092011)),])
nmcl1<-nrow(c4[which(c4$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(c4$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "c4 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=c4long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"c4 2006-2008";colnames(mds)[2]<-"c4 2009-2011"
c4cws<-cbind(Time1,Time2)
c4cws <-round(c4cws[,],2)
c4cws<-rbind(c4cws,mds)
rownames(c4cws)[4]<-"Mean difference (95% CI)"
c4cws <- rbind(c4cws, Type= c("c4","c4"))


# c5 cluster 
c5<-finaldatadups3[which(finaldatadups3$cluster.number==5),];dim(c5)
c5long <- gather(c5, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(c5long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(c5)
nav<-nrow(c5[which(!is.na(c5$ThreeYearAs20062008)),])
nmcl1<-nrow(c5[which(c5$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(c5$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "c5 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(c5)
nav<-nrow(c5[which(!is.na(c5$ThreeYearAs20092011)),])
nmcl1<-nrow(c5[which(c5$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(c5$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "c5 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=c5long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"c5 2006-2008";colnames(mds)[2]<-"c5 2009-2011"
c5cws<-cbind(Time1,Time2)
c5cws <-round(c5cws[,],2)
c5cws<-rbind(c5cws,mds)
rownames(c5cws)[4]<-"Mean difference (95% CI)"
c5cws <- rbind(c5cws, Type= c("c5","c5"))


# c6 cluster 
c6<-finaldatadups3[which(finaldatadups3$cluster.number==6),];dim(c6)
c6long <- gather(c6, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(c6long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(c6)
nav<-nrow(c6[which(!is.na(c6$ThreeYearAs20062008)),])
nmcl1<-nrow(c6[which(c6$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(c6$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "c6 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(c6)
nav<-nrow(c6[which(!is.na(c6$ThreeYearAs20092011)),])
nmcl1<-nrow(c6[which(c6$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(c6$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "c6 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=c6long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"c6 2006-2008";colnames(mds)[2]<-"c6 2009-2011"
c6cws<-cbind(Time1,Time2)
c6cws <-round(c6cws[,],2)
c6cws<-rbind(c6cws,mds)
rownames(c6cws)[4]<-"Mean difference (95% CI)"
c6cws <- rbind(c6cws, Type= c("c6","c6"))


# c7 cluster 
c7<-finaldatadups3[which(finaldatadups3$cluster.number==7),];dim(c7)
c7long <- gather(c7, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(c7long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(c7)
nav<-nrow(c7[which(!is.na(c7$ThreeYearAs20062008)),])
nmcl1<-nrow(c7[which(c7$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(c7$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "c7 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(c7)
nav<-nrow(c7[which(!is.na(c7$ThreeYearAs20092011)),])
nmcl1<-nrow(c7[which(c7$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(c7$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "c7 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=c7long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"c7 2006-2008";colnames(mds)[2]<-"c7 2009-2011"
c7cws<-cbind(Time1,Time2)
c7cws <-round(c7cws[,],2)
c7cws<-rbind(c7cws,mds)
rownames(c7cws)[4]<-"Mean difference (95% CI)"
c7cws <- rbind(c7cws, Type= c("c7","c7"))

# c8 cluster 
c8<-finaldatadups3[which(finaldatadups3$cluster.number==8),];dim(c8)
c8long <- gather(c8, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(c8long)
# Distributions by 3-year period
# 2006-2008
tn<-nrow(c8)
nav<-nrow(c8[which(!is.na(c8$ThreeYearAs20062008)),])
nmcl1<-nrow(c8[which(c8$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(c8$ThreeYearAs20062008,na.rm=T)
Time1<-as.data.frame(rbind(tn,nmcl,mean))
colnames(Time1)[1] <- "c8 2006-2008"
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "% above MCL";rownames(Time1)[3] <- "Arithmetic mean"
# 2009-2011
tn<-nrow(c8)
nav<-nrow(c8[which(!is.na(c8$ThreeYearAs20092011)),])
nmcl1<-nrow(c8[which(c8$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
mean<-mean(c8$ThreeYearAs20092011,na.rm=T)
Time2<-rbind(tn,nmcl,mean)
colnames(Time2)[1] <- "c8 2009-2011"
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "% above MCL";rownames(Time2)[3] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year+SWT, family=gaussian(link="identity"), data=c8long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"c8 2006-2008";colnames(mds)[2]<-"c8 2009-2011"
c8cws<-cbind(Time1,Time2)
c8cws <-round(c8cws[,],2)
c8cws<-rbind(c8cws,mds)
rownames(c8cws)[4]<-"Mean difference (95% CI)"
c8cws <- rbind(c8cws, Type= c("c8","c8"))


## Save to Table

Table1B<-cbind(allcws,q1cws,q2cws,q3cws,q4cws,q5cws,AKHIcws,CMWcws,EMWcws,MAcws,NEcws,PNWcws,SEcws,SWcws,c1cws,c2cws,c3cws,c4cws,c5cws,c6cws,c7cws,c8cws)
Table1B<-cbind(Column1 = "NAME", Table1B)

# Transpose into better format
Table1B <- data.frame(t(Table1B[-1]))
colnames(Table1B) <- Table1B[, 1]
colnames(Table1B)[1]<-"N";colnames(Table1B)[2]<-"% at/above MCL";colnames(Table1B)[3]<-"Arithmetic mean";colnames(Table1B)[4]<-"Mean difference"



write.xlsx(Table1B,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/Sensitivity Analysis Table 1.xlsx")





### end 
#######       1C. Extra unused findings: OR of MCL exceedance in second vs first time period ######

#### Set-up
library(tidyr) # for gather fxn
library(gee)  #for Generalized Estimating Equations  (function gee)     
cwslong3<-gather(cws, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(cwslong3) 
cwslong3$MCLExc<-0
cwslong3$MCLExc[which(cwslong3$arsenic>10)]<-1
table(cwslong3$MCLExc)

cwslong3$post<-0
cwslong3$post[which(cwslong3$year!="ThreeYearAs20062008")]<-1
table(cwslong3$post)
table(cwslong3$year)

surfwlong<-cwslong3[which(cwslong3$Source.Water.Type=="SW"),]
grwlong<-cwslong3[which(cwslong3$Source.Water.Type=="GW"),]

ps1<-cwslong3[which(cwslong3$popcat==1),]
ps2<-cwslong3[which(cwslong3$popcat==2),]
ps3<-cwslong3[which(cwslong3$popcat==3),]
ps4<-cwslong3[which(cwslong3$popcat==4),]
ps5<-cwslong3[which(cwslong3$popcat==5),]
table(cws$popcat)

akhilong<-cwslong3[which(cwslong3$Region=="AKHI"),]
cmwlong<-cwslong3[which(cwslong3$Region=="CMW"),]
emwlong<-cwslong3[which(cwslong3$Region=="EMW"),]
malong<-cwslong3[which(cwslong3$Region=="MA"),]
nelong<-cwslong3[which(cwslong3$Region=="NE"),]
pnwlong<-cwslong3[which(cwslong3$Region=="PNW"),]
swlong<-cwslong3[which(cwslong3$Region=="SW"),]
selong<-cwslong3[which(cwslong3$Region=="SE"),]

# For fdups3long
fdups3long$MCLExc<-0
fdups3long$MCLExc[which(fdups3long$arsenic>10)]<-1
table(fdups3long$MCLExc)
fdups3long$post<-0
fdups3long$post[which(fdups3long$year!="ThreeYearAs20062008")]<-1
table(fdups3long$post)
table(fdups3long$year)
c1<-fdups3long[which(fdups3long$cluster.number==1),]
c2<-fdups3long[which(fdups3long$cluster.number==2),]
c3<-fdups3long[which(fdups3long$cluster.number==3),]
c4<-fdups3long[which(fdups3long$cluster.number==4),]
c5<-fdups3long[which(fdups3long$cluster.number==5),]
c6<-fdups3long[which(fdups3long$cluster.number==6),]
c7<-fdups3long[which(fdups3long$cluster.number==7),]
c8<-fdups3long[which(fdups3long$cluster.number==8),]

#### Part 1. Unadjusted models for Table 1
# An example using GEE, gives same results for SW, but should check for all regions:
fit <- gee(MCLExc~post,  id=as.factor(PWSID), data = akhilong, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
geer <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
geer

### Overall:
fit <- glm(MCLExc~post, data = cwslong3, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
allr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
allr # Exact same results

### By source water type
fit <- glm(MCLExc~post, data = surfwlong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
surfwr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
surfwr

fit <- glm(MCLExc~post, data = grwlong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
gwr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
gwr #

### By population served size
fit <- glm(MCLExc~post, data = ps1, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
pop1r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
pop1r

fit <- glm(MCLExc~post, data = ps2, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
pop2r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
pop2r

fit <- glm(MCLExc~post, data = ps3, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
pop3r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
pop3r

fit <- glm(MCLExc~post, data = ps4, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
pop4r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
pop4r

fit <- glm(MCLExc~post, data = ps5, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
pop5r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
pop5r

### By region:
fit <- glm(MCLExc~post, data = akhilong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
akhir <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
akhir #

fit <- glm(MCLExc~post, data = cmwlong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
cmwr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
cmwr #

fit <- glm(MCLExc~post, data = emwlong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
emwr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
emwr #


fit <- glm(MCLExc~post, data = malong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
mar <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
mar #


fit <- glm(MCLExc~post, data = nelong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
ner <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
ner #


fit <- glm(MCLExc~post, data = pnwlong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
pnwr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
pnwr #


fit <- glm(MCLExc~post, data = selong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
ser <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
ser #

fit <- glm(MCLExc~post, data = swlong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
swr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
swr # Exact same results

### By sociodemographic county cluster
# 
fit <- glm(MCLExc~post, data = c1, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
c1r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c1r # Exact same results

fit <- glm(MCLExc~post, data = c2, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
c2r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c2r #

fit <- glm(MCLExc~post, data = c3, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
c3r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c3r #

fit <- glm(MCLExc~post, data = c4, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
c4r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c4r #

fit <- glm(MCLExc~post, data = c5, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
c5r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c5r #

fit <- glm(MCLExc~post, data = c6, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
c6r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c6r #

fit <- glm(MCLExc~post, data = c7, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
c7r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c7r #

fit <- glm(MCLExc~post, data = c8, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
c8r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c8r #

### Add all into new column
orats<-rbind(allr, gwr,surfwr,pop1r,pop2r,pop3r,pop4r,pop5r,akhir,cmwr,emwr,mar,ner,pnwr,ser,swr,c1r,c2r,c3r,c4r,c5r,c6r,c7r,c8r)
oratstobind<-rbind("ref",allr,"ref", gwr,"ref",surfwr,"ref",pop1r,"ref",pop2r,"ref",pop3r,"ref",pop4r,"ref",
                   pop5r,"ref",akhir,"ref",cmwr,"ref",emwr,"ref",mar,"ref",ner,"ref",pnwr,"ref",ser,"ref",swr,"ref",c1r,"ref",c2r,"ref",c3r,"ref",c4r,"ref",c5r,"ref",c6r,"ref",c7r,"ref",c8r)

# Add these to Table 1
Table1Add<-cbind(Table1,oratstobind)
write.xlsx(Table1Add,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/Table1 additions.xlsx")


#### Part 2: adjusting for source water type
# An example using GEE, gives same results for SW, but should check for all regions:
fit <- gee(MCLExc~post+SWT,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
geer <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
geer

### Overall:
fit <- glm(MCLExc~post+SWT, data = cwslong3, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
allr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
allr # Exact same results

### By population served size
fit <- glm(MCLExc~post+SWT, data = ps1, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
pop1r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
pop1r

fit <- glm(MCLExc~post+SWT, data = ps2, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
pop2r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
pop2r

fit <- glm(MCLExc~post+SWT, data = ps3, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
pop3r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
pop3r

fit <- glm(MCLExc~post+SWT, data = ps4, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
pop4r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
pop4r

fit <- glm(MCLExc~post+SWT, data = ps5, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
pop5r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
pop5r

### By region:
fit <- glm(MCLExc~post+SWT, data = akhilong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
akhir <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
akhir #

fit <- glm(MCLExc~post+SWT, data = cmwlong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
cmwr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
cmwr #

fit <- glm(MCLExc~post+SWT, data = emwlong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
emwr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
emwr #


fit <- glm(MCLExc~post+SWT, data = malong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
mar <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
mar #


fit <- glm(MCLExc~post+SWT, data = nelong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
ner <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
ner #


fit <- glm(MCLExc~post+SWT, data = pnwlong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
pnwr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
pnwr #


fit <- glm(MCLExc~post+SWT, data = selong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
ser <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
ser #

fit <- glm(MCLExc~post+SWT, data = swlong, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
swr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
swr # Exact same results

### By sociodemographic county cluster
# 
fit <- glm(MCLExc~post+SWT, data = c1, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
c1r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c1r # Exact same results

fit <- glm(MCLExc~post+SWT, data = c2, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
c2r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c2r #

fit <- glm(MCLExc~post+SWT, data = c3, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
c3r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c3r #

fit <- glm(MCLExc~post+SWT, data = c4, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
c4r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c4r #

fit <- glm(MCLExc~post+SWT, data = c5, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
c5r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c5r #

fit <- glm(MCLExc~post+SWT, data = c6, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
c6r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c6r #

fit <- glm(MCLExc~post+SWT, data = c7, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
c7r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c7r #

fit <- glm(MCLExc~post+SWT, data = c8, family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Std. Error" ]
c8r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c8r #

### Add all into new column
oratsadj<-rbind(allr,pop1r,pop2r,pop3r,pop4r,pop5r,akhir,cmwr,emwr,mar,ner,pnwr,ser,swr,c1r,c2r,c3r,c4r,c5r,c6r,c7r,c8r)
oratsadjtobind<-rbind("ref",allr,"ref",pop1r,"ref",pop2r,"ref",pop3r,"ref",pop4r,"ref",
                   pop5r,"ref",akhir,"ref",cmwr,"ref",emwr,"ref",mar,"ref",ner,"ref",pnwr,"ref",ser,"ref",swr,"ref",c1r,"ref",c2r,"ref",c3r,"ref",c4r,"ref",c5r,"ref",c6r,"ref",c7r,"ref",c8r)
# write.xlsx(oratsadj,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/oratsadj.xlsx")

# Add these to Table 1
STable15Add<-cbind(Table1B,oratsadjtobind)






### end


#######       1D. Table 2. OR of MCL exceedance (GEE, both 3-yr) comparing subgroup membership #######
# Create dummy vars
cws$akhi<-0; cws$akhi[which(cws$Region=="AKHI")]<-1
cws$cmw<-0; cws$cmw[which(cws$Region=="CMW")]<-1
cws$emw<-0; cws$emw[which(cws$Region=="EMW")]<-1
cws$ma<-0; cws$ma[which(cws$Region=="MA")]<-1
cws$ne<-0; cws$ne[which(cws$Region=="NE")]<-1
cws$pnw<-0; cws$pnw[which(cws$Region=="PNW")]<-1
cws$se<-0; cws$se[which(cws$Region=="SE")]<-1
cws$sw<-0; cws$sw[which(cws$Region=="SW")]<-1

cws$GrW<-0; cws$GrW[which(cws$Source.Water.Type=="GW")]<-1
cws$SrW<-0; cws$SrW[which(cws$Source.Water.Type=="SW")]<-1

cws$popcat1<-0;cws$popcat1[which(cws$popcat==1)]<-1
cws$popcat2<-0;cws$popcat2[which(cws$popcat==2)]<-1
cws$popcat3<-0;cws$popcat3[which(cws$popcat==3)]<-1
cws$popcat4<-0;cws$popcat4[which(cws$popcat==4)]<-1
cws$popcat5<-0;cws$popcat5[which(cws$popcat==5)]<-1

fdups3long$c1<-0;fdups3long$c1[which(fdups3long$cluster.number==1)]<-1
fdups3long$c2<-0;fdups3long$c2[which(fdups3long$cluster.number==2)]<-1
fdups3long$c3<-0;fdups3long$c3[which(fdups3long$cluster.number==3)]<-1
fdups3long$c4<-0;fdups3long$c4[which(fdups3long$cluster.number==4)]<-1
fdups3long$c5<-0;fdups3long$c5[which(fdups3long$cluster.number==5)]<-1
fdups3long$c6<-0;fdups3long$c6[which(fdups3long$cluster.number==6)]<-1
fdups3long$c7<-0;fdups3long$c7[which(fdups3long$cluster.number==7)]<-1
fdups3long$c8<-0;fdups3long$c8[which(fdups3long$cluster.number==8)]<-1

cwslong3<-gather(cws, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE);dim(cwslong3) 
cwslong3$MCLExc<-0
cwslong3$MCLExc[which(cwslong3$arsenic>10)]<-1
table(cwslong3$MCLExc)

### By region
fit <- gee(MCLExc~akhi,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
akhir <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
akhir

fit <- gee(MCLExc~cmw,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
cmwr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
cmwr

fit <- gee(MCLExc~emw,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
emwr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
emwr

fit <- gee(MCLExc~ma,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
mar <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
mar

fit <- gee(MCLExc~ne,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
ner <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
ner

fit <- gee(MCLExc~pnw,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
pnwr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
pnwr

fit <- gee(MCLExc~se,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
ser <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
ser

fit <- gee(MCLExc~sw,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
swr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
swr

### By source water type
fit <- gee(MCLExc~GrW,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
gwr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
gwr

fit <- gee(MCLExc~SrW,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
surwr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
surwr

### By population size served
fit <- gee(MCLExc~popcat1,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
popcat1r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
popcat1r

fit <- gee(MCLExc~popcat2,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
popcat2r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
popcat2r

fit <- gee(MCLExc~popcat3,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
popcat3r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
popcat3r

fit <- gee(MCLExc~popcat4,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
popcat4r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
popcat4r

fit <- gee(MCLExc~popcat5,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
popcat5r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
popcat5r


### By sociodemographic county cluster
fit <- gee(MCLExc~c1,  id=as.factor(PWSID), data = fdups3long, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
c1r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c1r

fit <- gee(MCLExc~c2,  id=as.factor(PWSID), data = fdups3long, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
c2r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c2r

fit <- gee(MCLExc~c3,  id=as.factor(PWSID), data = fdups3long, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
c3r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c3r

fit <- gee(MCLExc~c4,  id=as.factor(PWSID), data = fdups3long, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
c4r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c4r

fit <- gee(MCLExc~c5,  id=as.factor(PWSID), data = fdups3long, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
c5r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c5r

fit <- gee(MCLExc~c6,  id=as.factor(PWSID), data = fdups3long, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
c6r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c6r

fit <- gee(MCLExc~c7,  id=as.factor(PWSID), data = fdups3long, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
c7r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c7r

fit <- gee(MCLExc~c8,  id=as.factor(PWSID), data = fdups3long, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
c8r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c8r


orrs<-rbind(gwr,surwr,popcat1r,popcat2r,popcat3r,popcat4r,popcat5r,akhir,cmwr,emwr,mar,ner,pnwr,ser,swr,c1r,c2r,c3r,c4r,c5r,c6r,c7r,c8r)


### Part 2, adjust for source water type:
### By region
fit <- gee(MCLExc~akhi+SWT,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
akhir <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
akhir

fit <- gee(MCLExc~cmw+SWT,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
cmwr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
cmwr

fit <- gee(MCLExc~emw+SWT,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
emwr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
emwr

fit <- gee(MCLExc~ma+SWT,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
mar <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
mar

fit <- gee(MCLExc~ne+SWT,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
ner <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
ner

fit <- gee(MCLExc~pnw+SWT,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
pnwr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
pnwr

fit <- gee(MCLExc~se+SWT,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
ser <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
ser

fit <- gee(MCLExc~sw+SWT,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
swr <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
swr


### By population size served
fit <- gee(MCLExc~popcat1+SWT,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
popcat1r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
popcat1r

fit <- gee(MCLExc~popcat2+SWT,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
popcat2r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
popcat2r

fit <- gee(MCLExc~popcat3+SWT,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
popcat3r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
popcat3r

fit <- gee(MCLExc~popcat4+SWT,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
popcat4r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
popcat4r

fit <- gee(MCLExc~popcat5+SWT,  id=as.factor(PWSID), data = cwslong3, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
popcat5r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
popcat5r


### By sociodemographic county cluster
fit <- gee(MCLExc~c1+SWT,  id=as.factor(PWSID), data = fdups3long, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
c1r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c1r

fit <- gee(MCLExc~c2+SWT,  id=as.factor(PWSID), data = fdups3long, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
c2r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c2r

fit <- gee(MCLExc~c3+SWT,  id=as.factor(PWSID), data = fdups3long, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
c3r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c3r

fit <- gee(MCLExc~c4+SWT,  id=as.factor(PWSID), data = fdups3long, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
c4r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c4r

fit <- gee(MCLExc~c5+SWT,  id=as.factor(PWSID), data = fdups3long, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
c5r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c5r

fit <- gee(MCLExc~c6+SWT,  id=as.factor(PWSID), data = fdups3long, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
c6r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c6r


fit <- gee(MCLExc~c7+SWT,  id=as.factor(PWSID), data = fdups3long, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
c7r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c7r

fit <- gee(MCLExc~c8+SWT,  id=as.factor(PWSID), data = fdups3long, corstr="unstructured" , family = binomial(link = "logit") )
beta <- summary(fit)$coeff[2, "Estimate"]
se <- summary( fit )$coeff[ 2 , "Robust S.E." ]
c8r <- convert.to.ci(round(exp(c(
  beta , 
  beta - 1.96 * se ,
  beta + 1.96 * se)), 2))
c8r


orrsadjusted<-rbind("gwr","surwr",popcat1r,popcat2r,popcat3r,popcat4r,popcat5r,akhir,cmwr,emwr,mar,ner,pnwr,ser,swr,c1r,c2r,c3r,c4r,c5r,c6r,c7r,c8r)

ExtraTable<-cbind(orrs,orrsadjusted)
colnames(ExtraTable)[1]<-"Crude"
colnames(ExtraTable)[2]<-"Adjusted"

write.xlsx(ExtraTable,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/Table 2 ORs.xlsx")

### end

#######       1E. Table 1. N records by CWS characteristic (1000s) ######
# 'dat' has all records retained (230k)
# merge dat in with cws, keep only records which match PWSID from cws

dat2<-merge(dat,cws,by="PWSID",all=T)
describe(dat2$Region) #6725 missing Region; these are ones we excluded for not being able to match to SDWIS
dat2<-dat2[which(!is.na(dat2$Region)),];dim(dat2) #N = 223,540 records used in analysis

### N records for 2006-2008 averages
describe(dat2$ThreeYearAs20062008)
dat0608<-dat2[which(!is.na(dat2$ThreeYearAs20062008)),];dim(dat0608)
describe(dat0608$ThreeYearAs20062008)

nall<-nrow(dat0608)
ngw<-nrow(dat0608[which(dat0608$Source.Water.Type.y=="GW"),])
nsw<-nrow(dat0608[which(dat0608$Source.Water.Type.y=="SW"),])
npop1<-nrow(dat0608[which(dat0608$popcat==1),])
npop2<-nrow(dat0608[which(dat0608$popcat==2),])
npop3<-nrow(dat0608[which(dat0608$popcat==3),])
npop4<-nrow(dat0608[which(dat0608$popcat==4),])
npop5<-nrow(dat0608[which(dat0608$popcat==5),])
nAKHI<-nrow(dat0608[which(dat0608$Region=="AKHI"),])
nCMW<-nrow(dat0608[which(dat0608$Region=="CMW"),])
nEMW<-nrow(dat0608[which(dat0608$Region=="EMW"),])
nMA<-nrow(dat0608[which(dat0608$Region=="MA"),])
nNE<-nrow(dat0608[which(dat0608$Region=="NE"),])
nPNW<-nrow(dat0608[which(dat0608$Region=="PNW"),])
nSE<-nrow(dat0608[which(dat0608$Region=="SE"),])
nSW<-nrow(dat0608[which(dat0608$Region=="SW"),])

# By county-cluster, use:
fdat2<-merge(dat,finaldatadups3,by="PWSID",all=T)
fdat0608<-fdat2[which(!is.na(fdat2$ThreeYearAs20062008)),];dim(fdat0608)
nc1<-nrow(fdat0608[which(fdat0608$cluster.number==1),])
nc2<-nrow(fdat0608[which(fdat0608$cluster.number==2),])
nc3<-nrow(fdat0608[which(fdat0608$cluster.number==3),])
nc4<-nrow(fdat0608[which(fdat0608$cluster.number==4),])
nc5<-nrow(fdat0608[which(fdat0608$cluster.number==5),])
nc6<-nrow(fdat0608[which(fdat0608$cluster.number==6),])
nc7<-nrow(fdat0608[which(fdat0608$cluster.number==7),])
nc8<-nrow(fdat0608[which(fdat0608$cluster.number==8),])

N0608<-rbind(nall,ngw,nsw,npop1,npop2,npop3,npop4,npop5,nAKHI,nCMW,nEMW,nMA,nNE,nPNW,nSE,nSW,
             nc1,nc2,nc3,nc4,nc5,nc6,nc7,nc8)

N0608<-signif(N0608,digits=2)

### N records for 2009-2011 averages
describe(dat2$ThreeYearAs20092011)
dat0911<-dat2[which(!is.na(dat2$ThreeYearAs20092011)),];dim(dat0911)
describe(dat0911$ThreeYearAs20092011)

nall<-nrow(dat0911)
ngw<-nrow(dat0911[which(dat0911$Source.Water.Type.y=="GW"),])
nsw<-nrow(dat0911[which(dat0911$Source.Water.Type.y=="SW"),])
npop1<-nrow(dat0911[which(dat0911$popcat==1),])
npop2<-nrow(dat0911[which(dat0911$popcat==2),])
npop3<-nrow(dat0911[which(dat0911$popcat==3),])
npop4<-nrow(dat0911[which(dat0911$popcat==4),])
npop5<-nrow(dat0911[which(dat0911$popcat==5),])
nAKHI<-nrow(dat0911[which(dat0911$Region=="AKHI"),])
nCMW<-nrow(dat0911[which(dat0911$Region=="CMW"),])
nEMW<-nrow(dat0911[which(dat0911$Region=="EMW"),])
nMA<-nrow(dat0911[which(dat0911$Region=="MA"),])
nNE<-nrow(dat0911[which(dat0911$Region=="NE"),])
nPNW<-nrow(dat0911[which(dat0911$Region=="PNW"),])
nSE<-nrow(dat0911[which(dat0911$Region=="SE"),])
nSW<-nrow(dat0911[which(dat0911$Region=="SW"),])

# By county-cluster, use:
fdat2<-merge(dat,finaldatadups3,by="PWSID",all=T)
fdat0911<-fdat2[which(!is.na(fdat2$ThreeYearAs20062008)),];dim(fdat0911)
nc1<-nrow(fdat0911[which(fdat0911$cluster.number==1),])
nc2<-nrow(fdat0911[which(fdat0911$cluster.number==2),])
nc3<-nrow(fdat0911[which(fdat0911$cluster.number==3),])
nc4<-nrow(fdat0911[which(fdat0911$cluster.number==4),])
nc5<-nrow(fdat0911[which(fdat0911$cluster.number==5),])
nc6<-nrow(fdat0911[which(fdat0911$cluster.number==6),])
nc7<-nrow(fdat0911[which(fdat0911$cluster.number==7),])
nc8<-nrow(fdat0911[which(fdat0911$cluster.number==8),])

N0911<-rbind(nall,ngw,nsw,npop1,npop2,npop3,npop4,npop5,nAKHI,nCMW,nEMW,nMA,nNE,nPNW,nSE,nSW,
             nc1,nc2,nc3,nc4,nc5,nc6,nc7,nc8)

N0911<-signif(N0911,digits=2)

Nrecords<-cbind(N0608,N0911)

write.xlsx(Nrecords,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/NrecordsForTable1.xlsx")



### end

####### 2. Table 3 and Table S4. Characteristics of CWSs by compliance categories ####### 
cws$Tribal[which(cws$Tribal=="TRUE")]<-1
cws$Tribal[which(cws$Tribal=="FALSE")]<-0

### Create compliance category groupings at CWS and county-level for 10, 5, and 1 cut-points
# CWS-level
cws$group10<-0
cws$group10[cws$ThreeYearAs20062008<=10&cws$ThreeYearAs20092011<=10]<-1 #low/low
cws$group10[cws$ThreeYearAs20062008<=10&cws$ThreeYearAs20092011>10]<-2 #low/high
cws$group10[cws$ThreeYearAs20062008>10&cws$ThreeYearAs20092011<=10]<-3 #high/low
cws$group10[cws$ThreeYearAs20062008>10&cws$ThreeYearAs20092011>10]<-4 #high/high
table(cws$group10)
#    0     1     2     3     4 
# 9511 25817   165   411   502 
25817/26895 #96.0%
165/26895 #<1%
411/26895 #1.5%
502/26895 #1.9%
cws$group10[which(cws$group10==0)]<-NA

g1<-cws[which(cws$group10==1),] #low low
describe(g1$ThreeYearAs20062008);
describe(g1$ThreeYearAs20092011)

g2<-cws[which(cws$group10==2),] #low/high
describe(g2$ThreeYearAs20062008);
min(g2$ThreeYearAs20092011)

g3<-cws[which(cws$group10==3),] #high/low
describe(g3$ThreeYearAs20062008)
min(g3$ThreeYearAs20092011)

g4<-cws[which(cws$group10==4),] #high/high
describe(g4$ThreeYearAs20062008);
describe(g4$ThreeYearAs20092011)

cws$group5<-0
cws$group5[cws$ThreeYearAs20062008<=5&cws$ThreeYearAs20092011<=5]<-1 #low/low
cws$group5[cws$ThreeYearAs20062008<=5&cws$ThreeYearAs20092011>5]<-2 #low/high
cws$group5[cws$ThreeYearAs20062008>5&cws$ThreeYearAs20092011<=5]<-3 #high/low
cws$group5[cws$ThreeYearAs20062008>5&cws$ThreeYearAs20092011>5]<-4 #high/high
table(cws$group5)
#    0     1     2     3     4 
#9511 23703   482   683  2027 
23703/26895 #88.1%
482/26895 #1.8%
683/26895 #2.5%
2027/26895 #7.5%
cws$group5[which(cws$group5==0)]<-NA
describe(cws$group1)

cws$group1<-0
cws$group1[cws$ThreeYearAs20062008<=1&cws$ThreeYearAs20092011<=1]<-1 #low/low
cws$group1[cws$ThreeYearAs20062008<=1&cws$ThreeYearAs20092011>1]<-2 #low/high
cws$group1[cws$ThreeYearAs20062008>1&cws$ThreeYearAs20092011<=1]<-3 #high/low
cws$group1[cws$ThreeYearAs20062008>1&cws$ThreeYearAs20092011>1]<-4 #high/high
table(cws$group1)
#    0     1     2     3     4 
# 9511 17165  1426  1247  7057 
17165/26895 #63.8%
1426/26895 #5.3%
1247/26895 #4.6%
7057/26895 #26.2%
cws$group1[which(cws$group1==0)]<-NA

# County-level
county$group10<-0
county$group10[county$WeightedAs20062008<=10&county$WeightedAs20092011<=10]<-1 #low/low
county$group10[county$WeightedAs20062008<=10&county$WeightedAs20092011>10]<-2 #low/high
county$group10[county$WeightedAs20062008>10&county$WeightedAs20092011<=10]<-3 #high/low
county$group10[county$WeightedAs20062008>10&county$WeightedAs20092011>10]<-4 #high/high
table(county$group10)
#   0    1    2    3    4 
# 478 2225    5   14   18 
478/2262 #21.1%
2225/2262 #98.4%
5/2262 #<1%
14/2262 #<1%
18/2262 #<1%
county$group10[which(county$group10==0)]<-NA

county$group5<-0
county$group5[county$WeightedAs20062008<=5&county$WeightedAs20092011<=5]<-1 #low/low
county$group5[county$WeightedAs20062008<=5&county$WeightedAs20092011>5]<-2 #low/high
county$group5[county$WeightedAs20062008>5&county$WeightedAs20092011<=5]<-3 #high/low
county$group5[county$WeightedAs20062008>5&county$WeightedAs20092011>5]<-4 #high/high
table(county$group5)
#   0    1    2    3    4 
# 478 2083   30   42  107  
2083/2247 #92.7%
30/2247 #1.3%
42/2247 #1.9%
107/2247 #4.8%
county$group5[which(county$group5==0)]<-NA

county$group1<-0
county$group1[county$WeightedAs20062008<=1&county$WeightedAs20092011<=1]<-1 #low/low
county$group1[county$WeightedAs20062008<=1&county$WeightedAs20092011>1]<-2 #low/high
county$group1[county$WeightedAs20062008>1&county$WeightedAs20092011<=1]<-3 #high/low
county$group1[county$WeightedAs20062008>1&county$WeightedAs20092011>1]<-4 #high/high
table(county$group1)
#  0    1    2    3    4 
#478 1437  104  118  603 
1437/2247 #64.0%
104/2247 #4.6%
118/2247 #5.3%
603/2247 #26.8%
county$group1[which(county$group1==0)]<-NA
describe(county$group10)

#######       2.A 10 ug/L cut point ########
### CWS-level 
# only with 26,895 CWSs with both years available:

### All CWSs
col1<-cws[which(cws$group10>0),]
# Mean arsenic change
col1$change<-col1$ThreeYearAs20062008-col1$ThreeYearAs20092011
fit1 <- glm(change ~ +1, family=gaussian(link="identity"), data=col1)
summary(fit1)
mch<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

#-- Source Water Type
n<-nrow(col1)
ngw<-nrow(col1[which(col1$Source.Water.Type=="GW"|col1$Source.Water.Type=="GU"),])
ngwpr<-round(100*(ngw/n),1)
npgw<-to.n.prop(c(ngw,ngwpr))
nsw<-nrow(col1[which(col1$Source.Water.Type=="SW"),])
nswpr<-round(100*(nsw/n),1)
npsw<-to.n.prop(c(nsw,nswpr))

#-- Population Served
fit <- glm(Adjusted.Total.Population.Served~1,
           family=gaussian(link="identity"), data=col1)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col1[which(col1$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col1[which(col1$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col1[which(col1$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col1[which(col1$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col1[which(col1$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col1[which(col1$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col1[which(col1$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col1[which(col1$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- Tribal water systems-#
nTribal<-nrow(col1[which(col1$Tribal==1),])
nTribalpr<-round(100*(nTribal/n),1)
npTribal<-to.n.prop(c(nTribal,nTribalpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col1$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))


#-- By sociodemographic county cluster -#
### Row group 3: by sociodemographic cluster group:
## For this group we use: finaldatadups3
# First we need to assign each CWS to a county-cluster; lets pull thoe CWSs from _data_ which serve multiple counties
# and see if these are the same counties:
describe(finaldatadups3$PWSID) ##one record for each CWS serving each county, 3-yr averages 
describe(finaldatadups3$CountyFIPS)

# fix arsenic var names
colnames(finaldatadups3)[11] <- "ThreeYearAs20062008"
colnames(finaldatadups3)[12] <- "ThreeYearAs20092011"
library(tidyr)

# Redefine group10, group 5, and group1
finaldatadups3$group10<-0
finaldatadups3$group10[finaldatadups3$ThreeYearAs20062008<=10&finaldatadups3$ThreeYearAs20092011<=10]<-1 #low/low
finaldatadups3$group10[finaldatadups3$ThreeYearAs20062008<=10&finaldatadups3$ThreeYearAs20092011>10]<-2 #low/high
finaldatadups3$group10[finaldatadups3$ThreeYearAs20062008>10&finaldatadups3$ThreeYearAs20092011<=10]<-3 #high/low
finaldatadups3$group10[finaldatadups3$ThreeYearAs20062008>10&finaldatadups3$ThreeYearAs20092011>10]<-4 #high/high
table(finaldatadups3$group10)
#     0     1     2     3     4 
# 9598 26020   159   397   500 
26020/36674 #70.9%
159/36674 #<1%
397/36674 #1.1%
500/36674 #1.4%
finaldatadups3$group10[which(finaldatadups3$group10==0)]<-NA

finaldatadups3$group5<-0
finaldatadups3$group5[finaldatadups3$ThreeYearAs20062008<=5&finaldatadups3$ThreeYearAs20092011<=5]<-1 #low/low
finaldatadups3$group5[finaldatadups3$ThreeYearAs20062008<=5&finaldatadups3$ThreeYearAs20092011>5]<-2 #low/high
finaldatadups3$group5[finaldatadups3$ThreeYearAs20062008>5&finaldatadups3$ThreeYearAs20092011<=5]<-3 #high/low
finaldatadups3$group5[finaldatadups3$ThreeYearAs20062008>5&finaldatadups3$ThreeYearAs20092011>5]<-4 #high/high
table(finaldatadups3$group5)
# 
0     1     2     3     4 
#9598 23904   490   681  2001 

finaldatadups3$group5[which(finaldatadups3$group5==0)]<-NA


finaldatadups3$group1<-0
finaldatadups3$group1[finaldatadups3$ThreeYearAs20062008<=1&finaldatadups3$ThreeYearAs20092011<=1]<-1 #low/low
finaldatadups3$group1[finaldatadups3$ThreeYearAs20062008<=1&finaldatadups3$ThreeYearAs20092011>1]<-2 #low/high
finaldatadups3$group1[finaldatadups3$ThreeYearAs20062008>1&finaldatadups3$ThreeYearAs20092011<=1]<-3 #high/low
finaldatadups3$group1[finaldatadups3$ThreeYearAs20062008>1&finaldatadups3$ThreeYearAs20092011>1]<-4 #high/high
table(finaldatadups3$group1)
# 
#0     1     2     3     4 
#9598 17364  1424  1258  7030 
finaldatadups3$group1[which(finaldatadups3$group1==0)]<-NA





col1<-finaldatadups3[which(finaldatadups3$group10>0),]
nscc<-nrow(col1)

nc1<-nrow(col1[which(col1$cluster.number==1),])
nc1pr<-round(100*(nc1/nscc),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col1[which(col1$cluster.number==2),])
nc2pr<-round(100*(nc2/nscc),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col1[which(col1$cluster.number==3),])
nc3pr<-round(100*(nc3/nscc),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col1[which(col1$cluster.number==4),])
nc4pr<-round(100*(nc4/nscc),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col1[which(col1$cluster.number==5),])
nc5pr<-round(100*(nc5/nscc),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col1[which(col1$cluster.number==6),])
nc6pr<-round(100*(nc6/nscc),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col1[which(col1$cluster.number==7),])
nc7pr<-round(100*(nc7/nscc),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col1[which(col1$cluster.number==8),])
nc8pr<-round(100*(nc8/nscc),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cwsall<-rbind(n,mch,npgw,npsw,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8,npTribal,state1,state2,state3)
cwsall <- rbind(cwsall, Cateogry= c("All CWSs"))

### Cateogry 1 (low/low)
col2<-cws[which(cws$group10==1),]
# Mean arsenic change
col2$change<-col2$ThreeYearAs20062008-col2$ThreeYearAs20092011
fit1 <- glm(change ~ +1, family=gaussian(link="identity"), data=col2)
summary(fit1)
mch<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                           summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                           summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

#-- Source Water Type
n<-nrow(col2)
ngw<-nrow(col2[which(col2$Source.Water.Type=="GW"|col2$Source.Water.Type=="GU"),])
ngwpr<-round(100*(ngw/n),1)
npgw<-to.n.prop(c(ngw,ngwpr))
nsw<-nrow(col2[which(col2$Source.Water.Type=="SW"),])
nswpr<-round(100*(nsw/n),1)
npsw<-to.n.prop(c(nsw,nswpr))

#-- Population Served
fit <- glm(Adjusted.Total.Population.Served~1,
           family=gaussian(link="identity"), data=col2)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col2[which(col2$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col2[which(col2$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col2[which(col2$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col2[which(col2$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col2[which(col2$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col2[which(col2$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col2[which(col2$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col2[which(col2$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

##-- By SCC -#
col2<-finaldatadups3[which(finaldatadups3$group10==1),]
nscc<-nrow(col2)

nc1<-nrow(col2[which(col2$cluster.number==1),])
nc1pr<-round(100*(nc1/nscc),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col2[which(col2$cluster.number==2),])
nc2pr<-round(100*(nc2/nscc),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col2[which(col2$cluster.number==3),])
nc3pr<-round(100*(nc3/nscc),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col2[which(col2$cluster.number==4),])
nc4pr<-round(100*(nc4/nscc),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col2[which(col2$cluster.number==5),])
nc5pr<-round(100*(nc5/nscc),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col2[which(col2$cluster.number==6),])
nc6pr<-round(100*(nc6/nscc),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col2[which(col2$cluster.number==7),])
nc7pr<-round(100*(nc7/nscc),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col2[which(col2$cluster.number==8),])
nc8pr<-round(100*(nc8/nscc),1)
npc8<-to.n.prop(c(nc8,nc8pr))

col2<-cws[which(cws$group10==1),]

#-- Tribal water systems-#
nTribal<-nrow(col2[which(col2$Tribal==1),])
nTribalpr<-round(100*(nTribal/n),1)
npTribal<-to.n.prop(c(nTribal,nTribalpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col2$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

cat1<-rbind(n,mch,npgw,npsw,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8,npTribal,state1,state2,state3)
cat1 <- rbind(cat1, Cateogry= c("Category 1, Low/Low"))

### Category 3 /Column4 Low/High
col4<-cws[which(cws$group10==2),]
# Mean arsenic change
col4$change<-col4$ThreeYearAs20062008-col4$ThreeYearAs20092011
fit1 <- glm(change ~ +1, family=gaussian(link="identity"), data=col4)
summary(fit1)
mch<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                           summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                           summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

#-- Source Water Type
n<-nrow(col4)
ngw<-nrow(col4[which(col4$Source.Water.Type=="GW"|col4$Source.Water.Type=="GU"),])
ngwpr<-round(100*(ngw/n),1)
npgw<-to.n.prop(c(ngw,ngwpr))
nsw<-nrow(col4[which(col4$Source.Water.Type=="SW"),])
nswpr<-round(100*(nsw/n),1)
npsw<-to.n.prop(c(nsw,nswpr))

#-- Population Served
fit <- glm(Adjusted.Total.Population.Served~1,
           family=gaussian(link="identity"), data=col4)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col4[which(col4$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col4[which(col4$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col4[which(col4$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col4[which(col4$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col4[which(col4$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col4[which(col4$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col4[which(col4$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col4[which(col4$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- By SCC -#
col4<-finaldatadups3[which(finaldatadups3$group10==2),]
nscc<-nrow(col4)

nc1<-nrow(col4[which(col4$cluster.number==1),])
nc1pr<-round(100*(nc1/nscc),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col4[which(col4$cluster.number==2),])
nc2pr<-round(100*(nc2/nscc),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col4[which(col4$cluster.number==3),])
nc3pr<-round(100*(nc3/nscc),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col4[which(col4$cluster.number==4),])
nc4pr<-round(100*(nc4/nscc),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col4[which(col4$cluster.number==5),])
nc5pr<-round(100*(nc5/nscc),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col4[which(col4$cluster.number==6),])
nc6pr<-round(100*(nc6/nscc),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col4[which(col4$cluster.number==7),])
nc7pr<-round(100*(nc7/nscc),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col4[which(col4$cluster.number==8),])
nc8pr<-round(100*(nc8/nscc),1)
npc8<-to.n.prop(c(nc8,nc8pr))

col4<-cws[which(cws$group10==2),]

#-- Tribal water systems-#
nTribal<-nrow(col4[which(col4$Tribal==1),])
nTribalpr<-round(100*(nTribal/n),1)
npTribal<-to.n.prop(c(nTribal,nTribalpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col4$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

cat3<-rbind(n,mch,npgw,npsw,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8,npTribal,state1,state2,state3)
cat3 <- rbind(cat3, Cateogry= c("Category 3, Low/High"))

### Category 2/Column 3 (High/Low)
col3<-cws[which(cws$group10==3),]
# Mean arsenic change
col3$change<-col3$ThreeYearAs20062008-col3$ThreeYearAs20092011
fit1 <- glm(change ~ +1, family=gaussian(link="identity"), data=col3)
summary(fit1)
mch<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                           summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                           summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

#-- Source Water Type
n<-nrow(col3)
ngw<-nrow(col3[which(col3$Source.Water.Type=="GW"|col3$Source.Water.Type=="GU"),])
ngwpr<-round(100*(ngw/n),1)
npgw<-to.n.prop(c(ngw,ngwpr))
nsw<-nrow(col3[which(col3$Source.Water.Type=="SW"),])
nswpr<-round(100*(nsw/n),1)
npsw<-to.n.prop(c(nsw,nswpr))

#-- Population Served
fit <- glm(Adjusted.Total.Population.Served~1,
           family=gaussian(link="identity"), data=col3)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col3[which(col3$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col3[which(col3$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col3[which(col3$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col3[which(col3$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col3[which(col3$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col3[which(col3$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col3[which(col3$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col3[which(col3$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- By SCC-#
col3<-finaldatadups3[which(finaldatadups3$group10==3),]
nscc<-nrow(col3)

nc1<-nrow(col3[which(col3$cluster.number==1),])
nc1pr<-round(100*(nc1/nscc),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col3[which(col3$cluster.number==2),])
nc2pr<-round(100*(nc2/nscc),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col3[which(col3$cluster.number==3),])
nc3pr<-round(100*(nc3/nscc),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col3[which(col3$cluster.number==4),])
nc4pr<-round(100*(nc4/nscc),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col3[which(col3$cluster.number==5),])
nc5pr<-round(100*(nc5/nscc),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col3[which(col3$cluster.number==6),])
nc6pr<-round(100*(nc6/nscc),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col3[which(col3$cluster.number==7),])
nc7pr<-round(100*(nc7/nscc),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col3[which(col3$cluster.number==8),])
nc8pr<-round(100*(nc8/nscc),1)
npc8<-to.n.prop(c(nc8,nc8pr))

col3<-cws[which(cws$group10==3),]

#-- Tribal water systems-#
nTribal<-nrow(col3[which(col3$Tribal==1),])
nTribalpr<-round(100*(nTribal/n),1)
npTribal<-to.n.prop(c(nTribal,nTribalpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col3$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

cat2<-rbind(n,mch,npgw,npsw,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8,npTribal,state1,state2,state3)
cat2 <- rbind(cat2, Cateogry= c("Category 2, High/Low"))

### Category 4/Column 5 (High/High)
col5<-cws[which(cws$group10==4),]
# Mean arsenic change
col5$change<-col5$ThreeYearAs20062008-col5$ThreeYearAs20092011
fit1 <- glm(change ~ +1, family=gaussian(link="identity"), data=col5)
summary(fit1)
mch<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                           summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                           summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

#-- Source Water Type
n<-nrow(col5)
ngw<-nrow(col5[which(col5$Source.Water.Type=="GW"|col5$Source.Water.Type=="GU"),])
ngwpr<-round(100*(ngw/n),1)
npgw<-to.n.prop(c(ngw,ngwpr))
nsw<-nrow(col5[which(col5$Source.Water.Type=="SW"),])
nswpr<-round(100*(nsw/n),1)
npsw<-to.n.prop(c(nsw,nswpr))

#-- Population Served
fit <- glm(Adjusted.Total.Population.Served~1,
           family=gaussian(link="identity"), data=col5)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col5[which(col5$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col5[which(col5$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col5[which(col5$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col5[which(col5$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col5[which(col5$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col5[which(col5$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col5[which(col5$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col5[which(col5$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- By SCC -#
col5<-finaldatadups3[which(finaldatadups3$group10==4),]
nscc<-nrow(col5)

nc1<-nrow(col5[which(col5$cluster.number==1),])
nc1pr<-round(100*(nc1/nscc),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col5[which(col5$cluster.number==2),])
nc2pr<-round(100*(nc2/nscc),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col5[which(col5$cluster.number==3),])
nc3pr<-round(100*(nc3/nscc),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col5[which(col5$cluster.number==4),])
nc4pr<-round(100*(nc4/nscc),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col5[which(col5$cluster.number==5),])
nc5pr<-round(100*(nc5/nscc),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col5[which(col5$cluster.number==6),])
nc6pr<-round(100*(nc6/nscc),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col5[which(col5$cluster.number==7),])
nc7pr<-round(100*(nc7/nscc),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col5[which(col5$cluster.number==8),])
nc8pr<-round(100*(nc8/nscc),1)
npc8<-to.n.prop(c(nc8,nc8pr))


col5<-cws[which(cws$group10==4),]

#-- Tribal water systems-#
nTribal<-nrow(col5[which(col5$Tribal==1),])
nTribalpr<-round(100*(nTribal/n),1)
npTribal<-to.n.prop(c(nTribal,nTribalpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col5$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

cat4<-rbind(n,mch,npgw,npsw,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8,npTribal,state1,state2,state3)
cat4 <- rbind(cat4, Cateogry= c("Category 4, High/High"))

ExcelTable4A<-cbind(cwsall,cat1,cat2,cat3,cat4)
write.xlsx(ExcelTable4A,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/ExcelTable4A.xlsx")

### end 
#######       2.B 5 ug/L cut point #######
### All CWSs
col1<-cws[which(cws$group5>0),]
# Mean arsenic change
col1$change<-col1$ThreeYearAs20062008-col1$ThreeYearAs20092011
fit1 <- glm(change ~ +1, family=gaussian(link="identity"), data=col1)
summary(fit1)
mch<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                           summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                           summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

#-- Source Water Type
n<-nrow(col1)
ngw<-nrow(col1[which(col1$Source.Water.Type=="GW"|col1$Source.Water.Type=="GU"),])
ngwpr<-round(100*(ngw/n),1)
npgw<-to.n.prop(c(ngw,ngwpr))
nsw<-nrow(col1[which(col1$Source.Water.Type=="SW"),])
nswpr<-round(100*(nsw/n),1)
npsw<-to.n.prop(c(nsw,nswpr))

#-- Population Served
fit <- glm(Adjusted.Total.Population.Served~1,
           family=gaussian(link="identity"), data=col1)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col1[which(col1$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col1[which(col1$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col1[which(col1$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col1[which(col1$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col1[which(col1$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col1[which(col1$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col1[which(col1$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col1[which(col1$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- Tribal water systems-#
nTribal<-nrow(col1[which(col1$Tribal==1),])
nTribalpr<-round(100*(nTribal/n),1)
npTribal<-to.n.prop(c(nTribal,nTribalpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col1$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))


#-- By sociodemographic county cluster -#
### Row group 3: by sociodemographic cluster group:
## For this group we use: finaldatadups3
# First we need to assign each CWS to a county-cluster; lets pull thoe CWSs from _data_ which serve multiple counties
# and see if these are the same counties:
describe(finaldatadups3$PWSID) ##one record for each CWS serving each county, 3-yr averages 
describe(finaldatadups3$CountyFIPS)

# fix arsenic var names
colnames(finaldatadups3)[11] <- "ThreeYearAs20062008"
colnames(finaldatadups3)[12] <- "ThreeYearAs20092011"
library(tidyr)

col1<-finaldatadups3[which(finaldatadups3$group5>0),]
nscc<-nrow(col1)

nc1<-nrow(col1[which(col1$cluster.number==1),])
nc1pr<-round(100*(nc1/nscc),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col1[which(col1$cluster.number==2),])
nc2pr<-round(100*(nc2/nscc),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col1[which(col1$cluster.number==3),])
nc3pr<-round(100*(nc3/nscc),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col1[which(col1$cluster.number==4),])
nc4pr<-round(100*(nc4/nscc),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col1[which(col1$cluster.number==5),])
nc5pr<-round(100*(nc5/nscc),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col1[which(col1$cluster.number==6),])
nc6pr<-round(100*(nc6/nscc),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col1[which(col1$cluster.number==7),])
nc7pr<-round(100*(nc7/nscc),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col1[which(col1$cluster.number==8),])
nc8pr<-round(100*(nc8/nscc),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cwsall<-rbind(n,mch,npgw,npsw,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8,npTribal,state1,state2,state3)
cwsall <- rbind(cwsall, Cateogry= c("All CWSs"))

### Cateogry 1 (low/low)
col2<-cws[which(cws$group5==1),]
# Mean arsenic change
col2$change<-col2$ThreeYearAs20062008-col2$ThreeYearAs20092011
fit1 <- glm(change ~ +1, family=gaussian(link="identity"), data=col2)
summary(fit1)
mch<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                           summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                           summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

#-- Source Water Type
n<-nrow(col2)
ngw<-nrow(col2[which(col2$Source.Water.Type=="GW"|col2$Source.Water.Type=="GU"),])
ngwpr<-round(100*(ngw/n),1)
npgw<-to.n.prop(c(ngw,ngwpr))
nsw<-nrow(col2[which(col2$Source.Water.Type=="SW"),])
nswpr<-round(100*(nsw/n),1)
npsw<-to.n.prop(c(nsw,nswpr))

#-- Population Served
fit <- glm(Adjusted.Total.Population.Served~1,
           family=gaussian(link="identity"), data=col2)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col2[which(col2$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col2[which(col2$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col2[which(col2$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col2[which(col2$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col2[which(col2$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col2[which(col2$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col2[which(col2$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col2[which(col2$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

##-- By SCC -#
col2<-finaldatadups3[which(finaldatadups3$group5==1),]
nscc<-nrow(col2)

nc1<-nrow(col2[which(col2$cluster.number==1),])
nc1pr<-round(100*(nc1/nscc),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col2[which(col2$cluster.number==2),])
nc2pr<-round(100*(nc2/nscc),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col2[which(col2$cluster.number==3),])
nc3pr<-round(100*(nc3/nscc),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col2[which(col2$cluster.number==4),])
nc4pr<-round(100*(nc4/nscc),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col2[which(col2$cluster.number==5),])
nc5pr<-round(100*(nc5/nscc),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col2[which(col2$cluster.number==6),])
nc6pr<-round(100*(nc6/nscc),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col2[which(col2$cluster.number==7),])
nc7pr<-round(100*(nc7/nscc),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col2[which(col2$cluster.number==8),])
nc8pr<-round(100*(nc8/nscc),1)
npc8<-to.n.prop(c(nc8,nc8pr))

col2<-cws[which(cws$group5==1),]

#-- Tribal water systems-#
nTribal<-nrow(col2[which(col2$Tribal==1),])
nTribalpr<-round(100*(nTribal/n),1)
npTribal<-to.n.prop(c(nTribal,nTribalpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col2$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

cat1<-rbind(n,mch,npgw,npsw,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8,npTribal,state1,state2,state3)
cat1 <- rbind(cat1, Cateogry= c("Category 1, Low/Low"))

### Category 3 /Column4 Low/High
col4<-cws[which(cws$group5==2),]
# Mean arsenic change
col4$change<-col4$ThreeYearAs20062008-col4$ThreeYearAs20092011
fit1 <- glm(change ~ +1, family=gaussian(link="identity"), data=col4)
summary(fit1)
mch<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                           summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                           summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

#-- Source Water Type
n<-nrow(col4)
ngw<-nrow(col4[which(col4$Source.Water.Type=="GW"|col4$Source.Water.Type=="GU"),])
ngwpr<-round(100*(ngw/n),1)
npgw<-to.n.prop(c(ngw,ngwpr))
nsw<-nrow(col4[which(col4$Source.Water.Type=="SW"),])
nswpr<-round(100*(nsw/n),1)
npsw<-to.n.prop(c(nsw,nswpr))

#-- Population Served
fit <- glm(Adjusted.Total.Population.Served~1,
           family=gaussian(link="identity"), data=col4)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col4[which(col4$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col4[which(col4$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col4[which(col4$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col4[which(col4$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col4[which(col4$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col4[which(col4$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col4[which(col4$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col4[which(col4$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- By SCC -#
col4<-finaldatadups3[which(finaldatadups3$group5==2),]
nscc<-nrow(col4)

nc1<-nrow(col4[which(col4$cluster.number==1),])
nc1pr<-round(100*(nc1/nscc),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col4[which(col4$cluster.number==2),])
nc2pr<-round(100*(nc2/nscc),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col4[which(col4$cluster.number==3),])
nc3pr<-round(100*(nc3/nscc),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col4[which(col4$cluster.number==4),])
nc4pr<-round(100*(nc4/nscc),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col4[which(col4$cluster.number==5),])
nc5pr<-round(100*(nc5/nscc),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col4[which(col4$cluster.number==6),])
nc6pr<-round(100*(nc6/nscc),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col4[which(col4$cluster.number==7),])
nc7pr<-round(100*(nc7/nscc),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col4[which(col4$cluster.number==8),])
nc8pr<-round(100*(nc8/nscc),1)
npc8<-to.n.prop(c(nc8,nc8pr))

col4<-cws[which(cws$group5==2),]

#-- Tribal water systems-#
nTribal<-nrow(col4[which(col4$Tribal==1),])
nTribalpr<-round(100*(nTribal/n),1)
npTribal<-to.n.prop(c(nTribal,nTribalpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col4$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

cat3<-rbind(n,mch,npgw,npsw,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8,npTribal,state1,state2,state3)
cat3 <- rbind(cat3, Cateogry= c("Category 3, Low/High"))

### Category 2/Column 3 (High/Low)
col3<-cws[which(cws$group5==3),]
# Mean arsenic change
col3$change<-col3$ThreeYearAs20062008-col3$ThreeYearAs20092011
fit1 <- glm(change ~ +1, family=gaussian(link="identity"), data=col3)
summary(fit1)
mch<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                           summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                           summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

#-- Source Water Type
n<-nrow(col3)
ngw<-nrow(col3[which(col3$Source.Water.Type=="GW"|col3$Source.Water.Type=="GU"),])
ngwpr<-round(100*(ngw/n),1)
npgw<-to.n.prop(c(ngw,ngwpr))
nsw<-nrow(col3[which(col3$Source.Water.Type=="SW"),])
nswpr<-round(100*(nsw/n),1)
npsw<-to.n.prop(c(nsw,nswpr))

#-- Population Served
fit <- glm(Adjusted.Total.Population.Served~1,
           family=gaussian(link="identity"), data=col3)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col3[which(col3$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col3[which(col3$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col3[which(col3$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col3[which(col3$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col3[which(col3$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col3[which(col3$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col3[which(col3$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col3[which(col3$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- By SCC-#
col3<-finaldatadups3[which(finaldatadups3$group5==3),]
nscc<-nrow(col3)

nc1<-nrow(col3[which(col3$cluster.number==1),])
nc1pr<-round(100*(nc1/nscc),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col3[which(col3$cluster.number==2),])
nc2pr<-round(100*(nc2/nscc),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col3[which(col3$cluster.number==3),])
nc3pr<-round(100*(nc3/nscc),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col3[which(col3$cluster.number==4),])
nc4pr<-round(100*(nc4/nscc),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col3[which(col3$cluster.number==5),])
nc5pr<-round(100*(nc5/nscc),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col3[which(col3$cluster.number==6),])
nc6pr<-round(100*(nc6/nscc),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col3[which(col3$cluster.number==7),])
nc7pr<-round(100*(nc7/nscc),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col3[which(col3$cluster.number==8),])
nc8pr<-round(100*(nc8/nscc),1)
npc8<-to.n.prop(c(nc8,nc8pr))

col3<-cws[which(cws$group5==3),]

#-- Tribal water systems-#
nTribal<-nrow(col3[which(col3$Tribal==1),])
nTribalpr<-round(100*(nTribal/n),1)
npTribal<-to.n.prop(c(nTribal,nTribalpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col3$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

cat2<-rbind(n,mch,npgw,npsw,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8,npTribal,state1,state2,state3)
cat2 <- rbind(cat2, Cateogry= c("Category 2, High/Low"))

### Category 4/Column 5 (High/High)
col5<-cws[which(cws$group5==4),]
# Mean arsenic change
col5$change<-col5$ThreeYearAs20062008-col5$ThreeYearAs20092011
fit1 <- glm(change ~ +1, family=gaussian(link="identity"), data=col5)
summary(fit1)
mch<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                           summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                           summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

#-- Source Water Type
n<-nrow(col5)
ngw<-nrow(col5[which(col5$Source.Water.Type=="GW"|col5$Source.Water.Type=="GU"),])
ngwpr<-round(100*(ngw/n),1)
npgw<-to.n.prop(c(ngw,ngwpr))
nsw<-nrow(col5[which(col5$Source.Water.Type=="SW"),])
nswpr<-round(100*(nsw/n),1)
npsw<-to.n.prop(c(nsw,nswpr))

#-- Population Served
fit <- glm(Adjusted.Total.Population.Served~1,
           family=gaussian(link="identity"), data=col5)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col5[which(col5$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col5[which(col5$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col5[which(col5$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col5[which(col5$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col5[which(col5$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col5[which(col5$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col5[which(col5$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col5[which(col5$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- By SCC -#
col5<-finaldatadups3[which(finaldatadups3$group5==4),]
nscc<-nrow(col5)

nc1<-nrow(col5[which(col5$cluster.number==1),])
nc1pr<-round(100*(nc1/nscc),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col5[which(col5$cluster.number==2),])
nc2pr<-round(100*(nc2/nscc),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col5[which(col5$cluster.number==3),])
nc3pr<-round(100*(nc3/nscc),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col5[which(col5$cluster.number==4),])
nc4pr<-round(100*(nc4/nscc),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col5[which(col5$cluster.number==5),])
nc5pr<-round(100*(nc5/nscc),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col5[which(col5$cluster.number==6),])
nc6pr<-round(100*(nc6/nscc),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col5[which(col5$cluster.number==7),])
nc7pr<-round(100*(nc7/nscc),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col5[which(col5$cluster.number==8),])
nc8pr<-round(100*(nc8/nscc),1)
npc8<-to.n.prop(c(nc8,nc8pr))


col5<-cws[which(cws$group5==4),]

#-- Tribal water systems-#
nTribal<-nrow(col5[which(col5$Tribal==1),])
nTribalpr<-round(100*(nTribal/n),1)
npTribal<-to.n.prop(c(nTribal,nTribalpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col5$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

cat4<-rbind(n,mch,npgw,npsw,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8,npTribal,state1,state2,state3)
cat4 <- rbind(cat4, Cateogry= c("Category 4, High/High"))


ExcelTable4B<-cbind(cwsall,cat1,cat2,cat3,cat4)
write.xlsx(ExcelTable4B,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/ExcelTable4B.xlsx")

### end

#######       2.C 1 ug/L cut-point #########
### All CWSs
col1<-cws[which(cws$group1>0),]
# Mean arsenic change
col1$change<-col1$ThreeYearAs20062008-col1$ThreeYearAs20092011
fit1 <- glm(change ~ +1, family=gaussian(link="identity"), data=col1)
summary(fit1)
mch<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                           summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                           summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

#-- Source Water Type
n<-nrow(col1)
ngw<-nrow(col1[which(col1$Source.Water.Type=="GW"|col1$Source.Water.Type=="GU"),])
ngwpr<-round(100*(ngw/n),1)
npgw<-to.n.prop(c(ngw,ngwpr))
nsw<-nrow(col1[which(col1$Source.Water.Type=="SW"),])
nswpr<-round(100*(nsw/n),1)
npsw<-to.n.prop(c(nsw,nswpr))

#-- Population Served
fit <- glm(Adjusted.Total.Population.Served~1,
           family=gaussian(link="identity"), data=col1)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col1[which(col1$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col1[which(col1$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col1[which(col1$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col1[which(col1$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col1[which(col1$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col1[which(col1$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col1[which(col1$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col1[which(col1$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- Tribal water systems-#
nTribal<-nrow(col1[which(col1$Tribal==1),])
nTribalpr<-round(100*(nTribal/n),1)
npTribal<-to.n.prop(c(nTribal,nTribalpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col1$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))


#-- By sociodemographic county cluster -#
### Row group 3: by sociodemographic cluster group:
## For this group we use: finaldatadups3
# First we need to assign each CWS to a county-cluster; lets pull thoe CWSs from _data_ which serve multiple counties
# and see if these are the same counties:
describe(finaldatadups3$PWSID) ##one record for each CWS serving each county, 3-yr averages 
describe(finaldatadups3$CountyFIPS)

# fix arsenic var names
colnames(finaldatadups3)[11] <- "ThreeYearAs20062008"
colnames(finaldatadups3)[12] <- "ThreeYearAs20092011"
library(tidyr)

col1<-finaldatadups3[which(finaldatadups3$group1>0),]
nscc<-nrow(col1)

nc1<-nrow(col1[which(col1$cluster.number==1),])
nc1pr<-round(100*(nc1/nscc),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col1[which(col1$cluster.number==2),])
nc2pr<-round(100*(nc2/nscc),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col1[which(col1$cluster.number==3),])
nc3pr<-round(100*(nc3/nscc),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col1[which(col1$cluster.number==4),])
nc4pr<-round(100*(nc4/nscc),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col1[which(col1$cluster.number==5),])
nc5pr<-round(100*(nc5/nscc),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col1[which(col1$cluster.number==6),])
nc6pr<-round(100*(nc6/nscc),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col1[which(col1$cluster.number==7),])
nc7pr<-round(100*(nc7/nscc),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col1[which(col1$cluster.number==8),])
nc8pr<-round(100*(nc8/nscc),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cwsall<-rbind(n,mch,npgw,npsw,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8,npTribal,state1,state2,state3)
cwsall <- rbind(cwsall, Cateogry= c("All CWSs"))

### Cateogry 1 (low/low)
col2<-cws[which(cws$group1==1),]
# Mean arsenic change
col2$change<-col2$ThreeYearAs20062008-col2$ThreeYearAs20092011
fit1 <- glm(change ~ +1, family=gaussian(link="identity"), data=col2)
summary(fit1)
mch<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                           summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                           summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

#-- Source Water Type
n<-nrow(col2)
ngw<-nrow(col2[which(col2$Source.Water.Type=="GW"|col2$Source.Water.Type=="GU"),])
ngwpr<-round(100*(ngw/n),1)
npgw<-to.n.prop(c(ngw,ngwpr))
nsw<-nrow(col2[which(col2$Source.Water.Type=="SW"),])
nswpr<-round(100*(nsw/n),1)
npsw<-to.n.prop(c(nsw,nswpr))

#-- Population Served
fit <- glm(Adjusted.Total.Population.Served~1,
           family=gaussian(link="identity"), data=col2)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col2[which(col2$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col2[which(col2$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col2[which(col2$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col2[which(col2$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col2[which(col2$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col2[which(col2$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col2[which(col2$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col2[which(col2$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

##-- By SCC -#
col2<-finaldatadups3[which(finaldatadups3$group1==1),]
nscc<-nrow(col2)

nc1<-nrow(col2[which(col2$cluster.number==1),])
nc1pr<-round(100*(nc1/nscc),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col2[which(col2$cluster.number==2),])
nc2pr<-round(100*(nc2/nscc),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col2[which(col2$cluster.number==3),])
nc3pr<-round(100*(nc3/nscc),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col2[which(col2$cluster.number==4),])
nc4pr<-round(100*(nc4/nscc),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col2[which(col2$cluster.number==5),])
nc5pr<-round(100*(nc5/nscc),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col2[which(col2$cluster.number==6),])
nc6pr<-round(100*(nc6/nscc),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col2[which(col2$cluster.number==7),])
nc7pr<-round(100*(nc7/nscc),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col2[which(col2$cluster.number==8),])
nc8pr<-round(100*(nc8/nscc),1)
npc8<-to.n.prop(c(nc8,nc8pr))

col2<-cws[which(cws$group1==1),]

#-- Tribal water systems-#
nTribal<-nrow(col2[which(col2$Tribal==1),])
nTribalpr<-round(100*(nTribal/n),1)
npTribal<-to.n.prop(c(nTribal,nTribalpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col2$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

cat1<-rbind(n,mch,npgw,npsw,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8,npTribal,state1,state2,state3)
cat1 <- rbind(cat1, Cateogry= c("Category 1, Low/Low"))

### Category 3 /Column4 Low/High
col4<-cws[which(cws$group1==2),]
# Mean arsenic change
col4$change<-col4$ThreeYearAs20062008-col4$ThreeYearAs20092011
fit1 <- glm(change ~ +1, family=gaussian(link="identity"), data=col4)
summary(fit1)
mch<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                           summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                           summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

#-- Source Water Type
n<-nrow(col4)
ngw<-nrow(col4[which(col4$Source.Water.Type=="GW"|col4$Source.Water.Type=="GU"),])
ngwpr<-round(100*(ngw/n),1)
npgw<-to.n.prop(c(ngw,ngwpr))
nsw<-nrow(col4[which(col4$Source.Water.Type=="SW"),])
nswpr<-round(100*(nsw/n),1)
npsw<-to.n.prop(c(nsw,nswpr))

#-- Population Served
fit <- glm(Adjusted.Total.Population.Served~1,
           family=gaussian(link="identity"), data=col4)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col4[which(col4$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col4[which(col4$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col4[which(col4$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col4[which(col4$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col4[which(col4$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col4[which(col4$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col4[which(col4$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col4[which(col4$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- By SCC -#
col4<-finaldatadups3[which(finaldatadups3$group1==2),]
nscc<-nrow(col4)

nc1<-nrow(col4[which(col4$cluster.number==1),])
nc1pr<-round(100*(nc1/nscc),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col4[which(col4$cluster.number==2),])
nc2pr<-round(100*(nc2/nscc),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col4[which(col4$cluster.number==3),])
nc3pr<-round(100*(nc3/nscc),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col4[which(col4$cluster.number==4),])
nc4pr<-round(100*(nc4/nscc),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col4[which(col4$cluster.number==5),])
nc5pr<-round(100*(nc5/nscc),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col4[which(col4$cluster.number==6),])
nc6pr<-round(100*(nc6/nscc),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col4[which(col4$cluster.number==7),])
nc7pr<-round(100*(nc7/nscc),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col4[which(col4$cluster.number==8),])
nc8pr<-round(100*(nc8/nscc),1)
npc8<-to.n.prop(c(nc8,nc8pr))

col4<-cws[which(cws$group1==2),]

#-- Tribal water systems-#
nTribal<-nrow(col4[which(col4$Tribal==1),])
nTribalpr<-round(100*(nTribal/n),1)
npTribal<-to.n.prop(c(nTribal,nTribalpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col4$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

cat3<-rbind(n,mch,npgw,npsw,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8,npTribal,state1,state2,state3)
cat3 <- rbind(cat3, Cateogry= c("Category 3, Low/High"))

### Category 2/Column 3 (High/Low)
col3<-cws[which(cws$group1==3),]
# Mean arsenic change
col3$change<-col3$ThreeYearAs20062008-col3$ThreeYearAs20092011
fit1 <- glm(change ~ +1, family=gaussian(link="identity"), data=col3)
summary(fit1)
mch<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                           summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                           summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

#-- Source Water Type
n<-nrow(col3)
ngw<-nrow(col3[which(col3$Source.Water.Type=="GW"|col3$Source.Water.Type=="GU"),])
ngwpr<-round(100*(ngw/n),1)
npgw<-to.n.prop(c(ngw,ngwpr))
nsw<-nrow(col3[which(col3$Source.Water.Type=="SW"),])
nswpr<-round(100*(nsw/n),1)
npsw<-to.n.prop(c(nsw,nswpr))

#-- Population Served
fit <- glm(Adjusted.Total.Population.Served~1,
           family=gaussian(link="identity"), data=col3)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col3[which(col3$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col3[which(col3$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col3[which(col3$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col3[which(col3$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col3[which(col3$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col3[which(col3$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col3[which(col3$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col3[which(col3$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- By SCC-#
col3<-finaldatadups3[which(finaldatadups3$group1==3),]
nscc<-nrow(col3)

nc1<-nrow(col3[which(col3$cluster.number==1),])
nc1pr<-round(100*(nc1/nscc),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col3[which(col3$cluster.number==2),])
nc2pr<-round(100*(nc2/nscc),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col3[which(col3$cluster.number==3),])
nc3pr<-round(100*(nc3/nscc),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col3[which(col3$cluster.number==4),])
nc4pr<-round(100*(nc4/nscc),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col3[which(col3$cluster.number==5),])
nc5pr<-round(100*(nc5/nscc),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col3[which(col3$cluster.number==6),])
nc6pr<-round(100*(nc6/nscc),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col3[which(col3$cluster.number==7),])
nc7pr<-round(100*(nc7/nscc),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col3[which(col3$cluster.number==8),])
nc8pr<-round(100*(nc8/nscc),1)
npc8<-to.n.prop(c(nc8,nc8pr))

col3<-cws[which(cws$group1==3),]

#-- Tribal water systems-#
nTribal<-nrow(col3[which(col3$Tribal==1),])
nTribalpr<-round(100*(nTribal/n),1)
npTribal<-to.n.prop(c(nTribal,nTribalpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col3$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

cat2<-rbind(n,mch,npgw,npsw,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8,npTribal,state1,state2,state3)
cat2 <- rbind(cat2, Cateogry= c("Category 2, High/Low"))

### Category 4/Column 5 (High/High)
col5<-cws[which(cws$group1==4),]
# Mean arsenic change
col5$change<-col5$ThreeYearAs20062008-col5$ThreeYearAs20092011
fit1 <- glm(change ~ +1, family=gaussian(link="identity"), data=col5)
summary(fit1)
mch<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                           summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                           summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

#-- Source Water Type
n<-nrow(col5)
ngw<-nrow(col5[which(col5$Source.Water.Type=="GW"|col5$Source.Water.Type=="GU"),])
ngwpr<-round(100*(ngw/n),1)
npgw<-to.n.prop(c(ngw,ngwpr))
nsw<-nrow(col5[which(col5$Source.Water.Type=="SW"),])
nswpr<-round(100*(nsw/n),1)
npsw<-to.n.prop(c(nsw,nswpr))

#-- Population Served
fit <- glm(Adjusted.Total.Population.Served~1,
           family=gaussian(link="identity"), data=col5)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col5[which(col5$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col5[which(col5$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col5[which(col5$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col5[which(col5$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col5[which(col5$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col5[which(col5$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col5[which(col5$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col5[which(col5$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- By SCC -#
col5<-finaldatadups3[which(finaldatadups3$group1==4),]
nscc<-nrow(col5)

nc1<-nrow(col5[which(col5$cluster.number==1),])
nc1pr<-round(100*(nc1/nscc),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col5[which(col5$cluster.number==2),])
nc2pr<-round(100*(nc2/nscc),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col5[which(col5$cluster.number==3),])
nc3pr<-round(100*(nc3/nscc),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col5[which(col5$cluster.number==4),])
nc4pr<-round(100*(nc4/nscc),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col5[which(col5$cluster.number==5),])
nc5pr<-round(100*(nc5/nscc),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col5[which(col5$cluster.number==6),])
nc6pr<-round(100*(nc6/nscc),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col5[which(col5$cluster.number==7),])
nc7pr<-round(100*(nc7/nscc),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col5[which(col5$cluster.number==8),])
nc8pr<-round(100*(nc8/nscc),1)
npc8<-to.n.prop(c(nc8,nc8pr))


col5<-cws[which(cws$group1==4),]

#-- Tribal water systems-#
nTribal<-nrow(col5[which(col5$Tribal==1),])
nTribalpr<-round(100*(nTribal/n),1)
npTribal<-to.n.prop(c(nTribal,nTribalpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col5$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

cat4<-rbind(n,mch,npgw,npsw,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8,npTribal,state1,state2,state3)
cat4 <- rbind(cat4, Cateogry= c("Category 4, High/High"))

ExcelTable4C<-cbind(cwsall,cat1,cat2,cat3,cat4)
write.xlsx(ExcelTable4C,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/ExcelTable4C.xlsx")

###end





####### 3. Table S1. & Figure S2 Description of yearly averages for each CWS  ########
# For PWS level, final data is in finaldata
# for county, notlowcounties50subset (or, s50)
## Using finaldata, which has one row for each PWSID

## Supplemental Figure 2
# missingness pattern
library(mice)
myvars<-c("Y2006","Y2007","Y2008","Y2009","Y2010","Y2011")
mydata<-finaldata[myvars]
colnames(mydata)[colnames(mydata)=="Y2006"] <- "Year 2006"
colnames(mydata)[colnames(mydata)=="Y2007"] <- "Year 2007"
colnames(mydata)[colnames(mydata)=="Y2008"] <- "Year 2008"
colnames(mydata)[colnames(mydata)=="Y2009"] <- "Year 2009"
colnames(mydata)[colnames(mydata)=="Y2010"] <- "Year 2010"
colnames(mydata)[colnames(mydata)=="Y2011"] <- "Year 2011"

md.pattern(mydata,plot=TRUE, rotate.names=TRUE)
library(VIM)

folder.output <- "~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output"
if( !file.exists( folder.output ) ) {
  dir.create( file.path( folder.output ) )
}
pdf(file=paste0( folder.output , "/Figure S2.pdf"), 30, 20)
aggr(mydata, numbers = TRUE, prop = c(TRUE, FALSE),cex.lab= 2,cex.axis=2, cex.numbers=2,oma = c(12,10,10,10))
dev.off()


## Using finaldata, which has one row for each PWSID
finaldata$Y2006[which(finaldata$Y2006==".")]<-NA
finaldata$Y2007[which(finaldata$Y2007==".")]<-NA
finaldata$Y2008[which(finaldata$Y2008==".")]<-NA
finaldata$Y2009[which(finaldata$Y2009==".")]<-NA
finaldata$Y2010[which(finaldata$Y2010==".")]<-NA
finaldata$Y2011[which(finaldata$Y2011==".")]<-NA
finaldata$Y2006<-as.numeric(finaldata$Y2006)
finaldata$Y2007<-as.numeric(finaldata$Y2007)
finaldata$Y2008<-as.numeric(finaldata$Y2008)
finaldata$Y2009<-as.numeric(finaldata$Y2009)
finaldata$Y2010<-as.numeric(finaldata$Y2010)
finaldata$Y2011<-as.numeric(finaldata$Y2011)

describe(finaldata$PWSID)
## Supplemental Table 1A ##
# Distributions by year
# 2006
nav<-nrow(finaldata[which(!is.na(finaldata$Y2006)),])
nmiss<-nrow(finaldata[which(is.na(finaldata$Y2006)),])
quant<-as.data.frame(quantile(finaldata$Y2006,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(finaldata[which(finaldata$Y2006<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(finaldata[which(finaldata$Y2006>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(finaldata$Y2006),na.rm=T))
mean<-mean(finaldata$Y2006,na.rm=T)
colnames(quant)[1] <- "2006"
Y2006<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Y2006)[1] <- "N";rownames(Y2006)[2] <- "N missing";rownames(Y2006)[12] <- "% below MRL"
rownames(Y2006)[13] <- "% above MCL";rownames(Y2006)[14] <- "Geometric mean";rownames(Y2006)[15] <- "Arithmetic mean"
# 2007
nav<-nrow(finaldata[which(!is.na(finaldata$Y2007)),])
nmiss<-nrow(finaldata[which(is.na(finaldata$Y2007)),])
quant<-as.data.frame(quantile(finaldata$Y2007,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(finaldata[which(finaldata$Y2007<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(finaldata[which(finaldata$Y2007>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(finaldata$Y2007),na.rm=T))
mean<-mean(finaldata$Y2007,na.rm=T)
colnames(quant)[1] <- "2007"
Y2007<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Y2007)[1] <- "N";rownames(Y2007)[2] <- "N missing";rownames(Y2007)[12] <- "% below MRL"
rownames(Y2007)[13] <- "% above MCL";rownames(Y2007)[14] <- "Geometric mean";rownames(Y2007)[15] <- "Arithmetic mean"
# 2008
nav<-nrow(finaldata[which(!is.na(finaldata$Y2008)),])
nmiss<-nrow(finaldata[which(is.na(finaldata$Y2008)),])
quant<-as.data.frame(quantile(finaldata$Y2008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(finaldata[which(finaldata$Y2008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(finaldata[which(finaldata$Y2008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(finaldata$Y2008),na.rm=T))
mean<-mean(finaldata$Y2008,na.rm=T)
colnames(quant)[1] <- "2008"
Y2008<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Y2008)[1] <- "N";rownames(Y2008)[2] <- "N missing";rownames(Y2008)[12] <- "% below MRL"
rownames(Y2008)[13] <- "% above MCL";rownames(Y2008)[14] <- "Geometric mean";rownames(Y2008)[15] <- "Arithmetic mean"
# 2009
nav<-nrow(finaldata[which(!is.na(finaldata$Y2009)),])
nmiss<-nrow(finaldata[which(is.na(finaldata$Y2009)),])
quant<-as.data.frame(quantile(finaldata$Y2009,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(finaldata[which(finaldata$Y2009<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(finaldata[which(finaldata$Y2009>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(finaldata$Y2009),na.rm=T))
mean<-mean(finaldata$Y2009,na.rm=T)
colnames(quant)[1] <- "2009"
Y2009<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Y2009)[1] <- "N";rownames(Y2009)[2] <- "N missing";rownames(Y2009)[12] <- "% below MRL"
rownames(Y2009)[13] <- "% above MCL";rownames(Y2009)[14] <- "Geometric mean";rownames(Y2009)[15] <- "Arithmetic mean"
# 2010
nav<-nrow(finaldata[which(!is.na(finaldata$Y2010)),])
nmiss<-nrow(finaldata[which(is.na(finaldata$Y2010)),])
quant<-as.data.frame(quantile(finaldata$Y2010,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(finaldata[which(finaldata$Y2010<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(finaldata[which(finaldata$Y2010>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(finaldata$Y2010),na.rm=T))
mean<-mean(finaldata$Y2010,na.rm=T)
colnames(quant)[1] <- "2010"
Y2010<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Y2010)[1] <- "N";rownames(Y2010)[2] <- "N missing";rownames(Y2010)[12] <- "% below MRL"
rownames(Y2010)[13] <- "% above MCL";rownames(Y2010)[14] <- "Geometric mean";rownames(Y2010)[15] <- "Arithmetic mean"
# 2011
nav<-nrow(finaldata[which(!is.na(finaldata$Y2011)),])
nmiss<-nrow(finaldata[which(is.na(finaldata$Y2011)),])
quant<-as.data.frame(quantile(finaldata$Y2011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(finaldata[which(finaldata$Y2011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(finaldata[which(finaldata$Y2011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(finaldata$Y2011),na.rm=T))
mean<-mean(finaldata$Y2011,na.rm=T)
colnames(quant)[1] <- "2011"
Y2011<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Y2011)[1] <- "N";rownames(Y2011)[2] <- "N missing";rownames(Y2011)[12] <- "% below MRL"
rownames(Y2011)[13] <- "% above MCL";rownames(Y2011)[14] <- "Geometric mean";rownames(Y2011)[15] <- "Arithmetic mean"


# Mean difference models
# new dataset in long format
library(tidyr)
is.factor(finaldata$PWSID)
finaldata$PWSID<-as.factor(finaldata$PWSID)
finallong <- gather(finaldata, year, arsenic, Y2006:Y2011, factor_key=TRUE)
finallong
md2006<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=finallong)
summary(fit1)
md2007<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                              summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                              summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# 
md2008<-convert.to.ci(round(c(summary(fit1)$coeff[3,1],
                              summary(fit1)$coeff[3,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[3,2],
                              summary(fit1)$coeff[3,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[3,2]),2))
# 
md2009<-convert.to.ci(round(c(summary(fit1)$coeff[4,1],
                              summary(fit1)$coeff[4,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[4,2],
                              summary(fit1)$coeff[4,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[4,2]),2))
# 
md2010<-convert.to.ci(round(c(summary(fit1)$coeff[5,1],
                              summary(fit1)$coeff[5,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[5,2],
                              summary(fit1)$coeff[5,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[5,2]),2))
#
md2011<-convert.to.ci(round(c(summary(fit1)$coeff[6,1],
                              summary(fit1)$coeff[6,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[6,2],
                              summary(fit1)$coeff[6,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[6,2]),2))

mds<-cbind(md2006,md2007,md2008,md2009,md2010,md2011)
colnames(mds)[1]<-"2006";colnames(mds)[2]<-"2007";colnames(mds)[3]<-"2008";colnames(mds)[4]<-"2009";
colnames(mds)[5]<-"2010";colnames(mds)[6]<-"2011";
ST1dist<-cbind(Y2006,Y2007,Y2008,Y2009,Y2010,Y2011)
#View(ST1dist)
ST1dist <-round(ST1dist[,],2)
ST1dist<-rbind(ST1dist,mds)
rownames(ST1dist)[16]<-"Mean difference (95% CI)"
#View(ST1dist)

folder.output <- "~/Google Drive/Research/EPAAsTrajectories/March2020/output"
if( !file.exists( folder.output ) ) {
  dir.create( file.path( folder.output ) )
}
write.xlsx(ST1dist,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/Table S1 Yearly CWS Distributions.xlsx")

# what number of yearly average values are below the MRL of 1?
describe(finallong$arsenic)
finallong$bmrl<-NA
finallong$bmrl[finallong$arsenic>=1]<-0
finallong$bmrl[finallong$arsenic<1]<-1
table(finallong$bmrl)
# 
describe(finallong$arsenic) #N=88624records
describe(finallong$arsenic[which(finallong$year=="Y2006")])
describe(finallong$arsenic[which(finallong$year=="Y2007")])
describe(finallong$arsenic[which(finallong$year=="Y2008")])
describe(finallong$arsenic[which(finallong$year=="Y2009")])
describe(finallong$arsenic[which(finallong$year=="Y2010")])
describe(finallong$arsenic[which(finallong$year=="Y2011")])

### end


####### 4. Table S2. Sensitivity analysis: complete case analysis at CWS level ######
ccase<-finaldata[which(!is.na(finaldata$Y2006)),];dim(ccase) #16282
ccase<-ccase[which(!is.na(ccase$Y2007)),];dim(ccase) #5423
ccase<-ccase[which(!is.na(ccase$Y2008)),];dim(ccase) #4262
ccase<-ccase[which(!is.na(ccase$Y2009)),];dim(ccase) #3884
ccase<-ccase[which(!is.na(ccase$Y2010)),];dim(ccase) #3542
ccase<-ccase[which(!is.na(ccase$Y2011)),];dim(ccase) #3342

# Distributions by year
# 2006
nav<-nrow(ccase[which(!is.na(ccase$Y2006)),])
nmiss<-nrow(ccase[which(is.na(ccase$Y2006)),])
quant<-as.data.frame(quantile(ccase$Y2006,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(ccase[which(ccase$Y2006<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(ccase[which(ccase$Y2006>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(ccase$Y2006),na.rm=T))
mean<-mean(ccase$Y2006,na.rm=T)
colnames(quant)[1] <- "2006"
Y2006<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Y2006)[1] <- "N";rownames(Y2006)[2] <- "N missing";rownames(Y2006)[12] <- "% below MRL"
rownames(Y2006)[13] <- "% above MCL";rownames(Y2006)[14] <- "Geometric mean";rownames(Y2006)[15] <- "Arithmetic mean"
# 2007
nav<-nrow(ccase[which(!is.na(ccase$Y2007)),])
nmiss<-nrow(ccase[which(is.na(ccase$Y2007)),])
quant<-as.data.frame(quantile(ccase$Y2007,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(ccase[which(ccase$Y2007<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(ccase[which(ccase$Y2007>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(ccase$Y2007),na.rm=T))
mean<-mean(ccase$Y2007,na.rm=T)
colnames(quant)[1] <- "2007"
Y2007<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Y2007)[1] <- "N";rownames(Y2007)[2] <- "N missing";rownames(Y2007)[12] <- "% below MRL"
rownames(Y2007)[13] <- "% above MCL";rownames(Y2007)[14] <- "Geometric mean";rownames(Y2007)[15] <- "Arithmetic mean"
# 2008
nav<-nrow(ccase[which(!is.na(ccase$Y2008)),])
nmiss<-nrow(ccase[which(is.na(ccase$Y2008)),])
quant<-as.data.frame(quantile(ccase$Y2008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(ccase[which(ccase$Y2008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(ccase[which(ccase$Y2008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(ccase$Y2008),na.rm=T))
mean<-mean(ccase$Y2008,na.rm=T)
colnames(quant)[1] <- "2008"
Y2008<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Y2008)[1] <- "N";rownames(Y2008)[2] <- "N missing";rownames(Y2008)[12] <- "% below MRL"
rownames(Y2008)[13] <- "% above MCL";rownames(Y2008)[14] <- "Geometric mean";rownames(Y2008)[15] <- "Arithmetic mean"
# 2009
nav<-nrow(ccase[which(!is.na(ccase$Y2009)),])
nmiss<-nrow(ccase[which(is.na(ccase$Y2009)),])
quant<-as.data.frame(quantile(ccase$Y2009,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(ccase[which(ccase$Y2009<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(ccase[which(ccase$Y2009>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(ccase$Y2009),na.rm=T))
mean<-mean(ccase$Y2009,na.rm=T)
colnames(quant)[1] <- "2009"
Y2009<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Y2009)[1] <- "N";rownames(Y2009)[2] <- "N missing";rownames(Y2009)[12] <- "% below MRL"
rownames(Y2009)[13] <- "% above MCL";rownames(Y2009)[14] <- "Geometric mean";rownames(Y2009)[15] <- "Arithmetic mean"
# 2010
nav<-nrow(ccase[which(!is.na(ccase$Y2010)),])
nmiss<-nrow(ccase[which(is.na(ccase$Y2010)),])
quant<-as.data.frame(quantile(ccase$Y2010,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(ccase[which(ccase$Y2010<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(ccase[which(ccase$Y2010>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(ccase$Y2010),na.rm=T))
mean<-mean(ccase$Y2010,na.rm=T)
colnames(quant)[1] <- "2010"
Y2010<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Y2010)[1] <- "N";rownames(Y2010)[2] <- "N missing";rownames(Y2010)[12] <- "% below MRL"
rownames(Y2010)[13] <- "% above MCL";rownames(Y2010)[14] <- "Geometric mean";rownames(Y2010)[15] <- "Arithmetic mean"
# 2011
nav<-nrow(ccase[which(!is.na(ccase$Y2011)),])
nmiss<-nrow(ccase[which(is.na(ccase$Y2011)),])
quant<-as.data.frame(quantile(ccase$Y2011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(ccase[which(ccase$Y2011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(ccase[which(ccase$Y2011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(ccase$Y2011),na.rm=T))
mean<-mean(ccase$Y2011,na.rm=T)
colnames(quant)[1] <- "2011"
Y2011<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Y2011)[1] <- "N";rownames(Y2011)[2] <- "N missing";rownames(Y2011)[12] <- "% below MRL"
rownames(Y2011)[13] <- "% above MCL";rownames(Y2011)[14] <- "Geometric mean";rownames(Y2011)[15] <- "Arithmetic mean"


# Mean difference models
# new dataset in long format
library(tidyr)
is.factor(ccase$PWSID)
ccase$PWSID<-as.factor(ccase$PWSID)
ccaselong <- gather(ccase, year, arsenic, Y2006:Y2011, factor_key=TRUE)

md2006<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=ccaselong)
summary(fit1)
md2007<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                              summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                              summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
# 
md2008<-convert.to.ci(round(c(summary(fit1)$coeff[3,1],
                              summary(fit1)$coeff[3,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[3,2],
                              summary(fit1)$coeff[3,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[3,2]),2))
# 
md2009<-convert.to.ci(round(c(summary(fit1)$coeff[4,1],
                              summary(fit1)$coeff[4,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[4,2],
                              summary(fit1)$coeff[4,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[4,2]),2))
# 
md2010<-convert.to.ci(round(c(summary(fit1)$coeff[5,1],
                              summary(fit1)$coeff[5,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[5,2],
                              summary(fit1)$coeff[5,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[5,2]),2))
#
md2011<-convert.to.ci(round(c(summary(fit1)$coeff[6,1],
                              summary(fit1)$coeff[6,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[6,2],
                              summary(fit1)$coeff[6,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[6,2]),2))

mds<-cbind(md2006,md2007,md2008,md2009,md2010,md2011)
colnames(mds)[1]<-"2006";colnames(mds)[2]<-"2007";colnames(mds)[3]<-"2008";colnames(mds)[4]<-"2009";
colnames(mds)[5]<-"2010";colnames(mds)[6]<-"2011";

ST1Bdist<-cbind(Y2006,Y2007,Y2008,Y2009,Y2010,Y2011)
#View(ST1Bdist)
ST1Bdist <-round(ST1Bdist[,],2)
ST1Bdist<-rbind(ST1Bdist,mds)
rownames(ST1Bdist)[16]<-"Mean difference (95% CI)"
#View(ST1Bdist)

folder.output <- "~/Google Drive/Research/EPAAsTrajectories/March2020/output"
if( !file.exists( folder.output ) ) {
  dir.create( file.path( folder.output ) )
}
write.xlsx(ST1Bdist,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/Table S2 Complete Case.xlsx")


### end


####### 5. Table S5. Distribution of 3-year averages for CWSs and counties ####### 

## -- CWSs 
#how many records  are below the MRL at each year and overall?
# 
cws$ThreeYearAs20062008<-as.numeric(cws$ThreeYearAs20062008)
cws$ThreeYearAs20092011<-as.numeric(cws$ThreeYearAs20092011)
finallong <- gather(cws, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE)
finallong

describe(cws$ThreeYearAs20062008) #5586 missing
tdelete<-cws[which(cws$ThreeYearAs20062008<1),];dim(tdelete)#21057 /30820
describe(cws$ThreeYearAs20092011)
tdelete<-cws[which(cws$ThreeYearAs20092011<1),];dim(tdelete) #21920/32481

#howmany above the MCL?
tdelete<-cws[which(cws$ThreeYearAs20062008>=10),];nrow(tdelete)#995 /30820
tdelete<-cws[which(cws$ThreeYearAs20092011>10),];dim(tdelete) #725/32481

# chi-square for this:
cws$amcl1<-0
cws$amcl1[which(cws$ThreeYearAs20062008>=10)]<-1
cws$amcl2<-0
cws$amcl2[which(cws$ThreeYearAs20092011>=10)]<-1
table(cws$amcl1);table(cws$amcl2)
chisq.test(cws$amcl1,cws$amcl2)

# Distributions by 3-year period
# 2006-2008
nav<-nrow(cws[which(!is.na(cws$ThreeYearAs20062008)),])
nmiss<-nrow(cws[which(is.na(cws$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(cws$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(cws[which(cws$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(cws[which(cws$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(cws$ThreeYearAs20062008),na.rm=T))
#mean<-mean(cws$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=finallong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(cws[which(!is.na(cws$ThreeYearAs20092011)),])
nmiss<-nrow(cws[which(is.na(cws$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(cws$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(cws[which(cws$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(cws[which(cws$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(cws$ThreeYearAs20092011),na.rm=T))
#mean<-mean(cws$ThreeYearAs20092011,na.rm=T)
#mean<-mean(cws$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=finallong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

library(tidyr)
is.factor(cws$PWSID)
cws$PWSID<-as.factor(cws$PWSID)
finallong <- gather(cws, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE)
finallong


# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=finallong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
T1distCWS<-cbind(Time1,Time2)
#View(T1distCWS)
T1distCWS <-round(T1distCWS[,],2)
T1distCWS<-rbind(T1distCWS,mds)
rownames(T1distCWS)[16]<-"Mean difference (95% CI)"


##-- Counties 
county<-merge(county,clusters,by="CountyFIPS",all.x=T);dim(county)# 

county$WeightedAs20062008<-as.numeric(county$WeightedAs20062008)
county$WeightedAs20092011<-as.numeric(county$WeightedAs20092011)

#create long form data
countylong <- gather(county, year, arsenic, WeightedAs20062008:WeightedAs20092011, factor_key=TRUE)

#how many averages are belowMRL at each year and overall?
# averages <0.35 
tdelete<-county[which(county$WeightedAs20062008<1),];dim(tdelete)#1579 /2323
tdelete<-county[which(county$WeightedAs20092011<1),];dim(tdelete) #1727/2562

#howmany above the MCL?
tdelete<-county[which(county$WeightedAs20062008>10),];dim(tdelete)#33 /2323
tdelete<-county[which(county$WeightedAs20092011>10),];dim(tdelete) #25/2562

# Distributions by 3-year period
# 2006-2008
nav<-nrow(county[which(!is.na(county$WeightedAs20062008)),])
nmiss<-nrow(county[which(is.na(county$WeightedAs20062008)),])
quant<-as.data.frame(quantile(county$WeightedAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(county[which(county$WeightedAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(county[which(county$WeightedAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(county$WeightedAs20062008),na.rm=T))
#mean<-mean(county$WeightedAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=countylong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

colnames(quant)[1] <- "County 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(county[which(!is.na(county$WeightedAs20092011)),])
nmiss<-nrow(county[which(is.na(county$WeightedAs20092011)),])
quant<-as.data.frame(quantile(county$WeightedAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(county[which(county$WeightedAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(county[which(county$WeightedAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(county$WeightedAs20092011),na.rm=T))
#mean<-mean(county$WeightedAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=countylong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
colnames(quant)[1] <- "County 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

library(tidyr)
#create long form data
countylong <- gather(county, year, arsenic, WeightedAs20062008:WeightedAs20092011, factor_key=TRUE)

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=countylong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"County 2006-2008";colnames(mds)[2]<-"County 2009-2011"
T1distcounty<-cbind(Time1,Time2)
#View(T1distcounty)
T1distcounty <-round(T1distcounty[,],2)
T1distcounty<-rbind(T1distcounty,mds)
rownames(T1distcounty)[16]<-"Mean difference (95% CI)"

Table1<-cbind(T1distCWS,T1distcounty)
write.xlsx(Table1,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/Table S5.xlsx")

### end
####### 6. Table S3. Quantile regression results for CWS and county overall ####### 
finallong3 <- gather(cws, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE)
countylong <- gather(county, year, arsenic, WeightedAs20062008:WeightedAs20092011, factor_key=TRUE)
library(quantreg)
#tau is the quantile(s) to be estimated, this is generally a number strictly between 0 and 1
#The function computes an estimate on the tau-th conditional quantile function of the response, given
#the covariates, as specified by the formula argument. 

###  CWS-level
set.seed(123)
# 75%
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=finallong3)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.75, data = finallong3, method='fn', model=TRUE)
t175<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t275<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 80%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.80, data = finallong3, method='fn', model=TRUE)
t180<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t280<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 85%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = finallong3, method='fn', model=TRUE)
t185<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t285<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 90%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = finallong3, method='fn', model=TRUE)
t190<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
#
# change 
t290<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 
# 95%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = finallong3, method='fn', model=TRUE)
t195<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t295<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 96%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.96, data = finallong3, method='fn', model=TRUE)
t196<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t296<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 97%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.97, data = finallong3, method='fn', model=TRUE)
t197<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t297<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 98%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.98, data = finallong3, method='fn', model=TRUE)
t198<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
#
# change 
t298<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 99%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = finallong3, method='fn', model=TRUE)
t199<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
#
# change 
t299<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
cwst1<-rbind(t175,t180,t185,t190,t195,t196,t197,t198,t199)
rownames(cwst1)[1] <- "p 75th"
rownames(cwst1)[2] <- "p 80th"
rownames(cwst1)[3] <- "p 85th"
rownames(cwst1)[4] <- "p 90th"
rownames(cwst1)[5] <- "p 95th"
rownames(cwst1)[6] <- "p 96th"
rownames(cwst1)[7] <- "p 97th"
rownames(cwst1)[8] <- "p 98th"
rownames(cwst1)[9] <- "p 99th"
cwst2<-rbind(t275,t280,t285,t290,t295,t296,t297,t298,t299)
rownames(cwst2)[1] <- "p 75th"
rownames(cwst2)[2] <- "p 80th"
rownames(cwst2)[3] <- "p 85th"
rownames(cwst2)[4] <- "p 90th"
rownames(cwst2)[5] <- "p 95th"
rownames(cwst2)[6] <- "p 96th"
rownames(cwst2)[7] <- "p 97th"
rownames(cwst2)[8] <- "p 98th"
rownames(cwst2)[9] <- "p 99th"


###  County-level
# 75%
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=countylong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.75, data = countylong, method='fn', model=TRUE)
summary(qr2)

# 2006-2008
t175<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t275<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))

# 80%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.80, data = countylong, method='fn', model=TRUE)
summary(qr2)
# 2006-2008
t180<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t280<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 85%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = countylong, method='fn', model=TRUE)
summary(qr2)
# 2006-2008
t185<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t285<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 
# 90%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = countylong, method='fn', model=TRUE)
summary(qr2)
# 2006-2008
t190<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
#
# change 
t290<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 
# 95%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = countylong, method='fn', model=TRUE)
summary(qr2)
# 2006-2008
t195<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
#
# change 
t295<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 96%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.96, data = countylong, method='fn', model=TRUE)
summary(qr2)
# 2006-2008
t196<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
#
# change 
t296<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))

# 97%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.97, data = countylong, method='fn', model=TRUE)
summary(qr2)
# 2006-2008
t197<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
#
# change 
t297<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 98%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.98, data = countylong, method='fn', model=TRUE)
summary(qr2)
# 2006-2008
t198<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
#
# change 
t298<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 99%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = countylong, method='fn', model=TRUE)
summary(qr2)
# 2006-2008
t199<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
#
# change 
t299<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
countyt1<-rbind(t175,t180,t185,t190,t195,t196,t197,t198,t199)
rownames(countyt1)[1] <- "p 75th"
rownames(countyt1)[2] <- "p 80th"
rownames(countyt1)[3] <- "p 85th"
rownames(countyt1)[4] <- "p 90th"
rownames(countyt1)[5] <- "p 95th"
rownames(countyt1)[6] <- "p 96th"
rownames(countyt1)[7] <- "p 97th"
rownames(countyt1)[8] <- "p 98th"
rownames(countyt1)[9] <- "p 99th"
countyt2<-rbind(t275,t280,t285,t290,t295,t296,t297,t298,t299)
rownames(countyt2)[1] <- "p 75th"
rownames(countyt2)[2] <- "p 80th"
rownames(countyt2)[3] <- "p 85th"
rownames(countyt2)[4] <- "p 90th"
rownames(countyt2)[5] <- "p 95th"
rownames(countyt2)[6] <- "p 96th"
rownames(countyt2)[7] <- "p 97th"
rownames(countyt2)[8] <- "p 98th"
rownames(countyt2)[9] <- "p 99th"

STable4<-cbind(cwst1,cwst2,countyt1,countyt2)
STable4 <- rbind(STable4, Time= c("Baseline CWS","Change CWS","Baseline County","Change County"))
STable4A <- data.frame(t(STable4[]))
write.xlsx(STable4A,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/Table S3.xlsx")



### end



####### 7. Excel Table S1. Distribution of 3-year CWS averages, stratified by region  ####### 
## Stratified by geologic region (not by socio county-cluster)
## AKHI
AKHI<-cws[which(cws$Region=="AKHI"),];dim(AKHI)
# Distributions by 3-year period
# 2006-2008
nav<-nrow(AKHI[which(!is.na(AKHI$ThreeYearAs20062008)),])
nmiss<-nrow(AKHI[which(is.na(AKHI$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(AKHI$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(AKHI[which(AKHI$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(AKHI[which(AKHI$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(AKHI$ThreeYearAs20062008),na.rm=T))
mean<-mean(AKHI$ThreeYearAs20062008,na.rm=T)
colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(AKHI[which(!is.na(AKHI$ThreeYearAs20092011)),])
nmiss<-nrow(AKHI[which(is.na(AKHI$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(AKHI$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(AKHI[which(AKHI$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(AKHI[which(AKHI$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(AKHI$ThreeYearAs20092011),na.rm=T))
mean<-mean(AKHI$ThreeYearAs20092011,na.rm=T)
colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# mean difference models
AKHIlong<-finallong3[which(finallong3$Region=="AKHI"),]
# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=AKHIlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
AKHIcws<-cbind(Time1,Time2)
AKHIcws <-round(AKHIcws[,],2)
AKHIcws<-rbind(AKHIcws,mds)
rownames(AKHIcws)[16]<-"Mean difference (95% CI)"
AKHIcws <- rbind(AKHIcws, Region= c("AKHI","AKHI"))

## CMW
CMW<-cws[which(cws$Region=="CMW"),];dim(CMW)
nav<-nrow(CMW[which(!is.na(CMW$ThreeYearAs20062008)),])
nmiss<-nrow(CMW[which(is.na(CMW$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(CMW$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(CMW[which(CMW$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(CMW[which(CMW$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(CMW$ThreeYearAs20062008),na.rm=T))
mean<-mean(CMW$ThreeYearAs20062008,na.rm=T)
colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(CMW[which(!is.na(CMW$ThreeYearAs20092011)),])
nmiss<-nrow(CMW[which(is.na(CMW$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(CMW$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(CMW[which(CMW$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(CMW[which(CMW$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(CMW$ThreeYearAs20092011),na.rm=T))
mean<-mean(CMW$ThreeYearAs20092011,na.rm=T)
colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# mean difference models
CMWlong<-finallong3[which(finallong3$Region=="CMW"),]
# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=CMWlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
CMWcws<-cbind(Time1,Time2)
CMWcws <-round(CMWcws[,],2)
CMWcws<-rbind(CMWcws,mds)
rownames(CMWcws)[16]<-"Mean difference (95% CI)"
CMWcws <- rbind(CMWcws, Region= c("CMW","CMW"))


# 
## EMW
EMW<-cws[which(cws$Region=="EMW"),];dim(EMW)
#2006-2008
nav<-nrow(EMW[which(!is.na(EMW$ThreeYearAs20062008)),])
nmiss<-nrow(EMW[which(is.na(EMW$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(EMW$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(EMW[which(EMW$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(EMW[which(EMW$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(EMW$ThreeYearAs20062008),na.rm=T))
mean<-mean(EMW$ThreeYearAs20062008,na.rm=T)
colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(EMW[which(!is.na(EMW$ThreeYearAs20092011)),])
nmiss<-nrow(EMW[which(is.na(EMW$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(EMW$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(EMW[which(EMW$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(EMW[which(EMW$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(EMW$ThreeYearAs20092011),na.rm=T))
mean<-mean(EMW$ThreeYearAs20092011,na.rm=T)
colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# mean difference models
EMWlong<-finallong3[which(finallong3$Region=="EMW"),]
# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=EMWlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
EMWcws<-cbind(Time1,Time2)
EMWcws <-round(EMWcws[,],2)
EMWcws<-rbind(EMWcws,mds)
rownames(EMWcws)[16]<-"Mean difference (95% CI)"
EMWcws <- rbind(EMWcws, Region= c("EMW","EMW"))


## MA
MA<-cws[which(cws$Region=="MA"),];dim(MA)
#2006-2008
nav<-nrow(MA[which(!is.na(MA$ThreeYearAs20062008)),])
nmiss<-nrow(MA[which(is.na(MA$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(MA$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(MA[which(MA$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(MA[which(MA$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(MA$ThreeYearAs20062008),na.rm=T))
mean<-mean(MA$ThreeYearAs20062008,na.rm=T)
colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(MA[which(!is.na(MA$ThreeYearAs20092011)),])
nmiss<-nrow(MA[which(is.na(MA$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(MA$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(MA[which(MA$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(MA[which(MA$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(MA$ThreeYearAs20092011),na.rm=T))
mean<-mean(MA$ThreeYearAs20092011,na.rm=T)
colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# mean difference models
MAlong<-finallong3[which(finallong3$Region=="MA"),]
# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=MAlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
MAcws<-cbind(Time1,Time2)
MAcws <-round(MAcws[,],2)
MAcws<-rbind(MAcws,mds)
rownames(MAcws)[16]<-"Mean difference (95% CI)"
MAcws <- rbind(MAcws, Region= c("MA","MA"))


## NE
NE<-cws[which(cws$Region=="NE"),];dim(NE)
#2006-2008

nav<-nrow(NE[which(!is.na(NE$ThreeYearAs20062008)),])
nmiss<-nrow(NE[which(is.na(NE$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(NE$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(NE[which(NE$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(NE[which(NE$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(NE$ThreeYearAs20062008),na.rm=T))
mean<-mean(NE$ThreeYearAs20062008,na.rm=T)
colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(NE[which(!is.na(NE$ThreeYearAs20092011)),])
nmiss<-nrow(NE[which(is.na(NE$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(NE$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(NE[which(NE$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(NE[which(NE$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(NE$ThreeYearAs20092011),na.rm=T))
mean<-mean(NE$ThreeYearAs20092011,na.rm=T)
colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# mean difference models
NElong<-finallong3[which(finallong3$Region=="NE"),]
# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=NElong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
NEcws<-cbind(Time1,Time2)
NEcws <-round(NEcws[,],2)
NEcws<-rbind(NEcws,mds)
rownames(NEcws)[16]<-"Mean difference (95% CI)"
NEcws <- rbind(NEcws, Region= c("NE","NE"))


## PNW
PNW<-cws[which(cws$Region=="PNW"),];dim(PNW)
#2006-2008

nav<-nrow(PNW[which(!is.na(PNW$ThreeYearAs20062008)),])
nmiss<-nrow(PNW[which(is.na(PNW$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(PNW$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(PNW[which(PNW$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(PNW[which(PNW$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(PNW$ThreeYearAs20062008),na.rm=T))
mean<-mean(PNW$ThreeYearAs20062008,na.rm=T)
colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(PNW[which(!is.na(PNW$ThreeYearAs20092011)),])
nmiss<-nrow(PNW[which(is.na(PNW$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(PNW$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(PNW[which(PNW$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(PNW[which(PNW$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(PNW$ThreeYearAs20092011),na.rm=T))
mean<-mean(PNW$ThreeYearAs20092011,na.rm=T)
colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# mean difference models
PNWlong<-finallong3[which(finallong3$Region=="PNW"),]
# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=PNWlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
PNWcws<-cbind(Time1,Time2)
PNWcws <-round(PNWcws[,],2)
PNWcws<-rbind(PNWcws,mds)
rownames(PNWcws)[16]<-"Mean difference (95% CI)"
PNWcws <- rbind(PNWcws, Region= c("PNW","PNW"))


## SE
SE<-cws[which(cws$Region=="SE"),];dim(SE)
nav<-nrow(SE[which(!is.na(SE$ThreeYearAs20062008)),])
nmiss<-nrow(SE[which(is.na(SE$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(SE$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(SE[which(SE$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(SE[which(SE$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(SE$ThreeYearAs20062008),na.rm=T))
mean<-mean(SE$ThreeYearAs20062008,na.rm=T)
colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(SE[which(!is.na(SE$ThreeYearAs20092011)),])
nmiss<-nrow(SE[which(is.na(SE$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(SE$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(SE[which(SE$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(SE[which(SE$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(SE$ThreeYearAs20092011),na.rm=T))
mean<-mean(SE$ThreeYearAs20092011,na.rm=T)
colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# mean difference models
SElong<-finallong3[which(finallong3$Region=="SE"),]
# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=SElong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
SEcws<-cbind(Time1,Time2)
SEcws <-round(SEcws[,],2)
SEcws<-rbind(SEcws,mds)
rownames(SEcws)[16]<-"Mean difference (95% CI)"
SEcws <- rbind(SEcws, Region= c("SE","SE"))


## SW
SW<-cws[which(cws$Region=="SW"),];dim(SW)
nav<-nrow(SW[which(!is.na(SW$ThreeYearAs20062008)),])
nmiss<-nrow(SW[which(is.na(SW$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(SW$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(SW[which(SW$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(SW[which(SW$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(SW$ThreeYearAs20062008),na.rm=T))
mean<-mean(SW$ThreeYearAs20062008,na.rm=T)
colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(SW[which(!is.na(SW$ThreeYearAs20092011)),])
nmiss<-nrow(SW[which(is.na(SW$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(SW$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(SW[which(SW$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(SW[which(SW$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(SW$ThreeYearAs20092011),na.rm=T))
mean<-mean(SW$ThreeYearAs20092011,na.rm=T)
colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# mean difference models
SWlong<-finallong3[which(finallong3$Region=="SW"),]
# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=SWlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
SWcws<-cbind(Time1,Time2)
SWcws <-round(SWcws[,],2)
SWcws<-rbind(SWcws,mds)
rownames(SWcws)[16]<-"Mean difference (95% CI)"
SWcws <- rbind(SWcws, Region= c("SW","SW"))


STable5<-cbind(AKHIcws,CMWcws,EMWcws,MAcws,NEcws,PNWcws,SEcws,SWcws)
write.xlsx(STable5,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/Excel Table S1.xlsx")

### end

####### 8. Table S4. Quantile regression results stratified by region ######
set.seed(123)

### AKHI

akhilong<-finallong3[which(finallong3$Region=="AKHI"),]
# 85%
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=akhilong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = akhilong, method='fn', model=TRUE)
t185<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t285<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 90%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = akhilong, method='fn', model=TRUE)
t190<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t290<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 95%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = akhilong, method='fn', model=TRUE)
t195<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t295<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 99%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = akhilong, method='fn', model=TRUE)
t199<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t299<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
AKHIt1<-rbind(t185,t190,t195,t199)
rownames(AKHIt1)[1] <- "p 85th"
rownames(AKHIt1)[2] <- "p 90th"
rownames(AKHIt1)[3] <- "p 95th"
rownames(AKHIt1)[4] <- "p 99th"
AKHIt2<-rbind(t285,t290,t295,t299)
rownames(AKHIt2)[1] <- "p 85th"
rownames(AKHIt2)[2] <- "p 90th"
rownames(AKHIt2)[3] <- "p 95th"
rownames(AKHIt2)[4] <- "p 99th"
AKHI<-cbind(AKHIt1,AKHIt2)
AKHI <- rbind(AKHI, Time= c("Baseline CWS","Change CWS"))
AKHI <- rbind(AKHI, Region= c("AKHI","AKHI"))

### CMW
cmwlong<-finallong3[which(finallong3$Region=="CMW"),]
# 85%
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=cmwlong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = cmwlong, method='fn', model=TRUE)
t185<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t285<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 90%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = cmwlong, method='fn', model=TRUE)
t190<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t290<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 95%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = cmwlong, method='fn', model=TRUE)
t195<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t295<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 99%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = cmwlong, method='fn', model=TRUE)
t199<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t299<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
CMWt1<-rbind(t185,t190,t195,t199)
rownames(CMWt1)[1] <- "p 85th"
rownames(CMWt1)[2] <- "p 90th"
rownames(CMWt1)[3] <- "p 95th"
rownames(CMWt1)[4] <- "p 99th"
CMWt2<-rbind(t285,t290,t295,t299)
rownames(CMWt2)[1] <- "p 85th"
rownames(CMWt2)[2] <- "p 90th"
rownames(CMWt2)[3] <- "p 95th"
rownames(CMWt2)[4] <- "p 99th"
CMW<-cbind(CMWt1,CMWt2)
CMW <- rbind(CMW, Time= c("Baseline CWS","Change CWS"))
CMW <- rbind(CMW, Region= c("CMW","CMW"))


# emw
emwlong<-finallong3[which(finallong3$Region=="EMW"),]

# 85%
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=emwlong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = emwlong, method='fn', model=TRUE)
t185<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t285<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 90%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = emwlong, method='fn', model=TRUE)
t190<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t290<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 95%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = emwlong, method='fn', model=TRUE)
t195<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t295<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 99%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = emwlong, method='fn', model=TRUE)
t199<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t299<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
EMWt1<-rbind(t185,t190,t195,t199)
rownames(EMWt1)[1] <- "p 85th"
rownames(EMWt1)[2] <- "p 90th"
rownames(EMWt1)[3] <- "p 95th"
rownames(EMWt1)[4] <- "p 99th"
EMWt2<-rbind(t285,t290,t295,t299)
rownames(EMWt2)[1] <- "p 85th"
rownames(EMWt2)[2] <- "p 90th"
rownames(EMWt2)[3] <- "p 95th"
rownames(EMWt2)[4] <- "p 99th"
EMW<-cbind(EMWt1,EMWt2)
EMW <- rbind(EMW, Time= c("Baseline CWS","Change CWS"))
EMW <- rbind(EMW, Region= c("EMW","EMW"))


# ma
malong<-finallong3[which(finallong3$Region=="MA"),]
# 85%
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=malong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = malong, method='fn', model=TRUE)
t185<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t285<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 90%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = malong, method='fn', model=TRUE)
t190<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t290<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 95%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = malong, method='fn', model=TRUE)
t195<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t295<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 99%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = malong, method='fn', model=TRUE)
t199<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t299<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
MAt1<-rbind(t185,t190,t195,t199)
rownames(MAt1)[1] <- "p 85th"
rownames(MAt1)[2] <- "p 90th"
rownames(MAt1)[3] <- "p 95th"
rownames(MAt1)[4] <- "p 99th"
MAt2<-rbind(t285,t290,t295,t299)
rownames(MAt2)[1] <- "p 85th"
rownames(MAt2)[2] <- "p 90th"
rownames(MAt2)[3] <- "p 95th"
rownames(MAt2)[4] <- "p 99th"
MA<-cbind(MAt1,MAt2)
MA <- rbind(MA, Time= c("Baseline CWS","Change CWS"))
MA <- rbind(MA, Region= c("MA","MA"))

# ne
nelong<-finallong3[which(finallong3$Region=="NE"),]
# 85%
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=nelong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = nelong, method='fn', model=TRUE)
t185<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t285<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 90%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = nelong, method='fn', model=TRUE)
t190<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t290<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 95%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = nelong, method='fn', model=TRUE)
t195<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t295<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 99%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = nelong, method='fn', model=TRUE)
t199<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t299<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
NEt1<-rbind(t185,t190,t195,t199)
rownames(NEt1)[1] <- "p 85th"
rownames(NEt1)[2] <- "p 90th"
rownames(NEt1)[3] <- "p 95th"
rownames(NEt1)[4] <- "p 99th"
NEt2<-rbind(t285,t290,t295,t299)
rownames(NEt2)[1] <- "p 85th"
rownames(NEt2)[2] <- "p 90th"
rownames(NEt2)[3] <- "p 95th"
rownames(NEt2)[4] <- "p 99th"
NE<-cbind(NEt1,NEt2)
NE <- rbind(NE, Time= c("Baseline CWS","Change CWS"))
NE <- rbind(NE, Region= c("NE","NE"))


# pnw
pnwlong<-finallong3[which(finallong3$Region=="PNW"),]
# 85%
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=pnwlong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = pnwlong, method='fn', model=TRUE)
t185<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t285<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 90%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = pnwlong, method='fn', model=TRUE)
t190<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t290<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 95%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = pnwlong, method='fn', model=TRUE)
t195<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t295<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 99%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = pnwlong, method='fn', model=TRUE)
t199<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t299<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
PNWt1<-rbind(t185,t190,t195,t199)
rownames(PNWt1)[1] <- "p 85th"
rownames(PNWt1)[2] <- "p 90th"
rownames(PNWt1)[3] <- "p 95th"
rownames(PNWt1)[4] <- "p 99th"
PNWt2<-rbind(t285,t290,t295,t299)
rownames(PNWt2)[1] <- "p 85th"
rownames(PNWt2)[2] <- "p 90th"
rownames(PNWt2)[3] <- "p 95th"
rownames(PNWt2)[4] <- "p 99th"
PNW<-cbind(PNWt1,PNWt2)
PNW <- rbind(PNW, Time= c("Baseline CWS","Change CWS"))
PNW <- rbind(PNW, Region= c("PNW","PNW"))

# se
selong<-finallong3[which(finallong3$Region=="SE"),]
# 85%
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=selong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = selong, method='fn', model=TRUE)
t185<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t285<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 90%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = selong, method='fn', model=TRUE)
t190<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t290<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 95%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = selong, method='fn', model=TRUE)
t195<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t295<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 99%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = selong, method='fn', model=TRUE)
t199<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t299<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
SEt1<-rbind(t185,t190,t195,t199)
rownames(SEt1)[1] <- "p 85th"
rownames(SEt1)[2] <- "p 90th"
rownames(SEt1)[3] <- "p 95th"
rownames(SEt1)[4] <- "p 99th"
SEt2<-rbind(t285,t290,t295,t299)
rownames(SEt2)[1] <- "p 85th"
rownames(SEt2)[2] <- "p 90th"
rownames(SEt2)[3] <- "p 95th"
rownames(SEt2)[4] <- "p 99th"
SE<-cbind(SEt1,SEt2)
SE <- rbind(SE, Time= c("Baseline CWS","Change CWS"))
SE <- rbind(SE, Region= c("SE","SE"))


# SW
swlong<-finallong3[which(finallong3$Region=="SW"),]
# 85%
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=swlong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = swlong, method='fn', model=TRUE)
t185<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t285<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 90%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = swlong, method='fn', model=TRUE)
t190<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t290<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 95%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = swlong, method='fn', model=TRUE)
t195<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t295<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
# 99%
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = swlong, method='fn', model=TRUE)
t199<-convert.to.ci(round(c(summary(qr2)$coeff[1,1],
                            summary(qr2)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2],
                            summary(qr2)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[1,2]),2))
# change 
t299<-convert.to.ci(round(c(summary(qr2)$coeff[2,1],
                            summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2],
                            summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]),2))
SWt1<-rbind(t185,t190,t195,t199)
rownames(SWt1)[1] <- "p 85th"
rownames(SWt1)[2] <- "p 90th"
rownames(SWt1)[3] <- "p 95th"
rownames(SWt1)[4] <- "p 99th"
SWt2<-rbind(t285,t290,t295,t299)
rownames(SWt2)[1] <- "p 85th"
rownames(SWt2)[2] <- "p 90th"
rownames(SWt2)[3] <- "p 95th"
rownames(SWt2)[4] <- "p 99th"
SW<-cbind(SWt1,SWt2)
SW <- rbind(SW, Time= c("Baseline CWS","Change CWS"))
SW <- rbind(SW, Region= c("SW","SW"))

STable6<-cbind(AKHI,CMW,EMW,MA,NE,PNW,SE,SW)

# Flip orientation of table
# Transpose into better format
STable6A <- data.frame(t(STable6[]))

write.xlsx(STable6A,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/Table S4.xlsx")

### end



####### 9. Excel Table S2. Distribution of 3-year county averages, stratified by region  ###### 
#Stratified by geologic region and county cluster

## First, stratified by geologic region:

## Stratified by geologic region (not by socio county-cluster)
## AKHI
AKHI<-county[which(county$Region=="AKHI"),];dim(AKHI)
AKHIlong<-countylong[which(countylong$Region=="AKHI"),]
# Distributions by 3-year period
# 2006-2008
nav<-nrow(AKHI[which(!is.na(AKHI$WeightedAs20062008)),])
nmiss<-nrow(AKHI[which(is.na(AKHI$WeightedAs20062008)),])
quant<-as.data.frame(quantile(AKHI$WeightedAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(AKHI[which(AKHI$WeightedAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(AKHI[which(AKHI$WeightedAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(AKHI$WeightedAs20062008),na.rm=T))
#mean<-mean(AKHI$WeightedAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=AKHIlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

colnames(quant)[1] <- "County 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(AKHI[which(!is.na(AKHI$WeightedAs20092011)),])
nmiss<-nrow(AKHI[which(is.na(AKHI$WeightedAs20092011)),])
quant<-as.data.frame(quantile(AKHI$WeightedAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(AKHI[which(AKHI$WeightedAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(AKHI[which(AKHI$WeightedAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(AKHI$WeightedAs20092011),na.rm=T))
#mean<-mean(AKHI$WeightedAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=AKHIlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

colnames(quant)[1] <- "County 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# mean difference models
AKHIlong<-countylong[which(countylong$Region=="AKHI"),]
# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=AKHIlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"County 2006-2008";colnames(mds)[2]<-"County 2009-2011"
AKHIcounty<-cbind(Time1,Time2)
AKHIcounty <-round(AKHIcounty[,],2)
AKHIcounty<-rbind(AKHIcounty,mds)
rownames(AKHIcounty)[16]<-"Mean difference (95% CI)"
AKHIcounty <- rbind(AKHIcounty, Region= c("AKHI","AKHI"))

## CMW
CMW<-county[which(county$Region=="CMW"),];dim(CMW)
CMWlong<-countylong[which(countylong$Region=="CMW"),]
nav<-nrow(CMW[which(!is.na(CMW$WeightedAs20062008)),])
nmiss<-nrow(CMW[which(is.na(CMW$WeightedAs20062008)),])
quant<-as.data.frame(quantile(CMW$WeightedAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(CMW[which(CMW$WeightedAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(CMW[which(CMW$WeightedAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(CMW$WeightedAs20062008),na.rm=T))
#mean<-mean(CMW$WeightedAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=CMWlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

colnames(quant)[1] <- "County 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(CMW[which(!is.na(CMW$WeightedAs20092011)),])
nmiss<-nrow(CMW[which(is.na(CMW$WeightedAs20092011)),])
quant<-as.data.frame(quantile(CMW$WeightedAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(CMW[which(CMW$WeightedAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(CMW[which(CMW$WeightedAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(CMW$WeightedAs20092011),na.rm=T))
#mean<-mean(CMW$WeightedAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=CMWlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

colnames(quant)[1] <- "County 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# mean difference models
CMWlong<-countylong[which(countylong$Region=="CMW"),]
# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=CMWlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"County 2006-2008";colnames(mds)[2]<-"County 2009-2011"
CMWcounty<-cbind(Time1,Time2)
CMWcounty <-round(CMWcounty[,],2)
CMWcounty<-rbind(CMWcounty,mds)
rownames(CMWcounty)[16]<-"Mean difference (95% CI)"
CMWcounty <- rbind(CMWcounty, Region= c("CMW","CMW"))


# 
## EMW
EMW<-county[which(county$Region=="EMW"),];dim(EMW)
EMWlong<-countylong[which(countylong$Region=="EMW"),]

#2006-2008
nav<-nrow(EMW[which(!is.na(EMW$WeightedAs20062008)),])
nmiss<-nrow(EMW[which(is.na(EMW$WeightedAs20062008)),])
quant<-as.data.frame(quantile(EMW$WeightedAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(EMW[which(EMW$WeightedAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(EMW[which(EMW$WeightedAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(EMW$WeightedAs20062008),na.rm=T))
#mean<-mean(EMW$WeightedAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=EMWlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

colnames(quant)[1] <- "County 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(EMW[which(!is.na(EMW$WeightedAs20092011)),])
nmiss<-nrow(EMW[which(is.na(EMW$WeightedAs20092011)),])
quant<-as.data.frame(quantile(EMW$WeightedAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(EMW[which(EMW$WeightedAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(EMW[which(EMW$WeightedAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(EMW$WeightedAs20092011),na.rm=T))
#mean<-mean(EMW$WeightedAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=EMWlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

colnames(quant)[1] <- "County 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# mean difference models
EMWlong<-countylong[which(countylong$Region=="EMW"),]
# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=EMWlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"County 2006-2008";colnames(mds)[2]<-"County 2009-2011"
EMWcounty<-cbind(Time1,Time2)
EMWcounty <-round(EMWcounty[,],2)
EMWcounty<-rbind(EMWcounty,mds)
rownames(EMWcounty)[16]<-"Mean difference (95% CI)"
EMWcounty <- rbind(EMWcounty, Region= c("EMW","EMW"))


## MA
MA<-county[which(county$Region=="MA"),];dim(MA)
MAlong<-countylong[which(countylong$Region=="MA"),]
#2006-2008
nav<-nrow(MA[which(!is.na(MA$WeightedAs20062008)),])
nmiss<-nrow(MA[which(is.na(MA$WeightedAs20062008)),])
quant<-as.data.frame(quantile(MA$WeightedAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(MA[which(MA$WeightedAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(MA[which(MA$WeightedAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(MA$WeightedAs20062008),na.rm=T))
#mean<-mean(MA$WeightedAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=MAlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

colnames(quant)[1] <- "County 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(MA[which(!is.na(MA$WeightedAs20092011)),])
nmiss<-nrow(MA[which(is.na(MA$WeightedAs20092011)),])
quant<-as.data.frame(quantile(MA$WeightedAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(MA[which(MA$WeightedAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(MA[which(MA$WeightedAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(MA$WeightedAs20092011),na.rm=T))
#mean<-mean(MA$WeightedAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=MAlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
colnames(quant)[1] <- "County 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# mean difference models
MAlong<-countylong[which(countylong$Region=="MA"),]
# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=MAlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"County 2006-2008";colnames(mds)[2]<-"County 2009-2011"
MAcounty<-cbind(Time1,Time2)
MAcounty <-round(MAcounty[,],2)
MAcounty<-rbind(MAcounty,mds)
rownames(MAcounty)[16]<-"Mean difference (95% CI)"
MAcounty <- rbind(MAcounty, Region= c("MA","MA"))


## NE
NE<-county[which(county$Region=="NE"),];dim(NE)
NElong<-countylong[which(countylong$Region=="NE"),]
#2006-2008
nav<-nrow(NE[which(!is.na(NE$WeightedAs20062008)),])
nmiss<-nrow(NE[which(is.na(NE$WeightedAs20062008)),])
quant<-as.data.frame(quantile(NE$WeightedAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(NE[which(NE$WeightedAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(NE[which(NE$WeightedAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(NE$WeightedAs20062008),na.rm=T))
#mean<-mean(NE$WeightedAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=NElong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))


colnames(quant)[1] <- "County 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(NE[which(!is.na(NE$WeightedAs20092011)),])
nmiss<-nrow(NE[which(is.na(NE$WeightedAs20092011)),])
quant<-as.data.frame(quantile(NE$WeightedAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(NE[which(NE$WeightedAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(NE[which(NE$WeightedAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(NE$WeightedAs20092011),na.rm=T))
#mean<-mean(NE$WeightedAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=NElong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

colnames(quant)[1] <- "County 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# mean difference models
NElong<-countylong[which(countylong$Region=="NE"),]
# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=NElong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"County 2006-2008";colnames(mds)[2]<-"County 2009-2011"
NEcounty<-cbind(Time1,Time2)
NEcounty <-round(NEcounty[,],2)
NEcounty<-rbind(NEcounty,mds)
rownames(NEcounty)[16]<-"Mean difference (95% CI)"
NEcounty <- rbind(NEcounty, Region= c("NE","NE"))


## PNW
PNW<-county[which(county$Region=="PNW"),];dim(PNW)
PNWlong<-countylong[which(countylong$Region=="PNW"),]
#2006-2008
nav<-nrow(PNW[which(!is.na(PNW$WeightedAs20062008)),])
nmiss<-nrow(PNW[which(is.na(PNW$WeightedAs20062008)),])
quant<-as.data.frame(quantile(PNW$WeightedAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(PNW[which(PNW$WeightedAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(PNW[which(PNW$WeightedAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(PNW$WeightedAs20062008),na.rm=T))
#mean<-mean(PNW$WeightedAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=PNWlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))


colnames(quant)[1] <- "County 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(PNW[which(!is.na(PNW$WeightedAs20092011)),])
nmiss<-nrow(PNW[which(is.na(PNW$WeightedAs20092011)),])
quant<-as.data.frame(quantile(PNW$WeightedAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(PNW[which(PNW$WeightedAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(PNW[which(PNW$WeightedAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(PNW$WeightedAs20092011),na.rm=T))
#mean<-mean(PNW$WeightedAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=PNWlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))


colnames(quant)[1] <- "County 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# mean difference models
PNWlong<-countylong[which(countylong$Region=="PNW"),]
# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=PNWlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"County 2006-2008";colnames(mds)[2]<-"County 2009-2011"
PNWcounty<-cbind(Time1,Time2)
PNWcounty <-round(PNWcounty[,],2)
PNWcounty<-rbind(PNWcounty,mds)
rownames(PNWcounty)[16]<-"Mean difference (95% CI)"
PNWcounty <- rbind(PNWcounty, Region= c("PNW","PNW"))


## SE
SE<-county[which(county$Region=="SE"),];dim(SE)
SElong<-countylong[which(countylong$Region=="SE"),]

nav<-nrow(SE[which(!is.na(SE$WeightedAs20062008)),])
nmiss<-nrow(SE[which(is.na(SE$WeightedAs20062008)),])
quant<-as.data.frame(quantile(SE$WeightedAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(SE[which(SE$WeightedAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(SE[which(SE$WeightedAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(SE$WeightedAs20062008),na.rm=T))
#mean<-mean(SE$WeightedAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=SElong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

colnames(quant)[1] <- "County 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(SE[which(!is.na(SE$WeightedAs20092011)),])
nmiss<-nrow(SE[which(is.na(SE$WeightedAs20092011)),])
quant<-as.data.frame(quantile(SE$WeightedAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(SE[which(SE$WeightedAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(SE[which(SE$WeightedAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(SE$WeightedAs20092011),na.rm=T))
#mean<-mean(SE$WeightedAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=SElong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

colnames(quant)[1] <- "County 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# mean difference models
SElong<-countylong[which(countylong$Region=="SE"),]
# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=SElong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"County 2006-2008";colnames(mds)[2]<-"County 2009-2011"
SEcounty<-cbind(Time1,Time2)
SEcounty <-round(SEcounty[,],2)
SEcounty<-rbind(SEcounty,mds)
rownames(SEcounty)[16]<-"Mean difference (95% CI)"
SEcounty <- rbind(SEcounty, Region= c("SE","SE"))


## SW
SW<-county[which(county$Region=="SW"),];dim(SW)
SWlong<-countylong[which(countylong$Region=="SW"),]

nav<-nrow(SW[which(!is.na(SW$WeightedAs20062008)),])
nmiss<-nrow(SW[which(is.na(SW$WeightedAs20062008)),])
quant<-as.data.frame(quantile(SW$WeightedAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(SW[which(SW$WeightedAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(SW[which(SW$WeightedAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(SW$WeightedAs20062008),na.rm=T))
#mean<-mean(SW$WeightedAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=SWlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

colnames(quant)[1] <- "County 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(SW[which(!is.na(SW$WeightedAs20092011)),])
nmiss<-nrow(SW[which(is.na(SW$WeightedAs20092011)),])
quant<-as.data.frame(quantile(SW$WeightedAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(SW[which(SW$WeightedAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(SW[which(SW$WeightedAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(SW$WeightedAs20092011),na.rm=T))
#mean<-mean(SW$WeightedAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=SWlong)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
colnames(quant)[1] <- "County 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# mean difference models
SWlong<-countylong[which(countylong$Region=="SW"),]
# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=SWlong)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"County 2006-2008";colnames(mds)[2]<-"County 2009-2011"
SWcounty<-cbind(Time1,Time2)
SWcounty <-round(SWcounty[,],2)
SWcounty<-rbind(SWcounty,mds)
rownames(SWcounty)[16]<-"Mean difference (95% CI)"
SWcounty <- rbind(SWcounty, Region= c("SW","SW"))


STable7<-cbind(AKHIcounty,CMWcounty,EMWcounty,MAcounty,NEcounty,PNWcounty,SEcounty,SWcounty)
write.xlsx(STable7,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/Excel Table S2.xlsx")


### end


####### 10. Excel Table S3. Distribution of 3-year CWS averages, stratified by sociodemographic county-cluster  ####### 
## Stratified by sociodemogrpahic county cluster;

# First we need to assign each CWS to a county-cluster; lets pull thoe CWSs from _data_ which serve multiple counties
# and see if these are the same counties:
describe(finaldatadups3$PWSID) ##one record for each CWS serving each county, 3-yr averages 
describe(finaldatadups3$CountyFIPS)

# Do CWSs serving duplicate counties serve the same "sociodemographic" type of county?
dups<-finaldatadups3[duplicated(finaldatadups3$PWSID),];nrow(dups) #268
g<-finaldatadups3[finaldatadups3$PWSID %in% finaldatadups3$PWSID[duplicated(finaldatadups3$PWSID)],]
check<-g
describe(check$PWSID) #172 distinct PWSIDs to check 
table(check$PWSID,check$cluster.number)[1:50,] #  duplicated PWSIDs serve two diff county-types
table(check$PWSID,check$cluster.number)[51:100,]
table(finaldatadups3$cluster.number)


library(tidyr)
is.factor(finaldatadups3$PWSID)
finaldatadups3$PWSID<-as.factor(finaldatadups3$PWSID)
fdups3long <- gather(finaldatadups3, year, arsenic, ThreeYearAs20062008:ThreeYearAs20092011, factor_key=TRUE)

## c1
c1<-finaldatadups3[which(finaldatadups3$cluster.number=="1"),];dim(c1) #14,479
c2<-finaldatadups3[which(finaldatadups3$cluster.number=="2"),];dim(c2) #1,389
c3<-finaldatadups3[which(finaldatadups3$cluster.number=="3"),];dim(c3) #4,540
c4<-finaldatadups3[which(finaldatadups3$cluster.number=="4"),];dim(c4) #9,058
c5<-finaldatadups3[which(finaldatadups3$cluster.number=="5"),];dim(c5) # 532
c6<-finaldatadups3[which(finaldatadups3$cluster.number=="6"),];dim(c6) #1,027
c7<-finaldatadups3[which(finaldatadups3$cluster.number=="7"),];dim(c7)  #491
c8<-finaldatadups3[which(finaldatadups3$cluster.number=="8"),];dim(c8)#5,158
c1long<-fdups3long[which(fdups3long$cluster.number=="1"),]
c2long<-fdups3long[which(fdups3long$cluster.number=="2"),]
c3long<-fdups3long[which(fdups3long$cluster.number=="3"),]
c4long<-fdups3long[which(fdups3long$cluster.number=="4"),]
c5long<-fdups3long[which(fdups3long$cluster.number=="5"),]
c6long<-fdups3long[which(fdups3long$cluster.number=="6"),]
c7long<-fdups3long[which(fdups3long$cluster.number=="7"),]
c8long<-fdups3long[which(fdups3long$cluster.number=="8"),]



## Stratified by socio county-cluster
## c1
# Distributions by 3-year period
# 2006-2008
nav<-nrow(c1[which(!is.na(c1$ThreeYearAs20062008)),])
nmiss<-nrow(c1[which(is.na(c1$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(c1$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(c1[which(c1$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(c1[which(c1$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(c1$ThreeYearAs20062008),na.rm=T))
#mean<-mean(c1$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c1long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(c1[which(!is.na(c1$ThreeYearAs20092011)),])
nmiss<-nrow(c1[which(is.na(c1$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(c1$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(c1[which(c1$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(c1[which(c1$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(c1$ThreeYearAs20092011),na.rm=T))
#mean<-mean(c1$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c1long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c1long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
c1county<-cbind(Time1,Time2)
c1county <-round(c1county[,],2)
c1county<-rbind(c1county,mds)
rownames(c1county)[16]<-"Mean difference (95% CI)"
c1county <- rbind(c1county, Cluster= c("c1","c1"))


## c2
nav<-nrow(c2[which(!is.na(c2$ThreeYearAs20062008)),])
nmiss<-nrow(c2[which(is.na(c2$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(c2$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(c2[which(c2$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(c2[which(c2$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(c2$ThreeYearAs20062008),na.rm=T))
#mean<-mean(c2$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c2long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(c2[which(!is.na(c2$ThreeYearAs20092011)),])
nmiss<-nrow(c2[which(is.na(c2$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(c2$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(c2[which(c2$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(c2[which(c2$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(c2$ThreeYearAs20092011),na.rm=T))
#mean<-mean(c2$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c2long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c2long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
c2county<-cbind(Time1,Time2)
c2county <-round(c2county[,],2)
c2county<-rbind(c2county,mds)
rownames(c2county)[16]<-"Mean difference (95% CI)"
c2county <- rbind(c2county, Cluster= c("c2","c2"))


# 
## c3
#2006-2008
nav<-nrow(c3[which(!is.na(c3$ThreeYearAs20062008)),])
nmiss<-nrow(c3[which(is.na(c3$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(c3$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(c3[which(c3$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(c3[which(c3$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(c3$ThreeYearAs20062008),na.rm=T))
#mean<-mean(c3$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c3long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(c3[which(!is.na(c3$ThreeYearAs20092011)),])
nmiss<-nrow(c3[which(is.na(c3$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(c3$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(c3[which(c3$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(c3[which(c3$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(c3$ThreeYearAs20092011),na.rm=T))
#mean<-mean(c3$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c3long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c3long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
c3county<-cbind(Time1,Time2)
c3county <-round(c3county[,],2)
c3county<-rbind(c3county,mds)
rownames(c3county)[16]<-"Mean difference (95% CI)"
c3county <- rbind(c3county, Cluster= c("c3","c3"))


## c4
#2006-2008
nav<-nrow(c4[which(!is.na(c4$ThreeYearAs20062008)),])
nmiss<-nrow(c4[which(is.na(c4$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(c4$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(c4[which(c4$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(c4[which(c4$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(c4$ThreeYearAs20062008),na.rm=T))
#mean<-mean(c4$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c4long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(c4[which(!is.na(c4$ThreeYearAs20092011)),])
nmiss<-nrow(c4[which(is.na(c4$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(c4$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(c4[which(c4$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(c4[which(c4$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(c4$ThreeYearAs20092011),na.rm=T))
#mean<-mean(c4$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c4long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c4long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
c4county<-cbind(Time1,Time2)
c4county <-round(c4county[,],2)
c4county<-rbind(c4county,mds)
rownames(c4county)[16]<-"Mean difference (95% CI)"
c4county <- rbind(c4county, Cluster= c("c4","c4"))


## c5
#2006-2008
nav<-nrow(c5[which(!is.na(c5$ThreeYearAs20062008)),])
nmiss<-nrow(c5[which(is.na(c5$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(c5$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(c5[which(c5$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(c5[which(c5$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(c5$ThreeYearAs20062008),na.rm=T))
#mean<-mean(c5$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c5long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(c5[which(!is.na(c5$ThreeYearAs20092011)),])
nmiss<-nrow(c5[which(is.na(c5$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(c5$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(c5[which(c5$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(c5[which(c5$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(c5$ThreeYearAs20092011),na.rm=T))
#mean<-mean(c5$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c5long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c5long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
c5county<-cbind(Time1,Time2)
c5county <-round(c5county[,],2)
c5county<-rbind(c5county,mds)
rownames(c5county)[16]<-"Mean difference (95% CI)"
c5county <- rbind(c5county, Cluster= c("c5","c5"))


## c6
#2006-2008
nav<-nrow(c6[which(!is.na(c6$ThreeYearAs20062008)),])
nmiss<-nrow(c6[which(is.na(c6$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(c6$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(c6[which(c6$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(c6[which(c6$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(c6$ThreeYearAs20062008),na.rm=T))
#mean<-mean(c6$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c6long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(c6[which(!is.na(c6$ThreeYearAs20092011)),])
nmiss<-nrow(c6[which(is.na(c6$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(c6$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(c6[which(c6$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(c6[which(c6$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(c6$ThreeYearAs20092011),na.rm=T))
#mean<-mean(c6$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c6long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c6long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
c6county<-cbind(Time1,Time2)
c6county <-round(c6county[,],2)
c6county<-rbind(c6county,mds)
rownames(c6county)[16]<-"Mean difference (95% CI)"
c6county <- rbind(c6county, Cluster= c("c6","c6"))

## c7
nav<-nrow(c7[which(!is.na(c7$ThreeYearAs20062008)),])
nmiss<-nrow(c7[which(is.na(c7$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(c7$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(c7[which(c7$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(c7[which(c7$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(c7$ThreeYearAs20062008),na.rm=T))
#mean<-mean(c7$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c7long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(c7[which(!is.na(c7$ThreeYearAs20092011)),])
nmiss<-nrow(c7[which(is.na(c7$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(c7$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(c7[which(c7$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(c7[which(c7$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(c7$ThreeYearAs20092011),na.rm=T))
#mean<-mean(c7$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c7long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c7long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
c7county<-cbind(Time1,Time2)
c7county <-round(c7county[,],2)
c7county<-rbind(c7county,mds)
rownames(c7county)[16]<-"Mean difference (95% CI)"
c7county <- rbind(c7county, Cluster= c("c7","c7"))


## c8
nav<-nrow(c8[which(!is.na(c8$ThreeYearAs20062008)),])
nmiss<-nrow(c8[which(is.na(c8$ThreeYearAs20062008)),])
quant<-as.data.frame(quantile(c8$ThreeYearAs20062008,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(c8[which(c8$ThreeYearAs20062008<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(c8[which(c8$ThreeYearAs20062008>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(c8$ThreeYearAs20062008),na.rm=T))
#mean<-mean(c8$ThreeYearAs20062008,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c8long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                            summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                            summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

colnames(quant)[1] <- "CWS 2006-2008"
Time1<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time1)[1] <- "N";rownames(Time1)[2] <- "N missing";rownames(Time1)[12] <- "% below MRL"
rownames(Time1)[13] <- "% above MCL";rownames(Time1)[14] <- "Geometric mean";rownames(Time1)[15] <- "Arithmetic mean"
# 2009-2011
nav<-nrow(c8[which(!is.na(c8$ThreeYearAs20092011)),])
nmiss<-nrow(c8[which(is.na(c8$ThreeYearAs20092011)),])
quant<-as.data.frame(quantile(c8$ThreeYearAs20092011,  probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9,0.95, 0.99,1), na.rm=TRUE, scientific=F))
nmrl1<-nrow(c8[which(c8$ThreeYearAs20092011<1),])
nmrl<-round(100*(nmrl1/nav),1)
nmcl1<-nrow(c8[which(c8$ThreeYearAs20092011>=10),])
nmcl<-round(100*(nmcl1/nav),1)
gm<-exp(mean(log(c8$ThreeYearAs20092011),na.rm=T))
#mean<-mean(c8$ThreeYearAs20092011,na.rm=T)
fit1 <- glm(arsenic ~ -1+year, family=gaussian(link="identity"), data=c8long)
summary(fit1)
mean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                            summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                            summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

colnames(quant)[1] <- "CWS 2009-2011"
Time2<-rbind(nav,nmiss,quant,nmrl,nmcl,gm,mean)
rownames(Time2)[1] <- "N";rownames(Time2)[2] <- "N missing";rownames(Time2)[12] <- "% below MRL"
rownames(Time2)[13] <- "% above MCL";rownames(Time2)[14] <- "Geometric mean";rownames(Time2)[15] <- "Arithmetic mean"

# Mean difference models
T1<-"0 (reference)"
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=c8long)
summary(fit1)
T2<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))

mds<-cbind(T1,T2)
colnames(mds)[1]<-"CWS 2006-2008";colnames(mds)[2]<-"CWS 2009-2011"
c8county<-cbind(Time1,Time2)
c8county <-round(c8county[,],2)
c8county<-rbind(c8county,mds)
rownames(c8county)[16]<-"Mean difference (95% CI)"
c8county <- rbind(c8county, Cluster= c("c8","c8"))


STable9<-cbind(c1county,c2county,c3county,c4county,c5county,c6county,c7county,c8county)


write.xlsx(STable9,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/Excel Table S3.xlsx")




### end




####### 11. Excel Table S5A. Characteristics of counties by compliance categories using 10 ug/L cut-point ######

### County-level 
t<-county[which(!is.na(county$group10)),]
dim(t)

### All counties
t<-county[which(!is.na(county$group10)),]
dim(t)
alln<-nrow(t)

col1<-t[which(t$group10>0),]
n<-nrow(col1)

#-- Population Served
fit <- glm(PubWaterPop~1,
           family=gaussian(link="identity"), data=col1)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col1[which(col1$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col1[which(col1$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col1[which(col1$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col1[which(col1$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col1[which(col1$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col1[which(col1$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col1[which(col1$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col1[which(col1$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))



#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col1$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

#-- By County cluster
nc1<-nrow(col1[which(col1$cluster.number==1),])
nc1pr<-round(100*(nc1/n),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col1[which(col1$cluster.number==2),])
nc2pr<-round(100*(nc2/n),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col1[which(col1$cluster.number==3),])
nc3pr<-round(100*(nc3/n),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col1[which(col1$cluster.number==4),])
nc4pr<-round(100*(nc4/n),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col1[which(col1$cluster.number==5),])
nc5pr<-round(100*(nc5/n),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col1[which(col1$cluster.number==6),])
nc6pr<-round(100*(nc6/n),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col1[which(col1$cluster.number==7),])
nc7pr<-round(100*(nc7/n),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col1[which(col1$cluster.number==8),])
nc8pr<-round(100*(nc8/n),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cwsall<-rbind(n,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,state1,state2,state3,
              npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8)
cwsall <- rbind(cwsall, Cateogry= c("All Counties"))

### Cateogry 1 (low/low)
col2<-t[which(t$group10==1),]
n<-nrow(col2)
np<-round(100*(n/alln),1)
npp<-to.n.prop(c(n,np))

#-- Population Served
fit <- glm(PubWaterPop~1,
           family=gaussian(link="identity"), data=col2)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col2[which(col2$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col2[which(col2$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col2[which(col2$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col2[which(col2$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col2[which(col2$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col2[which(col2$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col2[which(col2$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col2[which(col2$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col2$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

#-- By County cluster
nc1<-nrow(col2[which(col2$cluster.number==1),])
nc1pr<-round(100*(nc1/n),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col2[which(col2$cluster.number==2),])
nc2pr<-round(100*(nc2/n),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col2[which(col2$cluster.number==3),])
nc3pr<-round(100*(nc3/n),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col2[which(col2$cluster.number==4),])
nc4pr<-round(100*(nc4/n),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col2[which(col2$cluster.number==5),])
nc5pr<-round(100*(nc5/n),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col2[which(col2$cluster.number==6),])
nc6pr<-round(100*(nc6/n),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col2[which(col2$cluster.number==7),])
nc7pr<-round(100*(nc7/n),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col2[which(col2$cluster.number==8),])
nc8pr<-round(100*(nc8/n),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cat1<-rbind(n,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,state1,state2,state3,
            npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8)

cat1 <- rbind(cat1, Cateogry= c("Category 1, Low/Low"))

### Category 3 /Column4 Low/High
col4<-t[which(t$group10==2),]
n<-nrow(col4)
np<-round(100*(n/alln),1)
npp<-to.n.prop(c(n,np))

#-- Population Served
fit <- glm(PubWaterPop~1,
           family=gaussian(link="identity"), data=col4)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col4[which(col4$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col4[which(col4$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col4[which(col4$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col4[which(col4$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col4[which(col4$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col4[which(col4$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col4[which(col4$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col4[which(col4$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col4$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

#-- By County cluster
nc1<-nrow(col4[which(col4$cluster.number==1),])
nc1pr<-round(100*(nc1/n),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col4[which(col4$cluster.number==2),])
nc2pr<-round(100*(nc2/n),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col4[which(col4$cluster.number==3),])
nc3pr<-round(100*(nc3/n),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col4[which(col4$cluster.number==4),])
nc4pr<-round(100*(nc4/n),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col4[which(col4$cluster.number==5),])
nc5pr<-round(100*(nc5/n),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col4[which(col4$cluster.number==6),])
nc6pr<-round(100*(nc6/n),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col4[which(col4$cluster.number==7),])
nc7pr<-round(100*(nc7/n),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col4[which(col4$cluster.number==8),])
nc8pr<-round(100*(nc8/n),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cat3<-rbind(n,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,state1,state2,state3,
            npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8)

cat3 <- rbind(cat3, Cateogry= c("Category 3, Low/High"))

### Category 2/Column 3 (High/Low)
col3<-t[which(t$group10==3),]
n<-nrow(col3)
np<-round(100*(n/alln),1)
npp<-to.n.prop(c(n,np))

#-- Population Served
fit <- glm(PubWaterPop~1,
           family=gaussian(link="identity"), data=col3)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col3[which(col3$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col3[which(col3$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col3[which(col3$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col3[which(col3$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col3[which(col3$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col3[which(col3$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col3[which(col3$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col3[which(col3$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col3$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

#-- By County cluster
nc1<-nrow(col3[which(col3$cluster.number==1),])
nc1pr<-round(100*(nc1/n),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col3[which(col3$cluster.number==2),])
nc2pr<-round(100*(nc2/n),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col3[which(col3$cluster.number==3),])
nc3pr<-round(100*(nc3/n),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col3[which(col3$cluster.number==4),])
nc4pr<-round(100*(nc4/n),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col3[which(col3$cluster.number==5),])
nc5pr<-round(100*(nc5/n),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col3[which(col3$cluster.number==6),])
nc6pr<-round(100*(nc6/n),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col3[which(col3$cluster.number==7),])
nc7pr<-round(100*(nc7/n),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col3[which(col3$cluster.number==8),])
nc8pr<-round(100*(nc8/n),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cat2<-rbind(n,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,state1,state2,state3,
            npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8)

cat2 <- rbind(cat2, Cateogry= c("Category 2, High/Low"))

### Category 4/Column 5 (High/High)
col5<-t[which(t$group10==4),]
n<-nrow(col5)
np<-round(100*(n/alln),1)
npp<-to.n.prop(c(n,np))

#-- Population Served
fit <- glm(PubWaterPop~1,
           family=gaussian(link="identity"), data=col5)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col5[which(col5$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col5[which(col5$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col5[which(col5$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col5[which(col5$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col5[which(col5$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col5[which(col5$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col5[which(col5$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col5[which(col5$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col5$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

#-- By County cluster
nc1<-nrow(col5[which(col5$cluster.number==1),])
nc1pr<-round(100*(nc1/n),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col5[which(col5$cluster.number==2),])
nc2pr<-round(100*(nc2/n),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col5[which(col5$cluster.number==3),])
nc3pr<-round(100*(nc3/n),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col5[which(col5$cluster.number==4),])
nc4pr<-round(100*(nc4/n),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col5[which(col5$cluster.number==5),])
nc5pr<-round(100*(nc5/n),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col5[which(col5$cluster.number==6),])
nc6pr<-round(100*(nc6/n),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col5[which(col5$cluster.number==7),])
nc7pr<-round(100*(nc7/n),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col5[which(col5$cluster.number==8),])
nc8pr<-round(100*(nc8/n),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cat4<-rbind(n,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,state1,state2,state3,
            npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8)

cat4 <- rbind(cat4, Cateogry= c("Category 4, High/High"))

STable10<-cbind(cwsall,cat1,cat2,cat3,cat4)
write.xlsx(STable10,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/Excel Table S5A.xlsx")



### end


####### 12. Excel Table S5B. Characteristics of counties by compliance category using 5 ug/L cut-points #####

### County-level 
t<-county[which(!is.na(county$group5)),]
dim(t)

### All counties
t<-county[which(!is.na(county$group5)),]
dim(t)
alln<-nrow(t)

col1<-t[which(t$group5>0),]
n<-nrow(col1)

#-- Population Served
fit <- glm(PubWaterPop~1,
           family=gaussian(link="identity"), data=col1)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col1[which(col1$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col1[which(col1$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col1[which(col1$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col1[which(col1$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col1[which(col1$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col1[which(col1$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col1[which(col1$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col1[which(col1$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))



#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col1$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

#-- By County cluster
nc1<-nrow(col1[which(col1$cluster.number==1),])
nc1pr<-round(100*(nc1/n),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col1[which(col1$cluster.number==2),])
nc2pr<-round(100*(nc2/n),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col1[which(col1$cluster.number==3),])
nc3pr<-round(100*(nc3/n),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col1[which(col1$cluster.number==4),])
nc4pr<-round(100*(nc4/n),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col1[which(col1$cluster.number==5),])
nc5pr<-round(100*(nc5/n),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col1[which(col1$cluster.number==6),])
nc6pr<-round(100*(nc6/n),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col1[which(col1$cluster.number==7),])
nc7pr<-round(100*(nc7/n),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col1[which(col1$cluster.number==8),])
nc8pr<-round(100*(nc8/n),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cwsall<-rbind(n,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,state1,state2,state3,
              npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8)
cwsall <- rbind(cwsall, Cateogry= c("All Counties"))

### Cateogry 1 (low/low)
col2<-t[which(t$group5==1),]
n<-nrow(col2)
np<-round(100*(n/alln),1)
npp<-to.n.prop(c(n,np))

#-- Population Served
fit <- glm(PubWaterPop~1,
           family=gaussian(link="identity"), data=col2)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col2[which(col2$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col2[which(col2$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col2[which(col2$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col2[which(col2$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col2[which(col2$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col2[which(col2$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col2[which(col2$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col2[which(col2$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col2$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

#-- By County cluster
nc1<-nrow(col2[which(col2$cluster.number==1),])
nc1pr<-round(100*(nc1/n),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col2[which(col2$cluster.number==2),])
nc2pr<-round(100*(nc2/n),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col2[which(col2$cluster.number==3),])
nc3pr<-round(100*(nc3/n),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col2[which(col2$cluster.number==4),])
nc4pr<-round(100*(nc4/n),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col2[which(col2$cluster.number==5),])
nc5pr<-round(100*(nc5/n),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col2[which(col2$cluster.number==6),])
nc6pr<-round(100*(nc6/n),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col2[which(col2$cluster.number==7),])
nc7pr<-round(100*(nc7/n),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col2[which(col2$cluster.number==8),])
nc8pr<-round(100*(nc8/n),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cat1<-rbind(n,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,state1,state2,state3,
            npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8)

cat1 <- rbind(cat1, Cateogry= c("Category 1, Low/Low"))

### Category 3 /Column4 Low/High
col4<-t[which(t$group5==2),]
n<-nrow(col4)
np<-round(100*(n/alln),1)
npp<-to.n.prop(c(n,np))

#-- Population Served
fit <- glm(PubWaterPop~1,
           family=gaussian(link="identity"), data=col4)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col4[which(col4$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col4[which(col4$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col4[which(col4$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col4[which(col4$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col4[which(col4$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col4[which(col4$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col4[which(col4$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col4[which(col4$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col4$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

#-- By County cluster
nc1<-nrow(col4[which(col4$cluster.number==1),])
nc1pr<-round(100*(nc1/n),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col4[which(col4$cluster.number==2),])
nc2pr<-round(100*(nc2/n),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col4[which(col4$cluster.number==3),])
nc3pr<-round(100*(nc3/n),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col4[which(col4$cluster.number==4),])
nc4pr<-round(100*(nc4/n),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col4[which(col4$cluster.number==5),])
nc5pr<-round(100*(nc5/n),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col4[which(col4$cluster.number==6),])
nc6pr<-round(100*(nc6/n),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col4[which(col4$cluster.number==7),])
nc7pr<-round(100*(nc7/n),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col4[which(col4$cluster.number==8),])
nc8pr<-round(100*(nc8/n),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cat3<-rbind(n,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,state1,state2,state3,
            npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8)

cat3 <- rbind(cat3, Cateogry= c("Category 3, Low/High"))

### Category 2/Column 3 (High/Low)
col3<-t[which(t$group5==3),]
n<-nrow(col3)
np<-round(100*(n/alln),1)
npp<-to.n.prop(c(n,np))

#-- Population Served
fit <- glm(PubWaterPop~1,
           family=gaussian(link="identity"), data=col3)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col3[which(col3$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col3[which(col3$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col3[which(col3$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col3[which(col3$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col3[which(col3$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col3[which(col3$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col3[which(col3$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col3[which(col3$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col3$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

#-- By County cluster
nc1<-nrow(col3[which(col3$cluster.number==1),])
nc1pr<-round(100*(nc1/n),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col3[which(col3$cluster.number==2),])
nc2pr<-round(100*(nc2/n),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col3[which(col3$cluster.number==3),])
nc3pr<-round(100*(nc3/n),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col3[which(col3$cluster.number==4),])
nc4pr<-round(100*(nc4/n),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col3[which(col3$cluster.number==5),])
nc5pr<-round(100*(nc5/n),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col3[which(col3$cluster.number==6),])
nc6pr<-round(100*(nc6/n),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col3[which(col3$cluster.number==7),])
nc7pr<-round(100*(nc7/n),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col3[which(col3$cluster.number==8),])
nc8pr<-round(100*(nc8/n),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cat2<-rbind(n,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,state1,state2,state3,
            npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8)

cat2 <- rbind(cat2, Cateogry= c("Category 2, High/Low"))

### Category 4/Column 5 (High/High)
col5<-t[which(t$group5==4),]
n<-nrow(col5)
np<-round(100*(n/alln),1)
npp<-to.n.prop(c(n,np))

#-- Population Served
fit <- glm(PubWaterPop~1,
           family=gaussian(link="identity"), data=col5)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col5[which(col5$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col5[which(col5$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col5[which(col5$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col5[which(col5$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col5[which(col5$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col5[which(col5$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col5[which(col5$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col5[which(col5$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col5$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

#-- By County cluster
nc1<-nrow(col5[which(col5$cluster.number==1),])
nc1pr<-round(100*(nc1/n),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col5[which(col5$cluster.number==2),])
nc2pr<-round(100*(nc2/n),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col5[which(col5$cluster.number==3),])
nc3pr<-round(100*(nc3/n),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col5[which(col5$cluster.number==4),])
nc4pr<-round(100*(nc4/n),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col5[which(col5$cluster.number==5),])
nc5pr<-round(100*(nc5/n),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col5[which(col5$cluster.number==6),])
nc6pr<-round(100*(nc6/n),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col5[which(col5$cluster.number==7),])
nc7pr<-round(100*(nc7/n),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col5[which(col5$cluster.number==8),])
nc8pr<-round(100*(nc8/n),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cat4<-rbind(n,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,state1,state2,state3,
            npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8)

cat4 <- rbind(cat4, Cateogry= c("Category 4, High/High"))

STable11<-cbind(cwsall,cat1,cat2,cat3,cat4)
write.xlsx(STable11,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/EHP resubmissoin/July 2020 R0 EHP/output/output/Excel Table S5B.xlsx")



### end

####### 13. Excel Table S5C. Characteristics of counties by compliance categories using 1 ug/L cut-point ##### 

### County-level 
t<-county[which(!is.na(county$group1)),]
dim(t)

### All counties
t<-county[which(!is.na(county$group1)),]
dim(t)
alln<-nrow(t)

col1<-t[which(t$group1>0),]
n<-nrow(col1)

#-- Population Served
fit <- glm(PubWaterPop~1,
           family=gaussian(link="identity"), data=col1)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col1[which(col1$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col1[which(col1$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col1[which(col1$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col1[which(col1$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col1[which(col1$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col1[which(col1$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col1[which(col1$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col1[which(col1$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))



#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col1$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

#-- By County cluster
nc1<-nrow(col1[which(col1$cluster.number==1),])
nc1pr<-round(100*(nc1/n),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col1[which(col1$cluster.number==2),])
nc2pr<-round(100*(nc2/n),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col1[which(col1$cluster.number==3),])
nc3pr<-round(100*(nc3/n),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col1[which(col1$cluster.number==4),])
nc4pr<-round(100*(nc4/n),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col1[which(col1$cluster.number==5),])
nc5pr<-round(100*(nc5/n),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col1[which(col1$cluster.number==6),])
nc6pr<-round(100*(nc6/n),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col1[which(col1$cluster.number==7),])
nc7pr<-round(100*(nc7/n),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col1[which(col1$cluster.number==8),])
nc8pr<-round(100*(nc8/n),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cwsall<-rbind(n,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,state1,state2,state3,
              npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8)
cwsall <- rbind(cwsall, Cateogry= c("All Counties"))

### Cateogry 1 (low/low)
col2<-t[which(t$group1==1),]
n<-nrow(col2)
np<-round(100*(n/alln),1)
npp<-to.n.prop(c(n,np))

#-- Population Served
fit <- glm(PubWaterPop~1,
           family=gaussian(link="identity"), data=col2)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col2[which(col2$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col2[which(col2$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col2[which(col2$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col2[which(col2$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col2[which(col2$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col2[which(col2$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col2[which(col2$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col2[which(col2$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col2$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

#-- By County cluster
nc1<-nrow(col2[which(col2$cluster.number==1),])
nc1pr<-round(100*(nc1/n),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col2[which(col2$cluster.number==2),])
nc2pr<-round(100*(nc2/n),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col2[which(col2$cluster.number==3),])
nc3pr<-round(100*(nc3/n),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col2[which(col2$cluster.number==4),])
nc4pr<-round(100*(nc4/n),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col2[which(col2$cluster.number==5),])
nc5pr<-round(100*(nc5/n),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col2[which(col2$cluster.number==6),])
nc6pr<-round(100*(nc6/n),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col2[which(col2$cluster.number==7),])
nc7pr<-round(100*(nc7/n),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col2[which(col2$cluster.number==8),])
nc8pr<-round(100*(nc8/n),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cat1<-rbind(n,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,state1,state2,state3,
            npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8)

cat1 <- rbind(cat1, Cateogry= c("Category 1, Low/Low"))

### Category 3 /Column4 Low/High
col4<-t[which(t$group1==2),]
n<-nrow(col4)
np<-round(100*(n/alln),1)
npp<-to.n.prop(c(n,np))

#-- Population Served
fit <- glm(PubWaterPop~1,
           family=gaussian(link="identity"), data=col4)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col4[which(col4$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col4[which(col4$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col4[which(col4$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col4[which(col4$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col4[which(col4$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col4[which(col4$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col4[which(col4$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col4[which(col4$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col4$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

#-- By County cluster
nc1<-nrow(col4[which(col4$cluster.number==1),])
nc1pr<-round(100*(nc1/n),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col4[which(col4$cluster.number==2),])
nc2pr<-round(100*(nc2/n),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col4[which(col4$cluster.number==3),])
nc3pr<-round(100*(nc3/n),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col4[which(col4$cluster.number==4),])
nc4pr<-round(100*(nc4/n),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col4[which(col4$cluster.number==5),])
nc5pr<-round(100*(nc5/n),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col4[which(col4$cluster.number==6),])
nc6pr<-round(100*(nc6/n),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col4[which(col4$cluster.number==7),])
nc7pr<-round(100*(nc7/n),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col4[which(col4$cluster.number==8),])
nc8pr<-round(100*(nc8/n),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cat3<-rbind(n,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,state1,state2,state3,
            npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8)

cat3 <- rbind(cat3, Cateogry= c("Category 3, Low/High"))

### Category 2/Column 3 (High/Low)
col3<-t[which(t$group1==3),]
n<-nrow(col3)
np<-round(100*(n/alln),1)
npp<-to.n.prop(c(n,np))

#-- Population Served
fit <- glm(PubWaterPop~1,
           family=gaussian(link="identity"), data=col3)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col3[which(col3$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col3[which(col3$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col3[which(col3$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col3[which(col3$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col3[which(col3$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col3[which(col3$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col3[which(col3$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col3[which(col3$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col3$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

#-- By County cluster
nc1<-nrow(col3[which(col3$cluster.number==1),])
nc1pr<-round(100*(nc1/n),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col3[which(col3$cluster.number==2),])
nc2pr<-round(100*(nc2/n),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col3[which(col3$cluster.number==3),])
nc3pr<-round(100*(nc3/n),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col3[which(col3$cluster.number==4),])
nc4pr<-round(100*(nc4/n),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col3[which(col3$cluster.number==5),])
nc5pr<-round(100*(nc5/n),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col3[which(col3$cluster.number==6),])
nc6pr<-round(100*(nc6/n),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col3[which(col3$cluster.number==7),])
nc7pr<-round(100*(nc7/n),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col3[which(col3$cluster.number==8),])
nc8pr<-round(100*(nc8/n),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cat2<-rbind(n,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,state1,state2,state3,
            npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8)

cat2 <- rbind(cat2, Cateogry= c("Category 2, High/Low"))

### Category 4/Column 5 (High/High)
col5<-t[which(t$group1==4),]
n<-nrow(col5)
np<-round(100*(n/alln),1)
npp<-to.n.prop(c(n,np))

#-- Population Served
fit <- glm(PubWaterPop~1,
           family=gaussian(link="identity"), data=col5)
mean<-round(summary(fit)$coeff[,1],0)
se<-round(summary(fit)$coeff[,2],0)
pops<-to.mean.se(c(mean,se))

#-- By Region
nAKHI<-nrow(col5[which(col5$Region=="AKHI"),])
nAKHIpr<-round(100*(nAKHI/n),1)
npAKHI<-to.n.prop(c(nAKHI,nAKHIpr))
nCMW<-nrow(col5[which(col5$Region=="CMW"),])
nCMWpr<-round(100*(nCMW/n),1)
npCMW<-to.n.prop(c(nCMW,nCMWpr))
nEMW<-nrow(col5[which(col5$Region=="EMW"),])
nEMWpr<-round(100*(nEMW/n),1)
npEMW<-to.n.prop(c(nEMW,nEMWpr))
nMA<-nrow(col5[which(col5$Region=="MA"),])
nMApr<-round(100*(nMA/n),1)
npMA<-to.n.prop(c(nMA,nMApr))
nNE<-nrow(col5[which(col5$Region=="NE"),])
nNEpr<-round(100*(nNE/n),1)
npNE<-to.n.prop(c(nNE,nNEpr))
nPNW<-nrow(col5[which(col5$Region=="PNW"),])
nPNWpr<-round(100*(nPNW/n),1)
npPNW<-to.n.prop(c(nPNW,nPNWpr))
nSE<-nrow(col5[which(col5$Region=="SE"),])
nSEpr<-round(100*(nSE/n),1)
npSE<-to.n.prop(c(nSE,nSEpr))
nSW<-nrow(col5[which(col5$Region=="SW"),])
nSWpr<-round(100*(nSW/n),1)
npSW<-to.n.prop(c(nSW,nSWpr))

#-- Top 3 states represented-#
tab<-as.data.frame(sort(table(col5$State.Code),decreasing=TRUE)[1:3])
tab$Var1<-as.character(tab$Var1)
state1<-to.mean.se(c(tab[1,1],tab[1,2]))
state2<-to.mean.se(c(tab[2,1],tab[2,2]))
state3<-to.mean.se(c(tab[3,1],tab[3,2]))

#-- By County cluster
nc1<-nrow(col5[which(col5$cluster.number==1),])
nc1pr<-round(100*(nc1/n),1)
npc1<-to.n.prop(c(nc1,nc1pr))
nc2<-nrow(col5[which(col5$cluster.number==2),])
nc2pr<-round(100*(nc2/n),1)
npc2<-to.n.prop(c(nc2,nc2pr))
nc3<-nrow(col5[which(col5$cluster.number==3),])
nc3pr<-round(100*(nc3/n),1)
npc3<-to.n.prop(c(nc3,nc3pr))
nc4<-nrow(col5[which(col5$cluster.number==4),])
nc4pr<-round(100*(nc4/n),1)
npc4<-to.n.prop(c(nc4,nc4pr))
nc5<-nrow(col5[which(col5$cluster.number==5),])
nc5pr<-round(100*(nc5/n),1)
npc5<-to.n.prop(c(nc5,nc5pr))
nc6<-nrow(col5[which(col5$cluster.number==6),])
nc6pr<-round(100*(nc6/n),1)
npc6<-to.n.prop(c(nc6,nc6pr))
nc7<-nrow(col5[which(col5$cluster.number==7),])
nc7pr<-round(100*(nc7/n),1)
npc7<-to.n.prop(c(nc7,nc7pr))
nc8<-nrow(col5[which(col5$cluster.number==8),])
nc8pr<-round(100*(nc8/n),1)
npc8<-to.n.prop(c(nc8,nc8pr))


cat4<-rbind(n,pops,npAKHI,npCMW,npEMW,npMA,npNE,npPNW,npSE,npSW,state1,state2,state3,
            npc1,npc2,npc3,npc4,npc5,npc6,npc7,npc8)

cat4 <- rbind(cat4, Cateogry= c("Category 4, High/High"))

STable12<-cbind(cwsall,cat1,cat2,cat3,cat4)
write.xlsx(STable12,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/EHP resubmissoin/July 2020 R0 EHP/output/output/Excel Table S5C.xlsx")


### end


####### 14. Excel Tables S6 and S7: Counties in High/High and High/Low compliance categories ######
highhigh<-county[which(county$group10==4),];nrow(highhigh) #18
payette<-cws[which(cws$CountyFIPS=="16075"),]
rawlins<-cws[which(cws$CountyFIPS=="20153"),]
lapeer<-cws[which(cws$CountyFIPS=="26087"),]
dundy<-cws[which(cws$CountyFIPS=="31057"),]
polk<-cws[which(cws$CountyFIPS=="31143"),]
esmeralda<-cws[which(cws$CountyFIPS=="32009"),]
eureka<-cws[which(cws$CountyFIPS=="32011"),]
lander<-cws[which(cws$CountyFIPS=="32015"),]
lincoln<-cws[which(cws$CountyFIPS=="32017"),]
socorro<-cws[which(cws$CountyFIPS=="35053"),]
ramsey<-cws[which(cws$CountyFIPS=="38071"),]
andrews<-cws[which(cws$CountyFIPS=="48003"),]
crane<-cws[which(cws$CountyFIPS=="48103"),]
duval<-cws[which(cws$CountyFIPS=="48131"),]
falls<-cws[which(cws$CountyFIPS=="48145"),]
gaines<-cws[which(cws$CountyFIPS=="48165"),]
hudspeth<-cws[which(cws$CountyFIPS=="48229"),]
jimhogg<-cws[which(cws$CountyFIPS=="48247"),]
hh<-rbind(payette,rawlins,lapeer,dundy,polk,esmeralda,eureka,lander,lincoln,socorro,ramsey,andrews,crane,duval,falls,gaines,hudspeth,jimhogg)
myvars<-c("CountyFIPS","Full.Name","PWSID","Adjusted.Total.Population.Served","ThreeYearAs20062008","ThreeYearAs20092011","Region")
STable13<-hh[myvars]
write.xlsx(STable13,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/output/Excel Tables S6.xlsx")



highlow<-county[which(county$group10==3),];nrow(highlow) #12
yavapai<-cws[which(cws$CountyFIPS=="04025"),]
scott<-cws[which(cws$CountyFIPS=="20171"),]
kalamazoo<-cws[which(cws$CountyFIPS=="26077"),]
ogemaw<-cws[which(cws$CountyFIPS=="26129"),]
boxbutte<-cws[which(cws$CountyFIPS=="31013"),]
deuel<-cws[which(cws$CountyFIPS=="31049"),]
lyon<-cws[which(cws$CountyFIPS=="32019"),]
sandoval<-cws[which(cws$CountyFIPS=="35043"),]
valencia<-cws[which(cws$CountyFIPS=="35061"),]
borden<-cws[which(cws$CountyFIPS=="48033"),]
millard<-cws[which(cws$CountyFIPS=="49027"),]
sanjuan<-cws[which(cws$CountyFIPS=="49037"),]
hl<-rbind(yavapai,scott,kalamazoo,ogemaw,boxbutte,deuel,lyon,sandoval,valencia,borden,millard,sanjuan)
myvars<-c("CountyFIPS","Full.Name","PWSID","Adjusted.Total.Population.Served","ThreeYearAs20062008","ThreeYearAs20092011","Region")
STable14<-hl[myvars]
write.xlsx(STable14,row.names=T, file="~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output/Excel Tables S7.xlsx")





### end





####------------------  FIGURES  ---------------------####
###end

####### 1. Figure 1: Raincloud plots by region: 2006-2008 ######

## Define geom_flat_violin fxn
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(gridExtra)
library(grid)


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )


## Example:
ggplot(diamonds, aes(cut, carat)) +
  geom_flat_violin() +
  coord_flip()
### end 

### Raincloud plots
## reference: https://micahallen.org/2018/03/15/introducing-raincloud-plots/
# First define pretty theme
raincloud_theme = theme(
  text = element_text(size = 14),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(hjust=0.5,lineheight=.8, face="bold", size = 18),
  panel.border = element_blank(),
  #panel.grid.minor = element_blank(),
  #panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))



rcdata<-cws

# Look at some color options
#display.brewer.pal(n = 8, name = 'Paired')
#brewer.pal(n = 8, name = 'Paired') 

# Four lowest:
#ccebc5 , #7bccc4 , #4eb3d3 , #08589e
# Four highest
#fee391 , #fe9929 , #cc4c02 , #8c2d04

# Create some new palettes of 4 each
pal1<-c("#ccebc5","#7bccc4","#2b8cbe","#08589e")
pal1a<-c("#08589e","#7bccc4","#2b8cbe","#ccebc5") # re-ordered for plotting
pal1a2<-c("#08589e","#7bccc4","#2b8cbe","#ccebc5","#525252") # re-ordered for plotting
pal2<-c("#fee391","#fe9929","#cc4c02","#8c2d04")
pal2a<-c("#cc4c02","#fee391","#fe9929","#8c2d04") # re-ordered for plotting

plot1pal<-c("#08589e","#2b8cbe","#7bccc4","#ccebc5") # SE, MA, CMW, EMW
plot2pal<-c("#fee391","#fe9929","#cc4c02","#8c2d04") # NE, PNW, AKHI, SW


# What are 3-year (2006-2009) means to plot?
mean(rcdata$ThreeYearAs20062008[which(cws$Region=="AKHI")],na.rm=T,) #2.17   #7   Plot2
mean(rcdata$ThreeYearAs20062008[which(cws$Region=="CMW")],na.rm=T,) #1.94    #3  Plot1
mean(rcdata$ThreeYearAs20062008[which(cws$Region=="EMW")],na.rm=T,) #2.03    #4  Plot1
mean(rcdata$ThreeYearAs20062008[which(cws$Region=="MA")],na.rm=T,) #0.88     #2  Plot1
mean(rcdata$ThreeYearAs20062008[which(cws$Region=="NE")],na.rm=T,) #2.05     #5  Plot2
mean(rcdata$ThreeYearAs20062008[which(cws$Region=="PNW")],na.rm=T,) #2.15    #6   Plot2
mean(rcdata$ThreeYearAs20062008[which(cws$Region=="SE")],na.rm=T,) #0.66     #1   Plot1
mean(rcdata$ThreeYearAs20062008[which(cws$Region=="SW")],na.rm=T,) #3.59     #8   Plot2

# Pull 6-yr averages per region with CIs: 
fit1 <- glm(ThreeYearAs20062008 ~-1+as.factor(Region), family=gaussian(link="identity"), data=rcdata)
summary(fit1)
akmean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                          summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                          summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

cmwmean<-convert.to.ci(round(c(summary(fit1)$coeff[2,1],
                          summary(fit1)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2],
                          summary(fit1)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[2,2]),2))
emwmean<-convert.to.ci(round(c(summary(fit1)$coeff[3,1],
                          summary(fit1)$coeff[3,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[3,2],
                          summary(fit1)$coeff[3,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[3,2]),2))
mamean<-convert.to.ci(round(c(summary(fit1)$coeff[4,1],
                          summary(fit1)$coeff[4,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[4,2],
                          summary(fit1)$coeff[4,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[4,2]),2))
nemean<-convert.to.ci(round(c(summary(fit1)$coeff[5,1],
                          summary(fit1)$coeff[5,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[5,2],
                          summary(fit1)$coeff[5,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[5,2]),2))
pnwmean<-convert.to.ci(round(c(summary(fit1)$coeff[6,1],
                          summary(fit1)$coeff[6,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[6,2],
                          summary(fit1)$coeff[6,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[6,2]),2))
semean<-convert.to.ci(round(c(summary(fit1)$coeff[7,1],
                          summary(fit1)$coeff[7,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[7,2],
                          summary(fit1)$coeff[7,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[7,2]),2))
swmean<-convert.to.ci(round(c(summary(fit1)$coeff[8,1],
                          summary(fit1)$coeff[8,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[8,2],
                          summary(fit1)$coeff[8,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[8,2]),2))



folder.output <- "~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output"
if( !file.exists( folder.output ) ) {
  dir.create( file.path( folder.output ) )
}

rcdata$Region[which(rcdata$Region=="AKHI")]<-"Alaska/Hawaii"
rcdata$Region[which(rcdata$Region=="CMW")]<-"Central Midwest"
rcdata$Region[which(rcdata$Region=="EMW")]<-"Eastern Midwest"
rcdata$Region[which(rcdata$Region=="MA")]<-"Mid-Atlantic"
rcdata$Region[which(rcdata$Region=="NE")]<-"New England"
rcdata$Region[which(rcdata$Region=="PNW")]<-"Pacific Northwest"
rcdata$Region[which(rcdata$Region=="SE")]<-"Southeast"
rcdata$Region[which(rcdata$Region=="SW")]<-"Southwest"
pdata1<-rcdata[which(rcdata$Region=="Southeast"|rcdata$Region=="Mid-Atlantic"|rcdata$Region=="Central Midwest"|rcdata$Region=="Eastern Midwest"),]
pdata2<-rcdata[which(rcdata$Region=="New England"|rcdata$Region=="Pacific Northwest"|rcdata$Region=="Alaska/Hawaii"|rcdata$Region=="Southwest"),]
pdata1$Region <- factor(pdata1$Region, levels = c("Southeast","Mid-Atlantic", "Central Midwest", "Eastern Midwest"))
pdata2$Region <- factor(pdata2$Region, levels = c("New England","Pacific Northwest", "Alaska/Hawaii", "Southwest"))


table(rcdata$Region)
rcdata$Region<-as.factor(rcdata$Region)
# Order:P1:  SE, MA, CMW, EMW; P2:  NE, PNW, AKHI, SW

pdf(file=paste0( folder.output , "/Figure 1. Raincloud plots, 20062008.pdf"), 23, 12)
p1<-ggplot(data = pdata1, aes(y = log(ThreeYearAs20062008), x = fct_relevel(as.factor(Region),"Eastern Midwest" ,"Central Midwest", "Mid-Atlantic","Southeast"), fill = as.factor(Region))) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8)+
  geom_point(aes(y = log(ThreeYearAs20062008), color = as.factor(Region)), position = position_jitter(width = .1), size = 2, alpha = 0.8) +
  geom_boxplot(width = .17,outlier.shape = NA, alpha = 0.5,lwd=1) +
  geom_hline(yintercept=2.302585,linetype = "dotdash", size=0.75,color="tomato4")+
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_manual(values = plot1pal) +
  scale_fill_manual(values = plot1pal) +
  coord_flip() +
  theme_bw() +
  theme(axis.line = element_line(), panel.border = element_blank())+
  raincloud_theme+
  scale_y_continuous(limits=c(-1.61,4), breaks=c(-1.61,-0.488, 0.634,1.756,2.302585,2.878,4.00 ) , labels =c("0.2","0.6","1.9","5.8","10.0","17.8","54.6") )+
  labs(y =expression(paste("Water arsenic concentration, µg/L (log scale)")), x = "")+
  scale_x_discrete(labels = c("Eastern Midwest\n(N=6,063)","Central Midwest\n(N=2,648)","Mid-Atlantic\n(N=4,879)","Southeast\n(N=7,795)"))+
  annotate("pointrange", x = 4.0, y = log(0.66), ymin = log(0.66), ymax = log(0.66), colour = "black", fill="white", size = 1.5,shape=21)+ # SE mean
  annotate("pointrange", x = 3.0, y = log(0.88), ymin = log(0.88), ymax = log(0.88), colour = "black", fill="white", size = 1.5,shape=21)+ # MA mean
  annotate("pointrange", x = 2.0, y = log(1.94), ymin = log(1.94), ymax = log(1.94), colour = "black",fill="white", size = 1.5,shape=21)+ # CMW mean
  annotate("pointrange", x = 1.0, y = log(2.03), ymin = log(2.03), ymax = log(2.03), colour = "black",fill="white", size = 1.5,shape=21)+ #EMW mean
  annotate(geom="text", x=4.5, y=0.9, label=expression(paste("Mean: 0.66 (0.54, 0.79) µg/L")),color="black",size=7,family ="Avenir Next")+ #SE 
  annotate(geom="text", x=3.5, y=0.9, label=expression(paste("Mean 0.88 (0.73, 1.03) µg/L")),color="black",size=7,family ="Avenir Next")+ # MA 
  annotate(geom="text", x=2.5, y=0.9, label=expression(paste("Mean 1.94 (1.72, 2.15) µg/L")),color="black",size=7,family ="Avenir Next")+ #  CMW
  annotate(geom="text", x=1.5, y=0.9, label=expression(paste("Mean 2.03 (1.88, 2.18) µg/L")),color="black",size=7,family ="Avenir Next")+ # EMW  
  annotate(geom="text", x=0.5, y=3.2, label=expression(bold(paste("Maximum\ncontaminant level"))),color="tomato4",size=7,family ="Avenir Next")+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))


p2<-ggplot(data = pdata2, aes(y = log(ThreeYearAs20062008), x = fct_relevel(as.factor(Region), "Southwest","Alaska/Hawaii","Pacific Northwest","New England"), fill = Region)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = log(ThreeYearAs20062008), color = as.factor(Region)), position = position_jitter(width = .1), size = 2, alpha = 0.8) +
  geom_boxplot(width = .17,outlier.shape = NA, alpha = 0.5,lwd=1) +
  geom_hline(yintercept=2.302585,linetype = "dotdash", size=0.75,color="tomato4")+
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_manual(values = plot2pal) +
  scale_fill_manual(values = plot2pal) +
  coord_flip() +
  theme_bw() +
  theme(axis.line = element_line(), panel.border = element_blank())+
  raincloud_theme+
  scale_y_continuous(limits=c(-1.61,4), breaks=c(-1.61,-0.488, 0.634,1.756,2.302585,2.878,4.00 ) , labels =c("0.2","0.6","1.9","5.8","10.0","17.8","54.6") )+
  labs(y =expression(paste("Water arsenic concentration, ",mu,"g/L (log scale)")), x = "")+
  scale_x_discrete(labels = c("Southwest\n(N=8,382)","Alaska/Hawaii\n (N=482)","Pacific Northwest\n(N=2,648)","New England\n (N=1,729)"))+
  annotate(geom="text", x=4.5, y=0.9, label=expression(paste("Mean 2.05 (1.79, 2.30) µg/L")),color="black",size=7,family ="Avenir Next")+ #NE
  annotate(geom="text", x=3.5, y=0.9, label=expression(paste("Mean 2.15 (1.99, 2.31) µg/L")),color="black",size=7,family ="Avenir Next")+ # PNW
  annotate(geom="text", x=2.5, y=0.9, label=expression(paste("Mean 2.17 (1.67, 2.67) µg/L")),color="black",size=7,family ="Avenir Next")+ # AKHI 
  annotate(geom="text", x=1.5, y=0.9, label=expression(paste("Mean 3.59 (3.46, 3.72) µg/L")),color="black",size=7,family ="Avenir Next")+ # SW 
  annotate(geom="text", x=0.5, y=3.2, label=expression(bold(paste("Maximum\ncontaminant level"))),color="tomato4",size=7,family ="Avenir Next")+
  annotate("pointrange", x = 4, y = log(2.05), ymin = log(2.05), ymax = log(2.05), colour = "black",fill="white", size = 1.5,shape=21)+ # NE
  annotate("pointrange", x = 3, y = log(2.15), ymin = log(2.15), ymax = log(2.15), colour = "black",fill="white", size = 1.5,shape=21)+ # PNW
  annotate("pointrange", x = 2, y = log(2.17), ymin = log(2.17), ymax = log(2.17), colour = "black",fill="white", size = 1.5,shape=21)+ # AKHI
  annotate("pointrange", x = 1, y = log(3.59), ymin = log(3.59), ymax = log(3.59), colour = "black",fill="white", size = 1.5,shape=21)+
  theme(axis.title =element_text(family="Avenir Next",color="black"))+theme(axis.text = element_text(family="Avenir Next",color="black"))+
  theme(axis.text.y = element_text(size=20))+  theme(axis.text.x = element_text(size=20))+theme(axis.title.x = element_text(size=20))

# SW 
fig1<-ggarrange(p1, p2, ncol=2, nrow=1, legend="none")

annotate_figure(fig1, top = text_grob("\nAverage water arsenic concencentrations in CWSs, 2006-2008\n", color = "black", face = "bold", size = 25, family="Avenir Next"))

dev.off()

### end

####### 2. Figures 3 and 4: MAPS   #####
# Files needed for this section: 
# 1. county - CountyAsSYR3.csv
# 2. statenames.csv - csv listing postal codes matching full state names
# 3. fipsnames.csv - csv listing US county FIPS code, state postal code, and county name

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(RColorBrewer)

folder.output <- "~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP/output"
if( !file.exists( folder.output ) ) {
  dir.create( file.path( folder.output ) )
}
setwd("~/Google Drive/Research/County level exposure and outcomes/Maps")


##### August 2020 update: plot all 3 three together with white background and colorblind accessible colors ##
# Old color palette:
#mypal <- c("#C3FA9F","#F6F1A9","#FDC030","#F08080", "#B7B3B3")

# Define colorblind accessible palette
display.brewer.pal(n=7, name = "YlOrBr")
brewer.pal(n=7, name = "YlOrBr")
# or using colorbrewer2.org
#cbpal <- c("#d7b5d8","#df65b0","#dd1c77","#980043", "#f0f9e8") # OPTION 1
cbpal <- c("#fed98e","#fe9929","#d95f0e","#993404", "#f0f9e8") # OPTION 2
#cbpal <- c("#fbb4b9","#f768a1","#c51b8a","#7a0177", "#f0f9e8") # OPTION 3
#cbpal <- c("#b3cde3","#8c96c6","#8856a7","#810f7c", "#f0f9e8") # OPTION 4

## Prepping data for plotting ## 
counties <- map_data("county") # we already did this, but we can do it again
a<-  ggplot() + geom_polygon(data = counties, aes(x=long, y = lat, group = group), fill = "grey", color = "black") + 
  coord_fixed(1.3)
a
names<-read.csv("statenames.csv") #simply a CSV file that lists postal codes with full state names
names$State <-sapply(names$State, tolower) 
colnames(names)[colnames(names)=="State"] <- "region"
colnames(names)[colnames(names)=="Postal.Code"] <- "State"

#####------ Figure 3. County-level estimated CWS water arsenic from 2006-2011 
# define color pal
#mypal <- c("#C3FA9F","#F6F1A9","#FDC030","#F08080", "#B7B3B3")
table(county$group10)
clusters<-read.csv("~/Google Drive/Research/EPAAsTrajectories/Data/clustershare.csv")
#Fix FIPS codes for counties, these need the proper number of decimals
library(stringr)
clusters$CountyFIPS<-str_pad(clusters$FIPS, width=5, pad="0")

#Reformat my data so we can merge with our "counties" dataframe created above from package
myvars<-c("CountyFIPS","WeightedAs20062011","WeightedAs20062008","WeightedAs20092011" ,"Full.Name","group10","Flag20062008","Flag20092011")
mydata<-county[myvars]
mydata<-merge(mydata,clusters,by="CountyFIPS",all.x=T);dim(mydata)# 
colnames(mydata)[colnames(mydata)=="Full.Name"] <- "subregion"
describe(mydata$subregion) # missing 3 subregions (names) in AK - we don't map AK so it's okay
table(mydata$group10)
describe(mydata$group10)
describe(mydata$WeightedAs20062011) #82 missing
# Counties missing WeightedAs20062011 because missing both years: give group10 value of 5
mydata$group10[which(is.na(mydata$WeightedAs20062011))]<-5
table(mydata$group10)

setwd("~/Google Drive/Research/County level exposure and outcomes/Maps")
fipsnames<-read.csv("fipsnames.csv") # csv listing US county FIPS code, state postal code, and county name
fipsnames$Name<-tolower(fipsnames$Name) #Change case all to lower case
colnames(fipsnames)[colnames(fipsnames)=="Name"] <- "subregion"
library(stringr)
fipsnames$FIPS<-str_pad(fipsnames$FIPS, width=5, pad="0")
colnames(fipsnames)[colnames(fipsnames)=="FIPS"] <- "CountyFIPS"
fipsnames$Flag<-1
fipsnames<-merge(fipsnames,names,by="State",all.x=T)

library(dplyr)
j <- left_join(counties, fipsnames, by = c("subregion","region")) #Merge fips with 'counties' retaining all the data in 'counties'
table(j$Flag) #87640 have Flag
dim(j) #87949
who<-j[which(is.na(j$Flag)),] #names from counties that dont match 'fipsnames' so we manually assign them a COUNTYFIPS
#miami-dade, FL: 
#lagrange, Indiana:
#obrien, Iowa
#ste genevieve, missouri:
#yellowstone national, montana
#newport news, virginia
# virginia beach, virginia
# broomfield, colorado
#These don't match becaue of issues with spacing (O Brien), hyphens, etc.
j$CountyFIPS[j$subregion=="miami-dade"]<-12086
j$CountyFIPS[j$subregion=="obrien"]<-19141
j$CountyFIPS[j$subregion=="ste genevieve"]<-29186
j$CountyFIPS[j$subregion=="yellowstone national"]<-30113
j$CountyFIPS[j$subregion=="newport news"]<-51700
j$CountyFIPS[j$subregion=="virginia beach"]<-51810
j$CountyFIPS[j$subregion=="broomfield"]<-09280
describe(j$CountyFIPS)
who<-j[is.na(j$CountyFIPS),]
j$CountyFIPS[j$subregion=="lagrange"]<-18087
describe(j$CountyFIPS)
who<-j[is.na(j$CountyFIPS),]
j$CountyFIPS[j$subregion=="suffolk"]<-51800
j$CountyFIPS[j$subregion=="hampton"]<-51650
j$CountyFIPS[j$subregion=="norfolk"]<-51710
mydata$subregion<-NULL
describe(mydata$WeightedAs20062011)


#Inner join
library(dplyr)
g <- left_join(j, mydata, by = "CountyFIPS")
describe(g$group)#0 missing; 87949 , 3085 unique
describe(g$CountyFIPS)#0 missing 
describe(g$WeightedAs20062011)#n=76914; 11035 missing


# Next, define group for each 3 maps
#Create categorical weighted arsenic variable to be plotted
g$Asgroup0611[g$group10==5]<-5 #missing both data periods
g$Asgroup0611[g$WeightedAs20062011<=1]<-1
g$Asgroup0611[g$WeightedAs20062011>1&g$WeightedAs20062011<=5]<-2
g$Asgroup0611[g$WeightedAs20062011>5&g$WeightedAs20062011<=10]<-3
g$Asgroup0611[g$WeightedAs20062011>10]<-4
table(g$Asgroup0611)

g$Asgroup0608<-NA
g$Asgroup0608[g$WeightedAs20062008<=1]<-1
g$Asgroup0608[g$WeightedAs20062008>1&g$WeightedAs20062008<=5]<-2
g$Asgroup0608[g$WeightedAs20062008>5&g$WeightedAs20062008<=10]<-3
g$Asgroup0608[g$WeightedAs20062008>10]<-4
g$Asgroup0608[g$Flag20062008==1]<-5
table(g$Asgroup0608)

g$Asgroup0911<-NA
g$Asgroup0911[g$WeightedAs20092011<=1]<-1
g$Asgroup0911[g$WeightedAs20092011>1&g$WeightedAs20092011<=5]<-2
g$Asgroup0911[g$WeightedAs20092011>5&g$WeightedAs20092011<=10]<-3
g$Asgroup0911[g$WeightedAs20092011>10]<-4
g$Asgroup0911[g$Flag20092011==1]<-5
table(g$Asgroup0911)
library(extrafonts)
loadfonts()
state_map <- map_data("state")

pdf(file=paste0( folder.output , "/Figure 3. County level arsenic estimates 3 panel map .pdf"), 15, 9)
mall<- ggplot() + geom_polygon(data = g, aes(x=long, y = lat, group = group, fill=as.factor(Asgroup0611)), color = "antiquewhite4") + 
  coord_fixed(1.3)+ scale_fill_manual(values = cbpal,  na.value = "#FFFFFF", labels=c("<=1",">1 - 5",
                                                                                      ">5 - 10",">10", "Inadequte data",
                                                                                      "No data available")) +
  
  geom_path( data = state_map, aes(x=long,y=lat, group=group), colour = "black",size=0.5)+
  theme(plot.title = element_text(face="bold",size=20))+
  theme(legend.position = c(.9, .2))+
  labs(fill=expression(paste("Concentration (µg/L)")))+ theme_void()+
  theme(legend.title=element_text(family="Avenir Next"))+
  theme(legend.text=element_text(family="Avenir Next"))+
  annotate(geom="text", x=-94, y=51, label="Panel A. County average water arsenic concentrations (µg/L), 2006-2011",size=8, family ="Avenir Next") #Add title
mall

ma0608<- ggplot() + geom_polygon(data = g, aes(x=long, y = lat, group = group, fill=as.factor(Asgroup0608)), color = "antiquewhite4") + 
  coord_fixed(1.3)+ scale_fill_manual(values = cbpal,  na.value = "#FFFFFF", labels=c("<=1",">1 - 5",
                                                                                         ">5 - 10",">10", "Inadequate data", "No data available")) +
  
  geom_path( data = state_map, aes(x=long,y=lat, group=group), colour = "black",size=0.5)+
  #theme(plot.title = element_text(face="bold",size=20))+
  theme(legend.position = c(.9, .2))+
  labs(fill=expression(paste("Concentration (µg/L)")))+
  theme_void()+
  theme(legend.title=element_text(family="Avenir Next"))+
  theme(legend.text=element_text(family="Avenir Next"))+
  annotate(geom="text", x=-94, y=51, label="Panel B. County average water arsenic concentrations (µg/L), 2006-2008",size=8, family= "Avenir Next") #Add title
ma0608

ma0911<- ggplot() + geom_polygon(data = g, aes(x=long, y = lat, group = group, fill=as.factor(Asgroup0911)), color = "antiquewhite4") + 
  coord_fixed(1.3)+ scale_fill_manual(values = cbpal,  na.value = "#FFFFFF", labels=c("<=1",">1 - 5",
                                                                                         ">5 - 10",">10", "Inadequate data", "No data available")) +
  
  geom_path( data = state_map, aes(x=long,y=lat, group=group), colour = "black",size=0.5)+
  theme(plot.title = element_text(face="bold",size=20))+
  theme(legend.position = c(.9, .2))+
  labs(fill=expression(paste("Concentration (µg/L)"))) + theme_void()+
  theme(legend.title=element_text(family="Avenir Next"))+
  theme(legend.text=element_text(family="Avenir Next"))+
  annotate(geom="text", x=-94, y=51, label="Panel C. County average water arsenic concentrations (µg/L), 2009-2011",size=8,family="Avenir Next") #Add title
ma0911
dev.off()



#####------ Figure 4 - Map of county-level compliance categories
ff<-county
county$group10[which(is.na(county$group10))]<-10
county$group5[which(is.na(county$group5))]<-10
county$group1[which(is.na(county$group1))]<-10

table(county$group10)
table(county$group5)
table(county$group1)

myvars<-c("CountyFIPS","WeightedAs20092011" ,"Full.Name","group10","group5","group1")
mydata<-ff[myvars]
colnames(mydata)[colnames(mydata)=="Full.Name"] <- "subregion"
describe(mydata$subregion)
setwd("~/Google Drive/Research/County level exposure and outcomes/Maps")
fipsnames<-read.csv("fipsnames.csv")
fipsnames$Name<-tolower(fipsnames$Name) #Change case all to lower case
colnames(fipsnames)[colnames(fipsnames)=="Name"] <- "subregion"
fipsnames$FIPS<-str_pad(fipsnames$FIPS, width=5, pad="0")
colnames(fipsnames)[colnames(fipsnames)=="FIPS"] <- "CountyFIPS"
fipsnames$Flag<-1
fipsnames<-merge(fipsnames,names,by="State",all.x=T)

library(dplyr)
j <- left_join(counties, fipsnames, by = c("subregion","region"))#Merge fips with 'counties' retaining all the data in 'counties'
table(j$Flag) #87640 have Flag
dim(j) #87949
who<-j[which(is.na(j$Flag)),] #names from counties that dont match 'fipsnames' so we manually assign them a COUNTYFIPS
#miami-dade, FL: 
#lagrange, Indiana:
#obrien, Iowa
#ste genevieve, missouri:
#yellowstone national, montana
#newport news, virginia
# virginia beach, virginia
# broomfield, colorado
#These don't match becaue of issues with spacing (O Brien), hyphens, etc.
j$CountyFIPS[j$subregion=="miami-dade"]<-12086
j$CountyFIPS[j$subregion=="obrien"]<-19141
j$CountyFIPS[j$subregion=="ste genevieve"]<-29186
j$CountyFIPS[j$subregion=="yellowstone national"]<-30113
j$CountyFIPS[j$subregion=="newport news"]<-51700
j$CountyFIPS[j$subregion=="virginia beach"]<-51810
j$CountyFIPS[j$subregion=="broomfield"]<-09280
describe(j$CountyFIPS)
who<-j[is.na(j$CountyFIPS),]
j$CountyFIPS[j$subregion=="lagrange"]<-18087
describe(j$CountyFIPS)
who<-j[is.na(j$CountyFIPS),]
j$CountyFIPS[j$subregion=="suffolk"]<-51800
j$CountyFIPS[j$subregion=="hampton"]<-51650
j$CountyFIPS[j$subregion=="norfolk"]<-51710
mydata$subregion<-NULL

#Inner join
library(dplyr)
g <- left_join(j, mydata, by = "CountyFIPS")
describe(g$group)#0 missing; 87949 , 3085 unique
describe(g$CountyFIPS)#0 missing 

#Create categorical weighted arsenic variable to be plotted - reordered 
table(g$group10)
g$group10m<-NA
g$group10m[which(g$group10==1)]<-1 #lowlow
g$group10m[which(g$group10==3)]<-2 #highlow
g$group10m[which(g$group10==2)]<-3 #lowhigh
g$group10m[which(g$group10==4)]<-4 #highhigh
g$group10m[which(g$group10==10)]<-5 #inadequate data (1 missing)
table(g$group10m)

table(g$group5)
g$group5m<-NA
g$group5m[which(g$group5==1)]<-1 #lowlow
g$group5m[which(g$group5==3)]<-2 #highlow
g$group5m[which(g$group5==2)]<-3 #lowhigh
g$group5m[which(g$group5==4)]<-4 #highhigh
g$group5m[which(g$group5==10)]<-5 #inadequate data (1 missing)
table(g$group5m)

table(g$group1)
g$group1m<-NA
g$group1m[which(g$group1==1)]<-1 #lowlow
g$group1m[which(g$group1==3)]<-2 #highlow
g$group1m[which(g$group1==2)]<-3 #lowhigh
g$group1m[which(g$group1==4)]<-4 #highhigh
g$group1m[which(g$group1==10)]<-5 #inadequate data (1 missing)
table(g$group1m)

#Select better colors for gg.cols
cbpal <- c("#a1dab4","#41b6c4","#2c7fb8","#253494", "#f0f9e8") # OPTION 1
#cbpal <- c("#b2df8a","#33a02c", "#a6cee3","#1f78b4", "#f0f9e8") # OPTION 1
#cbpal <- c("#92c5de","#0571b0", "#f4a582","#ca0020", "#f0f9e8") # OPTION 1


state_map <- map_data("state")

pdf(file=paste0( folder.output , "/Figure 4. County level compliance category map.pdf"), 15, 9)
traj10<- ggplot() + geom_polygon(data = g, aes(x=long, y = lat, group = group, fill=factor(group10)), color = "antiquewhite4") + 
  coord_fixed(1.3)+ scale_fill_manual(values = cbpal,  na.value = "#FFFFFF", labels=c("Low/Low","High/Low",
                                                                                     "Low/High","High/High",
                                                                                     "Missing one data period", "No data available"))+

  geom_path( data = state_map, aes(x=long,y=lat, group=group), colour = "black",size=0.5)+
  theme(legend.position = c(.9, .2))+
  labs(fill='Compliance category')+
  theme(plot.title = element_text(face="bold",size=20))+
  theme(legend.position = c(.9, .2))+ theme_void()+
  theme(legend.title=element_text(family="Avenir Next"))+
  theme(legend.text=element_text(family="Avenir Next"))+
  annotate(geom="text", x=-94, y=51, label="Panel A. County compliance categories, 10 µg/L cut-point",size=8,family="Avenir Next") #Add title
traj10

traj5<- ggplot() + geom_polygon(data = g, aes(x=long, y = lat, group = group, fill=factor(group5)), color = "antiquewhite4") + 
  coord_fixed(1.3)+ scale_fill_manual(values = cbpal,  na.value = "#FFFFFF", labels=c("Low/Low","High/Low",
                                                                                      "Low/High","High/High",
                                                                                      "Missing one data period", "No data available"))+
  geom_path( data = state_map, aes(x=long,y=lat, group=group), colour = "black",size=0.5)+
  theme(legend.position = c(.9, .2))+
  labs(fill='Compliance category')+
  theme(plot.title = element_text(face="bold",size=20))+
  theme(legend.position = c(.9, .2))+ theme_void()+
  theme(legend.title=element_text(family="Avenir Next"))+
  theme(legend.text=element_text(family="Avenir Next"))+
  annotate(geom="text", x=-94, y=51, label="Panel B. County compliance categories, 5 µg/L cut-point",size=8,family="Avenir Next") #Add title
traj5

traj1<- ggplot() + geom_polygon(data = g, aes(x=long, y = lat, group = group, fill=factor(group1)), color = "antiquewhite4") + 
  coord_fixed(1.3)+ scale_fill_manual(values = cbpal,  na.value = "#FFFFFF", labels=c("Low/Low","High/Low",
                                                                                      "Low/High","High/High",
                                                                                      "Missing one data period", "No data available"))+
  geom_path( data = state_map, aes(x=long,y=lat, group=group), colour = "black",size=0.5)+
  theme(legend.position = c(.9, .2))+
  labs(fill='Compliance category')+
  theme(plot.title = element_text(face="bold",size=20))+
  theme(legend.position = c(.9, .2))+ theme_void()+
  theme(legend.title=element_text(family="Avenir Next"))+
  theme(legend.text=element_text(family="Avenir Next"))+
  annotate(geom="text", x=-94, y=51, label="Panel C. County compliance categories, 1 µg/L cut-point",size=8,family="Avenir Next") #Add title
traj1
dev.off()




####### 3. Figure 2: Quantile regression results stratified by region ######

#A sequence of coefficient estimates for quantile regressions with varying tau parameters is visualized along with associated confidence bands.
# red dotted line is the OLS coefficient and confidence bands
#A sequence of coefficient estimates for quantile regressions with varying tau parameters is visualized.
## Overall 
# get quantiles for x axis:
library(quantreg)
quantile(cws$ThreeYearAs20062008,  probs = c(0.5,0.6,0.7,0.8,0.9,1.0), na.rm=TRUE)

qs <- c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99)
qs
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = qs, data = finallong3, method='fn', model=TRUE)
summary(qr2)

toplot<-summary(qr2)
setwd("~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP")
folder.output <- "output"
if( !file.exists( folder.output ) ) {
  dir.create( file.path( folder.output ) )
}
#pdf(file=paste0( folder.output , "/Quantreg overall.pdf"), 12, 8)

plot(toplot,xaxt='n', parm = 2, mar = c(6, 6, 4, 2), main = "", xlab = "", 
     ylab = "",  cex.lab=1.4,cex = 1.4, pch = 19,ols=TRUE)
mtext(expression(paste("0.35",mu,"g/L")), side=1, line=1.25, at=0.5)
mtext(expression(paste("0.35",mu,"g/L")), side=1, line=1.25, at=0.6)
mtext(expression(paste("1.20",mu,"g/L")), side=1, line=1.25, at=0.7)
mtext(expression(paste("2.27",mu,"g/L")), side=1, line=1.25, at=0.8)
mtext(expression(paste("5.00",mu,"g/L")), side=1, line=1.25, at=0.9)
mtext(expression(paste("453.95",mu,"g/L")), side=1, line=1.25, at=1.0)

title("Quantile regression coefficient results for\n CWSs overall (N= 36,406)", cex.main=1.5)
mtext("Change in quantile water arsenic concentration \nfrom 2006-2008 to 2009-2011",side=2,cex=1.4,line=1)
mtext("Quantile (tau)",side=1,cex=1.4,line=3)


dev.off()


## Plot each region separately
## Updated figure February 2020, adding 96,97,98 to plot
setwd("~/Google Drive/Research/EPAAsTrajectories/March2020/EHP resubmissoin/July 2020 R0 EHP")
folder.output <- "output"
if( !file.exists( folder.output ) ) {
  dir.create( file.path( folder.output ) )
}

set.seed(123)

### AKHI
akhilong<-finallong3[which(finallong3$Region=="AKHI"),]
# 50
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=akhilong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.50, data = akhilong, method='fn', model=TRUE)
# change 
t50b<-round(summary(qr2)$coeff[2,1],digits=2)
t50ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t50ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 55
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.55, data = akhilong, method='fn', model=TRUE)
# change 
t55b<-round(summary(qr2)$coeff[2,1],digits=2)
t55ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t55ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 60
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.60, data = akhilong, method='fn', model=TRUE)
# change 
t60b<-round(summary(qr2)$coeff[2,1],digits=2)
t60ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t60ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 65
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.65, data = akhilong, method='fn', model=TRUE)
# change 
t65b<-round(summary(qr2)$coeff[2,1],digits=2)
t65ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t65ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 70
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.70, data = akhilong, method='fn', model=TRUE)
# change 
t70b<-round(summary(qr2)$coeff[2,1],digits=2)
t70ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t70ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 75
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.75, data = akhilong, method='fn', model=TRUE)
# change 
t75b<-round(summary(qr2)$coeff[2,1],digits=2)
t75ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t75ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 80
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.80, data = akhilong, method='fn', model=TRUE)
# change 
t80b<-round(summary(qr2)$coeff[2,1],digits=2)
t80ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t80ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 85
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = akhilong, method='fn', model=TRUE)
# change 
t85b<-round(summary(qr2)$coeff[2,1],digits=2)
t85ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t85ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 90
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = akhilong, method='fn', model=TRUE)
# change 
t90b<-round(summary(qr2)$coeff[2,1],digits=2)
t90ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t90ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 95
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = akhilong, method='fn', model=TRUE)
# change 
t95b<-round(summary(qr2)$coeff[2,1],digits=2)
t95ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t95ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 96
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.96, data = akhilong, method='fn', model=TRUE)
# change 
t96b<-round(summary(qr2)$coeff[2,1],digits=2)
t96ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t96ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 97
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.97, data = akhilong, method='fn', model=TRUE)
# change 
t97b<-round(summary(qr2)$coeff[2,1],digits=2)
t97ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t97ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 98
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.98, data = akhilong, method='fn', model=TRUE)
# change 
t98b<-round(summary(qr2)$coeff[2,1],digits=2)
t98ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t98ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 99
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = akhilong, method='fn', model=TRUE)
# change 
t99b<-round(summary(qr2)$coeff[2,1],digits=2)
t99ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t99ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]

AKHIbeta<-rbind(t50b,t55b,t60b,t65b,t70b,t75b,t80b,t85b,t90b,t95b,t96b,t97b,t98b,t99b)
AKHIll<-rbind(t50ll,t55ll,t60ll,t65ll,t70ll,t75ll,t80ll,t85ll,t90ll,t95ll,t96ll,t97ll,t98ll,t99ll)
AKHIul<-rbind(t50ul,t55ul,t60ul,t65ul,t70ul,t75ul,t80ul,t85ul,t90ul,t95ul,t96ul,t97ul,t98ul,t99ul)

rownames(AKHIbeta)[1] <- "p 50th"
rownames(AKHIbeta)[2] <- "p 55th"
rownames(AKHIbeta)[3] <- "p 60th"
rownames(AKHIbeta)[4] <- "p 65th"
rownames(AKHIbeta)[5] <- "p 70th"
rownames(AKHIbeta)[6] <- "p 75th"
rownames(AKHIbeta)[7] <- "p 80th"
rownames(AKHIbeta)[8] <- "p 85th"
rownames(AKHIbeta)[9] <- "p 90th"
rownames(AKHIbeta)[10] <- "p 95th"
rownames(AKHIbeta)[11] <- "p 96th"
rownames(AKHIbeta)[12] <- "p 97th"
rownames(AKHIbeta)[13] <- "p 98th"
rownames(AKHIbeta)[14] <- "p 99th"
rownames(AKHIll)[1] <- "p 50th"
rownames(AKHIll)[2] <- "p 55th"
rownames(AKHIll)[3] <- "p 60th"
rownames(AKHIll)[4] <- "p 65th"
rownames(AKHIll)[5] <- "p 70th"
rownames(AKHIll)[6] <- "p 75th"
rownames(AKHIll)[7] <- "p 80th"
rownames(AKHIll)[8] <- "p 85th"
rownames(AKHIll)[9] <- "p 90th"
rownames(AKHIll)[10] <- "p 95th"
rownames(AKHIll)[11] <- "p 96th"
rownames(AKHIll)[12] <- "p 97th"
rownames(AKHIll)[13] <- "p 98th"
rownames(AKHIll)[14] <- "p 99th"
rownames(AKHIul)[1] <- "p 50th"
rownames(AKHIul)[2] <- "p 55th"
rownames(AKHIul)[3] <- "p 60th"
rownames(AKHIul)[4] <- "p 65th"
rownames(AKHIul)[5] <- "p 70th"
rownames(AKHIul)[6] <- "p 75th"
rownames(AKHIul)[7] <- "p 80th"
rownames(AKHIul)[8] <- "p 85th"
rownames(AKHIul)[9] <- "p 90th"
rownames(AKHIul)[10] <- "p 95th"
rownames(AKHIul)[11] <- "p 96th"
rownames(AKHIul)[12] <- "p 97th"
rownames(AKHIul)[13] <- "p 98th"
rownames(AKHIul)[14] <- "p 99th"
AKHI<-cbind(AKHIbeta,AKHIll,AKHIul)
AKHI2<-as.data.frame(AKHI)
colnames(AKHI2)[1] <- "Beta"
colnames(AKHI2)[2] <- "LL"
colnames(AKHI2)[3] <- "UL"
AKHI2$Region<-"Alaska/Hawaii"


## Central Midwest
### CMW
cmwlong<-finallong3[which(finallong3$Region=="CMW"),]
# 50
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=cmwlong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.50, data = cmwlong, method='fn', model=TRUE)
# change 
t50b<-round(summary(qr2)$coeff[2,1],digits=2)
t50ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t50ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 55
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.55, data = cmwlong, method='fn', model=TRUE)
# change 
t55b<-round(summary(qr2)$coeff[2,1],digits=2)
t55ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t55ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 60
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.60, data = cmwlong, method='fn', model=TRUE)
# change 
t60b<-round(summary(qr2)$coeff[2,1],digits=2)
t60ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t60ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 65
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.65, data = cmwlong, method='fn', model=TRUE)
# change 
t65b<-round(summary(qr2)$coeff[2,1],digits=2)
t65ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t65ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 70
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.70, data = cmwlong, method='fn', model=TRUE)
# change 
t70b<-round(summary(qr2)$coeff[2,1],digits=2)
t70ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t70ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 75
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.75, data = cmwlong, method='fn', model=TRUE)
# change 
t75b<-round(summary(qr2)$coeff[2,1],digits=2)
t75ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t75ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 80
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.80, data = cmwlong, method='fn', model=TRUE)
# change 
t80b<-round(summary(qr2)$coeff[2,1],digits=2)
t80ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t80ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 85
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = cmwlong, method='fn', model=TRUE)
# change 
t85b<-round(summary(qr2)$coeff[2,1],digits=2)
t85ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t85ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 90
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = cmwlong, method='fn', model=TRUE)
# change 
t90b<-round(summary(qr2)$coeff[2,1],digits=2)
t90ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t90ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 95
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = cmwlong, method='fn', model=TRUE)
# change 
t95b<-round(summary(qr2)$coeff[2,1],digits=2)
t95ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t95ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 96
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.96, data = cmwlong, method='fn', model=TRUE)
# change 
t96b<-round(summary(qr2)$coeff[2,1],digits=2)
t96ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t96ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 97
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.97, data = cmwlong, method='fn', model=TRUE)
# change 
t97b<-round(summary(qr2)$coeff[2,1],digits=2)
t97ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t97ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 98
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.98, data = cmwlong, method='fn', model=TRUE)
# change 
t98b<-round(summary(qr2)$coeff[2,1],digits=2)
t98ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t98ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 99
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = cmwlong, method='fn', model=TRUE)
# change 
t99b<-round(summary(qr2)$coeff[2,1],digits=2)
t99ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t99ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]

CMWbeta<-rbind(t50b,t55b,t60b,t65b,t70b,t75b,t80b,t85b,t90b,t95b,t96b,t97b,t98b,t99b)
CMWll<-rbind(t50ll,t55ll,t60ll,t65ll,t70ll,t75ll,t80ll,t85ll,t90ll,t95ll,t96ll,t97ll,t98ll,t99ll)
CMWul<-rbind(t50ul,t55ul,t60ul,t65ul,t70ul,t75ul,t80ul,t85ul,t90ul,t95ul,t96ul,t97ul,t98ul,t99ul)

rownames(CMWbeta)[1] <- "p 50th"
rownames(CMWbeta)[2] <- "p 55th"
rownames(CMWbeta)[3] <- "p 60th"
rownames(CMWbeta)[4] <- "p 65th"
rownames(CMWbeta)[5] <- "p 70th"
rownames(CMWbeta)[6] <- "p 75th"
rownames(CMWbeta)[7] <- "p 80th"
rownames(CMWbeta)[8] <- "p 85th"
rownames(CMWbeta)[9] <- "p 90th"
rownames(CMWbeta)[10] <- "p 95th"
rownames(CMWbeta)[11] <- "p 96th"
rownames(CMWbeta)[12] <- "p 97th"
rownames(CMWbeta)[13] <- "p 98th"
rownames(CMWbeta)[14] <- "p 99th"
rownames(CMWll)[1] <- "p 50th"
rownames(CMWll)[2] <- "p 55th"
rownames(CMWll)[3] <- "p 60th"
rownames(CMWll)[4] <- "p 65th"
rownames(CMWll)[5] <- "p 70th"
rownames(CMWll)[6] <- "p 75th"
rownames(CMWll)[7] <- "p 80th"
rownames(CMWll)[8] <- "p 85th"
rownames(CMWll)[9] <- "p 90th"
rownames(CMWll)[10] <- "p 95th"
rownames(CMWll)[11] <- "p 96th"
rownames(CMWll)[12] <- "p 97th"
rownames(CMWll)[13] <- "p 98th"
rownames(CMWll)[14] <- "p 99th"
rownames(CMWul)[1] <- "p 50th"
rownames(CMWul)[2] <- "p 55th"
rownames(CMWul)[3] <- "p 60th"
rownames(CMWul)[4] <- "p 65th"
rownames(CMWul)[5] <- "p 70th"
rownames(CMWul)[6] <- "p 75th"
rownames(CMWul)[7] <- "p 80th"
rownames(CMWul)[8] <- "p 85th"
rownames(CMWul)[9] <- "p 90th"
rownames(CMWul)[10] <- "p 95th"
rownames(CMWul)[11] <- "p 96th"
rownames(CMWul)[12] <- "p 97th"
rownames(CMWul)[13] <- "p 98th"
rownames(CMWul)[14] <- "p 99th"
CMW<-cbind(CMWbeta,CMWll,CMWul)
CMW2<-as.data.frame(CMW)
colnames(CMW2)[1] <- "Beta"
colnames(CMW2)[2] <- "LL"
colnames(CMW2)[3] <- "UL"
CMW2$Region<-"Central Midwest"


### EMW
emwlong<-finallong3[which(finallong3$Region=="EMW"),]
# 50
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=emwlong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.50, data = emwlong, method='fn', model=TRUE)
# change 
t50b<-round(summary(qr2)$coeff[2,1],digits=2)
t50ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t50ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 55
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.55, data = emwlong, method='fn', model=TRUE)
# change 
t55b<-round(summary(qr2)$coeff[2,1],digits=2)
t55ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t55ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 60
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.60, data = emwlong, method='fn', model=TRUE)
# change 
t60b<-round(summary(qr2)$coeff[2,1],digits=2)
t60ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t60ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 65
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.65, data = emwlong, method='fn', model=TRUE)
# change 
t65b<-round(summary(qr2)$coeff[2,1],digits=2)
t65ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t65ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 70
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.70, data = emwlong, method='fn', model=TRUE)
# change 
t70b<-round(summary(qr2)$coeff[2,1],digits=2)
t70ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t70ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 75
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.75, data = emwlong, method='fn', model=TRUE)
# change 
t75b<-round(summary(qr2)$coeff[2,1],digits=2)
t75ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t75ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 80
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.80, data = emwlong, method='fn', model=TRUE)
# change 
t80b<-round(summary(qr2)$coeff[2,1],digits=2)
t80ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t80ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 85
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = emwlong, method='fn', model=TRUE)
# change 
t85b<-round(summary(qr2)$coeff[2,1],digits=2)
t85ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t85ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 90
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = emwlong, method='fn', model=TRUE)
# change 
t90b<-round(summary(qr2)$coeff[2,1],digits=2)
t90ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t90ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 95
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = emwlong, method='fn', model=TRUE)
# change 
t95b<-round(summary(qr2)$coeff[2,1],digits=2)
t95ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t95ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 96
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.96, data = emwlong, method='fn', model=TRUE)
# change 
t96b<-round(summary(qr2)$coeff[2,1],digits=2)
t96ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t96ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 97
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.97, data = emwlong, method='fn', model=TRUE)
# change 
t97b<-round(summary(qr2)$coeff[2,1],digits=2)
t97ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t97ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 98
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.98, data = emwlong, method='fn', model=TRUE)
# change 
t98b<-round(summary(qr2)$coeff[2,1],digits=2)
t98ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t98ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 99
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = emwlong, method='fn', model=TRUE)
# change 
t99b<-round(summary(qr2)$coeff[2,1],digits=2)
t99ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t99ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]

EMWbeta<-rbind(t50b,t55b,t60b,t65b,t70b,t75b,t80b,t85b,t90b,t95b,t96b,t97b,t98b,t99b)
EMWll<-rbind(t50ll,t55ll,t60ll,t65ll,t70ll,t75ll,t80ll,t85ll,t90ll,t95ll,t96ll,t97ll,t98ll,t99ll)
EMWul<-rbind(t50ul,t55ul,t60ul,t65ul,t70ul,t75ul,t80ul,t85ul,t90ul,t95ul,t96ul,t97ul,t98ul,t99ul)

rownames(EMWbeta)[1] <- "p 50th"
rownames(EMWbeta)[2] <- "p 55th"
rownames(EMWbeta)[3] <- "p 60th"
rownames(EMWbeta)[4] <- "p 65th"
rownames(EMWbeta)[5] <- "p 70th"
rownames(EMWbeta)[6] <- "p 75th"
rownames(EMWbeta)[7] <- "p 80th"
rownames(EMWbeta)[8] <- "p 85th"
rownames(EMWbeta)[9] <- "p 90th"
rownames(EMWbeta)[10] <- "p 95th"
rownames(EMWbeta)[11] <- "p 96th"
rownames(EMWbeta)[12] <- "p 97th"
rownames(EMWbeta)[13] <- "p 98th"
rownames(EMWbeta)[14] <- "p 99th"
rownames(EMWll)[1] <- "p 50th"
rownames(EMWll)[2] <- "p 55th"
rownames(EMWll)[3] <- "p 60th"
rownames(EMWll)[4] <- "p 65th"
rownames(EMWll)[5] <- "p 70th"
rownames(EMWll)[6] <- "p 75th"
rownames(EMWll)[7] <- "p 80th"
rownames(EMWll)[8] <- "p 85th"
rownames(EMWll)[9] <- "p 90th"
rownames(EMWll)[10] <- "p 95th"
rownames(EMWll)[11] <- "p 96th"
rownames(EMWll)[12] <- "p 97th"
rownames(EMWll)[13] <- "p 98th"
rownames(EMWll)[14] <- "p 99th"
rownames(EMWul)[1] <- "p 50th"
rownames(EMWul)[2] <- "p 55th"
rownames(EMWul)[3] <- "p 60th"
rownames(EMWul)[4] <- "p 65th"
rownames(EMWul)[5] <- "p 70th"
rownames(EMWul)[6] <- "p 75th"
rownames(EMWul)[7] <- "p 80th"
rownames(EMWul)[8] <- "p 85th"
rownames(EMWul)[9] <- "p 90th"
rownames(EMWul)[10] <- "p 95th"
rownames(EMWul)[11] <- "p 96th"
rownames(EMWul)[12] <- "p 97th"
rownames(EMWul)[13] <- "p 98th"
rownames(EMWul)[14] <- "p 99th"
EMW<-cbind(EMWbeta,EMWll,EMWul)
EMW2<-as.data.frame(EMW)
colnames(EMW2)[1] <- "Beta"
colnames(EMW2)[2] <- "LL"
colnames(EMW2)[3] <- "UL"
EMW2$Region<-"Eastern Midwest"


### MA
malong<-finallong3[which(finallong3$Region=="MA"),]
# 50
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=malong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.50, data = malong, method='fn', model=TRUE)
# change 
t50b<-round(summary(qr2)$coeff[2,1],digits=2)
t50ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t50ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 55
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.55, data = malong, method='fn', model=TRUE)
# change 
t55b<-round(summary(qr2)$coeff[2,1],digits=2)
t55ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t55ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 60
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.60, data = malong, method='fn', model=TRUE)
# change 
t60b<-round(summary(qr2)$coeff[2,1],digits=2)
t60ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t60ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 65
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.65, data = malong, method='fn', model=TRUE)
# change 
t65b<-round(summary(qr2)$coeff[2,1],digits=2)
t65ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t65ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 70
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.70, data = malong, method='fn', model=TRUE)
# change 
t70b<-round(summary(qr2)$coeff[2,1],digits=2)
t70ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t70ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 75
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.75, data = malong, method='fn', model=TRUE)
# change 
t75b<-round(summary(qr2)$coeff[2,1],digits=2)
t75ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t75ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 80
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.80, data = malong, method='fn', model=TRUE)
# change 
t80b<-round(summary(qr2)$coeff[2,1],digits=2)
t80ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t80ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 85
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = malong, method='fn', model=TRUE)
# change 
t85b<-round(summary(qr2)$coeff[2,1],digits=2)
t85ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t85ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 90
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = malong, method='fn', model=TRUE)
# change 
t90b<-round(summary(qr2)$coeff[2,1],digits=2)
t90ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t90ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 95
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = malong, method='fn', model=TRUE)
# change 
t95b<-round(summary(qr2)$coeff[2,1],digits=2)
t95ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t95ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 96
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.96, data = malong, method='fn', model=TRUE)
# change 
t96b<-round(summary(qr2)$coeff[2,1],digits=2)
t96ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t96ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 97
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.97, data = malong, method='fn', model=TRUE)
# change 
t97b<-round(summary(qr2)$coeff[2,1],digits=2)
t97ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t97ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 98
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.98, data = malong, method='fn', model=TRUE)
# change 
t98b<-round(summary(qr2)$coeff[2,1],digits=2)
t98ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t98ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 99
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = malong, method='fn', model=TRUE)
# change 
t99b<-round(summary(qr2)$coeff[2,1],digits=2)
t99ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t99ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]

MAbeta<-rbind(t50b,t55b,t60b,t65b,t70b,t75b,t80b,t85b,t90b,t95b,t96b,t97b,t98b,t99b)
MAll<-rbind(t50ll,t55ll,t60ll,t65ll,t70ll,t75ll,t80ll,t85ll,t90ll,t95ll,t96ll,t97ll,t98ll,t99ll)
MAul<-rbind(t50ul,t55ul,t60ul,t65ul,t70ul,t75ul,t80ul,t85ul,t90ul,t95ul,t96ul,t97ul,t98ul,t99ul)

rownames(MAbeta)[1] <- "p 50th"
rownames(MAbeta)[2] <- "p 55th"
rownames(MAbeta)[3] <- "p 60th"
rownames(MAbeta)[4] <- "p 65th"
rownames(MAbeta)[5] <- "p 70th"
rownames(MAbeta)[6] <- "p 75th"
rownames(MAbeta)[7] <- "p 80th"
rownames(MAbeta)[8] <- "p 85th"
rownames(MAbeta)[9] <- "p 90th"
rownames(MAbeta)[10] <- "p 95th"
rownames(MAbeta)[11] <- "p 96th"
rownames(MAbeta)[12] <- "p 97th"
rownames(MAbeta)[13] <- "p 98th"
rownames(MAbeta)[14] <- "p 99th"
rownames(MAll)[1] <- "p 50th"
rownames(MAll)[2] <- "p 55th"
rownames(MAll)[3] <- "p 60th"
rownames(MAll)[4] <- "p 65th"
rownames(MAll)[5] <- "p 70th"
rownames(MAll)[6] <- "p 75th"
rownames(MAll)[7] <- "p 80th"
rownames(MAll)[8] <- "p 85th"
rownames(MAll)[9] <- "p 90th"
rownames(MAll)[10] <- "p 95th"
rownames(MAll)[11] <- "p 96th"
rownames(MAll)[12] <- "p 97th"
rownames(MAll)[13] <- "p 98th"
rownames(MAll)[14] <- "p 99th"
rownames(MAul)[1] <- "p 50th"
rownames(MAul)[2] <- "p 55th"
rownames(MAul)[3] <- "p 60th"
rownames(MAul)[4] <- "p 65th"
rownames(MAul)[5] <- "p 70th"
rownames(MAul)[6] <- "p 75th"
rownames(MAul)[7] <- "p 80th"
rownames(MAul)[8] <- "p 85th"
rownames(MAul)[9] <- "p 90th"
rownames(MAul)[10] <- "p 95th"
rownames(MAul)[11] <- "p 96th"
rownames(MAul)[12] <- "p 97th"
rownames(MAul)[13] <- "p 98th"
rownames(MAul)[14] <- "p 99th"
MA<-cbind(MAbeta,MAll,MAul)
MA2<-as.data.frame(MA)
colnames(MA2)[1] <- "Beta"
colnames(MA2)[2] <- "LL"
colnames(MA2)[3] <- "UL"
MA2$Region<-"Mid-Atlantic"

### NE
nelong<-finallong3[which(finallong3$Region=="NE"),]
# 50
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=nelong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.50, data = nelong, method='fn', model=TRUE)
# change 
t50b<-round(summary(qr2)$coeff[2,1],digits=2)
t50ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t50ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 55
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.55, data = nelong, method='fn', model=TRUE)
# change 
t55b<-round(summary(qr2)$coeff[2,1],digits=2)
t55ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t55ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 60
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.60, data = nelong, method='fn', model=TRUE)
# change 
t60b<-round(summary(qr2)$coeff[2,1],digits=2)
t60ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t60ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 65
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.65, data = nelong, method='fn', model=TRUE)
# change 
t65b<-round(summary(qr2)$coeff[2,1],digits=2)
t65ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t65ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 70
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.70, data = nelong, method='fn', model=TRUE)
# change 
t70b<-round(summary(qr2)$coeff[2,1],digits=2)
t70ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t70ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 75
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.75, data = nelong, method='fn', model=TRUE)
# change 
t75b<-round(summary(qr2)$coeff[2,1],digits=2)
t75ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t75ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 80
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.80, data = nelong, method='fn', model=TRUE)
# change 
t80b<-round(summary(qr2)$coeff[2,1],digits=2)
t80ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t80ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 85
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = nelong, method='fn', model=TRUE)
# change 
t85b<-round(summary(qr2)$coeff[2,1],digits=2)
t85ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t85ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 90
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = nelong, method='fn', model=TRUE)
# change 
t90b<-round(summary(qr2)$coeff[2,1],digits=2)
t90ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t90ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 95
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = nelong, method='fn', model=TRUE)
# change 
t95b<-round(summary(qr2)$coeff[2,1],digits=2)
t95ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t95ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 96
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.96, data = nelong, method='fn', model=TRUE)
# change 
t96b<-round(summary(qr2)$coeff[2,1],digits=2)
t96ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t96ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 97
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.97, data = nelong, method='fn', model=TRUE)
# change 
t97b<-round(summary(qr2)$coeff[2,1],digits=2)
t97ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t97ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 98
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.98, data = nelong, method='fn', model=TRUE)
# change 
t98b<-round(summary(qr2)$coeff[2,1],digits=2)
t98ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t98ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 99
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = nelong, method='fn', model=TRUE)
# change 
t99b<-round(summary(qr2)$coeff[2,1],digits=2)
t99ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t99ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]

NEbeta<-rbind(t50b,t55b,t60b,t65b,t70b,t75b,t80b,t85b,t90b,t95b,t96b,t97b,t98b,t99b)
NEll<-rbind(t50ll,t55ll,t60ll,t65ll,t70ll,t75ll,t80ll,t85ll,t90ll,t95ll,t96ll,t97ll,t98ll,t99ll)
NEul<-rbind(t50ul,t55ul,t60ul,t65ul,t70ul,t75ul,t80ul,t85ul,t90ul,t95ul,t96ul,t97ul,t98ul,t99ul)

rownames(NEbeta)[1] <- "p 50th"
rownames(NEbeta)[2] <- "p 55th"
rownames(NEbeta)[3] <- "p 60th"
rownames(NEbeta)[4] <- "p 65th"
rownames(NEbeta)[5] <- "p 70th"
rownames(NEbeta)[6] <- "p 75th"
rownames(NEbeta)[7] <- "p 80th"
rownames(NEbeta)[8] <- "p 85th"
rownames(NEbeta)[9] <- "p 90th"
rownames(NEbeta)[10] <- "p 95th"
rownames(NEbeta)[11] <- "p 96th"
rownames(NEbeta)[12] <- "p 97th"
rownames(NEbeta)[13] <- "p 98th"
rownames(NEbeta)[14] <- "p 99th"
rownames(NEll)[1] <- "p 50th"
rownames(NEll)[2] <- "p 55th"
rownames(NEll)[3] <- "p 60th"
rownames(NEll)[4] <- "p 65th"
rownames(NEll)[5] <- "p 70th"
rownames(NEll)[6] <- "p 75th"
rownames(NEll)[7] <- "p 80th"
rownames(NEll)[8] <- "p 85th"
rownames(NEll)[9] <- "p 90th"
rownames(NEll)[10] <- "p 95th"
rownames(NEll)[11] <- "p 96th"
rownames(NEll)[12] <- "p 97th"
rownames(NEll)[13] <- "p 98th"
rownames(NEll)[14] <- "p 99th"
rownames(NEul)[1] <- "p 50th"
rownames(NEul)[2] <- "p 55th"
rownames(NEul)[3] <- "p 60th"
rownames(NEul)[4] <- "p 65th"
rownames(NEul)[5] <- "p 70th"
rownames(NEul)[6] <- "p 75th"
rownames(NEul)[7] <- "p 80th"
rownames(NEul)[8] <- "p 85th"
rownames(NEul)[9] <- "p 90th"
rownames(NEul)[10] <- "p 95th"
rownames(NEul)[11] <- "p 96th"
rownames(NEul)[12] <- "p 97th"
rownames(NEul)[13] <- "p 98th"
rownames(NEul)[14] <- "p 99th"
NE<-cbind(NEbeta,NEll,NEul)
NE2<-as.data.frame(NE)
colnames(NE2)[1] <- "Beta"
colnames(NE2)[2] <- "LL"
colnames(NE2)[3] <- "UL"
NE2$Region<-"New England"


### PNW
pnwlong<-finallong3[which(finallong3$Region=="PNW"),]
# 50
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=pnwlong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.50, data = pnwlong, method='fn', model=TRUE)
# change 
t50b<-round(summary(qr2)$coeff[2,1],digits=2)
t50ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t50ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 55
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.55, data = pnwlong, method='fn', model=TRUE)
# change 
t55b<-round(summary(qr2)$coeff[2,1],digits=2)
t55ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t55ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 60
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.60, data = pnwlong, method='fn', model=TRUE)
# change 
t60b<-round(summary(qr2)$coeff[2,1],digits=2)
t60ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t60ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 65
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.65, data = pnwlong, method='fn', model=TRUE)
# change 
t65b<-round(summary(qr2)$coeff[2,1],digits=2)
t65ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t65ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 70
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.70, data = pnwlong, method='fn', model=TRUE)
# change 
t70b<-round(summary(qr2)$coeff[2,1],digits=2)
t70ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t70ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 75
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.75, data = pnwlong, method='fn', model=TRUE)
# change 
t75b<-round(summary(qr2)$coeff[2,1],digits=2)
t75ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t75ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 80
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.80, data = pnwlong, method='fn', model=TRUE)
# change 
t80b<-round(summary(qr2)$coeff[2,1],digits=2)
t80ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t80ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 85
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = pnwlong, method='fn', model=TRUE)
# change 
t85b<-round(summary(qr2)$coeff[2,1],digits=2)
t85ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t85ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 90
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = pnwlong, method='fn', model=TRUE)
# change 
t90b<-round(summary(qr2)$coeff[2,1],digits=2)
t90ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t90ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 95
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = pnwlong, method='fn', model=TRUE)
# change 
t95b<-round(summary(qr2)$coeff[2,1],digits=2)
t95ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t95ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 96
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.96, data = pnwlong, method='fn', model=TRUE)
# change 
t96b<-round(summary(qr2)$coeff[2,1],digits=2)
t96ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t96ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 97
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.97, data = pnwlong, method='fn', model=TRUE)
# change 
t97b<-round(summary(qr2)$coeff[2,1],digits=2)
t97ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t97ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 98
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.98, data = pnwlong, method='fn', model=TRUE)
# change 
t98b<-round(summary(qr2)$coeff[2,1],digits=2)
t98ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t98ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 99
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = pnwlong, method='fn', model=TRUE)
# change 
t99b<-round(summary(qr2)$coeff[2,1],digits=2)
t99ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t99ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]

PNWbeta<-rbind(t50b,t55b,t60b,t65b,t70b,t75b,t80b,t85b,t90b,t95b,t96b,t97b,t98b,t99b)
PNWll<-rbind(t50ll,t55ll,t60ll,t65ll,t70ll,t75ll,t80ll,t85ll,t90ll,t95ll,t96ll,t97ll,t98ll,t99ll)
PNWul<-rbind(t50ul,t55ul,t60ul,t65ul,t70ul,t75ul,t80ul,t85ul,t90ul,t95ul,t96ul,t97ul,t98ul,t99ul)

rownames(PNWbeta)[1] <- "p 50th"
rownames(PNWbeta)[2] <- "p 55th"
rownames(PNWbeta)[3] <- "p 60th"
rownames(PNWbeta)[4] <- "p 65th"
rownames(PNWbeta)[5] <- "p 70th"
rownames(PNWbeta)[6] <- "p 75th"
rownames(PNWbeta)[7] <- "p 80th"
rownames(PNWbeta)[8] <- "p 85th"
rownames(PNWbeta)[9] <- "p 90th"
rownames(PNWbeta)[10] <- "p 95th"
rownames(PNWbeta)[11] <- "p 96th"
rownames(PNWbeta)[12] <- "p 97th"
rownames(PNWbeta)[13] <- "p 98th"
rownames(PNWbeta)[14] <- "p 99th"
rownames(PNWll)[1] <- "p 50th"
rownames(PNWll)[2] <- "p 55th"
rownames(PNWll)[3] <- "p 60th"
rownames(PNWll)[4] <- "p 65th"
rownames(PNWll)[5] <- "p 70th"
rownames(PNWll)[6] <- "p 75th"
rownames(PNWll)[7] <- "p 80th"
rownames(PNWll)[8] <- "p 85th"
rownames(PNWll)[9] <- "p 90th"
rownames(PNWll)[10] <- "p 95th"
rownames(PNWll)[11] <- "p 96th"
rownames(PNWll)[12] <- "p 97th"
rownames(PNWll)[13] <- "p 98th"
rownames(PNWll)[14] <- "p 99th"
rownames(PNWul)[1] <- "p 50th"
rownames(PNWul)[2] <- "p 55th"
rownames(PNWul)[3] <- "p 60th"
rownames(PNWul)[4] <- "p 65th"
rownames(PNWul)[5] <- "p 70th"
rownames(PNWul)[6] <- "p 75th"
rownames(PNWul)[7] <- "p 80th"
rownames(PNWul)[8] <- "p 85th"
rownames(PNWul)[9] <- "p 90th"
rownames(PNWul)[10] <- "p 95th"
rownames(PNWul)[11] <- "p 96th"
rownames(PNWul)[12] <- "p 97th"
rownames(PNWul)[13] <- "p 98th"
rownames(PNWul)[14] <- "p 99th"
PNW<-cbind(PNWbeta,PNWll,PNWul)
PNW2<-as.data.frame(PNW)
colnames(PNW2)[1] <- "Beta"
colnames(PNW2)[2] <- "LL"
colnames(PNW2)[3] <- "UL"
PNW2$Region<-"Pacific Northwest"


### SE
selong<-finallong3[which(finallong3$Region=="SE"),]
# 50
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=selong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.50, data = selong, method='fn', model=TRUE)
# change 
t50b<-round(summary(qr2)$coeff[2,1],digits=2)
t50ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t50ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 55
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.55, data = selong, method='fn', model=TRUE)
# change 
t55b<-round(summary(qr2)$coeff[2,1],digits=2)
t55ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t55ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 60
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.60, data = selong, method='fn', model=TRUE)
# change 
t60b<-round(summary(qr2)$coeff[2,1],digits=2)
t60ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t60ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 65
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.65, data = selong, method='fn', model=TRUE)
# change 
t65b<-round(summary(qr2)$coeff[2,1],digits=2)
t65ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t65ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 70
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.70, data = selong, method='fn', model=TRUE)
# change 
t70b<-round(summary(qr2)$coeff[2,1],digits=2)
t70ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t70ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 75
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.75, data = selong, method='fn', model=TRUE)
# change 
t75b<-round(summary(qr2)$coeff[2,1],digits=2)
t75ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t75ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 80
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.80, data = selong, method='fn', model=TRUE)
# change 
t80b<-round(summary(qr2)$coeff[2,1],digits=2)
t80ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t80ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 85
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = selong, method='fn', model=TRUE)
# change 
t85b<-round(summary(qr2)$coeff[2,1],digits=2)
t85ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t85ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 90
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = selong, method='fn', model=TRUE)
# change 
t90b<-round(summary(qr2)$coeff[2,1],digits=2)
t90ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t90ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 95
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = selong, method='fn', model=TRUE)
# change 
t95b<-round(summary(qr2)$coeff[2,1],digits=2)
t95ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t95ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 96
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.96, data = selong, method='fn', model=TRUE)
# change 
t96b<-round(summary(qr2)$coeff[2,1],digits=2)
t96ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t96ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 97
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.97, data = selong, method='fn', model=TRUE)
# change 
t97b<-round(summary(qr2)$coeff[2,1],digits=2)
t97ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t97ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 98
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.98, data = selong, method='fn', model=TRUE)
# change 
t98b<-round(summary(qr2)$coeff[2,1],digits=2)
t98ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t98ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 99
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = selong, method='fn', model=TRUE)
# change 
t99b<-round(summary(qr2)$coeff[2,1],digits=2)
t99ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t99ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]

SEbeta<-rbind(t50b,t55b,t60b,t65b,t70b,t75b,t80b,t85b,t90b,t95b,t96b,t97b,t98b,t99b)
SEll<-rbind(t50ll,t55ll,t60ll,t65ll,t70ll,t75ll,t80ll,t85ll,t90ll,t95ll,t96ll,t97ll,t98ll,t99ll)
SEul<-rbind(t50ul,t55ul,t60ul,t65ul,t70ul,t75ul,t80ul,t85ul,t90ul,t95ul,t96ul,t97ul,t98ul,t99ul)

rownames(SEbeta)[1] <- "p 50th"
rownames(SEbeta)[2] <- "p 55th"
rownames(SEbeta)[3] <- "p 60th"
rownames(SEbeta)[4] <- "p 65th"
rownames(SEbeta)[5] <- "p 70th"
rownames(SEbeta)[6] <- "p 75th"
rownames(SEbeta)[7] <- "p 80th"
rownames(SEbeta)[8] <- "p 85th"
rownames(SEbeta)[9] <- "p 90th"
rownames(SEbeta)[10] <- "p 95th"
rownames(SEbeta)[11] <- "p 96th"
rownames(SEbeta)[12] <- "p 97th"
rownames(SEbeta)[13] <- "p 98th"
rownames(SEbeta)[14] <- "p 99th"
rownames(SEll)[1] <- "p 50th"
rownames(SEll)[2] <- "p 55th"
rownames(SEll)[3] <- "p 60th"
rownames(SEll)[4] <- "p 65th"
rownames(SEll)[5] <- "p 70th"
rownames(SEll)[6] <- "p 75th"
rownames(SEll)[7] <- "p 80th"
rownames(SEll)[8] <- "p 85th"
rownames(SEll)[9] <- "p 90th"
rownames(SEll)[10] <- "p 95th"
rownames(SEll)[11] <- "p 96th"
rownames(SEll)[12] <- "p 97th"
rownames(SEll)[13] <- "p 98th"
rownames(SEll)[14] <- "p 99th"
rownames(SEul)[1] <- "p 50th"
rownames(SEul)[2] <- "p 55th"
rownames(SEul)[3] <- "p 60th"
rownames(SEul)[4] <- "p 65th"
rownames(SEul)[5] <- "p 70th"
rownames(SEul)[6] <- "p 75th"
rownames(SEul)[7] <- "p 80th"
rownames(SEul)[8] <- "p 85th"
rownames(SEul)[9] <- "p 90th"
rownames(SEul)[10] <- "p 95th"
rownames(SEul)[11] <- "p 96th"
rownames(SEul)[12] <- "p 97th"
rownames(SEul)[13] <- "p 98th"
rownames(SEul)[14] <- "p 99th"
SE<-cbind(SEbeta,SEll,SEul)
SE2<-as.data.frame(SE)
colnames(SE2)[1] <- "Beta"
colnames(SE2)[2] <- "LL"
colnames(SE2)[3] <- "UL"
SE2$Region<-"Southeast"

### SW
swlong<-finallong3[which(finallong3$Region=="SW"),]
# 50
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=swlong)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.50, data = swlong, method='fn', model=TRUE)
# change 
t50b<-round(summary(qr2)$coeff[2,1],digits=2)
t50ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t50ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 55
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.55, data = swlong, method='fn', model=TRUE)
# change 
t55b<-round(summary(qr2)$coeff[2,1],digits=2)
t55ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t55ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 60
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.60, data = swlong, method='fn', model=TRUE)
# change 
t60b<-round(summary(qr2)$coeff[2,1],digits=2)
t60ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t60ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 65
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.65, data = swlong, method='fn', model=TRUE)
# change 
t65b<-round(summary(qr2)$coeff[2,1],digits=2)
t65ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t65ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 70
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.70, data = swlong, method='fn', model=TRUE)
# change 
t70b<-round(summary(qr2)$coeff[2,1],digits=2)
t70ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t70ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 75
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.75, data = swlong, method='fn', model=TRUE)
# change 
t75b<-round(summary(qr2)$coeff[2,1],digits=2)
t75ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t75ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 80
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.80, data = swlong, method='fn', model=TRUE)
# change 
t80b<-round(summary(qr2)$coeff[2,1],digits=2)
t80ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t80ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 85
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = swlong, method='fn', model=TRUE)
# change 
t85b<-round(summary(qr2)$coeff[2,1],digits=2)
t85ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t85ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 90
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = swlong, method='fn', model=TRUE)
# change 
t90b<-round(summary(qr2)$coeff[2,1],digits=2)
t90ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t90ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 95
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = swlong, method='fn', model=TRUE)
# change 
t95b<-round(summary(qr2)$coeff[2,1],digits=2)
t95ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t95ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 96
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.96, data = swlong, method='fn', model=TRUE)
# change 
t96b<-round(summary(qr2)$coeff[2,1],digits=2)
t96ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t96ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 97
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.97, data = swlong, method='fn', model=TRUE)
# change 
t97b<-round(summary(qr2)$coeff[2,1],digits=2)
t97ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t97ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 98
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.98, data = swlong, method='fn', model=TRUE)
# change 
t98b<-round(summary(qr2)$coeff[2,1],digits=2)
t98ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t98ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 99
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = swlong, method='fn', model=TRUE)
# change 
t99b<-round(summary(qr2)$coeff[2,1],digits=2)
t99ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t99ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]

SWbeta<-rbind(t50b,t55b,t60b,t65b,t70b,t75b,t80b,t85b,t90b,t95b,t96b,t97b,t98b,t99b)
SWll<-rbind(t50ll,t55ll,t60ll,t65ll,t70ll,t75ll,t80ll,t85ll,t90ll,t95ll,t96ll,t97ll,t98ll,t99ll)
SWul<-rbind(t50ul,t55ul,t60ul,t65ul,t70ul,t75ul,t80ul,t85ul,t90ul,t95ul,t96ul,t97ul,t98ul,t99ul)

rownames(SWbeta)[1] <- "p 50th"
rownames(SWbeta)[2] <- "p 55th"
rownames(SWbeta)[3] <- "p 60th"
rownames(SWbeta)[4] <- "p 65th"
rownames(SWbeta)[5] <- "p 70th"
rownames(SWbeta)[6] <- "p 75th"
rownames(SWbeta)[7] <- "p 80th"
rownames(SWbeta)[8] <- "p 85th"
rownames(SWbeta)[9] <- "p 90th"
rownames(SWbeta)[10] <- "p 95th"
rownames(SWbeta)[11] <- "p 96th"
rownames(SWbeta)[12] <- "p 97th"
rownames(SWbeta)[13] <- "p 98th"
rownames(SWbeta)[14] <- "p 99th"
rownames(SWll)[1] <- "p 50th"
rownames(SWll)[2] <- "p 55th"
rownames(SWll)[3] <- "p 60th"
rownames(SWll)[4] <- "p 65th"
rownames(SWll)[5] <- "p 70th"
rownames(SWll)[6] <- "p 75th"
rownames(SWll)[7] <- "p 80th"
rownames(SWll)[8] <- "p 85th"
rownames(SWll)[9] <- "p 90th"
rownames(SWll)[10] <- "p 95th"
rownames(SWll)[11] <- "p 96th"
rownames(SWll)[12] <- "p 97th"
rownames(SWll)[13] <- "p 98th"
rownames(SWll)[14] <- "p 99th"
rownames(SWul)[1] <- "p 50th"
rownames(SWul)[2] <- "p 55th"
rownames(SWul)[3] <- "p 60th"
rownames(SWul)[4] <- "p 65th"
rownames(SWul)[5] <- "p 70th"
rownames(SWul)[6] <- "p 75th"
rownames(SWul)[7] <- "p 80th"
rownames(SWul)[8] <- "p 85th"
rownames(SWul)[9] <- "p 90th"
rownames(SWul)[10] <- "p 95th"
rownames(SWul)[11] <- "p 96th"
rownames(SWul)[12] <- "p 97th"
rownames(SWul)[13] <- "p 98th"
rownames(SWul)[14] <- "p 99th"
SW<-cbind(SWbeta,SWll,SWul)
SW2<-as.data.frame(SW)
colnames(SW2)[1] <- "Beta"
colnames(SW2)[2] <- "LL"
colnames(SW2)[3] <- "UL"
SW2$Region<-"Southwest"

### ALL CWSs regardless of region
# 50
fit1 <- glm(arsenic ~ year, family=gaussian(link="identity"), data=finallong3)
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.50, data = finallong3, method='fn', model=TRUE)
# change 
t50b<-round(summary(qr2)$coeff[2,1],digits=2)
t50ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t50ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 55
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.55, data = finallong3, method='fn', model=TRUE)
# change 
t55b<-round(summary(qr2)$coeff[2,1],digits=2)
t55ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t55ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 60
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.60, data = finallong3, method='fn', model=TRUE)
# change 
t60b<-round(summary(qr2)$coeff[2,1],digits=2)
t60ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t60ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 65
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.65, data = finallong3, method='fn', model=TRUE)
# change 
t65b<-round(summary(qr2)$coeff[2,1],digits=2)
t65ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t65ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 70
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.70, data = finallong3, method='fn', model=TRUE)
# change 
t70b<-round(summary(qr2)$coeff[2,1],digits=2)
t70ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t70ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 75
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.75, data = finallong3, method='fn', model=TRUE)
# change 
t75b<-round(summary(qr2)$coeff[2,1],digits=2)
t75ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t75ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 80
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.80, data = finallong3, method='fn', model=TRUE)
# change 
t80b<-round(summary(qr2)$coeff[2,1],digits=2)
t80ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t80ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 85
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.85, data = finallong3, method='fn', model=TRUE)
# change 
t85b<-round(summary(qr2)$coeff[2,1],digits=2)
t85ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t85ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 90
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.90, data = finallong3, method='fn', model=TRUE)
# change 
t90b<-round(summary(qr2)$coeff[2,1],digits=2)
t90ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t90ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 95
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.95, data = finallong3, method='fn', model=TRUE)
# change 
t95b<-round(summary(qr2)$coeff[2,1],digits=2)
t95ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t95ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 96
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.96, data = finallong3, method='fn', model=TRUE)
# change 
t96b<-round(summary(qr2)$coeff[2,1],digits=2)
t96ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t96ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 97
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.97, data = finallong3, method='fn', model=TRUE)
# change 
t97b<-round(summary(qr2)$coeff[2,1],digits=2)
t97ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t97ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 98
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.98, data = finallong3, method='fn', model=TRUE)
# change 
t98b<-round(summary(qr2)$coeff[2,1],digits=2)
t98ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t98ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
# 99
qr2 <- rq(dither(arsenic, type="symmetric", value = 0.1 )~ year, tau = 0.99, data = finallong3, method='fn', model=TRUE)
# change 
t99b<-round(summary(qr2)$coeff[2,1],digits=2)
t99ll<-summary(qr2)$coeff[2,1]-qt(0.975, df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]
t99ul<-summary(qr2)$coeff[2,1]+qt(0.975,df=summary(fit1)$df[2])*summary(qr2)$coeff[2,2]

ALLbeta<-rbind(t50b,t55b,t60b,t65b,t70b,t75b,t80b,t85b,t90b,t95b,t96b,t97b,t98b,t99b)
ALLll<-rbind(t50ll,t55ll,t60ll,t65ll,t70ll,t75ll,t80ll,t85ll,t90ll,t95ll,t96ll,t97ll,t98ll,t99ll)
ALLul<-rbind(t50ul,t55ul,t60ul,t65ul,t70ul,t75ul,t80ul,t85ul,t90ul,t95ul,t96ul,t97ul,t98ul,t99ul)

rownames(ALLbeta)[1] <- "p 50th"
rownames(ALLbeta)[2] <- "p 55th"
rownames(ALLbeta)[3] <- "p 60th"
rownames(ALLbeta)[4] <- "p 65th"
rownames(ALLbeta)[5] <- "p 70th"
rownames(ALLbeta)[6] <- "p 75th"
rownames(ALLbeta)[7] <- "p 80th"
rownames(ALLbeta)[8] <- "p 85th"
rownames(ALLbeta)[9] <- "p 90th"
rownames(ALLbeta)[10] <- "p 95th"
rownames(ALLbeta)[11] <- "p 96th"
rownames(ALLbeta)[12] <- "p 97th"
rownames(ALLbeta)[13] <- "p 98th"
rownames(ALLbeta)[14] <- "p 99th"
rownames(ALLll)[1] <- "p 50th"
rownames(ALLll)[2] <- "p 55th"
rownames(ALLll)[3] <- "p 60th"
rownames(ALLll)[4] <- "p 65th"
rownames(ALLll)[5] <- "p 70th"
rownames(ALLll)[6] <- "p 75th"
rownames(ALLll)[7] <- "p 80th"
rownames(ALLll)[8] <- "p 85th"
rownames(ALLll)[9] <- "p 90th"
rownames(ALLll)[10] <- "p 95th"
rownames(ALLll)[11] <- "p 96th"
rownames(ALLll)[12] <- "p 97th"
rownames(ALLll)[13] <- "p 98th"
rownames(ALLll)[14] <- "p 99th"
rownames(ALLul)[1] <- "p 50th"
rownames(ALLul)[2] <- "p 55th"
rownames(ALLul)[3] <- "p 60th"
rownames(ALLul)[4] <- "p 65th"
rownames(ALLul)[5] <- "p 70th"
rownames(ALLul)[6] <- "p 75th"
rownames(ALLul)[7] <- "p 80th"
rownames(ALLul)[8] <- "p 85th"
rownames(ALLul)[9] <- "p 90th"
rownames(ALLul)[10] <- "p 95th"
rownames(ALLul)[11] <- "p 96th"
rownames(ALLul)[12] <- "p 97th"
rownames(ALLul)[13] <- "p 98th"
rownames(ALLul)[14] <- "p 99th"
ALL<-cbind(ALLbeta,ALLll,ALLul)
ALL2<-as.data.frame(ALL)
colnames(ALL2)[1] <- "Beta"
colnames(ALL2)[2] <- "LL"
colnames(ALL2)[3] <- "UL"
ALL2$Region<-"All CWSs"


QuantFig<-rbind(ALL2,AKHI2,CMW2,EMW2,MA2,NE2,PNW2,SE2,SW2)
# Keep row name as column
QuantFig<-setDT(QuantFig, keep.rownames = TRUE)[]
colnames(QuantFig)[1] <- "Quantile"
# keep only first few characters 
QuantFig$Quantile <- substr(QuantFig$Quantile, 0, 6) #only keep first 6 characters in character vector (keep date, not time)


# Means and 95% CIs by each region:
AKHI<-cws[which(cws$Region=="AKHI"),]
SE<-cws[which(cws$Region=="SE"),]
MA<-cws[which(cws$Region=="MA"),]
CMW<-cws[which(cws$Region=="CMW"),]
EMW<-cws[which(cws$Region=="EMW"),]
NE<-cws[which(cws$Region=="NE"),]
PNW<-cws[which(cws$Region=="PNW"),]
SW<-cws[which(cws$Region=="SW"),]

fit1 <- glm(ThreeYearAs20062008 ~ +1, family=gaussian(link="identity"), data=AKHI)
summary(fit1)
AKHImean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                          summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                          summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))
fit1 <- glm(ThreeYearAs20062008 ~ +1, family=gaussian(link="identity"), data=SE)
summary(fit1)
SEmean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                                summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                                summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))
fit1 <- glm(ThreeYearAs20062008 ~ +1, family=gaussian(link="identity"), data=MA)
summary(fit1)
MAmean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                                summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                                summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))
fit1 <- glm(ThreeYearAs20062008 ~ +1, family=gaussian(link="identity"), data=CMW)
summary(fit1)
CMWmean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                                summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                                summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))
fit1 <- glm(ThreeYearAs20062008 ~ +1, family=gaussian(link="identity"), data=EMW)
summary(fit1)
EMWmean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                                summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                                summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))
fit1 <- glm(ThreeYearAs20062008 ~ +1, family=gaussian(link="identity"), data=NE)
summary(fit1)
NEmean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                                summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                                summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))
fit1 <- glm(ThreeYearAs20062008 ~ +1, family=gaussian(link="identity"), data=PNW)
summary(fit1)
PNWmean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                                summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                                summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))
fit1 <- glm(ThreeYearAs20062008 ~ +1, family=gaussian(link="identity"), data=SW)
summary(fit1)
SWmean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                                summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                                summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))
fit1 <- glm(ThreeYearAs20062008 ~ +1, family=gaussian(link="identity"), data=cws)
summary(fit1)
CWSmean<-convert.to.ci(round(c(summary(fit1)$coeff[1,1],
                              summary(fit1)$coeff[1,1]-qt(0.975, df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2],
                              summary(fit1)$coeff[1,1]+qt(0.975,df=summary(fit1)$df[2])*summary(fit1)$coeff[1,2]),2))

# Create some new palettes of 4 each
# Reminder of colors for each region:
#plot1pal<-c("#08589e","#2b8cbe","#7bccc4","#ccebc5") # SE, MA, CMW, EMW
#plot2pal<-c("#fee391","#fe9929","#cc4c02","#8c2d04") # NE, PNW, AKHI, SW


plot3pal<-c("#525252" ,"#08589e","#2b8cbe","#7bccc4","#ccebc5","#fee391","#fe9929","#8c2d04") # Overall, SE, MA, CMW, EMW, NE, PNW, SW



raincloud_theme = theme(
  text = element_text(size = 14),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(hjust=0.5,lineheight=.8, face="bold", size = 18),
  panel.border = element_blank(),
  #panel.grid.minor = element_blank(),
  #panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

ggplot(QuantFig, aes(Quantile, group=1)) + 
  geom_line(aes(y=Beta), colour="royalblue4") + 
  geom_ribbon(aes(ymin=LL, ymax=UL), alpha=0.2,color="blue")+
  facet_wrap(~Region)

QuantFig$Quantile[which(QuantFig$Quantile=="p 50th")]<-"50th"
QuantFig$Quantile[which(QuantFig$Quantile=="p 55th")]<-"55th"
QuantFig$Quantile[which(QuantFig$Quantile=="p 60th")]<-"60th"
QuantFig$Quantile[which(QuantFig$Quantile=="p 65th")]<-"65th"
QuantFig$Quantile[which(QuantFig$Quantile=="p 70th")]<-"70th"
QuantFig$Quantile[which(QuantFig$Quantile=="p 75th")]<-"75th"
QuantFig$Quantile[which(QuantFig$Quantile=="p 80th")]<-"80th"
QuantFig$Quantile[which(QuantFig$Quantile=="p 85th")]<-"85th"
QuantFig$Quantile[which(QuantFig$Quantile=="p 90th")]<-"90th"
QuantFig$Quantile[which(QuantFig$Quantile=="p 95th")]<-"95th"
QuantFig$Quantile[which(QuantFig$Quantile=="p 96th")]<-"96th"
QuantFig$Quantile[which(QuantFig$Quantile=="p 97th")]<-"97th"
QuantFig$Quantile[which(QuantFig$Quantile=="p 98th")]<-"98th"
QuantFig$Quantile[which(QuantFig$Quantile=="p 99th")]<-"99th"
# Try to just plot quantiles >0.8
QuantFig2<-QuantFig[which(QuantFig$Quantile!="50th"&QuantFig$Quantile!="55th"&QuantFig$Quantile!="60th"&QuantFig$Quantile!="65th"&
                            QuantFig$Quantile!="70th"&QuantFig$Quantile!="75th"),]


# Ordered by mean in 2006-2008:
# This order is: All, SE, MA,CMW,EMW, NE, PNW, SW
QuantFig2$Region <- factor(QuantFig2$Region, levels = c("Alaska/Hawaii", "All CWSs", "Southeast", "Mid-Atlantic", "Central Midwest", 
                                                        "Eastern Midwest", "New England", "Pacific Northwest", "Southwest"))
# Make dataframe with text annotations
ann_text<-data.frame(label=c("Mean 2006-2008\n1.89 (1.83, 1.95) µg/L","Mean 2006-2008\n0.66 (0.54, 0.79) µg/L","Mean 2006-2008\n0.88 (0.73, 1.03) µg/L","Mean 2006-2008\n1.94 (1.72, 2.15) µg/L","Mean 2006-2008\n2.03 (1.88, 2.18) µg/L","Mean 2006-2008\n2.05 (1.79, 2.30) µg/L","Mean 2006-2008\n2.15 (1.99, 2.31) µg/L","Mean 2006-2008\n3.59 (3.46, 3.72) µg/L"), 
                     Region=c("All CWSs", "Southeast", "Mid-Atlantic", "Central Midwest","Eastern Midwest", "New England", "Pacific Northwest", "Southwest"),
                     x=c(4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5),
                     y=c(2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0))

pdf(file=paste0( folder.output , "/Figure 2. QR plots by region.pdf"), 15, 9)
ggplot(QuantFig2[which(QuantFig2$Region!="Alaska/Hawaii"),], aes(Quantile, group=1,fill=Region)) + 
  geom_line(aes(y=Beta),color="black")+ 
  geom_ribbon(aes(ymin=LL, ymax=UL), alpha=0.7)+
  facet_wrap(~Region, ncol=4)+
  theme_bw() +
  theme(strip.text = element_text(size=18,face="bold", color="black",family="Avenir Next"),strip.background = element_rect(color="white", fill="white", size=0, linetype=NULL))+
  raincloud_theme+
  scale_color_manual(values = plot3pal) +
  scale_fill_manual(values = plot3pal) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  geom_hline(yintercept=0,linetype = "dotdash", size=0.75,color="tomato4")+
  scale_y_continuous(breaks=c(-10.0,-6.9,-3.8,0,2.5) , labels =c("-10","-6.9","-3.8" ,"0","2.5"))+
  labs(y="Change in quantile water arsenic concentration (µg/L)\nfrom 2006-2008 to 2009-2011" )+
  coord_cartesian(ylim = c(-10, 2.5))+  # Use coord_cartesian instead of limits to prevent geo_ribbons from being cut off
  geom_text(data=ann_text, mapping=aes(x=x, y=y, label=label,color="black",fontface="bold", family="Avenir Next", size=12))+
  theme(axis.title =element_text(family="Avenir Next",color="black",size=20))+theme(axis.text = element_text(family="Avenir Next",color="black",size=20))+
  theme(axis.text.y = element_text(size=16))+  theme(axis.text.x = element_text(size=14))+theme(axis.title.x = element_text(size=14))
dev.off()


### end



