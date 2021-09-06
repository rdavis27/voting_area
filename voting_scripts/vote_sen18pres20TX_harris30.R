# INSTRUCTIONS TO RUN THIS SCRIPT
# 1. Download all-geocodes-v2020.xlsx from https://www2.census.gov/programs-surveys/popest/geographies/2020/ .
# 2. Download president.csv from https://data.capitol.texas.gov/dataset/2020_general
# by clicking Explore button to the right of "President Vice-President"
# and selecting "Go to resource".
# 3. Download 'u.s. sen.csv' from https://data.capitol.texas.gov/dataset/2018_general
# by clicking Explore button to the right of "U.S. Senator"
# and selecting "Go to resource".
# 4. Put all files in the working directory.
# 5. Change any variables after the library calls as described if desired.
# 6. Run or source this script.

library(tidyverse) # needed for read_csv and other functions
library(readxl) # needed for read_excel
library(ggplot2) # needed for ggplot

# Set xcounty to desired county or empty string for all of Texas
xcounty <- "Harris"
# Set minvotes to minimum number of votes of precincts to plot
minvotes <- 30

# Read CountyCodes for Texas
cc <- read_excel("all-geocodes-v2020.xlsx", sheet = "all-geocodes-v2019", skip = 4)
names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
cctx <- cc[cc$StateCode == "48",] # 48 is StateCode for Texas

# Read precinct data for 2020 Presidential race in Texas
xx <- read_csv("president.csv")
race2 <- "TX_2020_President"

# Add column containing county names
xx$COUNTY <- substring(xx$CNTYVTD,1,3) # CountyCode is first 3 characters of CNTYVTD
for (i in 1:NROW(xx)){
  xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]] # change COUNTY to Area name
  xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i]) # delete " County" at end of COUNTY
}

# Add columns containing area, total votes, and candidate votes
xx$AREA <- xx$CNTYVTD # can use substring(xx$CNTYVTD,4) or VTDKEY if present
xx$TOTAL <- 0 #initialize to zero
xx <- xx[,c(8,9,10,3:7)] # set columns to COUNTY, AREA, TOTAL, followed by candidates
names(xx) <- c("COUNTY","AREA","TOTAL","Biden","Trump","Jorgensen","Hawkins","Writein")
xx$TOTAL <- rowSums(xx[,4:NCOL(xx)]) # set TOTAL to sum of all candidate votes
#partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","GRN","Writein")
yy <- xx # make this the second race

# Read precinct data for 2018 Senate race in Texas
xx <- read_csv("u.s. sen.csv")
race1 <- "TX_2018_Senate"

# Add column containing county names
xx$COUNTY <- substring(xx$CNTYVTD,1,3) # CountyCode is first 3 characters of CNTYVTD
for (i in 1:NROW(xx)){
  xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]] # change COUNTY to Area name
  xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i]) # delete " County" at end of COUNTY
}

# Add columns containing area, total votes, and candidate votes
xx$AREA <- xx$VTDKEY # use substring(xx$CNTYVTD,4) if VTDKEY not present
xx$AREA <- xx$CNTYVTD # can use substring(xx$CNTYVTD,4) or VTDKEY if present
xx$TOTAL <- 0 #initialize to zero
# Change the following two lines to match file of a specific race
xx <- xx[,c(6,7,8,4,3,5)] # set columns to COUNTY, AREA, TOTAL, DEM, REP, other candidates
names(xx) <- c("COUNTY","AREA","TOTAL","ORourke","Cruz","Dikeman")
xx$TOTAL <- rowSums(xx[,4:NCOL(xx)]) # set TOTAL to sum of all candidate votes
#partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB")

#Prepare to merge races
aa <- xx[,1:5]
names(aa)[4:5] <- c("DEM1", "REP1") # temporarily rename to aid coding
aa$MARGIN1 <- aa$DEM1 - aa$REP1
aa$TOTAL1  <- aa$TOTAL
aa <- aa[,-3] # drop original TOTAL column

bb <- yy[,1:5]
names(bb)[4:5] <- c("DEM2", "REP2") # temporarily rename to aid coding
bb$MARGIN2 <- bb$DEM2 - bb$REP2
bb$TOTAL2  <- bb$TOTAL
bb <- bb[,-3] # drop original TOTAL column

dd <- as.data.frame(merge(aa, bb, by = c("COUNTY","AREA"), all = TRUE))

# Place all unmatched precincts into area named "Other"
ee <- dd[is.na(dd$MARGIN1) | is.na(dd$MARGIN2),]
if (NROW(ee) > 0){
  dd <- dd[!is.na(dd$MARGIN1) & !is.na(dd$MARGIN2),]
  ee$AREA <- "Other"
  ee <- ee %>%
    group_by(COUNTY,AREA) %>%
    summarize(DEM1=sum(DEM1, na.rm = TRUE),
              REP1=sum(REP1, na.rm = TRUE),
              MARGIN1=sum(MARGIN1, na.rm = TRUE),
              TOTAL1=sum(TOTAL1, na.rm = TRUE),
              DEM2=sum(DEM2, na.rm = TRUE),
              REP2=sum(REP2, na.rm = TRUE),
              MARGIN2=sum(MARGIN2, na.rm = TRUE),
              TOTAL2=sum(TOTAL2, na.rm = TRUE))
  ee <- as.data.frame(ee)
  dd <- rbind(dd, ee)
}else{
  print("***** No unmatched precincts")
}
# End of code to create "Other" area

# Calculate shifts between them
dd$DEM_SH <- dd$DEM2 - dd$DEM1
dd$REP_SH <- dd$REP2 - dd$REP1
dd$MAR_SH <- dd$MARGIN2 - dd$MARGIN1
dd$TOT_SH <- dd$TOTAL2  - dd$TOTAL1

#Calculate percentages
pp <- dd
pp$Votes1 <- pp$TOTAL1
pp$Votes2 <- pp$TOTAL2
pp$DEM1 <- 100 * pp$DEM1 / pp$TOTAL1
pp$REP1 <- 100 * pp$REP1 / pp$TOTAL1
pp$MARGIN1 <- 100 * pp$MARGIN1 / pp$TOTAL1
pp$DEM2 <- 100 * pp$DEM2 / pp$TOTAL2
pp$REP2 <- 100 * pp$REP2 / pp$TOTAL2
pp$MARGIN2 <- 100 * pp$MARGIN2 / pp$TOTAL2
pp$TOTAL1 <- pp$DEM1 + pp$REP1 # use this rather than 100
pp$TOTAL2 <- pp$DEM2 + pp$REP2 # use this rather than 100
pp$DEM_SH <- pp$DEM2 - pp$DEM1
pp$REP_SH <- pp$REP2 - pp$REP1
pp$MAR_SH <- pp$MARGIN2 - pp$MARGIN1
pp$TOT_SH <- pp$TOTAL2 - pp$TOTAL1
pp$Party <- "2_Toss-Up"
pp$Party[pp$MARGIN1 < -5] <- "1_Solid R"
pp$Party[pp$MARGIN1 >  5] <- "3_Solid D"

# Write out files
write_csv(xx, "vote_sen18TX_count.csv")
write_csv(yy, "vote_pres20TX_count.csv")
names(dd)[3:4] <- names(xx)[4:5] # restore candidate names
names(dd)[7:8] <- names(yy)[4:5]
write_csv(dd, "vote_sen18pres20TX_count.csv")
names(pp)[3:4] <- names(xx)[4:5] # restore candidate names
names(pp)[7:8] <- names(yy)[4:5]
write_csv(pp, "vote_sen18pres20TX_percent.csv")

# Prepare plot
if (xcounty != ""){
  pp <- pp[pp$COUNTY == xcounty,]
}
pp <- pp[pp$Votes2 >= minvotes,]

gg <- ggplot(pp, aes(x=MARGIN1, y=MAR_SH))
gg <- gg + geom_point(data=pp, alpha=0.7, aes(color=Party, size=Votes2))
#gg <- gg + geom_point(data=pp, alpha=0.7, aes(color=MARGIN1))
gg <- gg + geom_vline(xintercept=0, color="black")
gg <- gg + geom_hline(yintercept=0, color="black")
gg <- gg + geom_abline(intercept=0, slope=-1, color="black", linetype="dashed")
title <- paste(xcounty,"County, TX: Shift in Margin Vote Share from",
               race1,"to",race2,"in areas with",minvotes,"or more votes",
               "(positive direction is more Dem)")
gg <- gg + ggtitle(title)
gg <- gg + xlab(paste("Margin Vote Share for",race1))
gg <- gg + ylab(paste("Shift in Margin Vote Share for",race2))
if (NROW(ee) > 0){
  for (i in 1:NROW(ee)){
    gg <- gg + annotate("text", x = ee$MARGIN1, y =ee$MAR_SH, label = "Other", color="black")
  }
}
# Optional scale settings
# gg <- gg + scale_x_continuous(breaks = seq(-100,100,20), minor_breaks = seq(-100,100,20))
# gg <- gg + scale_y_continuous(breaks = seq(-80,80,20), minor_breaks = seq(-80,80,20))
# gg <- gg + coord_cartesian(xlim = c(-100,100), ylim = c(-80,80))
# Create window to contain plot
x11(width = 1000, height = 600)
print(gg)
