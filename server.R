library(tidyverse)
library(readxl)
library(ggplot2)
library(reshape2)

library(tigris)
library(leaflet)
library(htmltools)
library(xlsx)
library(RcppRoll)
library(cowplot)

areaWidth <- 900
areaHeight <- 600
input_dir <- "input/"
data_dir  <- "data/"
filerr <- "filerr.txt"
file.remove(filerr)

shinyServer(
    function(session,input, output) {
        options(width = 999, readr.show_progress = FALSE)
        options(max.print=999999)

        catmsg <- function(msg){
            line <- paste0(msg,"\n")
            cat(file = filerr, append = TRUE, line)
            cat(file = stderr(), line)
        }
        ## All ElectionWare (EW) counties and filenames
        # az_ew_counties <- c(
        #     "Apache","Cochise",
        #     "Gila","Graham",
        #     "Greenlee","La Paz",
        #     "Mohave","Navajo",
        #     "Pima","Pinal",
        #     "Santa Cruz","Yuma")
        # az_ew_files <- c(
        #     "6967.Apache.Detail.txt","6970.Cochise.Detail.txt",
        #     "6972.Gila_.Detail.txt","6887.Graham.Detail.txt",
        #     "6841.Greenlee.Detail.txt","6957.La Paz.Detail.txt",
        #     "6969.Mohave.Detail.txt","6955.Navajo.Detail.txt",
        #     "6981.Pima_.Detail.txt","6956.Pinal_.Detail.txt",
        #     "6971.Santa Cruz.Detail.txt","6979.Yuma_.Detail.txt")
        ## Only the following ElectionWare (EW) counties work currently
        ## due to changes in precinct names from 2018 to 2020.
        az_ew_counties <- c(
            "Apache","Cochise","Gila","Graham","Greenlee","La Paz",
            "Mohave","Navajo","Pima","Pinal","Santa Cruz","Yuma")
        az_ew_files <- c(
            "6967.Apache.Detail.txt","6970.Cochise.Detail.txt",
            "6972.Gila_.Detail.txt","6887.Graham.Detail.txt",
            "6841.Greenlee.Detail.txt","6957.La Paz.Detail.txt",
            "6969.Mohave.Detail.txt","6955.Navajo.Detail.txt","6981.Pima_.Detail.txt",
            "6956.Pinal_.Detail.txt","6971.Santa Cruz.Detail.txt","6979.Yuma_.Detail.txt")
        createAZ_2018_Senate <- function(){ # currently just loads Maricopa
            xx <- read_delim(paste0(input_dir,"AZ/2018/6989.Maricopa.Detail.txt"),'\t',
                             col_names = TRUE, col_types = "cccdddddcdddddddddd")
            office <- "US Senate" #UPDATE
            xx <- xx[xx$CONTEST_FULL_NAME == office,]
            xx <- xx[,c("PRECINCT_NAME","CANDIDATE_FULL_NAME","TOTAL")] #leave off "IS_WRITEIN","undervote","overvote"
            xx$COUNTY <- "Maricopa"
            xx$TOT <- 0
            xx <- xx[,c("COUNTY","PRECINCT_NAME","CANDIDATE_FULL_NAME","TOTAL")]
            names(xx) <- c("COUNTY","AREA","Candidate","Votes")
            xx$Candidate <- gsub(" - ","_",xx$Candidate)
            xx$Candidate <- gsub(" ","",xx$Candidate)
            xx$Candidate[xx$Candidate == "Write-InCandidate"] <- "NON_WRITEIN"
            xx <- xx %>%
                group_by(COUNTY,AREA,Candidate) %>%
                summarize(Votes=sum(Votes))
            xx <- xx %>% spread(Candidate,Votes)
            xx$TOTAL <- 0
            #COUNTY   AREA           `DEM_SINEMA,KYRSTEN` `GRN_GREEN,ANGELA` NON_WRITEIN `REP_MCSALLY,MARTHA` TOTAL
            #Maricopa 0001 ACACIA                    1209                 84           8                  891     0

            start <- c( 8,12,102,112,168,206)
            end   <- c(11,17,104,167,205,234)
            nms   <- c("AREA","Votes","Party","Contest","Candidate","AreaName")
            cc <- az_ew_files
            for (i in 1:length(cc)){
                filename <- paste0(input_dir,"AZ/2018/",cc[i])
                dd <- read_fwf(filename, fwf_positions(start, end, nms), col_types = "cicccc")
                if (az_ew_counties[i] == "Pima"){
                    office <- "U.S. SENATOR" #UPDATE
                }
                else if (az_ew_counties[i] %in% c("Gila","Greenlee","Mohave")){
                    office <- "United States Senator" #UPDATE
                }
                else{
                    office <- "U.S. Senator" #UPDATE
                }
                dd <- dd[substr(dd$Contest,1,nchar(office)) == office,]
                if (NROW(dd) == 0){
                    catmsg(paste0("====> WARNING: ",cc[i]," COUNTY had no ",office))
                    next
                }
                dd$COUNTY <- az_ew_counties[i]
                #dd$AREA[is.na(dd$AREA)] <- dd$AreaId[is.na(dd$AREA)]
                #dd$AREA[dd$AREA == ""] <- dd$AreaId[dd$AREA == ""]
                dd$AREA[dd$COUNTY == "Apache"] <- dd$AreaName[dd$COUNTY == "Apache"]
                dd$AREA[dd$COUNTY == "Cochise"] <- dd$AreaName[dd$COUNTY == "Cochise"]
                dd$AREA[dd$COUNTY == "Cochise"] <- dd$AreaName[dd$COUNTY == "Cochise"]
                dd$AREA[dd$COUNTY == "Gila"] <- dd$AreaName[dd$COUNTY == "Gila"]
                dd$AREA[dd$COUNTY == "Graham"] <- dd$AreaName[dd$COUNTY == "Graham"]
                dd$AREA[dd$COUNTY == "Greenlee"] <- dd$AreaName[dd$COUNTY == "Greenlee"]
                dd$AREA[dd$COUNTY == "La Paz"] <- dd$AreaName[dd$COUNTY == "La Paz"]
                dd$AREA[dd$COUNTY == "Mohave"] <- dd$AreaName[dd$COUNTY == "Mohave"]
                dd$AREA[dd$COUNTY == "Navajo"] <- dd$AreaName[dd$COUNTY == "Navajo"]
                dd$AREA[dd$COUNTY == "Pinal"] <- dd$AreaName[dd$COUNTY == "Pinal"]
                dd$AREA[dd$COUNTY == "Santa Cruz"] <- dd$AreaName[dd$COUNTY == "Santa Cruz"]
                dd <- dd[,c("COUNTY","AREA","Candidate","Party","Votes")]
                dd$AREA[dd$COUNTY == "Pima"] <- gsub("^0","",dd$AREA[dd$COUNTY == "Pima"])
                dd$AREA[dd$COUNTY == "Yuma"] <- gsub("^0","",dd$AREA[dd$COUNTY == "Yuma"])
                dd$Party[dd$Party == "."] <- "NON"
                dd$Candidate[dd$Candidate == "WRITE-IN"] <- "WRITEIN"
                for (j in 1:NROW(dd)){
                    dd$Candidate[j] <- head(strsplit(dd$Candidate[j],split="/")[[1]],1) #Biden / Harris
                    dd$Candidate[j] <- gsub(" ","",trimws(dd$Candidate[j]))
                    if (!is.na(dd$Party[j])){
                        dd$Candidate[j] <- paste0(dd$Party[j],"_",dd$Candidate[j])
                    }
                }
                dd <- dd[,-4] # delete Party
                # check for matches first???
                dd <- dd %>%
                    group_by(COUNTY,AREA,Candidate) %>%
                    summarize(Votes=sum(Votes))
                dd <- dd %>% spread(Candidate,Votes)
                dd$TOTAL <- 0
                dd <- subset(dd,select = -c(OVERVOTES,UNDERVOTES))
                xx <- rbind(xx,dd)
            }
            namesxx <- names(xx)
            partyxx <- namesxx
            for (j in 3:(NCOL(xx)-1)){
                if (namesxx[j] == "WRITE-IN"){
                    partyxx[j] <- "Writein"
                    namesxx[j] <- "Writein"
                }
                else{
                    strs <- unlist(strsplit(namesxx[j],split="_"))
                    partyxx[j] <- strs[1]
                    if (length(strs) >= 2){
                        fullname <- strs[2]
                        namesxx[j] <- unlist(strsplit(fullname,split=","))[1] #last name
                    }
                }
            }
            ii <- c(1,2,NCOL(xx))
            idem <- 0
            irep <- 0
            if ("DEM" %in% partyxx){
                idem <- which(partyxx == "DEM")
                ii <- c(ii, idem)
            }
            if ("REP" %in% partyxx){
                irep <- which(partyxx == "REP")
                ii <- c(ii, irep)
            }
            for (j in 3:(NCOL(xx)-1)){
                if (j != idem & j != irep){
                    ii <- c(ii, j)
                }
                # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
                #     xx$TOTAL <- xx$TOTAL + xx[,j]
                # }
            }
            xx <- xx[,ii]
            namesxx <- namesxx[ii]
            partyxx <- partyxx[ii]
            names(xx) <- namesxx
            write(paste(partyxx, collapse = " "), paste0(data_dir,"AZ_2018_Senate.csv"))
            write_delim(xx, paste0(data_dir,"AZ_2018_Senate.csv"), append = TRUE, col_names = TRUE)
        }
        createAZ_2020_President <- function(votetypes){
            txt <- "text"
            num <- "numeric"
            key <- "text"
            boo <- "text" # boo as numeric causes error
            xx <- read_excel(paste0(input_dir,"AZ/2020/Results.Detail_2020General.xml/",
                                    "Results.Detail_2020General .xls.xlsx"),
                             sheet = "Sheet1", skip = 0, col_names = TRUE,
                             col_types = c(txt,txt,txt,num,key, #key
                                           txt,num,num,num,num, #precinctsParticipating
                                           num,num,key,txt,key, #districtKey
                                           txt,num,num,boo,num, #countiesParticipating
                                           num,num,num,num,key, #key6
                                           txt,num,num,num,key, #key11
                                           txt,num,txt,num,boo, #isWriteIn
                                           key,txt,num,txt,num, #votes14
                                           key,txt,num,txt,num)) #votes19
            office <- "President of the United States" #UPDATE
            xx <- xx[xx$contestLongName == office,]
            if (votetypes != "" & votetypes != "all"){
                xx <- xx[xx$voteTypeName18 %in% votetypes,]
            }
            xx <- xx[,c("name13","name16","choiceName","party","votes19")]
            names(xx) <- c("COUNTY","AREA","Candidate","Party","Votes")
            xx <- xx[!is.na(xx$AREA),]
            xx$AREA[xx$COUNTY == "Yuma"] <- gsub("^PRECINCT ","",xx$AREA[xx$COUNTY == "Yuma"])
            xx$Party[xx$Party == "Party for Socialism and Liberation"] <- "PSL"
            for (j in 1:NROW(xx)){
                xx$Candidate[j] <- head(strsplit(xx$Candidate[j],split=",")[[1]],1) #use last name
                xx$Candidate[j] <- gsub(" ","",xx$Candidate[j]) # remove blanks
                if (!is.na(xx$Party[j])){
                    xx$Candidate[j] <- paste0(xx$Candidate[j],"_",xx$Party[j])
                }
            }
            xx <- xx[,-4] # delete Party
            # check for matches first???
            xx <- xx %>%
                group_by(COUNTY,AREA,Candidate) %>%
                summarize(Votes=sum(Votes))
            xx <- xx %>% spread(Candidate,Votes)
            xx$TOTAL <- 0
            #gxx2 <<- xx #DEBUG-RM
            # for (j in 4:(NCOL(xx)-1)){
            #     xx$TOTAL <- xx$TOTAL + xx[,j]
            # }
            namesxx <- names(xx)
            partyxx <- namesxx
            for (j in 3:(NCOL(xx)-1)){
                partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
                namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
            }
            ii <- c(1,2,NCOL(xx))
            idem <- 0
            irep <- 0
            if ("DEM" %in% partyxx){
                idem <- which(partyxx == "DEM")
                ii <- c(ii, idem)
            }
            if ("REP" %in% partyxx){
                irep <- which(partyxx == "REP")
                ii <- c(ii, irep)
            }
            for (j in 3:(NCOL(xx)-1)){
                if (j != idem & j != irep){
                    ii <- c(ii, j)
                }
                # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
                #     xx$TOTAL <- xx$TOTAL + xx[,j]
                # }
            }
            xx <- xx[,ii]
            namesxx <- namesxx[ii]
            partyxx <- partyxx[ii]
            #gxx3 <<- xx #DEBUG-RM
            #START OF CODE FOR PRESIDENT ONLY
            # xx <- xx[,c(1:5,8:10,7,6)]
            # names(xx)[9:10] <- c("Writein","Misc")
            # xx$Writein[is.na(xx$Writein)] <- 0
            # namesxx <- namesxx[c(1:5,8:10,7,6)]
            # partyxx <- partyxx[c(1:5,8:10,7,6)]
            #END OF CODE FOR PRESIDENT ONLY
            names(xx) <- namesxx
            if (votetypes[1] == "Early Ballots"){
                fileout <- paste0("AZ_2020_President_Early.csv")
            }
            else if (votetypes[1] == "Polling Place"){
                fileout <- paste0("AZ_2020_President_Polls.csv")
            }
            else if (votetypes[1] == "Provisional Ballots"){
                fileout <- paste0("AZ_2020_President_Prov.csv")
            }
            else{
                fileout <- paste0("AZ_2020_President.csv")
            }
            write(paste(partyxx, collapse = " "), paste0(data_dir,fileout))
            write_delim(xx, paste0(data_dir,fileout), append = TRUE, col_names = TRUE)
        }
        createAZ_2020_Senate <- function(votetypes){
            txt <- "text"
            num <- "numeric"
            key <- "text"
            boo <- "text" # boo as numeric causes error
            xx <- read_excel(paste0(input_dir,"AZ/2020/Results.Detail_2020General.xml/",
                                    "Results.Detail_2020General .xls.xlsx"),
                             sheet = "Sheet1", skip = 0, col_names = TRUE,
                             col_types = c(txt,txt,txt,num,key, #key
                                           txt,num,num,num,num, #precinctsParticipating
                                           num,num,key,txt,key, #districtKey
                                           txt,num,num,boo,num, #countiesParticipating
                                           num,num,num,num,key, #key6
                                           txt,num,num,num,key, #key11
                                           txt,num,txt,num,boo, #isWriteIn
                                           key,txt,num,txt,num, #votes14
                                           key,txt,num,txt,num)) #votes19
            office <- "U.S. Senator (Term Expires Jan. 2023)" #UPDATE
            xx <- xx[xx$contestLongName == office,]
            if (votetypes != "" & votetypes != "all"){
                xx <- xx[xx$voteTypeName18 %in% votetypes,]
            }
            xx <- xx[,c("name13","name16","choiceName","party","votes19")]
            names(xx) <- c("COUNTY","AREA","Candidate","Party","Votes")
            xx <- xx[!is.na(xx$AREA),]
            xx$AREA[xx$COUNTY == "Yuma"] <- gsub("^PRECINCT ","",xx$AREA[xx$COUNTY == "Yuma"])
            xx$Party[xx$Party == "Party for Socialism and Liberation"] <- "PSL"
            for (j in 1:NROW(xx)){
                xx$Candidate[j] <- head(strsplit(xx$Candidate[j],split=",")[[1]],1) #use last name
                xx$Candidate[j] <- gsub(" ","",xx$Candidate[j]) # remove blanks
                if (!is.na(xx$Party[j])){
                    xx$Candidate[j] <- paste0(xx$Candidate[j],"_",xx$Party[j])
                }
            }
            xx$Candidate[xx$Candidate == "Rodriguez_IND"] <- "Rodriguez1_IND"
            xx <- xx[,-4] # delete Party
            # check for matches first???
            xx <- xx %>%
                group_by(COUNTY,AREA,Candidate) %>%
                summarize(Votes=sum(Votes))
            xx <- xx %>% spread(Candidate,Votes)
            xx$TOTAL <- 0
            #gxx2 <<- xx #DEBUG-RM
            # for (j in 4:(NCOL(xx)-1)){
            #     xx$TOTAL <- xx$TOTAL + xx[,j]
            # }
            namesxx <- names(xx)
            partyxx <- namesxx
            for (j in 3:(NCOL(xx)-1)){
                partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
                namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
            }
            ii <- c(1,2,NCOL(xx))
            idem <- 0
            irep <- 0
            if ("DEM" %in% partyxx){
                idem <- which(namesxx == "Kelly") #specify name if multiple DEMs
                ii <- c(ii, idem)
            }
            if ("REP" %in% partyxx){
                irep <- which(namesxx == "McSally") #specify name if multiple REPs
                ii <- c(ii, irep)
            }
            for (j in 3:(NCOL(xx)-1)){
                if (j != idem & j != irep){
                    ii <- c(ii, j)
                }
                # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
                #     xx$TOTAL <- xx$TOTAL + xx[,j]
                # }
            }
            xx <- xx[,ii]
            namesxx <- namesxx[ii]
            partyxx <- partyxx[ii]
            #gxx3 <<- xx #DEBUG-RM
            #START OF CODE FOR PRESIDENT ONLY
            # xx <- xx[,c(1:5,8:10,7,6)]
            # names(xx)[9:10] <- c("Writein","Misc")
            # xx$Writein[is.na(xx$Writein)] <- 0
            # namesxx <- namesxx[c(1:5,8:10,7,6)]
            # partyxx <- partyxx[c(1:5,8:10,7,6)]
            #END OF CODE FOR PRESIDENT ONLY
            names(xx) <- namesxx
            if (votetypes[1] == "Early Ballots"){
                fileout <- paste0("AZ_2020_Senate_Early.csv")
            }
            else if (votetypes[1] == "Polling Place"){
                fileout <- paste0("AZ_2020_Senate_Polls.csv")
            }
            else if (votetypes[1] == "Provisional Ballots"){
                fileout <- paste0("AZ_2020_Senate_Prov.csv")
            }
            else{
                fileout <- paste0("AZ_2020_Senate.csv")
            }
            write(paste(partyxx, collapse = " "), paste0(data_dir,fileout))
            write_delim(xx, paste0(data_dir,fileout), append = TRUE, col_names = TRUE)
        }
        createFL_2020_County_Codes <- function(){
            files <- list.files(paste0(input_dir,"FL/2020-general-election-rev/"),
                                "*_PctResults20201103.txt")
            cc <- substr(files,1,3)
            print(cc)
            dd <- data.frame(cc)
            write_delim(dd, paste0(data_dir,"FL_County_Codes.csv"), append = FALSE, col_names = TRUE)
        }
        createFL_2020_Counties <- function(){
            filenamex <- paste0(data_dir,"FL_2020_President.csv")
            xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
            xx0 <<- read_delim(filenamex, ' ', skip = 1) #make xx0 available after tryCatch
            xx <- unique(xx0$COUNTY)
            dd <- data.frame(xx)
            names(dd) <- "COUNTY"
            write_delim(dd, paste0(data_dir,"FL_Counties.csv"), append = FALSE, col_names = TRUE)
        }
        fl_county_codes <- c(
            "ALA","BAK","BAY","BRA","BRE",
            "BRO","CAL","CHA","CIT","CLA",
            "CLL","CLM","DAD","DES","DIX",
            "DUV","ESC","FLA","FRA","GAD",
            "GIL","GLA","GUL","HAM","HAR",
            "HEN","HER","HIG","HIL","HOL",
            "IND","JAC","JEF","LAF","LAK",
            "LEE","LEO","LEV","LIB","MAD",
            "MAN","MON","MRN","MRT","NAS",
            "OKA","OKE","ORA","OSC","PAL",
            "PAS","PIN","POL","PUT","SAN",
            "SAR","SEM","STJ","STL","SUM",
            "SUW","TAY","UNI","VOL","WAK",
            "WAL","WAS")
        fl_counties <- c(
            "Alachua","Baker","Bay","Bradford","Brevard",
            "Broward","Calhoun","Charlotte","Citrus","Clay",
            "Collier","Columbia","Miami-Dade","Desoto","Dixie",
            "Duval","Escambia","Flagler","Franklin","Gadsden",
            "Gilchrist","Glades","Gulf","Hamilton","Hardee",
            "Hendry","Hernando","Highlands","Hillsborough","Holmes",
            "Indian River","Jackson","Jefferson","Lafayette","Lake",
            "Lee","Leon","Levy","Liberty","Madison",
            "Manatee","Monroe","Marion","Martin","Nassau",
            "Okaloosa","Okeechobee","Orange","Osceola","Palm Beach",
            "Pasco","Pinellas","Polk","Putnam","Santa Rosa",
            "Sarasota","Seminole","St. Johns","St. Lucie","Sumter",
            "Suwannee","Taylor","Union","Volusia","Wakulla",
            "Walton","Washington")
        createFL_2016_President <- function(){
            # input_dir <- "input/"
            # data_dir  <- "data/"
            cc <- fl_county_codes
            xx <- NULL
            for (i in 1:length(cc)){
                dd <- read_delim(paste0(input_dir,"FL/precinctlevelelectionresults2016gen/",
                                        cc[i],"_PctResults20161108.txt"), '\t', quote = "",
                                 col_names = FALSE, col_types = "ccdccccddddccdccddd")
                #Define names if col_names == FALSE
                names(dd) <- c("Code","COUNTY","ElectNo","ElectDate","ElectName",
                               "AreaId","AREA","RegAll","RegRep","RegDem",
                               "RegOth","Contest","DIST","ConCode","Candidate",
                               "Party","RegId","CandNo","Votes")
                #Filter by office (may vary by county)
                if (cc[i] == "SEM"){
                    office <- "PRESIDENT OF THE UNITED STATES" #UPDATE
                }
                else{
                    office <- "President of the United States" #UPDATE
                }
                dd <- dd[dd$Contest == office,]
                if (NROW(dd) == 0){
                    catmsg(paste0("====> WARNING: ",cc[i]," COUNTY had no ",office))
                    next
                }
                #Filter out blank lines, if necessary
                #dd$AREA[is.na(dd$AREA)] <- dd$AreaId[is.na(dd$AREA)]
                #dd$AREA[dd$AREA == ""] <- dd$AreaId[dd$AREA == ""]
                #Set AREA to match between races
                if (cc[i] %in% c("HEN","HER","JEF","LEV","MON","OKE","PUT","STL")){
                    dd$AREA <- sub("^0+", "", dd$AreaId)
                }
                else if (cc[i] %in% c("ALA","DES","DIX","FLA","HAR")){
                    dd$AREA <- str_pad(dd$AreaId,2,side = "left",pad = "0")
                }
                else if (cc[i] %in% c("PAS","SEM")){
                    dd$AREA <- str_pad(dd$AreaId,3,side = "left",pad = "0")
                }
                else if (cc[i] == "DAD"){
                    dd$AREA <- str_pad(paste0(dd$AreaId,"0"),4,side = "left",pad = "0")
                }
                else{
                    dd$AREA <- dd$AreaId
                }
                #Set dd to minimally required fields 
                dd <- dd[,c("DIST","COUNTY","AREA","Candidate","Party","Votes")]
                #Standardize candidates if necessary
                dd$Candidate[dd$Candidate == "WriteInVotes"] <- "WriteinVotes"
                #Following are standarizations for Seminole County
                dd$Candidate[dd$Candidate == "Write-in"] <- "WriteinVotes"
                dd$Candidate[dd$Candidate == "Times Blank Voted"] <- "UnderVotes"
                dd$Candidate[dd$Candidate == "Times Over Voted"] <- "OverVotes"
                dd$Candidate[dd$Candidate == "Roque De La Fuente"] <- "Roque DeLaFuente"
                #Combine candidate and party for matching
                for (j in 1:NROW(dd)){
                    if (cc[i] == "SEM"){
                        dd$Candidate[j] <- tail(strsplit(dd$Candidate[j],split=" ")[[1]],1) #last name
                        dd$COUNTY <- "Seminole"
                    }
                    else{
                        dd$Candidate[j] <- head(strsplit(dd$Candidate[j],split="/")[[1]],1) #Biden / Harris
                        dd$Candidate[j] <- gsub(" ","",trimws(dd$Candidate[j]))
                    }
                    if (!is.na(dd$Party[j])){
                        dd$Candidate[j] <- paste0(dd$Candidate[j],"_",dd$Party[j])
                    }
                }
                dd <- dd[,-5] # delete Party
                # Check for matches first???
                dd <- dd %>%
                    group_by(DIST,COUNTY,AREA,Candidate) %>%
                    summarize(Votes=sum(Votes))
                dd <- dd %>% spread(Candidate,Votes)
                dd$TOTAL <- 0
                # Compute total???
                # for (j in 4:(NCOL(dd)-1)){
                #     if (!is.na(dd[,j])){
                #         dd$TOTAL <- dd$TOTAL + dd[,j]
                #     }
                # }
                xx <- rbind(xx,dd)
            }
            namesxx <- names(xx)
            partyxx <- namesxx
            for (j in 4:(NCOL(xx)-1)){
                partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
                namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
            }
            ii <- c(1,2,3,NCOL(xx))
            idem <- 0
            irep <- 0
            if ("DEM" %in% partyxx){
                idem <- which(partyxx == "DEM")
                ii <- c(ii, idem)
            }
            if ("REP" %in% partyxx){
                irep <- which(partyxx == "REP")
                ii <- c(ii, irep)
            }
            for (j in 4:(NCOL(xx)-1)){
                if (j != idem & j != irep){
                    ii <- c(ii, j)
                }
                # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
                #     xx$TOTAL <- xx$TOTAL + xx[,j]
                # }
            }
            xx <- xx[,ii]
            namesxx <- namesxx[ii]
            partyxx <- partyxx[ii]
            names(xx) <- namesxx
            write(paste(partyxx, collapse = " "), paste0(data_dir,"FL_2016_President.csv"))
            write_delim(xx, paste0(data_dir,"FL_2016_President.csv"), append = TRUE, col_names = TRUE)
        }
        createFL_2018_Governor <- function(){
            cc <- fl_county_codes
            xx <- NULL
            for (i in 1:length(cc)){
                dd <- read_delim(paste0(input_dir,"FL/precinctlevelelectionresults2018gen/",
                                        cc[i],"_PctResults20181106.txt"), '\t', quote = "",
                                 col_names = FALSE, col_types = "ccdccccddddccdccddd")
                names(dd) <- c("Code","COUNTY","ElectNo","ElectDate","ElectName",
                               "AreaId","AREA","RegAll","RegRep","RegDem",
                               "RegOth","Contest","DIST","ConCode","Candidate",
                               "Party","RegId","CandNo","Votes")
                office <- "Governor" #UPDATE
                dd <- dd[dd$Contest == office,]
                if (NROW(dd) == 0){
                    catmsg(paste0("====> WARNING: ",cc[i]," COUNTY had no ",office))
                    next
                }
                #dd$AREA[is.na(dd$AREA)] <- dd$AreaId[is.na(dd$AREA)]
                #dd$AREA[dd$AREA == ""] <- dd$AreaId[dd$AREA == ""]
                if (cc[i] %in% c("HEN","HER","OKE","PUT")){
                    dd$AREA <- sub("^0+", "", dd$AreaId)
                }
                else if (cc[i] %in% c("ALA","DIX","HAR")){
                    dd$AREA <- str_pad(dd$AreaId,2,side = "left",pad = "0")
                }
                else if (cc[i] == "PAS"){
                    dd$AREA <- str_pad(dd$AreaId,3,side = "left",pad = "0")
                }
                else if (cc[i] == "DAD"){
                    dd$AREA <- str_pad(paste0(dd$AreaId,"0"),4,side = "left",pad = "0")
                }
                else{
                    dd$AREA <- dd$AreaId
                }
                dd <- dd[,c("DIST","COUNTY","AREA","Candidate","Party","Votes")]
                for (j in 1:NROW(dd)){
                    dd$Candidate[j] <- head(strsplit(dd$Candidate[j],split="/")[[1]],1) #Biden / Harris
                    dd$Candidate[j] <- gsub(" ","",trimws(dd$Candidate[j]))
                    if (!is.na(dd$Party[j])){
                        dd$Candidate[j] <- paste0(dd$Candidate[j],"_",dd$Party[j])
                    }
                }
                dd <- dd[,-5] # delete Party
                # check for matches first???
                dd <- dd %>%
                    group_by(DIST,COUNTY,AREA,Candidate) %>%
                    summarize(Votes=sum(Votes))
                dd <- dd %>% spread(Candidate,Votes)
                dd$TOTAL <- 0
                # for (j in 4:(NCOL(dd)-1)){
                #     dd$TOTAL <- dd$TOTAL + dd[,j]
                # }
                xx <- rbind(xx,dd)
            }
            namesxx <- names(xx)
            partyxx <- namesxx
            for (j in 4:(NCOL(xx)-1)){
                partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
                namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
            }
            ii <- c(1,2,3,NCOL(xx))
            idem <- 0
            irep <- 0
            if ("DEM" %in% partyxx){
                idem <- which(partyxx == "DEM")
                ii <- c(ii, idem)
            }
            if ("REP" %in% partyxx){
                irep <- which(partyxx == "REP")
                ii <- c(ii, irep)
            }
            for (j in 4:(NCOL(xx)-1)){
                if (j != idem & j != irep){
                    ii <- c(ii, j)
                }
                # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
                #     xx$TOTAL <- xx$TOTAL + xx[,j]
                # }
            }
            xx <- xx[,ii]
            namesxx <- namesxx[ii]
            partyxx <- partyxx[ii]
            names(xx) <- namesxx
            write(paste(partyxx, collapse = " "), paste0(data_dir,"FL_2018_Governor.csv"))
            write_delim(xx, paste0(data_dir,"FL_2018_Governor.csv"), append = TRUE, col_names = TRUE)
        }
        createFL_2018_Senate <- function(){
            cc <- fl_county_codes
            xx <- NULL
            for (i in 1:length(cc)){
                dd <- read_delim(paste0(input_dir,"FL/precinctlevelelectionresults2018gen/",
                                        cc[i],"_PctResults20181106.txt"), '\t', quote = "",
                                 col_names = FALSE, col_types = "ccdccccddddccdccddd")
                names(dd) <- c("Code","COUNTY","ElectNo","ElectDate","ElectName",
                               "AreaId","AREA","RegAll","RegRep","RegDem",
                               "RegOth","Contest","DIST","ConCode","Candidate",
                               "Party","RegId","CandNo","Votes")
                office <- "United States Senator" #UPDATE
                dd <- dd[dd$Contest == office,]
                if (NROW(dd) == 0){
                    catmsg(paste0("====> WARNING: ",cc[i]," COUNTY had no ",office))
                    next
                }
                #dd$AREA[is.na(dd$AREA)] <- dd$AreaId[is.na(dd$AREA)]
                #dd$AREA[dd$AREA == ""] <- dd$AreaId[dd$AREA == ""]
                if (cc[i] %in% c("HEN","HER","OKE","PUT")){
                    dd$AREA <- sub("^0+", "", dd$AreaId)
                }
                else if (cc[i] %in% c("ALA","DIX","HAR")){
                    dd$AREA <- str_pad(dd$AreaId,2,side = "left",pad = "0")
                }
                else if (cc[i] == "PAS"){
                    dd$AREA <- str_pad(dd$AreaId,3,side = "left",pad = "0")
                }
                else if (cc[i] == "DAD"){
                    dd$AREA <- str_pad(paste0(dd$AreaId,"0"),4,side = "left",pad = "0")
                }
                else{
                    dd$AREA <- dd$AreaId
                }
                dd <- dd[,c("DIST","COUNTY","AREA","Candidate","Party","Votes")]
                for (j in 1:NROW(dd)){
                    dd$Candidate[j] <- head(strsplit(dd$Candidate[j],split="/")[[1]],1) #Biden / Harris
                    dd$Candidate[j] <- gsub(" ","",trimws(dd$Candidate[j]))
                    if (!is.na(dd$Party[j])){
                        dd$Candidate[j] <- paste0(dd$Candidate[j],"_",dd$Party[j])
                    }
                }
                dd <- dd[,-5] # delete Party
                # check for matches first???
                dd <- dd %>%
                    group_by(DIST,COUNTY,AREA,Candidate) %>%
                    summarize(Votes=sum(Votes))
                dd <- dd %>% spread(Candidate,Votes)
                dd$TOTAL <- 0
                # for (j in 4:(NCOL(dd)-1)){
                #     dd$TOTAL <- dd$TOTAL + dd[,j]
                # }
                xx <- rbind(xx,dd)
            }
            namesxx <- names(xx)
            partyxx <- namesxx
            for (j in 4:(NCOL(xx)-1)){
                partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
                namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
            }
            ii <- c(1,2,3,NCOL(xx))
            idem <- 0
            irep <- 0
            if ("DEM" %in% partyxx){
                idem <- which(partyxx == "DEM")
                ii <- c(ii, idem)
            }
            if ("REP" %in% partyxx){
                irep <- which(partyxx == "REP")
                ii <- c(ii, irep)
            }
            for (j in 4:(NCOL(xx)-1)){
                if (j != idem & j != irep){
                    ii <- c(ii, j)
                }
                # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
                #     xx$TOTAL <- xx$TOTAL + xx[,j]
                # }
            }
            xx <- xx[,ii]
            namesxx <- namesxx[ii]
            partyxx <- partyxx[ii]
            names(xx) <- namesxx
            write(paste(partyxx, collapse = " "), paste0(data_dir,"FL_2018_Senate.csv"))
            write_delim(xx, paste0(data_dir,"FL_2018_Senate.csv"), append = TRUE, col_names = TRUE)
        }
        createFL_2020_President <- function(){
            cc <- fl_county_codes
            xx <- NULL
            for (i in 1:length(cc)){
                dd <- read_delim(paste0(input_dir,"FL/2020-general-election-rev/",
                                        cc[i],"_PctResults20201103.txt"), '\t', quote = "",
                                 col_names = FALSE, col_types = "ccdccccddddccdccddd")
                names(dd) <- c("Code","COUNTY","ElectNo","ElectDate","ElectName",
                               "AreaId","AREA","RegAll","RegRep","RegDem",
                               "RegOth","Contest","DIST","ConCode","Candidate",
                               "Party","RegId","CandNo","Votes")
                office <- "President of the United States" #UPDATE
                dd <- dd[dd$Contest == office,]
                if (NROW(dd) == 0){
                    catmsg(paste0("====> WARNING: ",cc[i]," COUNTY had no ",office))
                    next
                }
                #dd$AREA[is.na(dd$AREA)] <- dd$AreaId[is.na(dd$AREA)]
                #dd$AREA[dd$AREA == ""] <- dd$AreaId[dd$AREA == ""]
                dd$AREA <- dd$AreaId
                dd <- dd[,c("DIST","COUNTY","AREA","Candidate","Party","Votes")]
                for (j in 1:NROW(dd)){
                    dd$Candidate[j] <- head(strsplit(dd$Candidate[j],split="/")[[1]],1) #Biden / Harris
                    dd$Candidate[j] <- gsub(" ","",trimws(dd$Candidate[j]))
                    if (!is.na(dd$Party[j])){
                        dd$Candidate[j] <- paste0(dd$Candidate[j],"_",dd$Party[j])
                    }
                }
                dd <- dd[,-5] # delete Party
                # check for matches first???
                dd <- dd %>%
                    group_by(DIST,COUNTY,AREA,Candidate) %>%
                    summarize(Votes=sum(Votes))
                dd <- dd %>% spread(Candidate,Votes)
                dd$TOTAL <- 0
                # for (j in 4:(NCOL(dd)-1)){
                #     dd$TOTAL <- dd$TOTAL + dd[,j]
                # }
                xx <- rbind(xx,dd)
            }
            namesxx <- names(xx)
            partyxx <- namesxx
            for (j in 4:(NCOL(xx)-1)){
                partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
                namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
            }
            ii <- c(1,2,3,NCOL(xx))
            idem <- 0
            irep <- 0
            if ("DEM" %in% partyxx){
                idem <- which(partyxx == "DEM")
                ii <- c(ii, idem)
            }
            if ("REP" %in% partyxx){
                irep <- which(partyxx == "REP")
                ii <- c(ii, irep)
            }
            for (j in 4:(NCOL(xx)-1)){
                if (j != idem & j != irep){
                    ii <- c(ii, j)
                }
                # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
                #     xx$TOTAL <- xx$TOTAL + xx[,j]
                # }
            }
            xx <- xx[,ii]
            namesxx <- namesxx[ii]
            partyxx <- partyxx[ii]
            names(xx) <- namesxx
            write(paste(partyxx, collapse = " "), paste0(data_dir,"FL_2020_President.csv"))
            write_delim(xx, paste0(data_dir,"FL_2020_President.csv"), append = TRUE, col_names = TRUE)
        }
        createFL_2020_House <- function(){
            cc <- c(
                "ALA","BAK","BAY","BRA","BRE","BRO","CAL","CHA","CIT","CLA",
                "CLL","CLM","DAD","DES","DIX","DUV","ESC","FLA","FRA","GAD",
                "GIL","GLA","GUL","HAM","HAR","HEN","HER","HIG","HIL","HOL",
                "IND","JAC","JEF","LAF","LAK","LEE","LEO","LEV","LIB","MAD",
                "MAN","MON","MRN","MRT","NAS","OKA","OKE","ORA","OSC","PAL",
                "PAS","PIN","POL","PUT","SAN","SAR","SEM","STJ","STL","SUM",
                "SUW","TAY","UNI","VOL","WAK","WAL","WAS")
            zz <- NULL
            for (i in 1:length(cc)){
                xx <- read_delim(paste0(input_dir,"FL/2020-general-election-rev/",
                                        cc[i],"_PctResults20201103.txt"), '\t', quote = "",
                                 col_names = FALSE, col_types = "ccdccccddddccdccddd")
                names(xx) <- c("Code","County","ElectNo","ElectDate","ElectName",
                               "AreaId","AreaLoc","RegAll","RegRep","RegDem",
                               "RegOth","Contest","Dist","ConCode","Candidate",
                               "Party","RegId","CandNo","Votes")
                xx <- xx[xx$Contest == "Representative in Congress",]
                if (NROW(xx) == 0){
                    catmsg(paste0("====> WARNING: ",cc[i]," County had no Representative in Congress"))
                    next
                }
                xx <- xx[,c("Dist","County","AreaId","Candidate","Party","Votes")]
                for (j in 1:NROW(xx)){
                    if (!is.na(xx$Party[j])){
                        xx$Candidate[j] <- xx$Party[j]
                    }
                    else{
                        xx$Candidate[j] <- tail(strsplit(xx$Candidate[j],split=" ")[[1]],1) #use last name
                    }
                }
                xx <- xx[,-5] # delete Party
                xx <- xx %>%
                    group_by(Dist,County,AreaId,Candidate) %>%
                    summarize(Votes=sum(Votes))
                xx <- xx %>% spread(Candidate,Votes)
                xx$TOTAL <- 0
                yy <- xx[,c("Dist","County","AreaId","TOTAL")]
                yy$DEM <- 0
                yy$REP <- 0
                yy$NPA <- 0
                yy$WriteinVotes <- 0
                yy$OverVotes    <- 0
                yy$UnderVotes   <- 0
                if ("DEM" %in% names(xx)){
                    yy$DEM <- xx$DEM
                    yy$DEM[is.na(yy$DEM)] <- 0
                }
                if ("REP" %in% names(xx)){
                    yy$REP <- xx$REP
                    yy$REP[is.na(yy$REP)] <- 0
                }
                if ("NPA" %in% names(xx)){
                    yy$NPA <- xx$NPA
                    yy$NPA[is.na(yy$NPA)] <- 0
                }
                if ("WriteinVotes" %in% names(xx)){
                    yy$WriteinVotes <- xx$WriteinVotes
                    yy$WriteinVotes[is.na(yy$WriteinVotes)] <- 0
                }
                if ("OverVotes" %in% names(xx)){
                    yy$OverVotes <- xx$OverVotes
                    yy$OverVotes[is.na(yy$OverVotes)] <- 0
                }
                if ("UnderVotes" %in% names(xx)){
                    yy$UnderVotes <- xx$UnderVotes
                    yy$UnderVotes[is.na(yy$UnderVotes)] <- 0
                }
                yy$TOTAL <- yy$DEM + yy$REP + yy$NPA + yy$WriteinVotes
                zz <- rbind(zz,yy)
            }
            names(zz) <- c("DIST","COUNTY","AREA","TOTAL","DEM","REP","NPA","WRITEIN","OVERVOTES","UNDERVOTES")
            zz$DIST <- gsub("District ","",zz$DIST)
            zz$DIST <- gsub("^ ","",zz$DIST)
            write(paste(names(zz), collapse = " "), paste0(data_dir,"FL_2020_House.csv"))
            write_delim(zz, paste0(data_dir,"FL_2020_House.csv"), append = TRUE, col_names = TRUE)
        }
        createFL_2020_House_CD27 <- function(){
            catmsg("##### START createFL_2020_House_CD27 #####")
            xx <- read_delim(paste0(input_dir,"FL/2020-general-election-rev/",
                                    "DAD_PctResults20201103.txt"), '\t', quote = "",
                             col_names = FALSE, col_types = "ccdccccddddccdccddd")
            names(xx) <- c("Code","County","ElectNo","ElectDate","ElectName",
                           "AreaId","AreaLoc","RegAll","RegRep","RegDem",
                           "RegOth","Contest","Dist","ConCode","Candidate",
                           "Party","RegId","CandNo","Votes")
            xx027 <<- xx
            xx <- xx[xx$Contest == "Representative in Congress",]
            xx <- xx[xx$Dist == " District 27",]
            xx <- xx[,c("County","AreaId","Candidate","Votes")]
            for (i in 1:NROW(xx)){
                xx$Candidate[i] <- tail(strsplit(xx$Candidate[i],split=" ")[[1]],1) #use last name
            }
            xx27 <<- xx
            xx <- xx %>% spread(Candidate,Votes)
            xx$TOTAL <- 0
            xx <- xx[,c(1,2,8,5,4,7)]
            names(xx)[1:3] <- c("COUNTY","AREA","TOTAL")
            partyxx <- names(xx)
            partyxx[4:5] <- c("DEM","REP")
            write(paste(partyxx, collapse = " "), paste0(data_dir,"FL_2020_House_CD27.csv"))
            write_delim(xx, paste0(data_dir,"FL_2020_House_CD27.csv"), append = TRUE, col_names = TRUE)
        }
        createFL_2018_Registered <- function(){
            txt <- "text"
            num <- "numeric"
            xx <- read_excel(paste0(input_dir,"FL/precinctlevelelectionresults2018gen/2018gen_precinct.xlsx"),
                             sheet = "2018gen_precinct", skip = 8, col_names = TRUE,
                             col_types = c(txt,txt,num,num,num,num))
            xxr <<- xx #DEBUG-RM
            names(xx) <- c("CODE","AreaId","Republican","Democrat","Other","TOTAL")
            xx$COUNTY <- xx$CODE
            for (i in 1:(NROW(xx)-1)){
                if (xx$COUNTY[i] != "Total"){
                    xx$COUNTY[i] <- fl_counties[which(fl_county_codes == xx$CODE[i])]
                }
                if (xx$CODE[i] %in% c("HEN","HER","OKE","PUT")){
                    xx$AREA[i] <- sub("^0+", "", xx$AreaId[i])
                }
                else if (xx$CODE[i] %in% c("ALA","DIX","HAR")){
                    xx$AREA[i] <- str_pad(xx$AreaId[i],2,side = "left",pad = "0")
                }
                else if (xx$CODE[i] == "PAS"){
                    xx$AREA[i] <- str_pad(xx$AreaId[i],3,side = "left",pad = "0")
                }
                else if (xx$CODE[i] == "DAD"){
                    xx$AREA[i] <- str_pad(paste0(xx$AreaId[i],"0"),4,side = "left",pad = "0")
                }
                else{
                    xx$AREA[i] <- xx$AreaId[i]
                }
            }
            #xx$COUNTY <- fl_counties[which(fl_county_codes == xx$CODE)]
            xx <- xx[,c("COUNTY","AREA","TOTAL","Democrat","Republican","Other")]
            xxr2 <<- xx #DEBUG-RM
            partyxx <- names(xx)
            partyxx[4:5] <- c("DEM","REP")
            write(paste(partyxx, collapse = " "), paste0(data_dir,"FL_2018_Registered.csv"))
            write_delim(xx, paste0(data_dir,"FL_2018_Registered.csv"), append = TRUE, col_names = TRUE)
        }
        createFL_2020_Registered <- function(){
            txt <- "text"
            num <- "numeric"
            xx <- read_excel(paste0(input_dir,"FL/2020-general-election-rev/4-2020-gen-by-precinct.xlsx"),
                             sheet = "RegistrationByPrecinct", skip = 8, col_names = TRUE,
                             col_types = c(txt,txt,num,num,num,num))
            xxr <<- xx #DEBUG-RM
            names(xx) <- c("CODE","AreaId","Republican","Democrat","Other","TOTAL")
            xx$COUNTY <- xx$CODE
            for (i in 1:(NROW(xx)-1)){
                if (xx$COUNTY[i] != "Total"){
                    xx$COUNTY[i] <- fl_counties[which(fl_county_codes == xx$CODE[i])]
                }
                if (xx$CODE[i] %in% c("HEN","HER","OKE","PUT")){
                    xx$AREA[i] <- sub("^0+", "", xx$AreaId[i])
                }
                else if (xx$CODE[i] %in% c("ALA","DIX","HAR")){
                    xx$AREA[i] <- str_pad(xx$AreaId[i],2,side = "left",pad = "0")
                }
                else if (xx$CODE[i] == "PAS"){
                    xx$AREA[i] <- str_pad(xx$AreaId[i],3,side = "left",pad = "0")
                }
                else if (xx$CODE[i] == "DAD"){
                    xx$AREA[i] <- str_pad(paste0(xx$AreaId[i],"0"),4,side = "left",pad = "0")
                }
                else{
                    xx$AREA[i] <- xx$AreaId[i]
                }
            }
            #xx$COUNTY <- fl_counties[which(fl_county_codes == xx$CODE)]
            xx <- xx[,c("COUNTY","AREA","TOTAL","Democrat","Republican","Other")]
            xxr2 <<- xx #DEBUG-RM
            partyxx <- names(xx)
            partyxx[4:5] <- c("DEM","REP")
            write(paste(partyxx, collapse = " "), paste0(data_dir,"FL_2020_Registered.csv"))
            write_delim(xx, paste0(data_dir,"FL_2020_Registered.csv"), append = TRUE, col_names = TRUE)
        }
        createIA_2020_Counties <- function(){
            #input_dir <- "input/"
            files <- list.files(paste0(input_dir,"IA/2020/"),"*.xlsx")
            cc <- gsub(".xlsx","",files)
            print(cc)
            dd <- data.frame(cc)
            write_delim(dd, paste0(data_dir,"IA_Counties.csv"), append = FALSE, col_names = TRUE)
        }
        createIA_2018_Governor <- function(){
            #input_dir <- "input/"
            #data_dir  <- "data/"
            cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
            zz <- NULL
            for (i in 1:length(cc)){
                #for (i in 1:1){
                catmsg(paste0("START read_excel(",cc[i],")"))
                dd <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "3", skip = 1, n_max = 0) # read names
                xx <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "3", skip = 2)
                yy <- data.frame(xx[,1])
                yy$COUNTY <- cc[i]
                k <- 3
                nn <- names(dd)[seq(3,length(dd),3)]
                for (j in seq(5,NCOL(xx),3)){
                    yy[,k] <- xx[,j]
                    nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
                    names(yy)[k] <- nn[k-2]
                    k <- k+1
                }
                yy$TOTAL <- unlist(xx[,NCOL(xx)])
                # idem <- which(names(yy) == "Hart") #Hubbell
                # irep <- which(names(yy) == "Gregg") #Reynolds
                # ilib <- which(names(yy) == "Gentry") #Porter
                # ind1 <- which(names(yy) == "Blaskovich") #Siegwarth
                # iwri <- which(names(yy) == "Write-in")
                idem <- which(names(yy) %in% c("Hart", "Hubbell"))
                irep <- which(names(yy) %in% c("Gregg", "Reynolds"))
                ilib <- which(names(yy) %in% c("Gentry", "Porter"))
                ind1 <- which(names(yy) %in% c("Blaskovich", "Siegwarth"))
                iwri <- which(names(yy) == "Write-in")
                yy <- yy[,c(2,1,NCOL(yy),idem,irep,ilib,ind1,iwri)]
                names(yy) <- c("COUNTY","AREA","TOTAL","Hubbell","Reynolds",
                               "Porter","Siegwarth","Writein")
                yy <- yy[yy$AREA != "Total:",] #delete Total
                zz <- rbind(zz,yy)
            }
            partyzz <- names(zz)
            partyzz[4:7] <- c("DEM","REP","LIB","IND")
            write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2018_Governor.csv"))
            write_delim(zz, paste0(data_dir,"IA_2018_Governor.csv"), append = TRUE, col_names = TRUE)
        }
        createIA_2018_House_CD1 <- function(){
            cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
            zz <- NULL
            for (i in 1:length(cc)){
                catmsg(paste0("START read_excel(",cc[i],")"))
                aa <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 0, n_max = 0) # read names
                if (!grepl("^United States Representative District 1", names(aa))){
                    catmsg(paste0("SKIP ",names(aa)))
                    next
                }
                dd <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 1, n_max = 0) # read names
                xx <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 2)
                yy <- data.frame(xx[,1])
                yy$COUNTY <- cc[i]
                k <- 3
                nn <- names(dd)[seq(3,length(dd),3)]
                for (j in seq(5,NCOL(xx),3)){
                    yy[,k] <- xx[,j]
                    nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
                    names(yy)[k] <- nn[k-2]
                    k <- k+1
                }
                yy$TOTAL <- unlist(xx[,NCOL(xx)])
                idem <- which(names(yy) == "Finkenauer")
                irep <- which(names(yy) == "Blum")
                ilib <- which(names(yy) == "Hageman")
                iwri <- which(names(yy) == "Write-in")
                yy <- yy[,c(2,1,NCOL(yy),idem,irep,ilib,iwri)]
                names(yy) <- c("COUNTY","AREA","TOTAL","Finkenauer","Blum",
                               "Hageman","Writein")
                yy <- yy[yy$AREA != "Total:",] #delete Total
                zz <- rbind(zz,yy)
            }
            partyzz <- names(zz)
            partyzz[4:6] <- c("DEM","REP","LIB")
            write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2018_House_CD1.csv"))
            write_delim(zz, paste0(data_dir,"IA_2018_House_CD1.csv"), append = TRUE, col_names = TRUE)
        }
        createIA_2020_House_CD1 <- function(){
            cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
            zz <- NULL
            for (i in 1:length(cc)){
                catmsg(paste0("START read_excel(",cc[i],")"))
                aa <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 0, n_max = 0) # read names
                if (!grepl("^United States Representative District 1", names(aa))){
                    catmsg(paste0("SKIP ",names(aa)))
                    next
                }
                dd <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 1, n_max = 0) # read names
                xx <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 2)
                yy <- data.frame(xx[,1])
                yy$COUNTY <- cc[i]
                k <- 3
                nn <- names(dd)[seq(3,length(dd),3)]
                for (j in seq(5,NCOL(xx),3)){
                    yy[,k] <- xx[,j]
                    nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
                    names(yy)[k] <- nn[k-2]
                    k <- k+1
                }
                yy$TOTAL <- unlist(xx[,NCOL(xx)])
                idem <- which(names(yy) == "Finkenauer")
                irep <- which(names(yy) == "Hinson")
                iwri <- which(names(yy) == "Write-in")
                yy <- yy[,c(2,1,NCOL(yy),idem,irep,iwri)]
                names(yy) <- c("COUNTY","AREA","TOTAL","Finkenauer","Hinson","Writein")
                yy <- yy[yy$AREA != "Total:",] #delete Total
                zz <- rbind(zz,yy)
            }
            partyzz <- names(zz)
            partyzz[4:5] <- c("DEM","REP")
            write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2020_House_CD1.csv"))
            write_delim(zz, paste0(data_dir,"IA_2020_House_CD1.csv"), append = TRUE, col_names = TRUE)
        }
        createIA_2018_House_CD2 <- function(){
            cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
            zz <- NULL
            for (i in 1:length(cc)){
                catmsg(paste0("START read_excel(",cc[i],")"))
                #NEED TO VERIFY SHEET FOR ALL COUNTIES!!!
                aa <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 0, n_max = 0) #UPDATE YEAR & SHEET
                if (!grepl("^United States Representative District 2", names(aa))){ #UPDATE CD
                    catmsg(paste0("SKIP ",names(aa)))
                    next
                }
                dd <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 1, n_max = 0) #UPDATE YEAR & SHEET
                xx <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 2) #UPDATE YEAR & SHEET
                yy <- data.frame(xx[,1])
                gxx <<- xx #DEBUG-RM
                gyy <<- yy #DEBUG-RM
                yy$COUNTY <- cc[i]
                k <- 3
                nn <- names(dd)[seq(3,length(dd),3)]
                for (j in seq(5,NCOL(xx),3)){
                    yy[,k] <- xx[,j]
                    nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
                    names(yy)[k] <- nn[k-2]
                    k <- k+1
                }
                yy$TOTAL <- unlist(xx[,NCOL(xx)])
                idem <- which(names(yy) == "Loebsack") #UPDATE NAMES
                irep <- which(names(yy) == "Peters")
                ilib <- which(names(yy) == "Strauss")
                ind1 <- which(names(yy) == "Clark")
                iwri <- which(names(yy) == "Write-in")
                yy <- yy[,c(2,1,NCOL(yy),idem,irep,ilib,ind1,iwri)] #UPDATE PARTIES
                names(yy) <- c("COUNTY","AREA","TOTAL","Loebsack","Peters",
                               "Strauss","Clark","Writein") #UPDATE NAMES (change - to _)
                yy <- yy[yy$AREA != "Total:",] #delete Total
                zz <- rbind(zz,yy)
            }
            partyzz <- names(zz)
            partyzz[4:6] <- c("DEM","REP","LIB") #UPDATE PARTIES IF NEEDED (only DEM, REP critical)
            write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2018_House_CD2.csv")) #UPDATE FILENAME
            write_delim(zz, paste0(data_dir,"IA_2018_House_CD2.csv"), append = TRUE, col_names = TRUE) #UPDATE FILENAME
        }
        createIA_2020_House_CD2 <- function(){
            cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
            zz <- NULL
            for (i in 1:length(cc)){
                catmsg(paste0("START read_excel(",cc[i],")"))
                #NEED TO VERIFY SHEET FOR ALL COUNTIES!!!
                aa <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 0, n_max = 0) #UPDATE YEAR, SHEET & EXTENSION
                if (!grepl("^United States Representative District 2", names(aa))){ #UPDATE CD
                    catmsg(paste0("SKIP ",names(aa)))
                    next
                }
                dd <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 1, n_max = 0) #UPDATE YEAR, SHEET & EXTENSION
                xx <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 2) #UPDATE YEAR, SHEET & EXTENSION
                yy <- data.frame(xx[,1])
                gxx <<- xx #DEBUG-RM
                gyy <<- yy #DEBUG-RM
                yy$COUNTY <- cc[i]
                k <- 3
                nn <- names(dd)[seq(3,length(dd),3)]
                for (j in seq(5,NCOL(xx),3)){
                    yy[,k] <- xx[,j]
                    nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
                    names(yy)[k] <- nn[k-2]
                    k <- k+1
                }
                yy$TOTAL <- unlist(xx[,NCOL(xx)])
                idem <- which(names(yy) == "Hart") #UPDATE NAMES
                irep <- which(names(yy) == "Miller-Meeks")
                iwri <- which(names(yy) == "Write-in")
                yy <- yy[,c(2,1,NCOL(yy),idem,irep,iwri)] #UPDATE PARTIES
                names(yy) <- c("COUNTY","AREA","TOTAL","Hart","Miller_Meeks",
                               "Writein") #UPDATE NAMES (change - to _)
                yy <- yy[yy$AREA != "Total:",] #delete Total
                zz <- rbind(zz,yy)
            }
            partyzz <- names(zz)
            partyzz[4:5] <- c("DEM","REP") #UPDATE PARTIES IF NEEDED (only DEM, REP critical)
            write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2020_House_CD2.csv")) #UPDATE FILENAME
            write_delim(zz, paste0(data_dir,"IA_2020_House_CD2.csv"), append = TRUE, col_names = TRUE) #UPDATE FILENAME
        }
        createIA_2018_House_CD3 <- function(){
            cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
            zz <- NULL
            for (i in 1:length(cc)){
                catmsg(paste0("START read_excel(",cc[i],")"))
                #NEED TO VERIFY SHEET FOR ALL COUNTIES!!!
                aa <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 0, n_max = 0) #UPDATE YEAR & SHEET
                if (!grepl("^United States Representative District 3", names(aa))){ #UPDATE CD
                    catmsg(paste0("SKIP ",names(aa)))
                    next
                }
                dd <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 1, n_max = 0) #UPDATE YEAR & SHEET
                xx <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 2) #UPDATE YEAR & SHEET
                yy <- data.frame(xx[,1])
                gxx <<- xx #DEBUG-RM
                gyy <<- yy #DEBUG-RM
                yy$COUNTY <- cc[i]
                k <- 3
                nn <- names(dd)[seq(3,length(dd),3)]
                for (j in seq(5,NCOL(xx),3)){
                    yy[,k] <- xx[,j]
                    nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
                    names(yy)[k] <- nn[k-2]
                    k <- k+1
                }
                yy$TOTAL <- unlist(xx[,NCOL(xx)])
                idem <- which(names(yy) == "Axne") #UPDATE NAMES
                irep <- which(names(yy) == "Young")
                ilib <- which(names(yy) == "Holder")
                ind1 <- which(names(yy) == "Knupp")
                ind2 <- which(names(yy) == "Jr.")
                ind3 <- which(names(yy) == "Grandanette")
                iwri <- which(names(yy) == "Write-in")
                yy <- yy[,c(2,1,NCOL(yy),idem,irep,ilib,ind1,ind2,ind3,iwri)] #UPDATE PARTIES
                names(yy) <- c("COUNTY","AREA","TOTAL","Axne","Young",
                               "Holder","Knupp","Elworth_Jr","Grandanette","Writein") #UPDATE NAMES (change - to _)
                yy <- yy[yy$AREA != "Total:",] #delete Total
                zz <- rbind(zz,yy)
            }
            partyzz <- names(zz)
            partyzz[4:6] <- c("DEM","REP","LIB") #UPDATE PARTIES IF NEEDED (only DEM, REP critical)
            write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2018_House_CD3.csv")) #UPDATE FILENAME
            write_delim(zz, paste0(data_dir,"IA_2018_House_CD3.csv"), append = TRUE, col_names = TRUE) #UPDATE FILENAME
        }
        createIA_2020_House_CD3 <- function(){
            cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
            zz <- NULL
            for (i in 1:length(cc)){
                catmsg(paste0("START read_excel(",cc[i],")"))
                #NEED TO VERIFY SHEET FOR ALL COUNTIES!!!
                aa <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 0, n_max = 0) #UPDATE YEAR, SHEET & EXTENSION
                if (!grepl("^United States Representative District 3", names(aa))){ #UPDATE CD
                    catmsg(paste0("SKIP ",names(aa)))
                    next
                }
                dd <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 1, n_max = 0) #UPDATE YEAR, SHEET & EXTENSION
                xx <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 2) #UPDATE YEAR, SHEET & EXTENSION
                yy <- data.frame(xx[,1])
                gxx <<- xx #DEBUG-RM
                gyy <<- yy #DEBUG-RM
                yy$COUNTY <- cc[i]
                k <- 3
                nn <- names(dd)[seq(3,length(dd),3)]
                for (j in seq(5,NCOL(xx),3)){
                    yy[,k] <- xx[,j]
                    nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
                    names(yy)[k] <- nn[k-2]
                    k <- k+1
                }
                yy$TOTAL <- unlist(xx[,NCOL(xx)])
                idem <- which(names(yy) == "Axne") #UPDATE NAMES
                irep <- which(names(yy) == "Young")
                ilib <- which(names(yy) == "Holder")
                iwri <- which(names(yy) == "Write-in")
                yy <- yy[,c(2,1,NCOL(yy),idem,irep,ilib,iwri)] #UPDATE PARTIES
                names(yy) <- c("COUNTY","AREA","TOTAL","Axne","Young",
                               "Holder","Writein") #UPDATE NAMES (change - to _)
                yy <- yy[yy$AREA != "Total:",] #delete Total
                zz <- rbind(zz,yy)
            }
            partyzz <- names(zz)
            partyzz[4:6] <- c("DEM","REP","LIB") #UPDATE PARTIES IF NEEDED (only DEM, REP critical)
            write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2020_House_CD3.csv")) #UPDATE FILENAME
            write_delim(zz, paste0(data_dir,"IA_2020_House_CD3.csv"), append = TRUE, col_names = TRUE) #UPDATE FILENAME
        }
        createIA_2018_House_CD4 <- function(){
            cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
            zz <- NULL
            for (i in 1:length(cc)){
                catmsg(paste0("START read_excel(",cc[i],")"))
                #NEED TO VERIFY SHEET FOR ALL COUNTIES!!!
                aa <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 0, n_max = 0) #UPDATE YEAR & SHEET
                if (!grepl("^United States Representative District 4", names(aa))){ #UPDATE CD
                    catmsg(paste0("SKIP ",names(aa)))
                    next
                }
                dd <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 1, n_max = 0) #UPDATE YEAR & SHEET
                xx <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 2) #UPDATE YEAR & SHEET
                yy <- data.frame(xx[,1])
                gxx <<- xx #DEBUG-RM
                gyy <<- yy #DEBUG-RM
                yy$COUNTY <- cc[i]
                k <- 3
                nn <- names(dd)[seq(3,length(dd),3)]
                for (j in seq(5,NCOL(xx),3)){
                    yy[,k] <- xx[,j]
                    nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
                    names(yy)[k] <- nn[k-2]
                    k <- k+1
                }
                yy$TOTAL <- unlist(xx[,NCOL(xx)])
                idem <- which(names(yy) == "Scholten") #UPDATE NAMES
                irep <- which(names(yy) == "King")
                ilib <- which(names(yy) == "Aldrich")
                ind1 <- which(names(yy) == "Peterson")
                iwri <- which(names(yy) == "Write-in")
                yy <- yy[,c(2,1,NCOL(yy),idem,irep,ilib,ind1,iwri)] #UPDATE PARTIES
                names(yy) <- c("COUNTY","AREA","TOTAL","Scholten","King",
                               "Aldrich","Peterson","Writein") #UPDATE NAMES (change - to _)
                yy <- yy[yy$AREA != "Total:",] #delete Total
                zz <- rbind(zz,yy)
            }
            partyzz <- names(zz)
            partyzz[4:6] <- c("DEM","REP","LIB") #UPDATE PARTIES IF NEEDED (only DEM, REP critical)
            write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2018_House_CD4.csv")) #UPDATE FILENAME
            write_delim(zz, paste0(data_dir,"IA_2018_House_CD4.csv"), append = TRUE, col_names = TRUE) #UPDATE FILENAME
        }
        createIA_2020_House_CD4 <- function(){
            cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
            zz <- NULL
            for (i in 1:length(cc)){
                catmsg(paste0("START read_excel(",cc[i],")"))
                #NEED TO VERIFY SHEET FOR ALL COUNTIES!!!
                aa <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 0, n_max = 0) #UPDATE YEAR, SHEET & EXTENSION
                if (!grepl("^United States Representative District 4", names(aa))){ #UPDATE CD
                    catmsg(paste0("SKIP ",names(aa)))
                    next
                }
                dd <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 1, n_max = 0) #UPDATE YEAR, SHEET & EXTENSION
                xx <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 2) #UPDATE YEAR, SHEET & EXTENSION
                yy <- data.frame(xx[,1])
                gxx <<- xx #DEBUG-RM
                gyy <<- yy #DEBUG-RM
                yy$COUNTY <- cc[i]
                k <- 3
                nn <- names(dd)[seq(3,length(dd),3)]
                for (j in seq(5,NCOL(xx),3)){
                    yy[,k] <- xx[,j]
                    nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
                    names(yy)[k] <- nn[k-2]
                    k <- k+1
                }
                yy$TOTAL <- unlist(xx[,NCOL(xx)])
                idem <- which(names(yy) == "Scholten") #UPDATE NAMES
                irep <- which(names(yy) == "Feenstra")
                iwri <- which(names(yy) == "Write-in")
                yy <- yy[,c(2,1,NCOL(yy),idem,irep,iwri)] #UPDATE PARTIES
                names(yy) <- c("COUNTY","AREA","TOTAL","Scholten","Feenstra",
                               "Writein") #UPDATE NAMES (change - to _)
                yy <- yy[yy$AREA != "Total:",] #delete Total
                zz <- rbind(zz,yy)
            }
            partyzz <- names(zz)
            partyzz[4:5] <- c("DEM","REP") #UPDATE PARTIES IF NEEDED (only DEM, REP critical)
            write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2020_House_CD4.csv")) #UPDATE FILENAME
            write_delim(zz, paste0(data_dir,"IA_2020_House_CD4.csv"), append = TRUE, col_names = TRUE) #UPDATE FILENAME
        }
        createIA_2020_President <- function(){
            cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
            zz <- NULL
            for (i in 1:length(cc)){
                #for (i in 1:1){
                dd <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "2", skip = 1, n_max = 0) # read names
                xx <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "2", skip = 2)
                yy <- data.frame(xx[,1])
                yy$COUNTY <- cc[i]
                k <- 3
                nn <- names(dd)[seq(3,length(dd),3)]
                for (j in seq(5,NCOL(xx),3)){
                    yy[,k] <- xx[,j]
                    nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
                    names(yy)[k] <- nn[k-2]
                    k <- k+1
                }
                yy$TOTAL <- unlist(xx[,NCOL(xx)])
                idem <- which(names(yy) == "Harris") #Biden
                irep <- which(names(yy) == "Pence") #Trump
                ind1 <- which(names(yy) == "Richardson") #DeLaFuente
                ind2 <- which(names(yy) == "Mohr") #Blankenship
                ind3 <- which(names(yy) == "Chandler") #King
                ind4 <- which(names(yy) == "Walker") #Hawkins
                ind5 <- which(names(yy) == "Cohen") #Jorgensen
                ind6 <- which(names(yy) == "Ballard") #Pierce
                ind7 <- which(names(yy) == "Tidball") #West
                iwri <- which(names(yy) == "Write-in")
                yy <- yy[,c(2,1,NCOL(yy),idem,irep,ind1,ind2,ind3,ind4,ind5,ind6,ind7,iwri)]
                names(yy) <- c("COUNTY","AREA","TOTAL","Biden","Trump",
                               "DeLaFuente","Blankenship","King","Hawkins","Jorgensen",
                               "Pierce","West","Writein")
                yy <- yy[yy$AREA != "Total:",] #delete Total
                zz <- rbind(zz,yy)
            }
            partyzz <- names(zz)
            partyzz[4:5] <- c("DEM","REP")
            write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2020_President.csv"))
            write_delim(zz, paste0(data_dir,"IA_2020_President.csv"), append = TRUE, col_names = TRUE)
        }
        createIA_2020_Senate <- function(){
            cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
            zz <- NULL
            for (i in 1:length(cc)){
            #for (i in 1:1){
                dd <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "3", skip = 1, n_max = 0) # read names
                xx <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "3", skip = 2)
                yy <- data.frame(xx[,1])
                yy$COUNTY <- cc[i]
                k <- 3
                nn <- names(dd)[seq(3,length(dd),3)]
                for (j in seq(5,NCOL(xx),3)){
                    yy[,k] <- xx[,j]
                    nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
                    names(yy)[k] <- nn[k-2]
                    k <- k+1
                }
                yy$TOTAL <- unlist(xx[,NCOL(xx)])
                idem <- which(names(yy) == "Greenfield")
                irep <- which(names(yy) == "Ernst")
                ind1 <- which(names(yy) == "Herzog")
                iwri <- which(names(yy) == "Write-in")
                yy <- yy[,c(2,1,NCOL(yy),idem,irep,ind1,iwri)]
                names(yy)[2] <- "AREA"
                names(yy)[7] <- "Writein"
                yy <- yy[yy$AREA != "Total:",] #delete Total
                zz <- rbind(zz,yy)
            }
            partyzz <- names(zz)
            partyzz[4:5] <- c("DEM","REP")
            write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2020_Senate.csv"))
            write_delim(zz, paste0(data_dir,"IA_2020_Senate.csv"), append = TRUE, col_names = TRUE)
        }
        me_counties <- c(
            "Androscoggin","Aroostook","Cumberland","Franklin","Hancock",
            "Kennebec","Knox","Lincoln","Oxford","Penobscot",
            "Piscataquis","Sagadahoc","Somerset","Waldo","Washington",
            "York","STATE UOCAVA")
        me_cnts <- c(
            "AND","ARO","CUM","FRA","HAN",
            "KEN","KNO","LIN","OXF","PEN",
            "PIS","SAG","SOM","WAL","WAS",
            "YOR","STATE")
        createME_2014_Senate <- function(){
            xx <- read_excel(paste0(input_dir,"ME/2014/ussenategen.xlsx"), sheet = "Sheet1", skip = 2)
            names(xx) <- c("AREA","Bellows","Collins","Others","Blank","TOTAL")
            xx$COUNTY <- ""
            xx <- xx[,c("COUNTY","AREA","TOTAL","Bellows","Collins","Others")] # delete Blank
            j <- 1
            xx1 <<- xx #DEBUG-RM
            for (i in 1:NROW(xx)){
                xx$COUNTY[i] <- me_cnts[j]
                if (grepl(" County Totals",xx$AREA[i])){
                    j <- j+1
                }
            }
            xx2 <<- xx #DEBUG-RM
            xx$COUNTY[grepl(" UOCAVA", xx$AREA)] <- "STATE"
            xx$AREA[grepl(" UOCAVA", xx$AREA)] <- "UOCAVA"
            xx <- xx[!grepl(" Totals", xx$AREA) & (xx$AREA != "") & !is.na(xx$AREA),]
            #xx <- xx[!is.na(xx$COUNTY),]
            namesxx <- names(xx)
            namesxx[4:5] <- c("DEM","REP")
            xx3 <<- xx #DEBUG-RM
            write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_2014_Senate.csv"))
            write_delim(xx, paste0(data_dir,"ME_2014_Senate.csv"), append = TRUE, col_names = TRUE)
        }
        createME_2018_Governor <- function(){
            xx <- read_excel(paste0(input_dir,"ME/2018/governor11-6-18.xlsx"), sheet = "Gov", skip = 3)
            names(xx) <- c("COUNTY","AREA","Hayes","Mills","Moody","Others","Blank","TBC")
            xx$COUNTY[grepl(" UOCAVA", xx$AREA)] <- "STATE"
            xx$AREA[grepl(" UOCAVA", xx$AREA)] <- "UOCAVA"
            xx <- xx[!grepl(" Totals", xx$AREA) & (xx$AREA != "") & !is.na(xx$AREA),]
            xx <- xx[,c(1,2,NCOL(xx),4,5,3,6)] # delete Blank
            xx <- xx[!is.na(xx$COUNTY),]
            names(xx)[3] <- "TOTAL"
            namesxx <- names(xx)
            namesxx[4:5] <- c("DEM","REP")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_2018_Governor.csv"))
            write_delim(xx, paste0(data_dir,"ME_2018_Governor.csv"), append = TRUE, col_names = TRUE)
        }
        createME_2018_Senate <- function(){
            xx <- read_excel(paste0(input_dir,"ME/2018/us-senate11-6-18.xlsx"), sheet = "US", skip = 3)
            names(xx) <- c("COUNTY","AREA","Brakey","King","Ringelstein","Others","Blank","TBC")
            xx$COUNTY[grepl(" UOCAVA", xx$AREA)] <- "STATE"
            xx$AREA[grepl(" UOCAVA", xx$AREA)] <- "UOCAVA"
            xx <- xx[!grepl(" Totals", xx$AREA) & (xx$AREA != "") & !is.na(xx$AREA),]
            xx <- xx[,c(1,2,NCOL(xx),4,3,seq(5,(NCOL(xx)-2)))] # delete Blank
            xx <- xx[!is.na(xx$COUNTY),]
            names(xx)[3] <- "TOTAL"
            namesxx <- names(xx)
            namesxx[4:5] <- c("DEM","REP")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_2018_Senate.csv"))
            write_delim(xx, paste0(data_dir,"ME_2018_Senate.csv"), append = TRUE, col_names = TRUE)
        }
        createME_2020_President <- function(){
            xx <- read_excel(paste0(input_dir,"ME/2020/ME20_presandvisecnty1120.xlsx"), sheet = "Statewide", skip = 2)
            names(xx) <- c("COUNTY","AREA","Biden","De_La_Fuente","Hawkins","Jorgensen","Trump","Others","Blank","TBC")
            xx$COUNTY[grepl(" UOCAVA", xx$AREA)] <- "STATE"
            xx$AREA[grepl(" UOCAVA", xx$AREA)] <- "UOCAVA"
            xx <- xx[!grepl(" Total", xx$AREA) & (xx$AREA != "") & !is.na(xx$AREA),]
            xx <- xx[,c(1,2,10,3,7,4,5,6,8)] # delete Blank
            xx <- xx[!is.na(xx$COUNTY),]
            names(xx)[3] <- "TOTAL"
            namesxx <- names(xx)
            namesxx[4:5] <- c("DEM","REP")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_2020_President.csv"))
            write_delim(xx, paste0(data_dir,"ME_2020_President.csv"), append = TRUE, col_names = TRUE)
        }
        createME_2020_Senate <- function(){
            xx <- read_excel(paste0(input_dir,"ME/2020/ME20_ussenator1120.xlsx"), sheet = "US Senator", skip = 2)
            names(xx) <- c("COUNTY","AREA","Collins","Gideon","Linn","Savage","Others","Blank","TBC")
            xx$COUNTY[grepl(" UOCAVA", xx$AREA)] <- "STATE"
            xx$AREA[grepl(" UOCAVA", xx$AREA)] <- "UOCAVA"
            xx <- xx[!grepl(" Total", xx$AREA) & (xx$AREA != "") & !is.na(xx$AREA),]
            xx <- xx[,c(1,2,NCOL(xx),4,3,seq(5,(NCOL(xx)-2)))] # delete Blank
            xx <- xx[!is.na(xx$COUNTY),]
            names(xx)[3] <- "TOTAL"
            namesxx <- names(xx)
            namesxx[4:5] <- c("DEM","REP")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_2020_Senate.csv"))
            write_delim(xx, paste0(data_dir,"ME_2020_Senate.csv"), append = TRUE, col_names = TRUE)
        }
        createME_2020_House <- function(){
            xx <- read_excel(paste0(input_dir,"ME/2020/ME20_repcongress1120.xlsx"), sheet = "Dist 1", skip = 2)
            #names(xx) <- c("DIST","COUNTY","AREA","Allen","Pingree","Others","Blank","TBC")
            names(xx) <- c("DIST","COUNTY","AREA","REP","DEM","Others","Blank","TBC")
            xx$COUNTY[grepl(" UOCAVA", xx$AREA)] <- "STATE"
            xx$AREA[grepl(" UOCAVA", xx$AREA)] <- "UOCAVA"
            yy <- read_excel(paste0(input_dir,"ME/2020/ME20_repcongress1120.xlsx"), sheet = "Dist 2", skip = 2)
            #names(yy) <- c("DIST","COUNTY","AREA","Crafts","Golden","Others","Blank","TBC")
            names(yy) <- c("DIST","COUNTY","AREA","REP","DEM","Others","Blank","TBC")
            yy$COUNTY[grepl(" UOCAVA", yy$AREA)] <- "STATE"
            yy$AREA[grepl(" UOCAVA", yy$AREA)] <- "UOCAVA" # set to UOCAVA2 to combine
            xx <- rbind(xx,yy)
            # xx[!is.na(xx$AREA) & xx$AREA == "UOCAVA",4:8] <-
            #     xx[!is.na(xx$AREA) & xx$AREA == "UOCAVA",4:8] +
            #     xx[!is.na(xx$AREA) & xx$AREA == "UOCAVA2",4:8]
            # xx <- xx[xx$AREA != "UOCAVA2",]
            xx <- xx[!grepl(" Total", xx$AREA) & (xx$AREA != "") & !is.na(xx$AREA),]
            xx <- xx[,c(1,2,3,NCOL(xx),5,4,6)] # delete Blank
            xx <- xx[!is.na(xx$COUNTY),]
            names(xx)[4] <- "TOTAL"
            namesxx <- names(xx)
            #namesxx[5:6] <- c("DEM","REP")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_2020_House.csv"))
            write_delim(xx, paste0(data_dir,"ME_2020_House.csv"), append = TRUE, col_names = TRUE)
        }
        createMT_2018_Senate <- function(){
            xx <- read_excel(paste0(input_dir,"MT/2018/2018-GeneralPrecinct-by-Precinct_Votes.xlsx"), sheet = "Sheet1", skip = 6)
            office <- "UNITED STATES SENATOR" #UPDATE
            xx <- xx[xx$RaceName == office,]
            xx <- xx[,c("CountyName","PrecinctName","NameOnBallot","PartyCode","Votes")]
            names(xx) <- c("COUNTY","AREA","Candidate","Party","Votes")
            xx1 <<- xx #DEBUG-RM
            for (j in 1:NROW(xx)){
                if (grepl(" TESTER", xx$Candidate[j])){
                    xx$Candidate[j] <- "DEM_Tester"
                }
                else if (grepl(" ROSENDALE", xx$Candidate[j])){
                    xx$Candidate[j] <- "REP_Rosendale"
                }
                else if (grepl(" BRECKENRIDGE", xx$Candidate[j])){
                    xx$Candidate[j] <- "LIB_Breckenridge"
                }
                else{
                    xx$Candidate[j] <- paste0("IND_",xx$Candidate[j])
                }
            }
            xx2 <<- xx #DEBUG-RM
            xx <- xx[,-4] # delete Party
            # check for matches first???
            xx <- xx %>%
                group_by(COUNTY,AREA,Candidate) %>%
                summarize(Votes=sum(Votes))
            xx <- xx %>% spread(Candidate,Votes)
            xx$TOTAL <- 0
            xx3 <<- xx #DEBUG-RM
            
            namesxx <- names(xx)
            partyxx <- namesxx
            for (j in 3:(NCOL(xx)-1)){
                partyxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last segment
                namesxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last name
            }
            ii <- c(1,2,NCOL(xx))
            idem <- 0
            irep <- 0
            if ("DEM" %in% partyxx){
                idem <- which(partyxx == "DEM")
                ii <- c(ii, idem)
            }
            if ("REP" %in% partyxx){
                irep <- which(partyxx == "REP")
                ii <- c(ii, irep)
            }
            for (j in 3:(NCOL(xx)-1)){
                if (j != idem & j != irep){
                    ii <- c(ii, j)
                }
            }
            xx <- xx[,ii]
            namesxx <- namesxx[ii]
            partyxx <- partyxx[ii]
            names(xx) <- namesxx
            write(paste(partyxx, collapse = " "), paste0(data_dir,"MT_2018_Senate.csv"))
            write_delim(xx, paste0(data_dir,"MT_2018_Senate.csv"), append = TRUE, col_names = TRUE)
        }
        createMT_2020_President <- function(){
            xx <- read_excel(paste0(input_dir,"MT/2020/2020_General_Precinct-by-Precinct.xlsx"), sheet = "Sheet1", skip = 6)
            office <- "PRESIDENT" #UPDATE
            xx <- xx[xx$RaceName == office,]
            xx <- xx[,c("CountyName","PrecinctName","NameOnBallot","PartyCode","Votes")]
            names(xx) <- c("COUNTY","AREA","Candidate","Party","Votes")
            for (j in 1:NROW(xx)){
                if (grepl(" BIDEN", xx$Candidate[j])){
                    xx$Candidate[j] <- "DEM_Biden"
                }
                else if (grepl(" TRUMP", xx$Candidate[j])){
                    xx$Candidate[j] <- "REP_Trump"
                }
                else if (grepl(" JORGENSEN", xx$Candidate[j])){
                    xx$Candidate[j] <- "LIB_Jorgensen"
                }
                else{
                    xx$Candidate[j] <- paste0("IND_",xx$Candidate[j])
                }
            }
            xx <- xx[,-4] # delete Party
            # check for matches first???
            xx <- xx %>%
                group_by(COUNTY,AREA,Candidate) %>%
                summarize(Votes=sum(Votes))
            xx <- xx %>% spread(Candidate,Votes)
            xx$TOTAL <- 0
            
            namesxx <- names(xx)
            partyxx <- namesxx
            for (j in 3:(NCOL(xx)-1)){
                partyxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last segment
                namesxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last name
            }
            ii <- c(1,2,NCOL(xx))
            idem <- 0
            irep <- 0
            if ("DEM" %in% partyxx){
                idem <- which(partyxx == "DEM")
                ii <- c(ii, idem)
            }
            if ("REP" %in% partyxx){
                irep <- which(partyxx == "REP")
                ii <- c(ii, irep)
            }
            for (j in 3:(NCOL(xx)-1)){
                if (j != idem & j != irep){
                    ii <- c(ii, j)
                }
            }
            xx <- xx[,ii]
            namesxx <- namesxx[ii]
            partyxx <- partyxx[ii]
            names(xx) <- namesxx
            write(paste(partyxx, collapse = " "), paste0(data_dir,"MT_2020_President.csv"))
            write_delim(xx, paste0(data_dir,"MT_2020_President.csv"), append = TRUE, col_names = TRUE)
        }
        createMT_2020_Senate <- function(){
            xx <- read_excel(paste0(input_dir,"MT/2020/2020_General_Precinct-by-Precinct.xlsx"), sheet = "Sheet1", skip = 6)
            office <- "UNITED STATES SENATOR" #UPDATE
            xx <- xx[xx$RaceName == office,]
            xx <- xx[,c("CountyName","PrecinctName","NameOnBallot","PartyCode","Votes")]
            names(xx) <- c("COUNTY","AREA","Candidate","Party","Votes")
            xx1 <<- xx #DEBUG-RM
            for (j in 1:NROW(xx)){
                if (grepl(" BULLOCK", xx$Candidate[j])){
                    xx$Candidate[j] <- "DEM_Bullock"
                }
                else if (grepl(" DAINES", xx$Candidate[j])){
                    xx$Candidate[j] <- "REP_Daines"
                }
                else{
                    xx$Candidate[j] <- paste0("IND_",xx$Candidate[j])
                }
            }
            xx2 <<- xx #DEBUG-RM
            xx <- xx[,-4] # delete Party
            # check for matches first???
            xx <- xx %>%
                group_by(COUNTY,AREA,Candidate) %>%
                summarize(Votes=sum(Votes))
            xx <- xx %>% spread(Candidate,Votes)
            xx$TOTAL <- 0
            xx3 <<- xx #DEBUG-RM
            
            namesxx <- names(xx)
            partyxx <- namesxx
            for (j in 3:(NCOL(xx)-1)){
                partyxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last segment
                namesxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last name
            }
            ii <- c(1,2,NCOL(xx))
            idem <- 0
            irep <- 0
            if ("DEM" %in% partyxx){
                idem <- which(partyxx == "DEM")
                ii <- c(ii, idem)
            }
            if ("REP" %in% partyxx){
                irep <- which(partyxx == "REP")
                ii <- c(ii, irep)
            }
            # for (j in 3:(NCOL(xx)-1)){
            #     if (j != idem & j != irep){
            #         ii <- c(ii, j)
            #     }
            # }
            xx <- xx[,ii]
            namesxx <- namesxx[ii]
            partyxx <- partyxx[ii]
            names(xx) <- namesxx
            write(paste(partyxx, collapse = " "), paste0(data_dir,"MT_2020_Senate.csv"))
            write_delim(xx, paste0(data_dir,"MT_2020_Senate.csv"), append = TRUE, col_names = TRUE)
        }
        createNC_2018_House <- function(){
            xx <- read_delim(paste0(input_dir,"NC/results_pct_20181106/",
                                    "results_pct_20181106.txt"), '\t')
            #                 col_names = TRUE, col_types = "cccdccccddddddc")
            names(xx) <- c("COUNTY","ElectionDate","AREA","ContestID","ContestType",
                           "ContestName","Candidate","Party","VoteFor","ElectionDay",
                           "OneStop","Absentee","Provisional","Votes","RealPrecinct",
                           "NA16")
            office <- "US HOUSE OF REPRESENTATIVES DISTRICT " #UPDATE
            office_start <- "^US HOUSE OF REPRESENTATIVES" #UPDATE
            xx <- xx[grepl(office_start, xx$ContestName),]
            xx <- xx[,c("COUNTY","AREA","Candidate","Party","Votes")]
            gxx1 <<- xx #DEBUG-RM
            for (j in 1:NROW(xx)){
                #xx$Candidate[j] <- tail(strsplit(xx$Candidate[j],split=" ")[[1]],1) #use last name
                if (!is.na(xx$Party[j])){
                    #xx$Candidate[j] <- paste0(xx$Candidate[j],"_",xx$Party[j])
                    xx$Candidate[j] <- xx$Party[j]
                }
            }
            xx <- xx[,-4] # delete Party
            # check for matches first???
            xx <- xx %>%
                group_by(COUNTY,AREA,Candidate) %>%
                summarize(Votes=sum(Votes))
            gxx2 <<- xx #DEBUG-RM
            xx <- xx %>% spread(Candidate,Votes)
            xx$TOTAL <- 0
            gxx2 <<- xx #DEBUG-RM
            # for (j in 4:(NCOL(xx)-1)){
            #     xx$TOTAL <- xx$TOTAL + xx[,j]
            # }
            namesxx <- names(xx)
            partyxx <- namesxx
            # for (j in 3:(NCOL(xx)-1)){
            #     partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
            #     namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
            # }
            ii <- c(1,2,NCOL(xx))
            idem <- 0
            irep <- 0
            if ("DEM" %in% partyxx){
                idem <- which(partyxx == "DEM")
                ii <- c(ii, idem)
            }
            if ("REP" %in% partyxx){
                irep <- which(partyxx == "REP")
                ii <- c(ii, irep)
            }
            for (j in 3:(NCOL(xx)-1)){
                if (j != idem & j != irep){
                    ii <- c(ii, j)
                }
                # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
                #     xx$TOTAL <- xx$TOTAL + xx[,j]
                # }
            }
            xx <- xx[,ii]
            namesxx <- namesxx[ii]
            partyxx <- partyxx[ii]
            xx <- xx[!is.na(xx$DEM),] #drop if no DEM candidate
            xx <- xx[!is.na(xx$REP),] #drop if no REP candidate
            for (j in 6:NCOL(xx)){
                xx[,j][is.na(xx[,j])] <- 0
            }
            gxx3 <<- xx #DEBUG-RM
            # xx <- xx[,c(1:5,8:10,7,6)]
            # names(xx)[9:10] <- c("Writein","Misc")
            # xx$Writein[is.na(xx$Writein)] <- 0
            # namesxx <- namesxx[c(1:5,8:10,7,6)]
            # partyxx <- partyxx[c(1:5,8:10,7,6)]
            names(xx) <- namesxx
            write(paste(partyxx, collapse = " "), paste0(data_dir,"NC_2018_House.csv"))
            write_delim(xx, paste0(data_dir,"NC_2018_House.csv"), append = TRUE, col_names = TRUE)
        }
        createNC_2020_President <- function(){
            xx <- read_delim(paste0(input_dir,"NC/results_pct_20201103/",
                                    "results_pct_20201103.txt"), '\t')
            #                 col_names = TRUE, col_types = "cccdccccddddddc")
            names(xx) <- c("COUNTY","ElectionDate","AREA","ContestID","ContestType",
                           "ContestName","Candidate","Party","VoteFor","ElectionDay",
                           "OneStop","Absentee","Provisional","Votes","RealPrecinct",
                           "NA16")
            office <- "US PRESIDENT" #UPDATE
            xx <- xx[xx$ContestName == office,]
            xx <- xx[,c("COUNTY","AREA","Candidate","Party","Votes")]
            for (j in 1:NROW(xx)){
                xx$Candidate[j] <- tail(strsplit(xx$Candidate[j],split=" ")[[1]],1) #use last name
                if (!is.na(xx$Party[j])){
                    xx$Candidate[j] <- paste0(xx$Candidate[j],"_",xx$Party[j])
                }
            }
            xx <- xx[,-4] # delete Party
            # check for matches first???
            xx <- xx %>%
                group_by(COUNTY,AREA,Candidate) %>%
                summarize(Votes=sum(Votes))
            xx <- xx %>% spread(Candidate,Votes)
            xx$TOTAL <- 0
            #gxx2 <<- xx #DEBUG-RM
            # for (j in 4:(NCOL(xx)-1)){
            #     xx$TOTAL <- xx$TOTAL + xx[,j]
            # }
            namesxx <- names(xx)
            partyxx <- namesxx
            for (j in 3:(NCOL(xx)-1)){
                partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
                namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
            }
            ii <- c(1,2,NCOL(xx))
            idem <- 0
            irep <- 0
            if ("DEM" %in% partyxx){
                idem <- which(partyxx == "DEM")
                ii <- c(ii, idem)
            }
            if ("REP" %in% partyxx){
                irep <- which(partyxx == "REP")
                ii <- c(ii, irep)
            }
            for (j in 3:(NCOL(xx)-1)){
                if (j != idem & j != irep){
                    ii <- c(ii, j)
                }
                # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
                #     xx$TOTAL <- xx$TOTAL + xx[,j]
                # }
            }
            xx <- xx[,ii]
            namesxx <- namesxx[ii]
            partyxx <- partyxx[ii]
            #gxx3 <<- xx #DEBUG-RM
            #START OF CODE FOR PRESIDENT ONLY
            xx <- xx[,c(1:5,8:10,7,6)]
            names(xx)[9:10] <- c("Writein","Misc")
            xx$Writein[is.na(xx$Writein)] <- 0
            namesxx <- namesxx[c(1:5,8:10,7,6)]
            partyxx <- partyxx[c(1:5,8:10,7,6)]
            #END OF CODE FOR PRESIDENT ONLY
            names(xx) <- namesxx
            write(paste(partyxx, collapse = " "), paste0(data_dir,"NC_2020_President.csv"))
            write_delim(xx, paste0(data_dir,"NC_2020_President.csv"), append = TRUE, col_names = TRUE)
        }
        createNC_2020_Senate <- function(){
            xx <- read_delim(paste0(input_dir,"NC/results_pct_20201103/",
                                    "results_pct_20201103.txt"), '\t')
            #                 col_names = TRUE, col_types = "cccdccccddddddc")
            names(xx) <- c("COUNTY","ElectionDate","AREA","ContestID","ContestType",
                           "ContestName","Candidate","Party","VoteFor","ElectionDay",
                           "OneStop","Absentee","Provisional","Votes","RealPrecinct",
                           "NA16")
            office <- "US SENATE" #UPDATE
            xx <- xx[xx$ContestName == office,]
            xx <- xx[,c("COUNTY","AREA","Candidate","Party","Votes")]
            for (j in 1:NROW(xx)){
                xx$Candidate[j] <- tail(strsplit(xx$Candidate[j],split=" ")[[1]],1) #use last name
                if (!is.na(xx$Party[j])){
                    xx$Candidate[j] <- paste0(xx$Candidate[j],"_",xx$Party[j])
                }
            }
            xx <- xx[,-4] # delete Party
            # check for matches first???
            xx <- xx %>%
                group_by(COUNTY,AREA,Candidate) %>%
                summarize(Votes=sum(Votes))
            xx <- xx %>% spread(Candidate,Votes)
            xx$TOTAL <- 0
            gxx2 <<- xx #DEBUG-RM
            # for (j in 4:(NCOL(xx)-1)){
            #     xx$TOTAL <- xx$TOTAL + xx[,j]
            # }
            namesxx <- names(xx)
            partyxx <- namesxx
            for (j in 3:(NCOL(xx)-1)){
                partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
                namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
            }
            ii <- c(1,2,NCOL(xx))
            idem <- 0
            irep <- 0
            if ("DEM" %in% partyxx){
                idem <- which(partyxx == "DEM")
                ii <- c(ii, idem)
            }
            if ("REP" %in% partyxx){
                irep <- which(partyxx == "REP")
                ii <- c(ii, irep)
            }
            for (j in 3:(NCOL(xx)-1)){
                if (j != idem & j != irep){
                    ii <- c(ii, j)
                }
                # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
                #     xx$TOTAL <- xx$TOTAL + xx[,j]
                # }
            }
            xx <- xx[,ii]
            namesxx <- namesxx[ii]
            partyxx <- partyxx[ii]
            gxx3 <<- xx #DEBUG-RM
            # xx <- xx[,c(1:5,8:10,7,6)]
            # names(xx)[9:10] <- c("Writein","Misc")
            # xx$Writein[is.na(xx$Writein)] <- 0
            # namesxx <- namesxx[c(1:5,8:10,7,6)]
            # partyxx <- partyxx[c(1:5,8:10,7,6)]
            names(xx) <- namesxx
            write(paste(partyxx, collapse = " "), paste0(data_dir,"NC_2020_Senate.csv"))
            write_delim(xx, paste0(data_dir,"NC_2020_Senate.csv"), append = TRUE, col_names = TRUE)
        }
        createNC_2020_Governor <- function(){
            xx <- read_delim(paste0(input_dir,"NC/results_pct_20201103/",
                                    "results_pct_20201103.txt"), '\t')
            #                 col_names = TRUE, col_types = "cccdccccddddddc")
            names(xx) <- c("COUNTY","ElectionDate","AREA","ContestID","ContestType",
                           "ContestName","Candidate","Party","VoteFor","ElectionDay",
                           "OneStop","Absentee","Provisional","Votes","RealPrecinct",
                           "NA16")
            office <- "NC GOVERNOR" #UPDATE
            xx <- xx[xx$ContestName == office,]
            xx <- xx[,c("COUNTY","AREA","Candidate","Party","Votes")]
            for (j in 1:NROW(xx)){
                xx$Candidate[j] <- tail(strsplit(xx$Candidate[j],split=" ")[[1]],1) #use last name
                if (!is.na(xx$Party[j])){
                    xx$Candidate[j] <- paste0(xx$Candidate[j],"_",xx$Party[j])
                }
            }
            xx <- xx[,-4] # delete Party
            # check for matches first???
            xx <- xx %>%
                group_by(COUNTY,AREA,Candidate) %>%
                summarize(Votes=sum(Votes))
            xx <- xx %>% spread(Candidate,Votes)
            xx$TOTAL <- 0
            gxx2 <<- xx #DEBUG-RM
            # for (j in 4:(NCOL(xx)-1)){
            #     xx$TOTAL <- xx$TOTAL + xx[,j]
            # }
            namesxx <- names(xx)
            partyxx <- namesxx
            for (j in 3:(NCOL(xx)-1)){
                partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
                namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
            }
            ii <- c(1,2,NCOL(xx))
            idem <- 0
            irep <- 0
            if ("DEM" %in% partyxx){
                idem <- which(partyxx == "DEM")
                ii <- c(ii, idem)
            }
            if ("REP" %in% partyxx){
                irep <- which(partyxx == "REP")
                ii <- c(ii, irep)
            }
            for (j in 3:(NCOL(xx)-1)){
                if (j != idem & j != irep){
                    ii <- c(ii, j)
                }
                # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
                #     xx$TOTAL <- xx$TOTAL + xx[,j]
                # }
            }
            xx <- xx[,ii]
            namesxx <- namesxx[ii]
            partyxx <- partyxx[ii]
            gxx3 <<- xx #DEBUG-RM
            # xx <- xx[,c(1:5,8:10,7,6)]
            # names(xx)[9:10] <- c("Writein","Misc")
            # xx$Writein[is.na(xx$Writein)] <- 0
            # namesxx <- namesxx[c(1:5,8:10,7,6)]
            # partyxx <- partyxx[c(1:5,8:10,7,6)]
            names(xx) <- namesxx
            write(paste(partyxx, collapse = " "), paste0(data_dir,"NC_2020_Governor.csv"))
            write_delim(xx, paste0(data_dir,"NC_2020_Governor.csv"), append = TRUE, col_names = TRUE)
        }
        createSC_2016_President <- function(){
            #input_dir <- "input/"
            #data_dir  <- "data/"
            file <- list.files(paste0(input_dir,"SC/2016/"),"*.xls.xlsx")
            cc <- gsub(".xls.xlsx","",file)
            #cc <- unlist(read_delim(paste0(data_dir,"SC_Counties.csv")," "))
            zz <- NULL
            for (i in 1:length(cc)){
                #for (i in 1:1){
                #catmsg(paste0("START read_excel(",cc[i],") Pres"))
                dd <- read_excel(paste0(input_dir,"SC/2016/",cc[i],".xls.xlsx"), sheet = "3", skip = 1, n_max = 0) # read names
                xx <- read_excel(paste0(input_dir,"SC/2016/",cc[i],".xls.xlsx"), sheet = "3", skip = 2)
                gdd <<- dd #DEBUG-RM
                gxx <<- xx #DEBUG-RM
                yy <- data.frame(xx[,1])
                yy$COUNTY <- str_to_title(cc[i])
                k <- 3
                nn <- names(dd)[seq(3,length(dd),2)]
                gnn <<- nn #DEBUG-RM
                party <- rep("",length(nn)-1)
                # for (j in 1:(length(nn)-1)){
                #     party[j] <- head(strsplit(nn[j],split=" ")[[1]],1)
                # }
                #pp <- paste0(party,collapse = ",") #DEBUG
                #print(paste0(pp,"  ",cc[i])) #DEBUG
                for (j in seq(4,NCOL(xx),2)){
                    yy[,k] <- xx[,j]
                    lname <- head(strsplit(nn[k-2],split="/")[[1]],1) #Biden / Harris
                    #lname <- gsub(" ","",trimws(lname))
                    lname <- trimws(lname)
                    lname <- tail(strsplit(lname,split=" ")[[1]],1) #use last name
                    names(yy)[k] <- lname
                    k <- k+1
                }
                yy$TOTAL <- unlist(xx[,NCOL(xx)])
                gyy2 <<- yy #DEBUG-RM
                idem <- which(names(yy) == "Clinton")
                irep <- which(names(yy) == "Trump")
                ilib <- which(names(yy) == "Johnson")
                igrn <- which(names(yy) == "Stein")
                icon <- which(names(yy) == "Castle")
                ind1 <- which(names(yy) == "McMullin")
                iasc <- which(names(yy) == "Skewes")
                yy <- yy[,c(2,1,NCOL(yy),idem,irep,ilib,igrn,icon,ind1,iasc)]
                names(yy) <- c("COUNTY","AREA","TOTAL","Clinton","Trump",
                               "Johnson","Stein","Castle","McMullin","Skewes")
                yy <- yy[yy$AREA != "Total:",] #delete Total
                gyy3 <<- yy #DEBUG-RM
                zz <- rbind(zz,yy)
            }
            partyzz <- names(zz)
            partyzz[4:10] <- c("DEM","REP","LIB","GRN","CON","IND","ASC")
            write(paste(partyzz, collapse = " "), paste0(data_dir,"SC_2016_President.csv"))
            write_delim(zz, paste0(data_dir,"SC_2016_President.csv"), append = TRUE, col_names = TRUE)
        }
        createSC_2018_Governor <- function(){
            #input_dir <- "input/"
            #data_dir  <- "data/"
            file <- list.files(paste0(input_dir,"SC/2018/"),"*.xls.xlsx")
            cc <- gsub(".xls.xlsx","",file)
            #cc <- unlist(read_delim(paste0(data_dir,"SC_Counties.csv")," "))
            zz <- NULL
            for (i in 1:length(cc)){
                #for (i in 1:1){
                #catmsg(paste0("START read_excel(",cc[i],") 2018_Gov"))
                dd <- read_excel(paste0(input_dir,"SC/2018/",cc[i],".xls.xlsx"), sheet = "3", skip = 1, n_max = 0) # read names
                xx <- read_excel(paste0(input_dir,"SC/2018/",cc[i],".xls.xlsx"), sheet = "3", skip = 2)
                gdd <<- dd #DEBUG-RM
                gxx <<- xx #DEBUG-RM
                yy <- data.frame(xx[,1])
                yy$COUNTY <- str_to_title(cc[i])
                k <- 3
                nn <- names(dd)[seq(3,(length(dd)-1),2)]
                gnn <<- nn #DEBUG-RM
                
                for (j in 1:(length(nn))){
                    m <- 2 * (j+1)
                    if (grepl(" Smith", nn[j])){
                        yy[,k] <- xx[,m]
                        idem <- k
                    }
                    else if (grepl(" McMaster", nn[j])){
                        yy[,k] <- xx[,m]
                        irep <- k
                    }
                    else if (grepl("Write-In", nn[j])){
                        yy[,k] <- xx[,m]
                        iwri <- k
                    }
                    else if (grepl("WRITE-IN", nn[j])){
                        yy[,k] <- xx[,m]
                        iwri <- k
                    }
                    else{
                        catmsg(paste0("UNEXPECTED name=",nn[j]))
                    }
                    k <- k+1
                }
                yy <- yy[,c(2,1,NCOL(yy),idem,irep,iwri)]
                names(yy) <- c("COUNTY","AREA","TOTAL","Smith","McMaster","Writein")
                yy <- yy[yy$AREA != "Total:",] #delete Total
                zz <- rbind(zz,yy)
            }
            partyzz <- names(zz)
            partyzz[4:6] <- c("DEM","REP","WRI")
            write(paste(partyzz, collapse = " "), paste0(data_dir,"SC_2018_Governor.csv"))
            write_delim(zz, paste0(data_dir,"SC_2018_Governor.csv"), append = TRUE, col_names = TRUE)
        }
        createSC_2020_President <- function(){
            #input_dir <- "input/"
            #data_dir  <- "data/"
            file <- list.files(paste0(input_dir,"SC/2020/"),"*.xls.xlsx")
            cc <- gsub(".xls.xlsx","",file)
            #cc <- unlist(read_delim(paste0(data_dir,"SC_Counties.csv")," "))
            zz <- NULL
            for (i in 1:length(cc)){
                #for (i in 1:1){
                #catmsg(paste0("START read_excel(",cc[i],") Pres"))
                dd <- read_excel(paste0(input_dir,"SC/2020/",cc[i],".xls.xlsx"), sheet = "3", skip = 1, n_max = 0) # read names
                xx <- read_excel(paste0(input_dir,"SC/2020/",cc[i],".xls.xlsx"), sheet = "3", skip = 2)
                gdd <<- dd #DEBUG-RM
                gxx <<- xx #DEBUG-RM
                yy <- data.frame(xx[,1])
                yy$COUNTY <- str_to_title(cc[i])
                k <- 3
                nn <- names(dd)[seq(3,length(dd),7)]
                gnn <<- nn #DEBUG-RM
                party <- rep("",length(nn)-1)
                for (j in 1:(length(nn)-1)){
                    party[j] <- head(strsplit(nn[j],split=" ")[[1]],1)
                }
                #pp <- paste0(party,collapse = ",") #DEBUG
                #print(paste0(pp,"  ",cc[i])) #DEBUG
                for (j in seq(9,NCOL(xx),7)){
                    yy[,k] <- xx[,j]
                    #nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
                    names(yy)[k] <- party[k-2]
                    k <- k+1
                }
                yy$TOTAL <- unlist(xx[,NCOL(xx)])
                idem <- which(names(yy) == "DEM") #Biden
                irep <- which(names(yy) == "REP") #Trump
                igrn <- which(names(yy) == "GRN") #Hawkins
                ialn <- which(names(yy) == "ALN") #DeLaFuente
                ilib <- which(names(yy) == "LIB") #Jorgensen
                yy <- yy[,c(2,1,NCOL(yy),idem,irep,igrn,ialn,ilib)]
                names(yy) <- c("COUNTY","AREA","TOTAL","Biden","Trump",
                               "Hawkins","DeLaFuente","Jorgensen")
                yy <- yy[yy$AREA != "Total:",] #delete Total
                zz <- rbind(zz,yy)
            }
            partyzz <- names(zz)
            partyzz[4:8] <- c("DEM","REP","GRN","ALN","LIB")
            write(paste(partyzz, collapse = " "), paste0(data_dir,"SC_2020_President.csv"))
            write_delim(zz, paste0(data_dir,"SC_2020_President.csv"), append = TRUE, col_names = TRUE)
        }
        createSC_2020_Senate <- function(){
            #input_dir <- "input/"
            #data_dir  <- "data/"
            file <- list.files(paste0(input_dir,"SC/2020/"),"*.xls.xlsx")
            cc <- gsub(".xls.xlsx","",file)
            #cc <- unlist(read_delim(paste0(data_dir,"SC_Counties.csv")," "))
            zz <- NULL
            for (i in 1:length(cc)){
                #for (i in 1:1){
                #catmsg(paste0("START read_excel(",cc[i],") Senate"))
                dd <- read_excel(paste0(input_dir,"SC/2020/",cc[i],".xls.xlsx"), sheet = "4", skip = 1, n_max = 0) # read names
                xx <- read_excel(paste0(input_dir,"SC/2020/",cc[i],".xls.xlsx"), sheet = "4", skip = 2)
                gdd <<- dd #DEBUG-RM
                gxx <<- xx #DEBUG-RM
                yy <- data.frame(xx[,1])
                yy$COUNTY <- str_to_title(cc[i])
                k <- 3
                nn <- names(dd)[seq(3,length(dd),7)]
                gnn <<- nn #DEBUG-RM
                party <- rep("",length(nn)-1)
                for (j in 1:(length(nn)-1)){
                    party[j] <- head(strsplit(nn[j],split=" ")[[1]],1)
                }
                pp <- paste0(party,collapse = ",") #DEBUG
                catmsg(paste0(pp,"  ",cc[i])) #DEBUG
                for (j in seq(9,NCOL(xx),7)){
                    yy[,k] <- xx[,j]
                    #nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
                    names(yy)[k] <- party[k-2]
                    k <- k+1
                }
                yy$TOTAL <- unlist(xx[,NCOL(xx)])
                idem <- which(names(yy) == "DEM") #Harrison
                irep <- which(names(yy) == "REP") #Graham
                icon <- which(names(yy) == "CON") #Bledsoe
                if (cc[i] == "dorchester"){ #Writein
                    iwri <- which(names(yy) == "Write-in")
                }
                else if (cc[i] == "abbeville"){
                    iwri <- which(names(yy) == "name")
                }
                else{
                    iwri <- which(names(yy) == "Write-In")
                }
                yy <- yy[,c(2,1,NCOL(yy),idem,irep,icon,iwri)]
                names(yy) <- c("COUNTY","AREA","TOTAL","Harrison","Graham",
                               "Bledsoe","Writein")
                yy <- yy[yy$AREA != "Total:",] #delete Total
                zz <- rbind(zz,yy)
            }
            partyzz <- names(zz)
            partyzz[4:7] <- c("DEM","REP","CON","WRI")
            write(paste(partyzz, collapse = " "), paste0(data_dir,"SC_2020_Senate.csv"))
            write_delim(zz, paste0(data_dir,"SC_2020_Senate.csv"), append = TRUE, col_names = TRUE)
        }
        createSC_2020_Registered <- function(){
            #input_dir <- "input/"
            #data_dir  <- "data/"
            file <- list.files(paste0(input_dir,"SC/2020/"),"*.xls.xlsx")
            cc <- gsub(".xls.xlsx","",file)
            #cc <- unlist(read_delim(paste0(data_dir,"SC_Counties.csv")," "))
            zz <- NULL
            for (i in 1:length(cc)){
                #for (i in 1:1){
                #catmsg(paste0("START read_excel(",cc[i],") Pres"))
                #dd <- read_excel(paste0(input_dir,"SC/2020/",cc[i],".xls.xlsx"), sheet = "Registered Voters", skip = 1, n_max = 0) # read names
                xx <- read_excel(paste0(input_dir,"SC/2020/",cc[i],".xls.xlsx"), sheet = "Registered Voters", skip = 0)
                gxx <<- xx #DEBUG-RM
                yy <- data.frame(xx[,c(1,2)]) #AREA,Registered
                yy$COUNTY <- str_to_title(cc[i])
                yy <- yy[,c(3,1,2)] #put COUNTY in front
                gyy1 <<- yy #DEBUG-RM
                names(yy) <- c("COUNTY","AREA","TOTAL")
                yy$DEM <- 1
                yy$REP <- yy$TOTAL - 1
                gyy2 <<- yy #DEBUG-RM
                yy <- yy[yy$AREA != "Failsafe",]
                yy <- yy[yy$AREA != "Provisional",]
                yy <- yy[yy$AREA != "Failsafe Provisional",]
                yy <- yy[yy$AREA != "Total:",] #delete Total
                gyy3 <<- yy #DEBUG-RM
                zz <- rbind(zz,yy)
            }
            partyzz <- names(zz)
            write(paste(partyzz, collapse = " "), paste0(data_dir,"SC_2020_Registered.csv"))
            write_delim(zz, paste0(data_dir,"SC_2020_Registered.csv"), append = TRUE, col_names = TRUE)
        }
        createTX_2016_President <- function(){
            catmsg("##### START createTX_2016_President #####")
            cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
            names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
            cctx <- cc[cc$StateCode == "48",]
            xx <- read_csv(paste0(input_dir,"TX/2016/","president.csv"))
            # names(xx) <- c("CNTYVTD","ClintonD_16G_President","TrumpR_16G_President",
            #                "JohnsonL_16G_President","SteinG_16G_President","Write-InW_16G_President")
            xx$COUNTY <- substring(xx$CNTYVTD,1,3)
            for (i in 1:NROW(xx)){
                xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
                xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
            }
            xx$AREA <- xx$CNTYVTD
            xx$TOTAL <- 0
            xx <- xx[,c(7,8,9,2:6)]
            names(xx) <- c("COUNTY","AREA","TOTAL","Clinton","Trump","Johnson","Stein","Writein")
            partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","GRN","Writein")
            write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2016_President.csv"))
            write_delim(xx, paste0(data_dir,"TX_2016_President.csv"), append = TRUE, col_names = TRUE)
        }
        createTX_2018_AG <- function(){
            catmsg("##### START createTX_2018_AG #####")
            cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
            names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
            cctx <- cc[cc$StateCode == "48",]
            xx <- read_csv(paste0(input_dir,"TX/2018/","attorney gen.csv"))
            # names(xx) <- c("CNTYVTD","PaxtonR_18G_Attorney Gen","NelsonD_18G_Attorney Gen",
            #                "HarrisL_18G_Attorney Gen")
            xx$COUNTY <- substring(xx$CNTYVTD,1,3)
            for (i in 1:NROW(xx)){
                xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
                xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
            }
            xx$AREA <- xx$CNTYVTD
            xx$TOTAL <- 0
            xx <- xx[,c(5,6,7,3,2,4)]
            names(xx) <- c("COUNTY","AREA","TOTAL","Nelson","Paxton","Harris")
            partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB")
            write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2018_AG.csv"))
            write_delim(xx, paste0(data_dir,"TX_2018_AG.csv"), append = TRUE, col_names = TRUE)
        }
        createTX_2018_Governor <- function(){
            catmsg("##### START createTX_2018_Governor #####")
            cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
            names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
            cctx <- cc[cc$StateCode == "48",]
            xx <- read_csv(paste0(input_dir,"TX/2018/","governor.csv"))
            # names(xx) <- c("CNTYVTD","AbbottR_18G_Governor","ValdezD_18G_Governor",
            #                "TippettsL_18G_Governor")
            xx$COUNTY <- substring(xx$CNTYVTD,1,3)
            for (i in 1:NROW(xx)){
                xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
                xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
            }
            xx$AREA <- xx$CNTYVTD
            xx$TOTAL <- 0
            xx <- xx[,c(5,6,7,3,2,4)]
            names(xx) <- c("COUNTY","AREA","TOTAL","Valdez","Abbott","Tippetts")
            partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB")
            write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2018_Governor.csv"))
            write_delim(xx, paste0(data_dir,"TX_2018_Governor.csv"), append = TRUE, col_names = TRUE)
        }
        createTX_2018_Senate <- function(){
            catmsg("##### START createTX_2018_Senate #####")
            cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
            names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
            cctx <- cc[cc$StateCode == "48",]
            xx <- read_csv(paste0(input_dir,"TX/2018/","u.s. sen.csv"))
            # names(xx) <- c("CNTYVTD","CruzR_18G_U.S. Sen","O'RourkeD_18G_U.S. Sen",
            #                "DikemanL_18G_U.S. Sen")
            xx$COUNTY <- substring(xx$CNTYVTD,1,3)
            for (i in 1:NROW(xx)){
                xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
                xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
            }
            xx$AREA <- xx$CNTYVTD
            xx$TOTAL <- 0
            xx <- xx[,c(5,6,7,3,2,4)]
            names(xx) <- c("COUNTY","AREA","TOTAL","ORourke","Cruz","Dikeman")
            partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB")
            write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2018_Senate.csv"))
            write_delim(xx, paste0(data_dir,"TX_2018_Senate.csv"), append = TRUE, col_names = TRUE)
        }
        createTX_2020_President <- function(){
            catmsg("##### START createTX_2020_President #####")
            cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
            names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
            cctx <- cc[cc$StateCode == "48",]
            xx <- read_csv(paste0(input_dir,"TX/2020/","president.csv"))
            # names(xx) <- c("CNTYVTD","BidenD_20G_President","TrumpR_20G_President",
            #                "JorgensenL_20G_President","HawkinsG_20G_President","Write-InW_20G_President")
            xx$COUNTY <- substring(xx$CNTYVTD,1,3)
            for (i in 1:NROW(xx)){
                xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
                xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
            }
            xx$AREA <- xx$CNTYVTD
            xx$TOTAL <- 0
            xx <- xx[,c(7,8,9,2:6)]
            names(xx) <- c("COUNTY","AREA","TOTAL","Biden","Trump","Jorgensen","Hawkins","Writein")
            partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","GRN","Writein")
            write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2020_President.csv"))
            write_delim(xx, paste0(data_dir,"TX_2020_President.csv"), append = TRUE, col_names = TRUE)
        }
        createTX_2020_Senate <- function(){
            catmsg("##### START createTX_2020_Senate #####")
            cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
            names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
            cctx <- cc[cc$StateCode == "48",]
            xx <- read_csv(paste0(input_dir,"TX/2020/","u.s. sen.csv"))
            # names(xx) <- c("CNTYVTD","CornynR_20G_U.S. Sen","HegarD_20G_U.S. Sen",
            #                "McKennonL_20G_U.S. Sen","CollinsG_20G_U.S. Sen")
            xx$COUNTY <- substring(xx$CNTYVTD,1,3)
            for (i in 1:NROW(xx)){
                xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
                xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
            }
            xx$AREA <- xx$CNTYVTD
            xx$TOTAL <- 0
            xx <- xx[,c(6,7,8,3,2,4,5)]
            names(xx) <- c("COUNTY","AREA","TOTAL","Hegar","Cornyn","McKennon","Collins")
            partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","IND1","IND2")
            write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2020_Senate.csv"))
            write_delim(xx, paste0(data_dir,"TX_2020_Senate.csv"), append = TRUE, col_names = TRUE)
        }
        createWI_2020_President <- function(){
            catmsg("##### START createWI_2020_President #####")
            xx0 <- read_excel(paste0(input_dir,"WI/2020/","Ward by Ward Report PRESIDENT OF THE UNITED STATES by State Representive District - After Recount.xlsx"),
                              sheet = "Sheet1")
            xx <- xx0[,c(1,3,4,7:19)]
            names(xx) <- c("COUNTY","AREA","TOTAL","Biden","Trump","Blankenship","Jorgensen","Carroll","WRI1","WRI2","WRI3","WRI4","WRI5","WRI6","WRI7","SCATTERING")
            xx$TOTAL <- rowSums(xx[,4:NCOL(xx)], na.rm = TRUE)
            partyxx <- names(xx)
            partyxx[4:5] <- c("DEM","REP")
            write(paste(partyxx, collapse = " "), paste0(data_dir,"WI_2020_President.csv"))
            write_delim(xx, paste0(data_dir,"WI_2020_President.csv"), append = TRUE, col_names = TRUE)
        }
        createWI_2016_President_Original <- function(){
            catmsg("##### START createWI_2016_President_Original #####")
            xx0 <- read_excel(paste0(input_dir,"WI/2016/","Ward by Ward Original and Recount President of the United States.xlsx"),
                              sheet = "Sheet1", skip = 1)
            xx <- xx0[,c(1,3,4,6,5,7:21)] #ORIGINAL
            #xx <- xx0[,c(1,3,23,25,24,26:40)] #RECOUNT
            names(xx) <- c("COUNTY","AREA","TOTAL","Clinton","Trump","Castle","Johnson","Stein","Moorehead","De.La.Fuente","WRI1","WRI2","WRI3","WRI4","WRI5","WRI6","WRI7","WRI8","WRI9","SCATTERING")
            partyxx <- paste0('"',names(xx),'"')
            partyxx[4:5] <- c("DEM","REP")
            write(paste(partyxx, collapse = " "), paste0(data_dir,"WI_2016_President_Original.csv"))
            write_delim(xx, paste0(data_dir,"WI_2016_President_Original.csv"), append = TRUE, col_names = TRUE)
        }
        createWI_2016_President_Recount <- function(){
            catmsg("##### START createWI_2016_President_Recount #####")
            xx0 <- read_excel(paste0(input_dir,"WI/2016/","Ward by Ward Original and Recount President of the United States.xlsx"),
                              sheet = "Sheet1", skip = 1)
            #xx <- xx0[,c(1,3,4,6,5,7:21)] #ORIGINAL
            xx <- xx0[,c(1,3,23,25,24,26:40)] #RECOUNT
            names(xx) <- c("COUNTY","AREA","TOTAL","Clinton","Trump","Castle","Johnson","Stein","Moorehead","De.La.Fuente","WRI1","WRI2","WRI3","WRI4","WRI5","WRI6","WRI7","WRI8","WRI9","SCATTERING")
            partyxx <- paste0('"',names(xx),'"')
            partyxx[4:5] <- c("DEM","REP")
            write(paste(partyxx, collapse = " "), paste0(data_dir,"WI_2016_President_Recount.csv"))
            write_delim(xx, paste0(data_dir,"WI_2016_President_Recount.csv"), append = TRUE, col_names = TRUE)
        }
        createWI_2016_President <- function(){
            catmsg("##### START createWI_2016_President #####")
            xx <- read_excel(paste0(input_dir,"WI/2016/","Ward by Ward Report-President_0.xlsx"),
                             sheet = "Ward by Ward Report", skip = 10)
            xx <- xx[,-8] # delete column 8 due to spreadsheet error  
            names(xx) <- c("COUNTY","AREA","TOTAL","Trump","Clinton","Castle","Johnson","Stein","IND1","IND2","IND3","IND4","IND5","IND6","IND7","IND8","IND9","IND10","IND11","SCATTERING")
            lastCOUNTY <- ""
            for (i in 1:NROW(xx)){
                if (is.na(xx$COUNTY[i])){
                    xx$COUNTY[i] <- lastCOUNTY
                }
                else{
                    lastCOUNTY <- xx$COUNTY[i]
                }
            }
            xx <- xx[!endsWith(xx$AREA,"Totals:"),]
            xx <- xx[!is.na(xx$COUNTY),]
            xx <- xx[,c(1,2,3,5,4,6:20)]
            partyxx <- names(xx)
            partyxx[4:19] <- c("DEM","REP","CON","LIB","WGR","IND1","IND2","IND3","IND4","IND5","IND6","IND7","IND8","IND9","IND10","IND11")
            write(paste(partyxx, collapse = " "), paste0(data_dir,"WI_2016_President.csv"))
            write_delim(xx, paste0(data_dir,"WI_2016_President.csv"), append = TRUE, col_names = TRUE)
        }
        addScales <- function(gg, xscale, yscale){
            xx <- NULL
            yy <- NULL
            if(xscale != ""){
                sxx <- unlist(strsplit(xscale, ","))
                xx <- as.numeric(sxx)
                if (length(sxx) == 3){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[3]))
                }
                else if (length(sxx) == 4){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[4]))
                }
            }
            if(yscale != ""){
                syy <- unlist(strsplit(yscale, ","))
                yy <- as.numeric(syy)
                if (length(syy) == 3){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[3]))
                }
                else if (length(syy) == 4){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[4]))
                }
            }
            if (length(xx) >= 2){
                if (length(yy) >= 2){
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]), ylim = c(yy[1], yy[2]))
                }
                else{
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]))
                }
            }
            else if (length(yy) >= 2){
                gg <- gg + coord_cartesian(ylim = c(yy[1], yy[2]))
            }
            return(gg)
        }
        orderdf <- function(dd, sortcol, sortdesc){
            if (sortcol != 0){
                if (!sortdesc){
                    dd <- dd[order(dd[sortcol]),]
                }
                else{
                    if (class(dd[sortcol]) == "numeric"){
                        dd <- dd[order(-dd[sortcol]),]
                    }
                    else{
                        dd <- dd[order(dd[sortcol]),]
                        dd <- dd %>% arrange(desc(row_number()))
                    }
                }
            }
            return(dd)
        }
        getlabels <- function(type, xcounty, xtype){
            tloc <- input$state2
            if (tloc == ""){
                tloc <- "U.S."
            }
            if (xcounty != "" & xcounty != "(all)"){
                tloc <- paste0(xcounty," County, ",tloc)
            }
            if (input$dist != ""){
                tloc <- paste0("District ",input$dist,", ",tloc)
            }
            tloc <- paste0(tloc,":")
            tshift <- "Shift"
            tshiftin <- "Shift in"
            if (input$units == "Percent"){
                tunits <- "Vote Share"
            }
            else{
                tunits <- "Votes"
            }
            if (type != "plot" & type != "plot2b"){
                tnote <- paste0("(",input$units,")")
            }
            else if (input$party == "Margin"){
                if (input$plusnote != ""){
                    plusnote <- input$plusnote
                }
                else if (xcounty == "" | xcounty == "(all)"){
                    plusnote <- "(positive direction is more Democratic)"
                }
                else{
                    #plusnote <- "(+ = Dem)"
                    plusnote <- "(positive direction is more Democratic)"
                }
                tnote <- paste("in areas with",input$minvotes,"or more votes",plusnote)
            }
            else{
                tnote <- paste("in areas with",input$minvotes,"or more votes")
            }
            racex <- input$races[1]
            racey <- input$races[2]
            if (input$xdxplot2){
                if (xtype >= 2){
                    title <- paste(tloc,tshiftin, input$party, tunits)
                }
                else{
                    title <- paste(tloc,tshiftin, input$party, tunits, "from",
                                   racex, "to", racey, tnote)
                }
            }
            else{
                title <- paste(tloc, input$party, tunits, "for",
                               racex, "and", racey, tnote)
            }
            if (type == "plot2b"){
                #START areaPlot2b code
                ylabel <- paste("Areas ordered by ",input$party, tunits, "for", racex)
                xlabel <- paste0(input$party," ",tunits," for ", racex," and ",racey,
                                 "\nSources: see http://econdataus.com/voting_area.htm")
                #STOP areaPlot2b code
            }
            else{
                if (input$xdxplot2){
                    if (xtype >= 2){
                        ylabel <- paste(tshift, "for", racey)
                    }
                    else{
                        ylabel <- paste(tshiftin, input$party, tunits, "for", racey)
                    }
                }
                else{
                    ylabel <- paste(input$party, tunits, "for", racey)
                }
                xlabel <- paste0(input$party," ",tunits," for ", racex,
                                 "\nSources: see http://econdataus.com/voting_area.htm")
            }
            labels <- c(title, xlabel, ylabel)
            return(labels)
        }
        output$myUsage <- renderUI({
            includeHTML("http://econdataus.com/voting_area.htm")
        })
        output$areaPlot <- renderPlot({
            areaWidth <<- input$areaWidth
            areaHeight <<- input$areaHeight
            dd <- getdata()
            dd <- dd[is.na(dd$TOTAL) | dd$TOTAL >= input$minvotes,]
            row.names(dd) <- seq(1:NROW(dd))
            if (input$xcounty != "" & input$xcounty != "(all)"){
                dd <- dd[dd$COUNTY == input$xcounty,]
            }
            else{
                dd <- dd[dd$COUNTY != "" & !is.na(dd$COUNTY),]
            }
            dd <- orderdf(dd,input$xsortcol,input$xsortdesc)
            row.names(dd) <- seq(1,NROW(dd)) # set before removing votes == 0
            if (input$area_x0vote){
                dd <- dd[dd[4] > 0 & dd[5] > 0,] # delete if DEM or REP votes == 0 
            }
            xx <- dd
            xx$Margin <- 100 * (xx[,4] - xx[,5]) / xx$TOTAL
            
            xx$Party <- ""
            if (input$xlimit != ""){
                vlimit <- as.numeric(unlist(strsplit(input$xlimit, ",")))
                vparty <- unlist(strsplit(input$xparty, ","))
                xx$Party <- vparty[length(vparty)]
                xx$Party[xx[["Margin"]] < vlimit[1]] <- vparty[1]
                for (i in 1:length(vlimit)){
                    xx$Party[xx[["Margin"]] >= vlimit[i] & xx[["Margin"]] < vlimit[i+1]] <- vparty[i+1]
                }
            }

            xx$POS   <- 0
            if (input$showrow){
                xx$POS <- 2 # default to right
            }
            xx$LABEL <- row.names(xx)
            spos1 <- unlist(strsplit(input$pos1, ","))
            xx$POS[xx$LABEL %in% spos1] <- 1
            spos2 <- unlist(strsplit(input$pos2, ","))
            xx$POS[xx$LABEL %in% spos2] <- 2
            spos3 <- unlist(strsplit(input$pos3, ","))
            xx$POS[xx$LABEL %in% spos3] <- 3
            xx$VJUST <- 0.5
            xx$VJUST[xx$POS == 1] <- -1
            xx$VJUST[xx$POS == 3] <- 2
            xx$PREPEND <- ""
            xx$PREPEND[xx$POS == 2] <- "  "
            xx$LABEL <- paste0(xx$PREPEND,xx$LABEL)
            xx$LABEL[xx$POS == 0] <- ""
            title <- paste0(input$xcounty," County, ",input$races[1]," - Total Votes and Vote Margin by Area")
            xlabel <- "Total Votes"
            ylabel <- "Vote Margin"
            
            gxx <<- xx
            gg <- ggplot(xx, aes(x = TOTAL, y = Margin))
            gg <- gg + geom_point(aes(color=Party))
            gg <- gg + ggtitle(title)
            gg <- gg + xlab(xlabel) + ylab(ylabel)
            gg <- gg + annotate("text", x = xx$TOTAL, y =xx$Margin, label = xx$LABEL,
                                color="red", hjust = 0, vjust = xx$VJUST)
            isParty <- NULL
            for (i in 1:length(vparty)){
                isParty <- c(isParty, any(xx$Party == vparty[i]))
            }
            vcolor <- unlist(strsplit(input$areaColor, ","))
            vcolor <- vcolor[isParty]
            if (length(vcolor) > 1){
                gg <- gg + scale_fill_manual(values = vcolor) # Bar Plot
                gg <- gg + scale_color_manual(values = vcolor) # Line Graph
            }
            gg <- addScales(gg,input$areaxscale,input$areayscale)
            gg
        }, width = areaWidth, height = areaHeight)
        doCvtPlot <- function(xx, xcounty, xtype){
            xx <- xx[is.na(xx$TOTAL) | xx$TOTAL >= input$minvotes,]
            row.names(xx) <- seq(1:NROW(xx))
            if (input$cvt_x0vote){
                xx <- xx[xx[4] > 0 & xx[5] > 0,] # delete if DEM or REP votes == 0 
            }
            if (xcounty != "" & xcounty != "(all)"){
                xx <- xx[xx$COUNTY == xcounty,]
            }
            else{
                xx <- xx[xx$COUNTY != "" & !is.na(xx$COUNTY),] # removes TOTAL
            }
            votesM <- getDeltaM(xx, xcounty)[2]
            yy <- xx
            yy <- orderdf(yy,input$xsortcol,input$xsortdesc)
            names(yy)[3] <- "Votes"
            if (input$cvt_window > 0){
                nn <- input$cvt_window
                totsum <- roll_sum(unlist(yy[3]),nn)
                demsum <- roll_sum(unlist(yy[4]),nn)
                repsum <- roll_sum(unlist(yy[5]),nn)
            }
            yy[3] <- cumsum(yy[3])
            for (i in 4:NCOL(yy)){
                yy[i] <- 100 * cumsum(yy[i]) / yy[3]
            }
            if (input$plotbyarea){
                len <- length(unlist(yy[3]))
                yy[3] <- seq(1:len)
                votesM <- median(seq(1:len))
            }
            if (input$cvt_window > 0){
                yy$Dem_SMA <- NA
                yy$Rep_SMA <- NA
                yy$Dem_SMA[nn:NROW(yy)] <- 100 * demsum / totsum
                yy$Rep_SMA[nn:NROW(yy)] <- 100 * repsum / totsum
            }
            zz <- gather(yy, "Candidate", "Share", 4:NCOL(yy))
            zz <- zz[!startsWith(zz$Candidate,"Write.in") & !startsWith(zz$Candidate,"IND") &
                         !startsWith(zz$Candidate,"WRI") & zz$Candidate != "SCATTERING",]
            zz$Candidate <- factor(zz$Candidate, levels = names(yy)[4:NCOL(yy)])
            xsortdir <- "Ascending"
            if (input$xsortdesc) xsortdir <- "Desc"
            if (xtype >= 2){
                title <- paste0(xcounty," County, ",input$races[1])
            }
            else{
                title <- paste0(xcounty," County, ",input$races[1]," - Cumulative Vote Tally, ordered by ",names(zz)[input$xsortcol],", ",xsortdir)
            }
            xlabel <- "Votes"
            ylabel <- "Percent of Votes"
            if (input$plotbyarea){
                xlabel <- "Number of Areas"
            }
            else if (input$votes1000){
                votesM <- votesM/1000
                zz$Votes <- zz$Votes/1000
                xlabel <- "Votes (thousands)"
            }
            gg <- ggplot(zz, aes(x = Votes, y = Share))
            gg <- gg + geom_point(aes_string(color="Candidate",shape="Candidate"), size=3, alpha=as.numeric(input$xalpha))
            gg <- gg + geom_line(aes_string(color="Candidate"), size=2, alpha=as.numeric(input$xalpha))
            gg <- gg + geom_vline(aes(xintercept = votesM))
            gg <- gg + ggtitle(title)
            gg <- gg + xlab(xlabel) + ylab(ylabel)
            vcolor <- unlist(strsplit(input$xcolor, ","))
            if (length(vcolor) > 1){
                ncand <- NCOL(yy)-3
                vcolor <- rep(vcolor, length.out=ncand)
                if (input$cvt_window > 0){
                    vcolor[ncand-1] <- "lightblue"
                    vcolor[ncand] <- "pink"
                }
                gg <- gg + scale_fill_manual(values = vcolor) # Bar Plot
                gg <- gg + scale_color_manual(values = vcolor) # Line Graph
            }
            else{
                colorCount = NCOL(yy)-3
                if (colorCount > 12){
                    getPalette = colorRampPalette(brewer.pal(
                        brewer.pal.info[vcolor[[1]],]$maxcolors, vcolor[[1]]))
                    gg <- gg + scale_colour_manual(values = getPalette(colorCount))
                    gg <- gg + scale_fill_manual(values = getPalette(colorCount))
                }
                else{
                    gg <- gg + scale_colour_brewer(palette = vcolor[[1]])
                    gg <- gg + scale_fill_brewer(palette = vcolor[[1]])
                }
            }
            vshape <- as.numeric(unlist(strsplit(input$xshape, ",")))
            if (length(vshape) > 1){
                ncand <- length(unique(zz$Candidate))
                len <- length(vshape)
                if (len < ncand){
                    vshape <- c(vshape, rep(16,(ncand-len)))
                }
                vshape[ncand-1] <- 20
                vshape[ncand] <- 20
                gg <- gg + scale_shape_manual(values = vshape) # Line Graph
            }
            gg <- addScales(gg,input$xscale,input$yscale)
            gg
        }
        output$cvtPlot <- renderPlot({
            xx <- getdata()
            doCvtPlot(xx, input$xcounty, 1)
        })
        output$cvtPlots <- renderPlot({
            xx <- getdata()
            cc <- getCounties()
            gcc <<- cc #DEBUG-RM
            nn <- input$cvt_cols * input$cvt_rows
            ist <- input$cvt_start
            imx <- min(nn, (1+nrow(cc)-ist))
            pp <- NULL
            for (i in 1:nn) pp[[i]] <- ggplot() + theme_void()
            for (i in 1:imx) pp[[i]] <- doCvtPlot(xx, cc[(ist+i-1),"COUNTY"], 2)
            plot_grid(plotlist = pp, ncol = input$cvt_cols)
        #})
        }, height = 600, width = 1000)
        doAreaPlot2 <- function(xx, xcounty, xtype){
            if (xcounty != "" & xcounty != "(all)"){
                xx <- xx[xx$COUNTY == xcounty,]
            }
            else{
                xx <- xx[xx$COUNTY != "" & !is.na(xx$COUNTY),]
            }
            if (!input$displaytotal){
                xx <- xx[xx$AREA != "TOTAL",]
            }
            names(xx)[3:10] <- c("DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2")
            xx <- xx[(is.na(xx$TOT1_N) | xx$TOT1_N >= input$minvotes) |
                     (is.na(xx$TOT2_N) | xx$TOT2_N >= input$minvotes),]
            row.names(xx) <- seq(1:NROW(xx))
            xx <- xx[xx$DEM1 > 0 & xx$REP1 > 0 & xx$DEM2 > 0 & xx$REP2 > 0,]
            if (input$party == "Democrat"){
                preparty <- "DEM"
                party1 <- "DEM1"
                party2 <- "DEM2"
            }
            else if (input$party == "Republican"){
                preparty <- "REP"
                party1 <- "REP1"
                party2 <- "REP2"
            }
            else if (input$party == "Total"){
                preparty <- "TOT"
                party1 <- "TOTAL1"
                party2 <- "TOTAL2"
            }
            else{
                preparty <- "MAR"
                party1 <- "MARGIN1"
                party2 <- "MARGIN2"
            }
            party_sh <- paste0(preparty,"_SH")
            party1n <- "TOT1_N"
            party2n <- "TOT2_N"
            xx$Party <- ""
            if (input$xlimit2 != ""){
                vlimit <- as.numeric(unlist(strsplit(input$xlimit2, ",")))
                vcolor <- unlist(strsplit(input$lcolor2, ","))
                vcolor <- rep_len(vcolor,(length(vlimit)+1))
                vparty <- unlist(strsplit(input$xparty2, ","))
                xx$Party <- vparty[length(vparty)]
                xx$Party[xx[["MARGIN1"]] < vlimit[1]] <- vparty[1]
                xx$Color <- vcolor[length(vcolor)]
                xx$Color[xx[["MARGIN1"]] < vlimit[1]] <- vcolor[1]
                for (i in 1:(length(vlimit)-1)){
                    xx$Party[xx[["MARGIN1"]] >= vlimit[i] & xx[["MARGIN1"]] < vlimit[i+1]] <- vparty[i+1]
                    xx$Color[xx[["MARGIN1"]] >= vlimit[i] & xx[["MARGIN1"]] < vlimit[i+1]] <- vcolor[i+1]
                }
            }
            isParty <- NULL
            for (i in 1:length(vparty)){
                isParty <- c(isParty, any(xx$Party == vparty[i]))
            }
            if (input$sizefor2){
                xx$Votes <- xx[[party2n]]
            }
            else{
                xx$Votes <- xx[[party1n]]
            }
            if (input$xdxplot2){
                gg <- ggplot(xx, aes_string(x=party1, y=party_sh))
            }
            else{
                gg <- ggplot(xx, aes_string(x=party1, y=party2))
            }
            gg <- gg + geom_point(data=xx, alpha=as.numeric(input$xalpha2),
                                  aes_string(color="Party",size="Votes"))
            if (input$party == "Margin"){
                if (input$xdxplot2){
                    gg <- gg + geom_abline(intercept=0, slope=-1, color=input$ncolor2, linetype="dashed")
                }
                else{
                    gg <- gg + geom_abline(intercept=0, slope=1, color=input$ncolor2, linetype="dashed")
                }
            }
            if (input$party == "Margin" | input$units == "Count"){
                gg <- gg + geom_vline(xintercept=0, color=input$ncolor2)
            }
            if (input$party == "Margin" &
                    min(xx$MAR_SH, na.rm = TRUE) <= 0 &
                    max(xx$MAR_SH, na.rm = TRUE) >= 0){
                gg <- gg + geom_hline(yintercept=0, color=input$ncolor2)
            }
            vcolor <- unlist(strsplit(input$xcolor2, ","))
            vcolor <- vcolor[isParty]
            if (length(vcolor) > 0){
                gg <- gg + scale_fill_manual(values = vcolor) # Bar Plot
                gg <- gg + scale_color_manual(values = vcolor) # Line Graph
            }
            vrange_n <- as.numeric(unlist(strsplit(input$vrange, ",")))
            if (input$vbreaks != ""){
                vbreaks_n <- as.numeric(unlist(strsplit(input$vbreaks, ",")))
                gg <- gg + scale_size_continuous(range = vrange_n,
                                                 breaks = vbreaks_n)
            }
            else if (input$vtrans != "" & substr(input$vtrans,1,1) != "#"){
                gg <- gg + scale_size_continuous(range = vrange_n,
                                                 trans = input$vtrans)
            }
            labels <- getlabels("plot", xcounty, xtype)
            gg <- gg + ggtitle(labels[1])
            gg <- gg + xlab(labels[2])
            gg <- gg + ylab(labels[3])
            xx$POS   <- 0
            if (input$showall2){
                xx$POS <- 2 # default to right
            }
            if (input$label2 == "Index"){
                xx$LABEL <- row.names(xx)
            }
            else if (input$label2 == "County"){
                xx$LABEL <- xx$COUNTY
            }
            else if (input$label2 == "CountyID"){
                if (input$state2 == "TX"){
                    xx$LABEL <- sub("^0+", "", substring(xx$AREA,1,3))
                }
                else{
                    xx$LABEL <- xx$COUNTY
                }
            }
            else if (input$label2 == "Area"){
                if (input$state2 == "TX"){
                    xx$LABEL <- sub("^0+", "", substring(xx$AREA,4))
                }
                else{
                    xx$LABEL <- xx$AREA
                }
            }
            else if (input$label2 == "CNTYVTD"){
                xx$LABEL <- xx$AREA
            }
            spos1_2 <- unlist(strsplit(input$pos1_2, ","))
            xx$POS[xx$AREA %in% spos1_2] <- 1
            xx$POS[row.names(xx) %in% spos1_2] <- 1
            spos2_2 <- unlist(strsplit(input$pos2_2, ","))
            xx$POS[xx$AREA %in% spos2_2] <- 2
            xx$POS[row.names(xx) %in% spos2_2] <- 2
            spos3_2 <- unlist(strsplit(input$pos3_2, ","))
            xx$POS[xx$AREA %in% spos3_2] <- 3
            xx$POS[row.names(xx) %in% spos3_2] <- 3
            xx$VJUST <- 0.5
            xx$VJUST[xx$POS == 1] <- -1
            xx$VJUST[xx$POS == 3] <- 2
            xx$PREPEND <- ""
            xx$PREPEND[xx$POS == 2] <- "  "
            xx$LABEL <- paste0(xx$PREPEND,xx$LABEL)
            xx$LABEL[xx$POS == 0] <- ""
            if (input$xdxplot2){
                if (input$party == "Democrat"){
                    gg <- gg + annotate("text", x = xx$DEM1, y =xx$DEM_SH, label = xx$LABEL,
                                        color=xx$Color, hjust = 0, vjust = xx$VJUST)
                }
                else if (input$party == "Republican"){
                    gg <- gg + annotate("text", x = xx$REP1, y =xx$REP_SH, label = xx$LABEL,
                                        color=xx$Color, hjust = 0, vjust = xx$VJUST)
                }
                else if (input$party == "Total"){
                    gg <- gg + annotate("text", x = xx$TOTAL1, y =xx$TOT_SH, label = xx$LABEL,
                                        color=xx$Color, hjust = 0, vjust = xx$VJUST)
                }
                else{
                    gg <- gg + annotate("text", x = xx$MARGIN1, y =xx$MAR_SH, label = xx$LABEL,
                                        color=xx$Color, hjust = 0, vjust = xx$VJUST)
                }
            }
            else{
                if (input$party == "Democrat"){
                    gg <- gg + annotate("text", x = xx$DEM1, y =xx$DEM2, label = xx$LABEL,
                                        color=xx$Color, hjust = 0, vjust = xx$VJUST)
                }
                else if (input$party == "Republican"){
                    gg <- gg + annotate("text", x = xx$REP1, y =xx$REP2, label = xx$LABEL,
                                        color=xx$Color, hjust = 0, vjust = xx$VJUST)
                }
                else if (input$party == "Total"){
                    gg <- gg + annotate("text", x = xx$TOTAL1, y =xx$TOT2, label = xx$LABEL,
                                        color=xx$Color, hjust = 0, vjust = xx$VJUST)
                }
                else{
                    gg <- gg + annotate("text", x = xx$MARGIN1, y =xx$MARGIN2, label = xx$LABEL,
                                        color=xx$Color, hjust = 0, vjust = xx$VJUST)
                }
            }
            xx <- NULL
            yy <- NULL
            if(input$xscale2 != ""){
                sxx <- unlist(strsplit(input$xscale2, ","))
                xx <- as.numeric(sxx)
                if (length(sxx) == 3){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[3]))
                }
                else if (length(sxx) == 4){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[4]))
                }
            }
            if(input$yscale2 != ""){
                syy <- unlist(strsplit(input$yscale2, ","))
                yy <- as.numeric(syy)
                if (length(syy) == 3){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[3]))
                }
                else if (length(syy) == 4){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[4]))
                }
            }
            if (length(xx) >= 2){
                if (length(yy) >= 2){
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]), ylim = c(yy[1], yy[2]))
                }
                else{
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]))
                }
            }
            else if (length(yy) >= 2){
                gg <- gg + coord_cartesian(ylim = c(yy[1], yy[2]))
            }
            return(gg)
        }
        output$areaPlot2 <- renderPlot({
            xx <- getdata12()
            doAreaPlot2(xx, input$xcounty, 1)
        }, height = 600, width = 1000)
        output$areaPlot2s <- renderPlot({
            xx <- getdata12()
            cc <- getCounties()
            gcc <<- cc #DEBUG-RM
            nn <- input$aplot2_cols * input$aplot2_rows
            ist <- input$aplot2_start
            imx <- min(nn, (1+nrow(cc)-ist))
            pp <- NULL
            for (i in 1:nn) pp[[i]] <- ggplot() + theme_void()
            for (i in 1:imx) pp[[i]] <- doAreaPlot2(xx, cc[(ist+i-1),"COUNTY"], 2)
            plot_grid(plotlist = pp, ncol = input$aplot2_cols)
        }, height = 600, width = 1000)
        output$areaPlot2b <- renderPlot({
            xx <- getdata12()
            # Move filtering to after getdata12()
            if (input$xcounty != "" & input$xcounty != "(all)"){
                xx <- xx[xx$COUNTY == input$xcounty,]
            }
            else{
                xx <- xx[xx$COUNTY != "" & !is.na(xx$COUNTY),]
            }
            if (!input$displaytotal){
                xx <- xx[xx$AREA != "TOTAL",]
            }
            names(xx)[3:10] <- c("DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2")
            xx <- xx[(is.na(xx$TOT1_N) | xx$TOT1_N >= input$minvotes) |
                     (is.na(xx$TOT2_N) | xx$TOT2_N >= input$minvotes),]
            row.names(xx) <- seq(1:NROW(xx))
            xx <- xx[xx$DEM1 > 0 & xx$REP1 > 0 & xx$DEM2 > 0 & xx$REP2 > 0,]
            if (input$party == "Democrat"){
                preparty <- "DEM"
                party1 <- "DEM1"
                party2 <- "DEM2" # areaPlot2b code
            }
            else if (input$party == "Republican"){
                preparty <- "REP"
                party1 <- "REP1"
                party2 <- "REP2" # areaPlot2b code
            }
            else if (input$party == "Total"){
                preparty <- "TOT"
                party1 <- "TOTAL1"
                party2 <- "TOTAL2" # areaPlot2b code
            }
            else{
                preparty <- "MAR"
                party1 <- "MARGIN1"
                party2 <- "MARGIN2" # areaPlot2b code
            }
            party_sh <- paste0(preparty,"_SH")
            party1n <- "TOT1_N"
            party2n <- "TOT2_N"
            xx$Party <- ""
            if (input$xlimit2b != ""){
                vlimit <- as.numeric(unlist(strsplit(input$xlimit2b, ",")))
                vcolor <- unlist(strsplit(input$lcolor2b, ","))
                vcolor <- rep_len(vcolor,(length(vlimit)+1))
                vparty <- unlist(strsplit(input$xparty2b, ","))
                xx$Party <- vparty[length(vparty)]
                xx$Party[xx[["MARGIN1"]] < vlimit[1]] <- vparty[1]
                xx$Color <- vcolor[length(vcolor)]
                xx$Color[xx[["MARGIN1"]] < vlimit[1]] <- vcolor[1]
                #START areaPlot2b code
                xx$Party2 <- vparty[length(vparty)]
                xx$Party2[xx[["MARGIN2"]] < vlimit[1]] <- vparty[1]
                xx$Color2 <- vcolor[length(vcolor)]
                xx$Color2[xx[["MARGIN2"]] < vlimit[1]] <- vcolor[1]
                #STOP areaPlot2b code
                for (i in 1:(length(vlimit)-1)){
                    xx$Party[xx[["MARGIN1"]] >= vlimit[i] & xx[["MARGIN1"]] < vlimit[i+1]] <- vparty[i+1]
                    xx$Color[xx[["MARGIN1"]] >= vlimit[i] & xx[["MARGIN1"]] < vlimit[i+1]] <- vcolor[i+1]
                    #START areaPlot2b code
                    xx$Party2[xx[["MARGIN2"]] >= vlimit[i] & xx[["MARGIN2"]] < vlimit[i+1]] <- vparty[i+1]
                    xx$Color2[xx[["MARGIN2"]] >= vlimit[i] & xx[["MARGIN2"]] < vlimit[i+1]] <- vcolor[i+1]
                    #STOP areaPlot2b code
                }
            }
            isParty <- NULL
            for (i in 1:length(vparty)){
                # isParty <- c(isParty, any(xx$Party == vparty[i])) # areaPlot2 code
                isParty <- c(isParty, any(xx$Party == vparty[i]) | any(xx$Party2 == vparty[i])) # areaPlot2b code
            }
            #START areaPlot2b code
            # gg <- ggplot(xx, aes_string(x=party1, y=party_sh))
            # gg <- gg + geom_point(data=xx, size=3, alpha=as.numeric(input$xalpha2b),
            #                       aes_string(color="Party",shape="Votes"))
            xx <- xx[order(xx[[party1]]),]
            if (substr(input$races[1], 9, 12) == "Pres"){
                xx$Race <- substr(input$races[1], 4, 12)
            }
            else{
                xx$Race <- substr(input$races[1], 4, 11)
            }
            xx[[party_sh]] <- seq(1,NROW(xx))
            xx2 <- xx
            if (substr(input$races[2], 9, 12) == "Pres"){
                xx2$Race <- substr(input$races[2], 4, 12)
            }
            else{
                xx2$Race <- substr(input$races[2], 4, 11)
            }
            xx2[[party1]] <- xx2[[party2]]
            xx$Votes <- xx[[party1n]]
            xx2$Votes <- xx2[[party2n]]
            gg <- ggplot(xx, aes_string(x=party1, y=party_sh))
            gg <- gg + geom_point(data=xx, alpha=as.numeric(input$xalpha2b),
                                  aes_string(color="Party",shape="Race",size="Votes"))
            gg <- gg + geom_point(data=xx2, alpha=as.numeric(input$xalpha2b),
                                  aes_string(color="Party2",shape="Race",size="Votes"))
            gg <- gg + scale_y_reverse()
            #STOP areaPlot2b code
            # if (input$party == "Margin"){ # areaPlot2 code only
            #     gg <- gg + geom_abline(intercept=0, slope=-1, color=input$ncolor2b, linetype="dashed")
            # }
            if (input$party == "Margin" | input$units == "Count"){
                gg <- gg + geom_vline(xintercept=0, color=input$ncolor2b)
            }
            if (input$party == "Margin" &
                min(xx$MAR_SH, na.rm = TRUE) <= 0 &
                max(xx$MAR_SH, na.rm = TRUE) >= 0){
                gg <- gg + geom_hline(yintercept=0, color=input$ncolor2b)
            }
            vcolor <- unlist(strsplit(input$xcolor2b, ","))
            vcolor <- vcolor[isParty]
            if (length(vcolor) > 0){
                gg <- gg + scale_fill_manual(values = vcolor) # Bar Plot
                gg <- gg + scale_color_manual(values = vcolor) # Line Graph
            }
            vshape <- as.numeric(unlist(strsplit(input$vshapeb, ",")))
            if (length(vshape) > 1){
                gg <- gg + scale_shape_manual(values = vshape) # Line Graph
            }
            vrange_n <- as.numeric(unlist(strsplit(input$vrange2b, ",")))
            if (input$vbreaks2b != ""){
                vbreaks_n <- as.numeric(unlist(strsplit(input$vbreaks2b, ",")))
                gg <- gg + scale_size_continuous(range = vrange_n,
                                                 breaks = vbreaks_n)
            }
            else if (input$vtrans2b != "" & substr(input$vtrans2b,1,1) != "#"){
                gg <- gg + scale_size_continuous(range = vrange_n,
                                                 trans = input$vtrans2b)
            }
            labels <- getlabels("plot2b", input$xcounty, 1)
            gg <- gg + ggtitle(labels[1])
            gg <- gg + xlab(labels[2])
            gg <- gg + ylab(labels[3])
            xx$POS   <- 0
            if (input$showall2b){
                xx$POS <- 2 # default to right
            }
            if (input$label2b == "Index"){
                xx$LABEL <- row.names(xx)
            }
            else if (input$label2b == "County"){
                xx$LABEL <- xx$COUNTY
            }
            else if (input$label2b == "CountyID"){
                if (input$state2 == "TX"){
                    xx$LABEL <- sub("^0+", "", substring(xx$AREA,1,3))
                }
                else{
                    xx$LABEL <- xx$COUNTY
                }
            }
            else if (input$label2b == "Area"){
                if (input$state2 == "TX"){
                    xx$LABEL <- sub("^0+", "", substring(xx$AREA,4))
                }
                else{
                    xx$LABEL <- xx$AREA
                }
            }
            else if (input$label2b == "CNTYVTD"){
                xx$LABEL <- xx$AREA
            }
            spos1_2 <- unlist(strsplit(input$pos1_2b, ","))
            xx$POS[xx$AREA %in% spos1_2] <- 1
            xx$POS[row.names(xx) %in% spos1_2] <- 1
            spos2_2 <- unlist(strsplit(input$pos2_2b, ","))
            xx$POS[xx$AREA %in% spos2_2] <- 2
            xx$POS[row.names(xx) %in% spos2_2] <- 2
            spos3_2 <- unlist(strsplit(input$pos3_2b, ","))
            xx$POS[xx$AREA %in% spos3_2] <- 3
            xx$POS[row.names(xx) %in% spos3_2] <- 3
            xx$VJUST <- 0.5
            xx$VJUST[xx$POS == 1] <- -1
            xx$VJUST[xx$POS == 3] <- 2
            xx$PREPEND <- ""
            xx$PREPEND[xx$POS == 2] <- "  "
            xx$LABEL <- paste0(xx$PREPEND,xx$LABEL)
            xx$LABEL[xx$POS == 0] <- ""
            if (input$party == "Democrat"){
                gg <- gg + annotate("text", x = xx$DEM1, y =xx$DEM_SH, label = xx$LABEL,
                                    color=xx$Color, hjust = 0, vjust = xx$VJUST)
            }
            else if (input$party == "Republican"){
                gg <- gg + annotate("text", x = xx$REP1, y =xx$REP_SH, label = xx$LABEL,
                                    color=xx$Color, hjust = 0, vjust = xx$VJUST)
            }
            else if (input$party == "Total"){
                gg <- gg + annotate("text", x = xx$TOTAL1, y =xx$TOT_SH, label = xx$LABEL,
                                    color=xx$Color, hjust = 0, vjust = xx$VJUST)
            }
            else{
                gg <- gg + annotate("text", x = xx$MARGIN1, y =xx$MAR_SH, label = xx$LABEL,
                                    color=xx$Color, hjust = 0, vjust = xx$VJUST)
                gg <- gg + annotate("text", x = xx$MARGIN2, y =xx$MAR_SH, label = xx$LABEL,
                                    color=xx2$Color2, hjust = 0, vjust = xx$VJUST) # areaPlot2b code
            }
            xx <- NULL
            yy <- NULL
            if(input$xscale2b != ""){
                sxx <- unlist(strsplit(input$xscale2b, ","))
                xx <- as.numeric(sxx)
                if (length(sxx) == 3){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[3]))
                }
                else if (length(sxx) == 4){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[4]))
                }
            }
            if(input$yscale2b != ""){
                syy <- unlist(strsplit(input$yscale2b, ","))
                yy <- as.numeric(syy)
                if (length(syy) == 3){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[3]))
                }
                else if (length(syy) == 4){
                    gg <- gg + scale_x_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[4]))
                }
            }
            if (length(xx) >= 2){
                if (length(yy) >= 2){
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]), ylim = c(yy[1], yy[2]))
                }
                else{
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]))
                }
            }
            else if (length(yy) >= 2){
                gg <- gg + coord_cartesian(ylim = c(yy[1], yy[2]))
            }
            return(gg)
        }, height = 600, width = 1000)
        getDeltaM <- function(xx, county){
            xx <- xx[xx$COUNTY == county,]
            oo <- orderdf(xx,input$xsortcol,input$xsortdesc)
            
            nrow <- NROW(oo)
            if (nrow %% 2 == 1){
                imed <- ceiling(NROW(oo) / 2)
                dem1 <- sum(oo[1:imed,4])
                rep1 <- sum(oo[1:imed,5])
                tot1 <- sum(oo[1:imed,3])
                vot1 <- oo[imed,3]
            }
            else{
                imed <- NROW(oo) / 2
                imed2 <- imed + 1
                dem1 <- sum(oo[1:imed,4]) + round(oo[imed2,4]/2)
                rep1 <- sum(oo[1:imed,5]) + round(oo[imed2,5]/2)
                tot1 <- sum(oo[1:imed,3]) + round(oo[imed2,3]/2)
                vot1 <- round((oo[imed,3] + oo[imed2,3])/2)
            }
            dem2 <- sum(oo[,4])
            rep2 <- sum(oo[,5])
            tot2 <- sum(oo[,3])
            deltaM <- 100 * ((rep2-dem2)/tot2 - (rep1-dem1)/tot1)
            ret <- c(deltaM, tot1, vot1)
            return(ret)
        }
        output$myTextCounties <- renderPrint({
            dd <- getCounties()
            dp <- 1
            for (i in 2:NCOL(dd)){
                if (i == 4){
                    dd[,i] <- format(round(dd[,i], dp), big.mark=",", scientific=FALSE)
                }
                else if (i == 5){
                    dd[,i] <- format(round(dd[,i], 0), big.mark=",", scientific=FALSE)
                }
                else{
                    dd[,i] <- format(dd[,i], big.mark=",", scientific=FALSE)
                }
            }
            dd
        })
        getAreas <- function(){
            dd <- getdata()
            dd <- dd[dd$AREA != "TOTAL",] #remove TOTAL
            dd <- dd[is.na(dd$TOTAL) | dd$TOTAL >= input$minvotes,]
            row.names(dd) <- seq(1:NROW(dd))
            #TODO - USING CHECKBOX FROM CVT INPUT PANEL, MOVE TO MAIN INPUT PANEL
            if (input$cvt_x0vote){
                dd <- dd[dd[4] > 0 & dd[5] > 0,] # delete if DEM or REP votes == 0 
            }
            if (input$xcounty != "" & input$xcounty != "(all)"){
                dd <- dd[dd$COUNTY == input$xcounty,]
            }
            ir <- 1
            if (input$xsortcol != 0){
                if (!input$xsortdesc){
                    dd <- dd[order(dd[input$xsortcol]),]
                }
                else{
                    if (class(dd[input$xsortcol]) == "numeric"){
                        dd <- dd[order(-dd[input$xsortcol]),]
                    }
                    else{
                        dd <- dd[order(dd[input$xsortcol]),]
                        dd <- dd %>% arrange(desc(row_number()))
                    }
                }
            }
            dd <- rbind(dd, data.frame(COUNTY="",AREA="TOTAL",t(colSums(dd[,c(-1,-2)]))))
            return(dd)
        }
        output$myTextAreas <- renderPrint({
            dd <- getAreas()
            #START OF CODE THAT DOWNLOADED AS (c(...))
            csum <- cumsum(dd[3])
            dd$DEM_CVT <- 100 * cumsum(dd[4]) / csum
            dd$REP_CVT <- 100 * cumsum(dd[5]) / csum
            #dd <- dd %>% mutate(DDEM=100*(DEM-lag(DEM)))
            #dd <- dd %>% mutate(DREP=100*(REP-lag(REP)))
            #END OF CODE THAT DOWNLOADED AS (c(...))
            dp <- 2
            for (i in 3:NCOL(dd)){
                dd[,i] <- format(round(dd[,i], dp), big.mark=",", scientific=FALSE)
            }
            row.names(dd) <- seq(1:NROW(dd))
            dd
        })
        getAreas2 <- function(){
            dd <- getdata12()
            # Move filtering to after getdata12()
            if (input$xcounty != "" & input$xcounty != "(all)"){
                dd <- dd[dd$COUNTY == input$xcounty,]
            }
            else{
                dd <- dd[dd$COUNTY != "" & !is.na(dd$COUNTY),]
            }
            ddd <<- dd #DEBUG-RM
            dd <- dd[(is.na(dd$TOT1_N) | dd$TOT1_N >= input$minvotes) |
                     (is.na(dd$TOT2_N) | dd$TOT2_N >= input$minvotes),]
            row.names(dd) <- seq(1:NROW(dd))
            #dd <- dd[dd$DEM1 > 0 & dd$REP1 > 0 & dd$DEM2 > 0 & dd$REP2 > 0,] #keep
            if (input$units == "Percent"){
                dd <- dd[,c(1:(NCOL(dd)-5),(NCOL(dd)-1),NCOL(dd))]
            }
            else{
                dd <- dd[,1:(NCOL(dd)-5)]
            }
            if (input$xsortcol2 != 0){
                if (!input$xsortdesc2){
                    dd <- dd[order(dd[input$xsortcol2]),]
                }
                else{
                    if (class(dd[input$xsortcol2]) == "numeric"){
                        dd <- dd[order(-dd[input$xsortcol2]),]
                    }
                    else{
                        dd <- dd[order(dd[input$xsortcol2]),]
                        dd <- dd %>% arrange(desc(row_number()))
                    }
                }
            }
            return(dd)
        }
        output$myTextAreas2 <- renderPrint({
            dd <- getAreas2()
            # Format decimal numbers into character strings
            dp <- 2
            for (i in 3:NCOL(dd)){
                dd[,i] <- format(round(dd[,i], dp), big.mark=",", scientific=FALSE)
            }
            cat(paste0(getlabels("text", input$xcounty, 1)[1],"\n\n"))
            print(dd)
        })
        output$getcsv <- downloadHandler(
            filename = function(){
                paste0(input$races[1],"_",input$xcounty,"_",input$units,".csv")
            },
            content = function(file){
                xx <- getAreas()
                fn <- paste0(input$races[1],"_",input$xcounty,"_",input$units,".csv")
                catmsg(paste0("====> write_cvs(",fn,")"))
                write_csv(xx, file)
            }
        )
        output$getexcel <- downloadHandler(
            filename = function(){
                paste0(input$races[1],"_",input$xcounty,"_",input$units,".xlsx")
            },
            content = function(file){
                xx <- getAreas()
                fn <- paste0(input$races[1],"_",input$xcounty,"_",input$units,".xlsx")
                catmsg(paste0("====> write.xlsx(",fn,")"))
                write.xlsx(xx,file,sheetName = input$xcounty)
            }
        )
        output$getcsv2 <- downloadHandler(
            filename = function(){
                paste0(input$races[1],"_",input$races[2],"_",
                       input$xcounty,"_",input$units,".csv")
            },
            content = function(file){
                xx <- getAreas2()
                fn <- paste0(input$races[1],"_",input$races[2],"_",
                             input$xcounty,"_",input$units,".csv")
                catmsg(paste0("====> write_cvs(",fn,")"))
                write_csv(xx, file)
            }
        )
        output$getexcel2 <- downloadHandler(
            filename = function(){
                paste0(input$races[1],"_",input$races[2],"_",
                       input$xcounty,"_",input$units,".xlsx")
            },
            content = function(file){
                xx <- getAreas2()
                fn <- paste0(input$races[1],"_",input$races[2],"_",
                             input$xcounty,"_",input$units,".xlsx")
                catmsg(paste0("====> write.xlsx(",fn,")"))
                write.xlsx(xx,file,sheetName = input$xcounty)
            }
        )
        createfiles <- function(races){
            if (input$createfiles){
                for (i in 1:length(races)){
                    if (races[i] == "AZ_2018_Senate"){
                        createAZ_2018_Senate()
                    }
                    else if (races[i] == "AZ_2020_President"){
                        createAZ_2020_President("")
                    }
                    else if (races[i] == "AZ_2020_President_Early"){
                        createAZ_2020_President("Early Ballots")
                    }
                    else if (races[i] == "AZ_2020_President_Polls"){
                        createAZ_2020_President("Polling Place")
                    }
                    else if (races[i] == "AZ_2020_President_Prov"){
                        createAZ_2020_President("Provisional Ballots")
                    }
                    else if (races[i] == "AZ_2020_Senate"){
                        createAZ_2020_Senate("")
                    }
                    else if (races[i] == "AZ_2020_Senate_Early"){
                        createAZ_2020_Senate("Early Ballots")
                    }
                    else if (races[i] == "AZ_2020_Senate_Polls"){
                        createAZ_2020_Senate("Polling Place")
                    }
                    else if (races[i] == "AZ_2020_Senate_Prov"){
                        createAZ_2020_Senate("Provisional Ballots")
                    }
                    else if (races[i] == "FL_2016_President"){
                        createFL_2016_President()
                    }
                    else if (races[i] == "FL_2018_Governor"){
                        createFL_2018_Governor()
                    }
                    else if (races[i] == "FL_2018_Senate"){
                        createFL_2018_Senate()
                    }
                    else if (races[i] == "FL_2020_President"){
                        createFL_2020_President()
                        #createFL_2020_Counties()
                        #createFL_2020_County_Codes()
                    }
                    else if (races[i] == "FL_2020_House"){
                        createFL_2020_House()
                    }
                    else if (races[i] == "FL_2020_House_CD27"){
                        createFL_2020_House_CD27()
                    }
                    else if (races[i] == "FL_2018_Registered"){
                        createFL_2018_Registered()
                    }
                    else if (races[i] == "FL_2020_Registered"){
                        createFL_2020_Registered()
                    }
                    else if (races[i] == "IA_2018_Governor"){
                        createIA_2018_Governor()
                    }
                    else if (races[i] == "IA_2018_House_CD1"){
                        createIA_2018_House_CD1()
                    }
                    else if (races[i] == "IA_2020_House_CD1"){
                        createIA_2020_House_CD1()
                    }
                    else if (races[i] == "IA_2018_House_CD2"){
                        createIA_2018_House_CD2()
                    }
                    else if (races[i] == "IA_2020_House_CD2"){
                        createIA_2020_House_CD2()
                    }
                    else if (races[i] == "IA_2018_House_CD3"){
                        createIA_2018_House_CD3()
                    }
                    else if (races[i] == "IA_2020_House_CD3"){
                        createIA_2020_House_CD3()
                    }
                    else if (races[i] == "IA_2018_House_CD4"){
                        createIA_2018_House_CD4()
                    }
                    else if (races[i] == "IA_2020_House_CD4"){
                        createIA_2020_House_CD4()
                    }
                    else if (races[i] == "IA_2020_President"){
                        #createIA_2020_Counties()
                        createIA_2020_President()
                    }
                    else if (races[i] == "IA_2020_Senate"){
                        createIA_2020_Senate()
                    }
                    else if (races[i] == "ME_2014_Senate"){
                        createME_2014_Senate()
                    }
                    else if (races[i] == "ME_2018_Governor"){
                        createME_2018_Governor()
                    }
                    else if (races[i] == "ME_2018_Senate"){
                        createME_2018_Senate()
                    }
                    else if (races[i] == "ME_2020_President"){
                        createME_2020_President()
                    }
                    else if (races[i] == "ME_2020_Senate"){
                        createME_2020_Senate()
                    }
                    else if (races[i] == "ME_2020_House"){
                        createME_2020_House()
                    }
                    else if (races[i] == "MT_2018_Senate"){
                        createMT_2018_Senate()
                    }
                    else if (races[i] == "MT_2020_President"){
                        createMT_2020_President()
                    }
                    else if (races[i] == "MT_2020_Senate"){
                        createMT_2020_Senate()
                    }
                    else if (races[i] == "NC_2018_House"){
                        createNC_2018_House()
                    }
                    else if (races[i] == "NC_2020_President"){
                        createNC_2020_President()
                    }
                    else if (races[i] == "NC_2020_Senate"){
                        createNC_2020_Senate()
                    }
                    else if (races[i] == "NC_2020_Governor"){
                        createNC_2020_Governor()
                    }
                    else if (races[i] == "SC_2016_President"){
                        createSC_2016_President()
                    }
                    else if (races[i] == "SC_2018_Governor"){
                        createSC_2018_Governor()
                    }
                    else if (races[i] == "SC_2020_President"){
                        createSC_2020_President()
                    }
                    else if (races[i] == "SC_2020_Senate"){
                        createSC_2020_Senate()
                    }
                    else if (races[i] == "SC_2020_Registered"){
                        createSC_2020_Registered()
                    }
                    else if (races[i] == "TX_2016_President"){
                        createTX_2016_President()
                    }
                    else if (races[i] == "TX_2018_AG"){
                        createTX_2018_AG()
                    }
                    else if (races[i] == "TX_2018_Governor"){
                        createTX_2018_Governor()
                    }
                    else if (races[i] == "TX_2018_Senate"){
                        createTX_2018_Senate()
                    }
                    else if (races[i] == "TX_2020_President"){
                        createTX_2020_President()
                    }
                    else if (races[i] == "TX_2020_Senate"){
                        createTX_2020_Senate()
                    }
                    else if (races[i] == "WI_2020_President"){
                        createWI_2020_President()
                    }
                    else if (races[i] == "WI_2016_President"){
                        createWI_2016_President()
                    }
                    else if (races[i] == "WI_2016_President_Recount"){
                        createWI_2016_President_Recount()
                    }
                   else{
                        catmsg(paste0("Unknown race: ",races[i]))
                    }
                }
            }
        }
        getCounties <- reactive({
            xx <- getdata()
            xx <- xx[is.na(xx$TOTAL) | xx$TOTAL >= input$minvotes,]
            row.names(xx) <- seq(1:NROW(xx))
            yy <- xx %>%
                group_by(COUNTY) %>%
                summarize(AREAS = length(AREA), VOTES = sum(TOTAL))
            yy <- yy[yy$COUNTY != "",]
            yy$deltaM <- 0
            yy$deltaMxV <- 0
            yy$totalM <- 0
            yy$votesM <- 0
            for (i in 1:NROW(yy)){
                ret <- getDeltaM(xx, yy$COUNTY[i])
                yy$deltaM[i] <- ret[1]
                yy$deltaMxV[i] <- ret[1] * yy$VOTES[i] / 100
                yy$totalM[i] <- ret[2]
                yy$votesM[i] <- ret[3]
            }
            yy <- orderdf(yy,input$sortcounty,(input$sortcountydir == "Desc"))
            counties <- c(yy$COUNTY,"(all)")
            current_county <- input$xcounty
            if (current_county == ""){
                updateSelectInput(session,"xcounty",choices = counties)
            }
            else{
                updateSelectInput(session,"xcounty",choices = counties,selected = current_county)
            }
            return(data.frame(yy))
        })
        getrace <- function(race){
            filenamex <- paste0(data_dir,race,".csv")
            createfiles(race)
            tryCatch(
                expr = {
                    xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
                    xx0 <<- read_delim(filenamex, ' ', skip = 1) #make xx0 available after tryCatch
                },
                error = function(e){
                    message('Caught an error!')
                    print(e)
                },
                warning = function(w){
                    message('Caught a warning!')
                    print(w)
                },
                finally = {
                    message('All done, quitting.')
                }
            )    
            if (names(xx0)[1] == "DIST"){
                if (input$dist != ""){
                    xx0 <- xx0[xx0$DIST == input$dist,]
                }
                xx0 <- xx0[,-1]
                xxparty <- xxparty[,-1]
            }
            # Remove columns where __party starts with X_, rows where AREA starts with X_
            xx0 <- xx0[,!grepl("^X_",xxparty)]
            xxparty <- xxparty[,!grepl("^X_",xxparty)]
            xx0 <- xx0[ !grepl("^X_",xx0$AREA),]
            
            idem <- which(xxparty == "DEM")[1]
            irep <- which(xxparty == "REP")[1]
            if (NCOL(xx0) > 5){
                xx <- xx0[,c(1,2,3,idem,irep,seq(6,NCOL(xx0)))] # COUNTY,AREA,TOTAL,DEM1,REP1,...
            }
            else{
                xx <- xx0[,c(1,2,3,idem,irep)] # COUNTY,AREA,TOTAL,DEM1,REP1
            }
            xx <- as.data.frame(xx)
            
            if (input$skip_rows != ""){
                vskip_rows <- as.numeric(unlist(strsplit(input$skip_rows, ",")))
                for (i in length(vskip_rows):1){
                    xx <- xx[-vskip_rows[i],]
                }
            }
            #Set all candidate NAs to zero
            for (j in 3:NCOL(xx)){
                xx[is.na(xx[,j]),j] <- 0
            }
            if (input$incl_cand != ""){
                vincl_cand <- as.numeric(unlist(strsplit(input$incl_cand, ","))) + 3
                xx$TOTAL <- 0
                for (i in 1:length(vincl_cand)){
                    if (vincl_cand[i] <= NCOL(xx)){
                        xx$TOTAL <- xx$TOTAL + xx[,vincl_cand[i]]
                    }
                    else{
                        vincl_cand <- vincl_cand[1:(i-1)]
                        break
                    }
                }
                xx <- xx[,c(1,2,3,vincl_cand)]
            }

            dd <- xx
            ddtot <- data.frame(COUNTY="",AREA="TOTAL",t(colSums(dd[,c(-1,-2)])))
            gdd <<- dd
            gddtot <<- ddtot
            dd <- rbind(dd,ddtot)
            return(dd)
        }
        getdata <- reactive({
            races <- input$races
            nraces <- length(races)
            dd <- NULL
            if (nraces >= 1){
                dd <- getrace(races[1])
            }
            dd
        })
        getdata2 <- reactive({
            races <- input$races
            nraces <- length(races)
            dd <- NULL
            if (nraces >= 2){
                dd <- getrace(races[2])
                }
            dd
        })
        getdata12 <- reactive({
            xx <- getdata()
            if (is.null(xx)){
                cat("ERROR: Select two races\n")
                return(NULL)
            }
            yy <- getdata2()
            if (is.null(yy)){
                cat("ERROR: Select second race\n")
                return(NULL)
            }
            namesxx <- names(xx)
            namesyy <- names(yy)
            names(xx)[3:5] <- c("TOTAL1","DEM1","REP1")
            names(yy)[3:5] <- c("TOTAL2","DEM2","REP2")
            xx$MARGIN1 <- xx$DEM1 - xx$REP1
            yy$MARGIN2 <- yy$DEM2 - yy$REP2
            xx <- xx[,c(1,2,4,5,NCOL(xx),3),]
            yy <- yy[,c(1,2,4,5,NCOL(yy),3),]
            gxx <<- xx
            gyy <<- yy
            if (input$cleanlevel > 0){
                #xx$COUNTY <- toupper(xx$COUNTY)
                #yy$COUNTY <- toupper(yy$COUNTY)
                xx$AREA <- toupper(xx$AREA)
                yy$AREA <- toupper(yy$AREA)
            }
            if (input$cleanlevel > 1){
                xx$AREA <- gsub(" WARDS "," WARD ",xx$AREA)
                yy$AREA <- gsub(" WARDS "," WARD ",yy$AREA)
            }
            dd <- as.data.frame(merge(xx, yy, by = c("COUNTY","AREA"), all = TRUE))
            # Move filtering to after getdata12()
            # if (input$xcounty != "" & input$xcounty != "(all)"){
            #     dd <- dd[dd$COUNTY == input$xcounty,]
            # }
            # else{
            #     dd <- dd[dd$COUNTY != "" & !is.na(dd$COUNTY),]
            # }
            if (input$areamod != ""){
                ch1 <- substr(input$areamod,1,1)
                pat <- substring(input$areamod,2)
                if (ch1 == "-"){
                    dd$AREA <- gsub(pat,"",dd$AREA)
                    dd <- dd %>%
                        group_by(COUNTY,AREA) %>%
                        summarize(DEM1=sum(DEM1, na.rm = TRUE),
                                  REP1=sum(REP1, na.rm = TRUE),
                                  MARGIN1=sum(MARGIN1, na.rm = TRUE),
                                  TOTAL1=sum(TOTAL1, na.rm = TRUE),
                                  DEM2=sum(DEM2, na.rm = TRUE),
                                  REP2=sum(REP2, na.rm = TRUE),
                                  MARGIN2=sum(MARGIN2, na.rm = TRUE),
                                  TOTAL2=sum(TOTAL2, na.rm = TRUE))
                    dd <- as.data.frame(dd)
                }
            }
            if (input$showother){
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
                }
            }
            dd$DEM_SH <- dd$DEM2 - dd$DEM1
            dd$REP_SH <- dd$REP2 - dd$REP1
            dd$MAR_SH <- dd$MARGIN2 - dd$MARGIN1
            dd$TOT_SH <- dd$TOTAL2 - dd$TOTAL1
            dd$DEM1_N <- dd$DEM1
            dd$REP1_N <- dd$REP1
            dd$MAR1_N <- dd$MARGIN1
            dd$TOT1_N <- dd$TOTAL1
            dd$TOT2_N <- dd$TOTAL2
            dd <- rbind(dd, data.frame(COUNTY="TOTAL",AREA="TOTAL",
                                       t(colSums(dd[,c(-1,-2)],na.rm = TRUE))))
            
            if (input$units == "Percent"){
                dd$DEM1 <- 100 * dd$DEM1 / dd$TOTAL1
                dd$REP1 <- 100 * dd$REP1 / dd$TOTAL1
                dd$MARGIN1 <- 100 * dd$MARGIN1 / dd$TOTAL1
                dd$DEM2 <- 100 * dd$DEM2 / dd$TOTAL2
                dd$REP2 <- 100 * dd$REP2 / dd$TOTAL2
                dd$MARGIN2 <- 100 * dd$MARGIN2 / dd$TOTAL2
                dd$TOTAL1 <- dd$DEM1 + dd$REP1
                dd$TOTAL2 <- dd$DEM2 + dd$REP2
                dd$DEM_SH <- dd$DEM2 - dd$DEM1
                dd$REP_SH <- dd$REP2 - dd$REP1
                dd$MAR_SH <- dd$MARGIN2 - dd$MARGIN1
                dd$TOT_SH <- dd$TOTAL2 - dd$TOTAL1
            }
            else if (input$units == "Percent ratio"){
                dd$DEM_SH <- 100 * dd$DEM1 / dd$DEM2
                dd$REP_SH <- 100 * dd$REP1 / dd$REP2
                dd$MAR_SH <- 100 * dd$MARGIN1 / dd$MARGIN2
                dd$TOT_SH <- 100 * dd$TOTAL1 / dd$TOTAL2
            }
            names(dd)[3:4] <- namesxx[4:5] # reset names
            names(dd)[7:8] <- namesyy[4:5]
            row.names(dd) <- seq(1:NROW(dd))
            dd
        })
        observeEvent(input$mapsave,{
            eventid <- "Map"
            parmid <- c("maplimitset", "maplimits",
                        "mapyear","mapvar","mapcolors")
            filename <- paste0(data_dir,input$state2,"_",eventid,"_Parms.csv")
            if (file.exists(filename)){
                parms <- read_csv(filename)
                newversion <- max(parms$version) + 1
            }
            else{
                parms <- data.frame(version=integer(),
                                    label=character(),
                                    value=character(),
                                    stringsAsFactors = FALSE)
                newversion <- 1
            }
            nr <- NROW(parms)
            version <- rep(newversion, length(parmid))
            label <- parmid
            value <- NULL
            for (i in 1:length(parmid)){
                value <- c(value, input[[parmid[i]]])
            }
            aparms <- data.frame(version, label, value)
            parms <- rbind(parms, aparms)
            write_csv(parms, filename)
        })
        observe({
            eventid <- "Plot"
            loadid <- "plotload"
            parmid <- c("showrow", "pos1", "pos3", "xscale", "yscale",
                        "xlimit", "xcolor", "xparty", "noparty",
                        "vlimit", "vshape", "vdesc")
            parmup <- c("checkbox", "pos1", "pos3", "xscale", "yscale",
                        "xlimit", "xcolor", "xparty", "select",
                        "vlimit", "vshape", "vdesc")
            filename <- paste0(data_dir,input$state2,"_",eventid,"_Parms.csv")
            if (file.exists(filename)){
                parms <- read_csv(filename)
                loadversion <- input[[loadid]]
                pp <- parms[parms$version == loadversion,]
                for (i in 1:length(parmid)){
                    if (parmup[i] == "numeric"){
                        updateNumericInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "select"){
                        updateSelectInput(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "checkbox"){
                        updateCheckboxInput(session, parmid[i], value = as.logical(pp$value[pp$label == parmid[i]]))
                    }
                    else if (parmup[i] == "radio"){
                        updateRadioButtons(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else{
                        updateTextInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                }
            }
        })
        observeEvent(input$plotsave,{
            eventid <- "Plot"
            parmid <- c("showrow", "pos1", "pos3", "xscale", "yscale",
                        "xlimit", "xcolor", "xparty", "noparty",
                        "vlimit", "vshape", "vdesc")
            filename <- paste0(data_dir,input$state2,"_",eventid,"_Parms.csv")
            if (file.exists(filename)){
                parms <- read_csv(filename)
                newversion <- max(parms$version) + 1
            }
            else{
                parms <- data.frame(version=integer(),
                                    label=character(),
                                    value=character(),
                                    stringsAsFactors = FALSE)
                newversion <- 1
            }
            nr <- NROW(parms)
            version <- rep(newversion, length(parmid))
            label <- parmid
            value <- NULL
            for (i in 1:length(parmid)){
                value <- c(value, input[[parmid[i]]])
            }
            aparms <- data.frame(version, label, value)
            parms <- rbind(parms, aparms)
            write_csv(parms, filename)
        })
        observe({
            eventid <- "Map"
            loadid <- "mapload"
            parmid <- c("minpop", "longoff", "skipcity",
                        "showcity", "maplimitset", "maplimits",
                        "mapyear","mapvar","mapcolors")
            parmup <- c("numeric", "numeric", "skipcity",
                        "showcity", "select", "maplimits",
                        "numeric","select","mapcolors")
            filename <- paste0(data_dir,input$state2,"_",eventid,"_Parms.csv")
            if (file.exists(filename)){
                parms <- read_csv(filename)
                loadversion <- input[[loadid]]
                pp <- parms[parms$version == loadversion,]
                for (i in 1:length(parmid)){
                    if (parmup[i] == "numeric"){
                        updateNumericInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "select"){
                        updateSelectInput(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "checkbox"){
                        updateCheckboxInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "radio"){
                        updateRadioButtons(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else{
                        updateTextInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                }
            }
        })
        observeEvent(input$state2,{
            if (input$state2 == "AZ"){
                files <- c("AZ_2020_President","AZ_2020_President_Early","AZ_2020_President_Polls","AZ_2020_President_Prov",
                           "AZ_2020_Senate","AZ_2020_Senate_Early","AZ_2020_Senate_Polls","AZ_2020_Senate_Prov",
                           "AZ_2018_Senate")
            }
            else if (input$state2 == "FL"){
                files <- c("FL_2020_President","FL_2020_House","FL_2020_House_CD27","FL_2020_Registered","FL_2018_Governor","FL_2018_Senate","FL_2018_Registered","FL_2016_President")
            }
            else if (input$state2 == "IA"){
                files <- c("IA_2020_President","IA_2020_Senate","IA_2020_House_CD1","IA_2020_House_CD2","IA_2020_House_CD3","IA_2020_House_CD4","IA_2018_Governor","IA_2018_House_CD1","IA_2018_House_CD2","IA_2018_House_CD3","IA_2018_House_CD4")
            }
            else if (input$state2 == "ME"){
                files <- c("ME_2020_President","ME_2020_Senate","ME_2020_House","ME_2018_Governor","ME_2018_Senate","ME_2014_Senate")
            }
            else if (input$state2 == "MT"){
                files <- c("MT_2020_President","MT_2020_Senate","MT_2018_Senate")
            }
            else if (input$state2 == "NC"){
                files <- c("NC_2020_President","NC_2020_Senate","NC_2020_Governor","NC_2018_House")
            }
            else if (input$state2 == "SC"){
                files <- c("SC_2020_President","SC_2020_Senate","SC_2018_Governor","SC_2016_President","SC_2020_Registered")
            }
            else if (input$state2 == "TX"){
                files <- c("TX_2020_President","TX_2020_Senate","TX_2018_AG","TX_2018_Governor","TX_2018_Senate","TX_2016_President")
            }
            else if (input$state2 == "WI"){
                files <- c("WI_2020_President","WI_2016_President","WI_2016_President_Recount")
            }
            updateSelectInput(session,"races",choices = files,selected = files[1])
        })
        observeEvent(input$races,{
            getCounties()
        })
        observeEvent(input$sortcounty,{
            if (input$sortcounty == "COUNTY"){
                updateRadioButtons(session,"sortcountydir",selected = "Ascending")
            }
            else{
                updateRadioButtons(session,"sortcountydir",selected = "Desc")
            }
            getCounties()
        })
        observe({
            catmsg(paste0("v3: ",input$state2," ",input$tabs))
        })
    }
)
