library(tidyverse)
library(readxl)
library(ggplot2)
library(reshape2)

library(tigris)
library(leaflet)
library(htmltools)
library(xlsx)
library(RcppRoll)

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
        createFL_2020_House_CD27 <- function(){
            catmsg("##### START createFL_2020_House_CD27 #####")
            xx <- read_delim(paste0(input_dir,"FL/2020-general-election-rev/",
                                    "DAD_PctResults20201103.txt"), '\t',
                             col_names = FALSE, col_types = "ccdccccddddccdccddd")
            names(xx) <- c("Code","County","ElectNo","ElectDate","ElectName",
                           "AreaId","AreaLoc","RegAll","RegRep","RegDem",
                           "RegOth","Contest","Dist","ConCode","Candidate",
                           "Party","RegId","CandNo","Votes")
            xx027 <<- xx
            xx <- xx[xx$Contest == "Representative in Congress",]
            xx <- xx[xx$Dist == " District 27",]
            xx <- xx[,c("County","AreaLoc","Candidate","Votes")]
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
            xx$COUNTY[grepl(" UOCAVA", xx$AREA),] <- "STATE"
            xx$AREA[grepl(" UOCAVA", xx$AREA),] <- "UOCAVA"
            xx <- xx[!grepl(" Total", xx$AREA) & (xx$AREA != "") & !is.na(xx$AREA),]
            xx <- xx[,c(1,2,NCOL(xx),4,3,seq(5,(NCOL(xx)-2)))] # delete Blank
            xx <- xx[!is.na(xx$COUNTY),]
            names(xx)[3] <- "TOTAL"
            namesxx <- names(xx)
            namesxx[4:5] <- c("DEM","REP")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_2020_Senate.csv"))
            write_delim(xx, paste0(data_dir,"ME_2020_Senate.csv"), append = TRUE, col_names = TRUE)
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
        }
        orderdf <- function(dd, sortcol, sortdir){
            if (sortcol != 0){
                if (sortdir == "Ascending"){
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
        output$myUsage <- renderUI({
            includeHTML("http://econdataus.com/voting_area.htm")
        })
        output$areaPlot <- renderPlot({
            areaWidth <<- input$areaWidth
            areaHeight <<- input$areaHeight
            dd <- getdata()
            if (input$xcounty != ""){
                dd <- dd[dd$COUNTY == input$xcounty,]
            }
            else{
                dd <- dd[dd$COUNTY != "" & !is.na(dd$COUNTY),]
            }
            dd <- orderdf(dd,input$xsortcol,input$xsortdir)
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
        output$cvtPlot <- renderPlot({
            xx <- getdata()
            if (input$cvt_x0vote){
                xx <- xx[xx[4] > 0 & xx[5] > 0,] # delete if DEM or REP votes == 0 
            }
            if (input$xcounty != ""){
                xx <- xx[xx$COUNTY == input$xcounty,]
            }
            votesM <- getDeltaM(xx, input$xcounty)[2]
            yy <- xx
            yy <- orderdf(yy,input$xsortcol,input$xsortdir)
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
            title <- paste0(input$xcounty," County, ",input$races[1]," - Cumulative Vote Tally, ordered by ",names(zz)[input$xsortcol],", ",input$xsortdir)
            xlabel <- "Votes"
            ylabel <- "Percent of Votes"
            if (input$plotbyarea){
                xlabel <- "Areas"
            }
            else if (input$votes1000){
                votesM <- votesM/1000
                zz$Votes <- zz$Votes/1000
                xlabel <- "Votes (thousands)"
            }
            gg <- ggplot(zz, aes(x = Votes, y = Share))
            gg <- gg + geom_point(aes_string(color="Candidate",shape="Candidate"), size=3, alpha=0.7)
            gg <- gg + geom_line(aes_string(color="Candidate"), size=2, alpha=0.7)
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
        })
        getDeltaM <- function(xx, county){
            xx <- xx[xx$COUNTY == county,]
            oo <- orderdf(xx,input$xsortcol,input$xsortdir)
            
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
        output$myTextAreas <- renderPrint({
            dd <- getdata()
            gd0 <<- dd
            if (input$xcounty != ""){
                dd <- dd[dd$COUNTY == input$xcounty,]
                dd <- rbind(dd, data.frame(COUNTY="",AREA="TOTAL",t(colSums(dd[,c(-1,-2)]))))
            }
            gd1 <<- dd
            ir <- 1
            if (input$xsortcol != 0){
                if (input$xsortdir == "Ascending"){
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
            dp <- 2
            for (i in 3:NCOL(dd)){
                dd[,i] <- format(round(dd[,i], dp), big.mark=",", scientific=FALSE)
            }
            row.names(dd) <- seq(1:NROW(dd))
            dd
        })
        createfiles <- function(races){
            if (input$createfiles){
                for (i in 1:length(races)){
                    if (races[i] == "FL_2020_House_CD27"){
                        createFL_2020_House_CD27()
                    }
                    else if (races[i] == "ME_2020_President"){
                        createME_2020_President()
                    }
                    else if (races[i] == "ME_2020_Senate"){
                        createME_2020_Senate()
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
            yy <- orderdf(yy,input$sortcounty,input$sortcountydir)
            counties <- c(yy$COUNTY,"")  # "" not necessary?
            current_county <- input$xcounty
            if (current_county == ""){
                updateSelectInput(session,"xcounty",choices = counties)
            }
            else{
                updateSelectInput(session,"xcounty",choices = counties,selected = current_county)
            }
            return(data.frame(yy))
        })
        getdata <- reactive({
            races <- input$races
            nraces <- length(races)
            filenamex <- paste0(data_dir,races[1],".csv")
            createfiles(races[1])
            xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
            
            xx0 <- read_delim(filenamex, ' ', skip = 1)
            # Remove columns where __party starts with X_, rows where AREA starts with X_
            xx0 <- xx0[,!grepl("^X_",xxparty)]
            xxparty <- xxparty[,!grepl("^X_",xxparty)]
            xx0 <- xx0[ !grepl("^X_",xx0$AREA),]

            idem <- which(xxparty == "DEM")
            irep <- which(xxparty == "REP")
            xx <- xx0[,c(1,2,3,idem,irep,seq(6,NCOL(xx0)))] # COUNTY,AREA,TOTAL,DEM1,REP1,...
            xx <- as.data.frame(xx)
            
            if (input$skip_rows != ""){
                vskip_rows <- as.numeric(unlist(strsplit(input$skip_rows, ",")))
                for (i in length(vskip_rows):1){
                    xx <- xx[-vskip_rows[i],]
                }
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
            dd <- dd[dd$TOTAL >= input$minvotes,]
            return(dd)
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
            if (input$state2 == "FL"){
                files <- c("FL_2020_House_CD27")
            }
            else if (input$state2 == "ME"){
                files <- c("ME_2020_President","ME_2020_Senate")
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
