library(leaflet)

shinyUI(pageWithSidebar(
    headerPanel("Analysis of Reported Voting Areas"),
    sidebarPanel(
        width = 2,
        selectInput("state2", "STATE",
                    choices = c("AZ","CA","CO","FL","IA","ME","MN","MT","NC","OH","SC","TX","WI"),
                    selected = "WI",
                    multiple = FALSE),
        selectInput("races", "RACE",
                    choices = c("WI_2020_President",
                                "WI_2016_President",
                                "WI_2016_President_Recount"),
                    selected = "WI_2020_President",
                    multiple = TRUE),
        selectInput("xcounty", "COUNTY",
                    choices = c(""),
                    selected = "",
                    multiple = FALSE),
        #textInput("xarea", "AREA", value = ""),
        splitLayout(
            textInput("areamod", "AREA modify", value = "#=TOTAL>MERGE"),
            textInput("areafilter", "filter", value = "")
        ),
        checkboxInput("showother","Show Other Areas",value = TRUE),
        splitLayout(
            numericInput("minvotes","Min Votes",30,min = 0),
            textInput("dist", "DIST", value = "")
        ),
        selectInput("units", "Units",
                    choices = c("Count","Percent","Percent ratio"),
                    selected = "Percent",
                    multiple = FALSE),
        selectInput("sortcounty", "Sort Counties",
                    choices = c("COUNTY","AREAS","VOTES","deltaM","deltaMxV","totalM","votesM"),
                    selected = "COUNTY",
                    multiple = FALSE),
        radioButtons("sortcountydir", NULL, c("Ascending","Desc"), "Ascending", inline = TRUE),
        # numericInput("xsortcol_1", "Sort Areas (column)", 3),
        # radioButtons("xsortdir", NULL, c("Ascending","Desc"), "Ascending", inline = TRUE),
        splitLayout(
            numericInput("xsortcol", "Sort Areas,", 3),
            numericInput("xsortcol2", "Areas2 (col)", 0)
        ),
        splitLayout(
            checkboxInput("xsortdesc","Desc",value = FALSE),
            checkboxInput("xsortdesc2","Desc",value = FALSE)
        ),
        selectInput("party", "Party",
                    choices = c("Democrat","Republican","Margin","Total"),
                    selected = "Margin",
                    multiple = FALSE),
        numericInput("cleanlevel", "Clean Level", 2, min = 0),
        textInput("incl_cand", "Include candidates", value = "1,2,3,4,5"),
        textInput("skip_rows", "Skip rows", value = ""),
        checkboxInput("displaytotal","Display TOTAL",value = FALSE),
        checkboxInput("createfiles","Create Data Files",value = FALSE)
    ),
    mainPanel(
        tabsetPanel(id = "tabs",
            tabPanel("Counties",
                mainPanel(
                    width = 12,
                    verbatimTextOutput("myTextCounties")
                )
            ),
            tabPanel("Areas",
                     sidebarPanel(
                         width = 2,
                         downloadButton("getcsv","Get CSV"),
                         downloadButton("getexcel","Get Excel")
                     ),
                     mainPanel(
                         width = 12,
                         verbatimTextOutput("myTextAreas")
                     )
            ),
            tabPanel("Area Plot",
                sidebarPanel(
                    width = 3,
                    checkboxInput("showrow","Show rows",value = TRUE),
                    textInput("pos1", "Position above", value = ""),
                    textInput("pos2", "Position right", value = ""),
                    textInput("pos3", "Position below", value = ""),
                    textInput("areaxscale", "X From,To,Step,Tick", value = ""),
                    textInput("areayscale", "Y From,To,Step,Tick", value = ""),
                    splitLayout(
                        textInput("xlimit","Limit",value = "-5,5"),
                        textInput("xalpha","Alpha",value = "0.5")
                    ),
                    textInput("areaColor","Color",value = "red3,green3,blue3"),
                    textInput("xparty","Party",value = "1_Solid R,2_Toss-Up,3_Solid D"),
                    checkboxInput("area_x0vote","Exclude 0 votes",value = TRUE),
                    splitLayout(
                        numericInput("areaWidth", "Plot Width", 800),
                        numericInput("areaHeight", "Plot Height", 600)
                    ),
                    splitLayout(
                        numericInput("plotload", "Load", 1),
                        actionButton("plotsave", "Save")
                    )
                ),
                mainPanel(
                    width = 9,
                    plotOutput("areaPlot")
                )
            ),
            tabPanel("CVT",
                     sidebarPanel(
                         width = 3,
                         textInput("xscale", "X From,To,Step,Tick", value = ""),
                         textInput("yscale", "Y From,To,Step,Tick", value = ""),
                         textInput("xcolor","Color",value = "blue3,red3,orange,green3,violet"),
                         textInput("xshape","Shape",value = "3,8,0,1,2,15,16,17"),
                         numericInput("cvt_window","Rolling window",0,min = 0),
                         checkboxInput("cvt_x0vote","Exclude 0 votes",value = TRUE),
                         checkboxInput("votes1000","Votes in 1000s",value = TRUE),
                         checkboxInput("plotbyarea","Plot by Area",value = TRUE),
                         splitLayout(
                             numericInput("plotload", "Load", 1),
                             actionButton("plotsave", "Save")
                         )
                     ),
                     mainPanel(
                         width = 9,
                         plotOutput("cvtPlot")
                     )
            ),
            tabPanel("CVTs",
                     sidebarPanel(
                         width = 2,
                         numericInput("cvt_start","Starting index",1,min = 1,step = 9),
                         numericInput("cvt_cols","Number of columns",3,min = 1),
                         numericInput("cvt_rows","Number of rows",3,min = 1)
                         # textInput("xscale", "X From,To,Step,Tick", value = ""),
                         # textInput("yscale", "Y From,To,Step,Tick", value = ""),
                         # textInput("xcolor","Color",value = "blue3,red3,orange,green3,violet"),
                         # textInput("xshape","Shape",value = "3,8,0,1,2,15,16,17"),
                         # numericInput("cvt_window","Rolling window",0,min = 0),
                         # checkboxInput("cvt_x0vote","Exclude 0 votes",value = TRUE),
                         # checkboxInput("votes1000","Votes in 1000s",value = TRUE),
                         # checkboxInput("plotbyarea","Plot by Area",value = TRUE),
                         # splitLayout(
                         #     numericInput("plotload", "Load", 1),
                         #     actionButton("plotsave", "Save")
                         # )
                     ),
                     mainPanel(
                         width = 9,
                         plotOutput("cvtPlots")
                     )
            ),
            tabPanel("Areas2",
                     sidebarPanel(
                         width = 2,
                         downloadButton("getcsv2","Get CSV"),
                         downloadButton("getexcel2","Get Excel")
                     ),
                     mainPanel(
                         width = 12,
                         verbatimTextOutput("myTextAreas2")
                     )
            ),
            tabPanel("Area Plot2",
                     sidebarPanel(
                         width = 3,
                         checkboxInput("xdxplot2","x/dx plot (else x/y)",value = TRUE),
                         checkboxInput("showall2","Show all labels",value = TRUE),
                         checkboxInput("sizefor2","Size for race 2",value = TRUE),
                         selectInput("label2", "Label type",
                                     choices = c("Index","County","CountyID","Area","CNTYVTD"),
                                     selected = "Index",
                                     multiple = FALSE),
                         textInput("pos1_2", "Position above", value = ""),
                         textInput("pos2_2", "Position right", value = ""),
                         textInput("pos3_2", "Position below", value = ""),
                         textInput("xscale2", "X From,To,Step,Tick", value = ""),
                         textInput("yscale2", "Y From,To,Step,Tick", value = ""),
                         checkboxInput("forcex","Force x-axis",value = TRUE),
                         splitLayout(
                             textInput("xlimit2","Limit",value = "-5,5"),
                             textInput("xalpha2","Alpha",value = "0.5")
                         ),
                         textInput("xcolor2","Color (points)",value = "red3,green3,blue3"),
                         textInput("lcolor2","Color (labels)",value = "red3,green3,blue3"),
                         textInput("ncolor2","Color (lines)",value = "black"),
                         textInput("xparty2","Party",value = "1_Solid R,2_Toss-Up,3_Solid D"),
                         textInput("vrange","Vote Point Range",value = "1,4"),
                         textInput("vtrans","Vote Transform",value = "#log10"),
                         textInput("vbreaks","Vote Breaks", value = ""),
                         textInput("plusnote","Add to title",value = ""),
                         splitLayout(
                             numericInput("plotload2", "Load", 1),
                             actionButton("plotsave2", "Save")
                         )
                     ),
                     mainPanel(
                         width = 9,
                         plotOutput("areaPlot2")
                     )
            ),
            tabPanel("Area Plot2s",
                     sidebarPanel(
                         width = 2,
                         numericInput("aplot2_start","Starting index",1,min = 1,step = 9),
                         numericInput("aplot2_cols","Number of columns",3,min = 1),
                         numericInput("aplot2_rows","Number of rows",3,min = 1)
                     ),
                     mainPanel(
                         width = 9,
                         plotOutput("areaPlot2s")
                     )
            ),
            tabPanel("Area Plot2b",
                     sidebarPanel(
                         width = 3,
                         checkboxInput("showall2b","Show all labels",value = TRUE),
                         selectInput("label2b", "Label type",
                                     choices = c("Index","County","CountyID","Area","CNTYVTD"),
                                     selected = "Index",
                                     multiple = FALSE),
                         textInput("pos1_2b", "Position above", value = ""),
                         textInput("pos2_2b", "Position right", value = ""),
                         textInput("pos3_2b", "Position below", value = ""),
                         textInput("xscale2b", "X From,To,Step,Tick", value = ""),
                         textInput("yscale2b", "Y From,To,Step,Tick", value = ""),
                         splitLayout(
                             textInput("xlimit2b","Limit",value = "-5,5"),
                             textInput("xalpha2b","Alpha",value = "0.5")
                         ),
                         textInput("xcolor2b","Color (points)",value = "red3,orange,green3,purple,blue3"),
                         textInput("lcolor2b","Color (labels)",value = "red3,orange,green3,purple,blue3"),
                         textInput("ncolor2b","Color (lines)",value = "darkgray"),
                         textInput("xparty2b","Party",value = "1_Solid R,2_Toss-Up,3_Solid D"),
                         textInput("vlimitb","Vote Limit (1000s)",value = "0.1,0.5,1,2"),
                         textInput("vshapeb","Vote Shape",value = "1,16,17,15"),
                         textInput("vrange2b","Vote Point Range",value = "1,4"),
                         textInput("vtrans2b","Vote Transform",value = "#log10"),
                         textInput("vbreaks2b","Vote Breaks", value = ""),
                         textInput("plusnoteb","Add to title",value = ""),
                         splitLayout(
                             numericInput("plotload2b", "Load", 1),
                             actionButton("plotsave2b", "Save")
                         )
                     ),
                     mainPanel(
                         width = 9,
                         plotOutput("areaPlot2b")
                     )
            ),
            tabPanel("Usage",
                     htmlOutput(outputId = "myUsage")
            )
        ),
        mainPanel(
            
        )
    )
))