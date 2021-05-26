library(leaflet)

shinyUI(pageWithSidebar(
    headerPanel("Analysis of Reported Voting Areas"),
    sidebarPanel(
        width = 2,
        selectInput("state2", "STATE",
                    choices = c("FL","ME","WI"),
                    selected = "WI",
                    multiple = FALSE),
        selectInput("races", "RACE",
                    choices = c("WI_2020_President",
                                "WI_2016_President",
                                "WI_2016_President_Recount"),
                    selected = "WI_2020_President",
                    multiple = FALSE),
        selectInput("xcounty", "COUNTY",
                    choices = c(""),
                    selected = "",
                    multiple = FALSE),
        textInput("xarea", "AREA", value = ""),
        selectInput("sortcounty", "Sort Counties",
                    choices = c("COUNTY","AREAS","VOTES","deltaM","deltaMxV","totalM","votesM"),
                    selected = "COUNTY",
                    multiple = FALSE),
        radioButtons("sortcountydir", NULL, c("Ascending","Desc"), "Ascending", inline = TRUE),
        numericInput("xsortcol", "Sort Areas (column)", 3),
        radioButtons("xsortdir", NULL, c("Ascending","Desc"), "Ascending", inline = TRUE),
        numericInput("minvotes","Min Votes",0,min = 0),
        textInput("incl_cand", "Include candidates", value = "1,2,3,4,5"),
        textInput("skip_rows", "Skip rows", value = ""),
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
                    textInput("xlimit","Limit",value = "-9,-3,3,9"),
                    textInput("areaColor","Color",value = "red3,orange,green3,violet,blue3"),
                    textInput("xparty","Party",value = "1_Solid R,2_Leans R,3_Toss-Up,4_Leans D,5_Solid D"),
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
                         checkboxInput("plotbyarea","Plot by Area",value = FALSE),
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
            tabPanel("Usage",
                     htmlOutput(outputId = "myUsage")
            )
        ),
        mainPanel(
            
        )
    )
))