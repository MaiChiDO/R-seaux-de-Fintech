library(shinydashboard)

dashboardPage(
    
    dashboardHeader(title = "Fintech Network"),
    
    dashboardSidebar(
        dateRangeInput(inputId = "id_daterange", 
                       label = "Select a date range:",
                       min = "2005-10-11", max = "2021-05-10",
                       start = "2015-01-01", end = "2015-12-31", 
                       format = "yyyy-mm-dd",language = "en", 
                       separator = " to "),
        radioButtons(inputId = "id_radio", label = "Select one:",
                     choices =c("Degree (In and Out)" = "degree.all", 
                                "Indegree" = "indegree", 
                                "Outdegree" = "outdegree",
                                "Weighted degree" = "weighted.degree"),
                     selected = "degree.all"),
        textInput(inputId = "id_txt", label = "Enter company/investor's name(s):", value = ""),
        selectInput("Focus", "Focus on node :", selected = "",
                       choices = ((create_objnetwork(filter_data("2015-01-01","2015-12-31"))%>% activate(nodes) %>% as_tibble())$label)),
        column(12,align = "center",offset = 0,downloadButton("downloadData", "Download",
                       style="background-color:#2d9cc2; border-color:#2d9cc2;color:white"))
    ),
    
    dashboardBody(
        tags$head(tags$style(HTML('
         .skin-blue .left-side, .skin-blue .wrapper {
                        background-color: #ecf0f5;
                        }
         '))),
        fluidRow(
            valueBoxOutput("company"),
            valueBoxOutput("investor"),
            valueBoxOutput("round")
        ),
        tabBox(width = 12,
            tabPanel("Map",visNetworkOutput("network")),
            tabPanel("Table",dataTableOutput("table"))
        )
    )
    
)
