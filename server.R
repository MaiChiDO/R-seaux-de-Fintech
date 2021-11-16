library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    round <- reactive(filter_data(input$id_daterange[1],input$id_daterange[2]))
    object_network <- reactive(create_objnetwork(round()))
    nodes <- reactive(object_network() %>% activate(nodes) %>% as_tibble())
    links <- reactive(object_network() %>% activate(edges) %>% as_tibble())

    ### Map ------------------------------------------------------------------
    observe({
        output$network <- renderVisNetwork({
                visu_network(nodes(),links(),input$id_radio,input$id_txt)
        })
    })

    observe({
        updateSelectInput(session, "Focus", "Focus on node :", selected = "",
                       choices = nodes()$label)
    })

    observe({
        if (!is.null(input$Focus)){
            isolate({
                visNetworkProxy("network") %>%
                    visFocus(as.character(id_chosen(nodes(),input$Focus)), scale = 3)
            })
        }
    })

    ### Table --------------------------------------------------------------------
    output$table <- renderDataTable(nodes(), options = list(pageLength = 5, autoWidth = TRUE,selection = 'single'))

    ### Value box ----------------------------------------------------------------
    nb_comp <- reactive(value_box(nodes(),"Company"))
    output$company <- renderValueBox({
        valueBox(
            nb_comp(), "Companies", icon = icon("building"),
            color = "blue"
        )
    })

    nb_inv <- reactive(value_box(nodes(),"Investor"))
    output$investor <- renderValueBox({
        valueBox(
            nb_inv(), "Investors", icon = icon("hand-holding-usd"),
            color = "purple"
        )
    })
    
    nb_round <- reactive(round() %>% filter(!is.na(Investors)) %>% nrow())
    output$round <- renderValueBox({
        valueBox(
            nb_round(), "Rounds", icon = icon("handshake"),
            color = "yellow"
        )
    })
    
    ### Download csv file -------------------------------------------------------
    output$downloadData <- downloadHandler(
        filename = "nodes.csv",
        content = function(file) {
            write.csv(nodes(), file, row.names = FALSE)
        }
    )

})
