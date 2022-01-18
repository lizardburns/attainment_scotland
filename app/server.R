
server <- function(input, output, session) {
  # plots tab ----
  dat_lev <- shiny::reactive(
    dat %>% dplyr::filter(Qualification == input$qlevel)
  )
  
  shiny::observeEvent(input$qlevel, {
    req(dat_lev())
    
    subj_choices <- dat_lev() %>% 
      dplyr::filter(Subject != "All subjects") %>% 
      dplyr::distinct(Subject) %>% 
      dplyr::arrange(Subject) %>% 
      dplyr::pull(Subject) %>% 
      c("All subjects", .)
      
    shiny::updateSelectInput(session, 
                             "subject", 
                             choices = subj_choices, 
                             selected = "All subjects")
  })
  
  dat_ts <- shiny::reactive({
    req(input$qlevel, input$subject, input$threshold)
    
    dat_lev() %>% 
      filter_summary(qlevel = input$qlevel, 
                     subj = input$subject, 
                     threshold = input$threshold)
  })
  
  dat_dist <- shiny::reactive( calculate_grade_dist(dat_lev(), input$subject) )
  
  output$plot <- shiny::renderUI({
    if(input$ptype == "Attainment over time") {
      tagList(
        shiny::fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel("Plot", plotly::plotlyOutput("ts_plot")),
            tabPanel("Table", fluidRow(br(), DT::DTOutput("ts_table")) )
          )
        ),
        shiny::fluidRow(
          shiny::helpText("Source: Scottish Qualifications Authority")
        )
      )
    } else {
      shiny::tagList(
        shiny::fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel("Plot", shiny::plotOutput("dist_plot")),
            tabPanel("Table", fluidRow(br(), DT::DTOutput("dist_table")) )
          )
        ),
        shiny::fluidRow(
          shiny::helpText("Source: Scottish Qualifications Authority")
        )
      )
    }
  })
  
  output$ts_plot <- plotly::renderPlotly( 
    ts_plot(dat_ts(), input$threshold) 
    )
  output$dist_plot <- shiny::renderPlot( 
    dist_plot(dat_dist()) 
    )
  
  output$ts_table <- DT::renderDT( 
    ts_table(dat_ts(), threshold = input$threshold) 
    )
  output$dist_table <- DT::renderDT( 
    dist_table(dat_dist()) 
    )
  
  # interrogate tab ----
  observeEvent(input$int_year1, {
    updateSelectInput(session, 
                      "int_year2", 
                      choices = setdiff(2019:2021, input$int_year1))
  })
  
  filt_int <- reactive({
    if(input$int_direction == "lower") {
      dat_int %>% 
        filter(
          Grade == input$int_grade,
          .data[[input$int_year1]] < .data[[input$int_year2]]
        )
    } else {
      dat_int %>% 
        filter(
          Grade == input$int_grade,
          .data[[input$int_year1]] > .data[[input$int_year2]]
        )
    }
  })
  
  output$int_report <- renderText(
    paste(
      "The table below lists the qualifications where the percentage of candidates awarded grade",
      input$int_grade,
      "was",
      input$int_direction,
      "in",
      input$int_year1,
      "than in",
      input$int_year2
      )
  )
  
  output$int_table <- DT::renderDT({
    export_name <- paste(
      "interrogate", 
      input$int_grade,
      input$int_year1,
      input$int_direction,
      "than",
      input$int_year2,
      sep = "_"
    )
    
    DT::datatable(
      filt_int() %>% 
        dplyr::mutate(
          dplyr::across(
            c(`2019`, `2020`, `2021`), 
            ~scales::percent(., accuracy = .1)
            ),
          dplyr::across(c(Qualification, Subject), factor)
          ),
      rownames = FALSE,
      filter = "top",
      extensions = 'Buttons',
      # container = sketch,
      options = list(
        dom = 'Btp',
        pageLength = 10,
        # columnDefs = list(list(className = 'dt-right', targets = 1:8)),
        buttons = list(
          list(extend = 'csv', 
               filename = export_name, 
               text = 'Download CSV'),
          list(extend = 'excel', 
               title = NULL, 
               filename = export_name, 
               text = 'Download XLSX')
        ) # end of button list
      ) # end of options
    )
  })
  
  # Q&A tab ----
  output$excluded <- renderTable(
    exc
  )
}
