
ui <- shiny::fluidPage(
  # includeCSS("www/custom.css"),
  
  # prep for ofqual logo in tabbar ----
  list(tags$head(HTML('<link rel="icon", href="sqa.org.uk", type="image/jpg" />'))),
  div(style="padding: 1px 0px; width: '100%'", titlePanel(title="", windowTitle="Attainment")),
  
  navbarPage(
    # insert ofqual logo in tabbar ----
    title = div(img(src="sqa.jpg", height = 40), strong("Summer attainment")),
    
    # Item tab ----
    tabPanel(
      "Plots and tables",
      
      sidebarLayout(
        sidebarPanel(
          helpText(strong("Use the menus below to plot attainment over time:")),
          radioButtons("qlevel", 
                       "Select a qualification:", 
                       choices = dat %>% dplyr::distinct(Qualification) %>% dplyr::pull(Qualification)),
          selectInput("subject", 
                      "Select a subject:", 
                      choices = init_subjects,
                      selected = "All subjects"),
          radioButtons("ptype",
                       "Select a plot type:",
                       choices = c("Attainment over time", "Attainment distribution")),
          conditionalPanel(
            condition = 'input.ptype == "Attainment over time"',
            radioButtons("threshold", 
                         "Select a grade threshold:", 
                         choices = c("All", dat %>% dplyr::distinct(`Grade threshold`) %>% dplyr::pull(`Grade threshold`)),
                         selected = "All")
          ),
          ), # end of sidebar panel
        
        mainPanel(
          uiOutput("plot")
          ) # end of main panel
        ) # end of sidebarLayout
      ), # end of plots tabpanel
    
    tabPanel(
      "Interrogate the data",
      sidebarLayout(
        sidebarPanel(
          helpText("Build a query below to filter subjects of interest based on comparisons of attainment data from 2019 to 2021."),
          selectInput(
            inputId = "int_grade", 
            label = "Percentage of candidates awarded grade ", 
            choices = list("A", "B", "C", "D", "U")
          ),
          selectInput(
            "int_direction",
            "was ",
            choices = list("higher", "lower")
          ),
          selectInput(
            "int_year1",
            "in ",
            choices = list(2019, 2020, 2021)
          ),
          selectInput(
            "int_year2",
            "than in ",
            choices = list(2019, 2020, 2021)
          )
        ), # end of sidebar panel
        
        mainPanel(
          textOutput("int_report"),
          hr(),
          DT::DTOutput("int_table"),
          br()
        ) # end of main panel
      ) # end of sidebarLayout
    ), # end of interrogate tabpanel
    
    tabPanel(
      "Questions and answers",
      navlistPanel(
        tabPanel("How should I use this app?", includeMarkdown("www/howto.md")),
        tabPanel("What do the graphs and tables show?", includeMarkdown("www/graphs.md")),
        tabPanel("Where do the data come from?", includeMarkdown("www/data.md")),
        tabPanel("Why are there generally more high grades in 2020 and 2021?", includeMarkdown("www/covid.md")),
        tabPanel("I have a query. Who can help?", includeMarkdown("www/query.md"))
      )
    ), # end of q&a tabpanel
  ) # end of navbar
) # end of fluidpage
