#enableBookmarking(store = "url")

shinyUI(fluidPage(
  titlePanel("Doenet Data Analyzer"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      actionButton("update", "Update Data", icon = icon("sync")),
      #bookmarkButton(),
      downloadButton('downloadData', 'Download Data')
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        
        tabPanel("Histogram by Problem", plotOutput("hist_prob")),
        tabPanel("Histogram of Total Scores", plotOutput("hist_total")),
        tabPanel("Time Plot", plotOutput("time_plot")),
        tabPanel(
          "Brief Summary",
          textOutput("num_students"),
          textOutput("num_pages"),
          textOutput("num_doenetIds")
        ),
        tabPanel("Summary Data", dataTableOutput("summary")),
        tabPanel("Raw Data", dataTableOutput("raw")),
        tabPanel("Cleaned Data", dataTableOutput("cleaned_data"))
        
      )
    )
  )
))
