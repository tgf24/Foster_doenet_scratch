library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)

shinyServer(function(input, output) {
  source("./functions.R")
  
  # I SHOULD USE COMMENTS!!!!!
  
  
  # What this code is doing is pulling in the data
  # getQueryString() is a function that takes a query string and turns it into
  # a list, which allows us to find the "data" item of that list.
  # By default it pulls the query string from the app environment (?)
  # renderText turns it that list into a string and then checks if it is null
  # This is a check to make sure we are in fact looking for data that exists

  #Stream_in unpacks the json file we get from the URL into a 1 by 3 dataframe 
  #First element is a boolean that tells if it was successful or not
  #Second element is a message (typically empty right now)
  #Third is the list that contains the event log
  #df contains this 1 by 3 frame at the end of this block
  if (!is.null(renderText(getQueryString()[["data"]]))) {
    df <- eventReactive(input$update, {
      stream_in(file(
        paste0(
          "https://www.doenet.org/api/getEventData.php?doenetId[]=",
          getQueryString()[["data"]]
        )
      ))
    })
    
    # get set dataset (for testing)
    # df <- eventReactive(input$update, {
    #   stream_in(
    #     file(
    #       "https://www.doenet.org/api/getEventData.php?doenetId[]=_OQEiWXV5CaSTOp-1UomjR"
    #     )
    #   )
    # })
    
    #This block pulls out the events log, which is a dataframe, within a 
    # 1 element list within a 1 by 3 dataframe. So df is the frame,
    # events is an named column of df, which contains one element, which is a 
    # dataframe containing the events log, which we are then assigning to a local
    # variable called events. Note the difference between the events column of df
    # and our local events object (even though they are essentially the same data)
    
    events <- reactive({
      df()$events[[1]]
    })
    
  
    # Takes our events and cleans them up and adds some helpful columns
    # See file functions.R for more information.
    cleaned <- reactive({
      clean_events(events())
    })
    
    # creates a table of cleaned data
    output$cleaned_data <- renderDataTable(cleaned())
    
    # creates a table of raw data
    output$raw <- renderDataTable(events())
    
    #creates an output text detailing how many students in the data set
    output$num_students <-
      renderText(paste0(
        "There is/are ",
        n_distinct(events()$userId, na.rm = TRUE),
        " student(s)"
      ))
    
    #creates an output text detailing how many different doenet experiments 
    #are represented in this set.
    output$num_doenetIds <-
      renderText(paste0(
        "There is/are ",
        n_distinct(events()$doenetId, na.rm = TRUE),
        " doenetId(s)"
      ))
    #creates an output text detailing how many pages are included in this dataset
    output$num_pages <-
      renderText(paste0(
        "There is/are ",
        n_distinct(summary_data()$pageNumber, na.rm = TRUE),
        " page(s)"
      ))
    
    
    summary_data <- reactive({
      cleaned() %>%
        select(userId, starts_with("X"), time, timestamp, pageNumber) %>%
        group_by(userId, pageNumber) %>%
        summarize_all(max, na.rm = T) %>%
        pivot_longer(cols = starts_with("X"),
                     names_to = "problem",
                     values_to = "score") %>%
        ungroup() %>%
        filter(score != -Inf)
    })
    
    # summary data
    output$summary <- renderDataTable(summary_data())
    
    
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('events-', Sys.Date(), '.csv', sep = '')
      },
      content = function(file) {
        write.csv(events(), file)
      }
    )
    
    output$hist_prob <- renderPlot(
      summary_data() %>%
        ggplot(aes(x = score)) +
        geom_histogram() +
        facet_grid(pageNumber ~ problem) +
        labs(x = "Score on Problem", y = "Count", title = "Breakdown by Problem")
    )
    
    output$hist_total <- renderPlot(
      summary_data() %>%
        group_by(userId) %>%
        summarize(total = sum(score)) %>%
        ggplot(aes(x = total)) +
        geom_histogram() +
        labs(x = "Total Points", y = "Number of Students", title = "Total Scores on Assignment")
    )
    
    output$time_plot <- renderPlot({
    cleaned() %>%
      filter(!is.na(itemCreditAchieved)) %>% 
      ggplot(aes(y = itemCreditAchieved, x = time, color=userId))+
      geom_line()+
      theme(legend.position = "none")+
      facet_wrap(~pageNumber)+
      labs(x = "Time", y = "Total Credit on Page")
    })
  }
})
