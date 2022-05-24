library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)

shinyServer(function(input, output) {
  source("./functions.R")
  
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
    
    events <- reactive({
      df()$events[[1]]
    })
    
    
    cleaned <- reactive({
      clean_events(events())
    })
    
    # cleaned data
    output$cleaned_data <- renderDataTable(cleaned())
    
    # raw data
    output$raw <- renderDataTable(events())
    
    output$num_students <-
      renderText(paste0(
        "There is/are ",
        n_distinct(events()$userId, na.rm = TRUE),
        " student(s)"
      ))
    
    output$num_doenetIds <-
      renderText(paste0(
        "There is/are ",
        n_distinct(events()$doenetId, na.rm = TRUE),
        " doenetId(s)"
      ))
    
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
