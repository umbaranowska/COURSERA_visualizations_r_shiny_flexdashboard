library(shiny)
library(tidyverse)
library(plotly)
library(DT)

##### Import Data

data = read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
data = data %>% select(c("pid7","ideo5","newsint","gender","educ","CC18_308a","region"))
data = drop_na(data)

##### UI

ui = navbarPage("My application",
                
                tabPanel('Page 1',
                         sidebarPanel(
                           sliderInput(inputId = 'ideo',
                                       label = 'Select Five Point Ideology (1=Very liberal, 5=Very conservative))', 
                                       min = 1, max = 5, value = 3, step = 1)
                         ),
                         
                         mainPanel(
                           tabsetPanel(type = 'tabs',
                                       tabPanel('Tab1',
                                                plotOutput('plot_page1_tab1')),
                                       tabPanel('Tab2',
                                                plotOutput('plot_page1_tab2'))
                           )
                         )),
                
                tabPanel('Page 2',
                         sidebarPanel(
                           checkboxGroupInput(inputId = 'gender',
                                              label = 'Select Gender',
                                              choices = list(1,2))
                         ),
                         
                         mainPanel(
                           plotlyOutput('plot_page2')
                           )
                         
                         ),
                
                tabPanel('Page 3',
                         sidebarPanel(
                           selectInput(inputId = 'region',
                                       label = 'Select Region',
                                       choices = c('1' = 1,
                                                   '2' = 2,
                                                   '3' = 3,
                                                   '4' = 4),
                                       multiple = TRUE)
                         ),
                         
                         mainPanel(
                           dataTableOutput('table_page3')
                         )
                         )
                
                )

##### Server

server = function(input, output){
  
  output$plot_page1_tab1 = renderPlot({
    ggplot(data %>% filter(ideo5 == input$ideo), aes(x = pid7)) +
      geom_bar(stat = 'count') +
      xlab('7 Point Party ID, 1 = very D, 7 = very R') +
      ylab('Count')
    })
    
  output$plot_page1_tab2 = renderPlot({
    ggplot(data %>% filter(ideo5 == input$ideo), aes(x = CC18_308a)) +
      geom_bar(stat = 'count') +
      xlab('Trump Support') +
      ylab('Count')
  })
  
  output$plot_page2 = renderPlotly({
    plotly::ggplotly(
    ggplot(data %>% filter(gender %in% input$gender), aes(x = educ, y = pid7)) +
      geom_point(position = 'jitter') +
      geom_smooth(method = lm)
    )
  })
  
  output$table_page3 = renderDataTable({
    data %>% filter(region %in% as.numeric(input$region))
  })
  
}

##### Run app

shinyApp(ui,server)