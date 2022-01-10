library(shiny)
library(tidyverse)

##### Import Data

data = read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
data = data %>% select(c("pid7","ideo5"))
data = drop_na(data)

##### UI

ui = fluidPage(
  sliderInput(inputId = 'ideo',
              label = 'Select Five Point Ideology (1=Very liberal, 5=Very conservative))', 
              min = 1, max = 5, value = 3, step = 1),
  plotOutput('plot')
)

##### Server

server = function(input, output){
  output$plot = renderPlot({
    ggplot(data %>% filter(ideo5 == input$ideo), aes(x = pid7)) +
      geom_bar(stat = 'count') +
      xlab('7 Point Party ID, 1 = very D, 7 = very R') +
      ylab('Count')
  })
}

##### Run app

shinyApp(ui,server)