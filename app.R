#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
fr <- read_csv("freedom.csv")
# Define UI for application that draws a histogram
ui <- navbarPage("Freedom in the World",
             tabPanel("About Data", 
                      uiOutput("page1")
             ),
             
             tabPanel("Plot",
                      uiOutput("page2")
             ),
             
             tabPanel("Table",
                      uiOutput("page3"))
             
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot1 <- renderPlot({
    if (input$graph == 1) {
    fr %>% filter (year == input$yr) %>% filter (status == "F") %>% 
    ggplot(aes(x = pr, y = cl)) + geom_point() + 
    geom_smooth(se = FALSE, method = "lm") + ggtitle ("Political Rights vs. Civil Liberty") +
    labs(x = "Political Rights Index Score", y = "Civil Liberty Index Score")
    } else if(input$graph == 2) {
      fr %>% filter (year == input$yr) %>% filter(status == "F") %>% 
        ggplot(aes(x = pr)) + geom_histogram() + 
        ggtitle ("Political Rights Count") +
        labs(x = "Political Rights Index Score", y = "counts")
    } else if(input$graph == 3){
      fr %>% filter (year == input$yr) %>% filter(status == "F") %>% 
        ggplot(aes(x = cl)) + geom_histogram() + 
        ggtitle ("Civil Liberty Count") +
        labs(x = "Civil Liberty Index Score", y = "counts")
    } else {
      fr %>% filter (year == input$yr) %>% filter(status == "F") %>% 
      ggplot(aes(x = total, y = gdp_pc)) + geom_point() + 
      geom_smooth(se = FALSE, method = "lm") + ggtitle ("Freedom Index Score vs. GDP per capita") +
      labs(x = "Freedom Index Score", y = "GDP per Capita")
    }
  })
  
  output$table <- renderTable({
      fr %>% 
      filter(year == input$time) %>% 
      filter(status == "F") %>% 
      group_by(country_territory) %>% 
      summarize("GDP per capita over Freedom" = gdp_pc / total,
                Index_of_Political_Freedom = total,
                "Index of Political Rights" = pr,
                "Index of Liberty Cvil" = cl) %>% 
    arrange(desc(Index_of_Political_Freedom))
  })
  
  output$table2 <- renderTable({
    fr %>% 
      filter(year == 2021) %>% 
      filter(status == "F") %>% 
      arrange(desc(total)) %>%
      select(country_territory, year, pr, cl, total) %>%
      head(10) %>% 
      rename("Country" = "country_territory", 
             "Year" = "year",
             "Political Rigts Index Score" = "pr",
             "Civil Liberty Index Score" = "cl",
             "Freedom Index Score" = "total") %>% tibble()
  })
  output$text1 <- renderText({
    if(input$graph == 1) {
      print("Based on the graphs from 2005 - 2021, it seems like that the political rights and civil 
        liberty of those free countries tend to have linear relationship, which as the index score
        of political rights gets higher, the index score of civil rights also gets higher. And also
        as the index score of political rights gets lower, the index score of civil rights also 
        gets lower.")
    } else if(input$graph == 2) {
      print("Based on the histograms from 2005 - 2021 of political rights count, the free countries
            have the index score of political rights mostly higher than 25.")
    } else if(input$graph == 3) {
      print("Based on the histograms from 2005 - 2021 of civil liberty count, most of the free 
            countries have the index score of civil liberty higher than 40.")
    } else {
      print("Based on the graphs from 2005 - 2021, although there do exist some outliers in the graph,
            it seems most of the countries follow the pattern that as the index score of political 
            freedom, the higher that the GDP per capita will be.")
    }
  })
  output$text2 <- renderPrint({
    temp <- fr %>% 
      filter(year == input$time) %>% 
      filter(status == "F") %>% 
      arrange(desc(total))
    p("The mean Index Score of Political Freedom for free countries this year is", mean(temp$total), 
      "The country that has the highest index of Political Freedom among free countries this year is", 
      head(temp, 1)$country_territory, 
      "The country that has the lowesr index of political Freedom among free countries this year is", 
      tail(temp, 1)$country_territory)
  })


output$page1 <- renderUI({
  mainPanel(
  h2("Background:"),
  h4("Political Freedom is described as freedom that comes from oppression or coercion, no
    disabling conditions for any citizens and fulfill what the citizens want, or without bad
    living conditions. It is also related to a country's political rigts and civil liberty. And some
    social scientist nowadays trying to find out whether or not a county's economics have some 
    relation with its political freedom"),
  hr(),
  h2("Research Question:"),
  h4("The relationship between civil rights and political rights in free country"),
  h4("The relationship between political Freedom and gdp per capita in free country."),
  hr(),
  h2("Values:"),
  h4("country_teritory: name of each country"),
  h4("year = the year that recorded the data of that row"),
  h4("pr = the index score of the country's political rights"),
  h4("cl = the index score of the country's civil liberty"),
  h4("total = the index score of the country's political freedom"),
  h4("gdp_pc = GDP per capita of the country"),
  hr(),
  h2("Sample data"),
  tableOutput("table2")
  )
})  
  

output$page2 <- renderUI({
  sidebarLayout(
    sidebarPanel(
      radioButtons("graph", "What grpah?", 
                 list("Political Rights vs. Civil Liberty" = 1,
                      "Freedom vs. GDP per capita" = 4,
                      "Political Rights" = 2,
                      "Civil Liberty" = 3)),
      
  
     fluidRow(
     column(6, sliderInput("yr", "Which Year:",
                  min = 2005,
                  max = 2021,
                  value = 1),),
    
     )
   ),
   mainPanel(
     plotOutput("plot1"),
     textOutput("text1")
   )
  )
})

output$page3 <- renderUI({
  sidebarLayout(
    sidebarPanel(
      sliderInput("time", "Which Year？",
                  min = 2005, 
                  max = 2021, 
                  value = 1)
    ),
    mainPanel(
      verbatimTextOutput("text2"),
      tableOutput("table")
    )
  )
})
}


# Run the application 
shinyApp(ui = ui, server = server)
