
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Scraping SME"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
#       textInput("keyword","keywords for search","incendie accidentel entreprise"),
      
      textInput("name","name of firm",""),
      
      selectInput("tab","Which tab ?",choices = c("compteresultat","actif","passif"),multiple = F,selected = ""),
      selectInput("port","which port should be used ?",sort(c(80,8080))),
      uiOutput("years"),
      uiOutput("var"),
      uiOutput("custom"),
      uiOutput("firm"),
      submitButton("Submit")
      

    ),

    # Show a plot of the generated distribution
    mainPanel(
#       textOutput("content"),
      plotOutput("res"),
      tableOutput("tab")
    )
  )
))
