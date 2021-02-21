# LIBEK ----
library(rhandsontable)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)
library(DT)

# BEOLVAS ----
datafile<-read.csv("data.csv", header=TRUE, sep=",", quote="")

ui <- dashboardPage( skin = "green",
                     
                     #header                        
                     dashboardHeader(title = "Advanced Tables"), #dashboardHeader()
                     
                     dashboardSidebar(
                       
                       sidebarMenu(
                         # Setting id makes input$tabs give the tabName of currently-selected tab
                         id = "tabs",
                          
                         menuItem("Table", icon = icon("list"), tabName = "dataset"),
                         
                         menuItem("Edit table", tabName = "edit_table", icon = icon("filter")) 
                         
                          ) #sidebarMenu()
                       
                     ), #dashboardSidebar()
                     
                     #body      
                     dashboardBody(
                       
                       
                       tabItems(


                         tabItem("edit_table",
                                 div(h1("Edit values"),
                                     
                                     sidebarLayout(
                                       sidebarPanel(
                                         actionButton("runButton","Change Dataframes")
                                       ),
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("OldData", rHandsontableOutput('OldDatafile')),
                                           tabPanel("NewData", DT::dataTableOutput("NewDatafile"))
                                         )))     
                                     
                                     
                                 ), #div(p())
                                 
                         ), #tabItem(dashboard)
                         

                         
                         tabItem("dataset",
                                 fluidPage(
                                   
                                   fluidRow(
                                     
                                     h1("data.csv dataset overview"), #h1
                                     
                                     sidebarPanel(
                                       textInput("a.oszlop", label="column a", value=""),
                                       textInput("b.oszlop", label="column b", value=""),
                                       textInput("c.oszlop", label="column c", value=""),
                                       actionButton("addButton", "insert")
                                     ),
                                     
                                     dataTableOutput("table")
                                     
                                   )
                                   
                                 ) #fluidPage()
                                 
                         ) #dataset
                       ) #tabItems()
                       
                       
                     ) #dashboardBody()
                     
                     
) #dashboardPage()

#server
server <- function(input, output) {
  
  #making changes
  
  values <- reactiveValues()
  output$OldDatafile <- renderRHandsontable({
    rhandsontable(datafile)
  })
  
  observeEvent(input$runButton, {
    values$data <-  hot_to_r(input$OldDatafile)
  })
  
  output$NewDatafile <- DT::renderDataTable({
    values$data
  })
  
  
  
  
  
  #adding new rows
  
  datafile_sample <- datafile[sample(nrow(datafile)),]
  row.names(datafile_sample) <- NULL
  
  values <- reactiveValues()
  values$df <- datafile_sample
  addData <- observe({
    
    if(input$addButton > 0) {
      newLine <- isolate(c(input$a.oszlop, input$b.oszlop, input$c.oszlop))
      isolate(values$df <- rbind(as.matrix(values$df), unlist(newLine)))
      write.csv(values$df, file = "data.csv", row.names=F, quote=F)
    }
  })
  
  
  output$table <- renderDataTable({values$df}, rownames=FALSE)
  
}



#shinyapp function call ----

shinyApp(ui, server)

