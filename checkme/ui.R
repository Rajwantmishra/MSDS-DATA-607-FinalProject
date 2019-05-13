#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# install.packages("shinyjs")
library(shiny)
# library(DT)
library(shinyjs)
library(wordcloud)
library(tidyverse)
library(stringr)
library(tm)

## ui.R ##
htmlTemplate("www/bd/index.html",
             
            
             
             rowf = fluidRow(
               column(4, wellPanel(
                 fluidRow(column(8,  h2("Probelm"))),
                 # sliderInput("n", "N:", min = 10, max = 1000, value = 200,
                 #             step = 10),
                 textInput("Symptoms_text", "What is your problem :", "I have ear infection."),
                 textInput("BodyName", "Select Body Part:", "head"),
                 
                 # selectInput("in_Section", label = "Select Body Parts:",
                 #             choices = c("Head" ,
                 #                         "Shoulder",
                 #                         "Arm",
                 #                         "Cheast",
                 #                         "Stomach",
                 #                         "Legs",
                 #                         "Hands"), selected = "Head"),
                 submitButton("Submit"),
                 fluidRow(column(12, plotOutput("wordsym_plot")))
                 # plotOutput("wordsym_plot")
               )),
               column(8,
                      # plotOutput("plot1", width = 400, height = 300),
                      # DT::dataTableOutput("plotDT"),
                      
                      fluidRow(column(8,  h2("Result"))),
                      fluidRow(column(12,h4('Only Drug supported: biaxin,lamictal, depakene, sarafem. Terms used:'),verbatimTextOutput("wordsym_Model_plot"))),
                      fluidRow(column(12,HTML("<span class='label label-danger'>Question</span>"),
                                      h3(textOutput("Symptoms")))),
                      
                     
                      fluidRow(column(12, HTML("<span class='label label-success'>Prediction</span>"),
                                      h3(textOutput("result")))),
                      
                      fluidRow(column(12, HTML("<span class='label label-info'>Projected Condition</span>"),
                                    textOutput("Condition"))),
                      
                      fluidRow(column(12, HTML("<span class='label label-info'>Possible Symptoms</span>"),
                                      htmlOutput("readMore"),textOutput("Overview"))),
                      
                      HTML(paste("<div id='accordion'>
  <div class='card'>
                                 <div class='card-header' id='headingOne'>
                                 <h5 class='mb-0'>
                                 <button class='btn btn-link' data-toggle='collapse' data-target='#collapseOne' aria-expanded='true' aria-controls='collapseOne'>
                                 Model Output
                                 </button>
                                   </h5>
                                   </div>
                                   
                                   <div id='collapseOne' class='collapse show' aria-labelledby='headingOne' data-parent='#accordion'>
                                   <div class='card-body'>",verbatimTextOutput("fullResult_plot"),"</div>
                                   </div>
                                   </div></div>"))
                     
                      
                      # withTags({
                      #   div(class="header", checked=NA,
                      #       p("Ready to take the Shiny tutorial? If so"),
                      #       a(href="shiny.rstudio.com/tutorial", "Click Here!")
                      #   )
                      # }),
                      # 
                      # withTags({
                      #   div(class="headersym", checked=NA,
                      #       verbatimTextOutput("result")
                      #   )
                      # }),
                      
                      
                      # fluidRow(column(12, verbatimTextOutput("fullResult_plot")))
                      
                      # verbatimTextOutput("text") 
                      
               )
              
             )
             # textInput("searchTerm", "Search on NYT", "science")
)

