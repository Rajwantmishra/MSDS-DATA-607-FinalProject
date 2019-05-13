#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
# library(DT)
# install.packages("gridExtra")
library(mongolite)
library(lubridate)
library(gridExtra)
library(shinyjs)
library(wordcloud)
library(tidyverse)
library(e1071)

library(stringr)
library(tm)
library(gmodels)

function(input, output, session) {
  # dtquestion <- input$Symptoms_text
  outText <- "BIN "
  dtquestion <- reactiveVal("")
  dtAnswer <- reactiveVal("")
  dtTerm <- reactiveVal("")
  dtModelTerm <- reactiveVal("")
  mongo_run <- TRUE
  # Here you read the URL parameter from session$clientData$url_search
  observe({
    # shinyjs::disable("in_Section")
    # shinyjs::disable("text")
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['bins']])) {
      outText  = query[['bins']]
      output$text <- renderText({
        paste("Input text is:", outText,input$text ,  input$in_Section, input$BodyName, input$data,session$clientData)
      })
    }
  })
  
  
  ## To make sure our JS-created textInput works:
  output$txt2Test <- renderText({ input$txt2 })
  
  # output$plot1 <- renderPlot({
  #   hist(rnorm(input$n))
  # })
  
  output$text <- renderText({
    paste("Input text is:", input$text , input$BodyName, input$in_Section,  input$data)
  })
  
  
  


#----------------------------------------------------------------------------------#

#--------------------------Model Fetch And Apply Logic-----------------------------#

#----------------------------------------------------------------------------------#

  # Define a reactive expression for the Symptoms predication

  # Start Predict Start
#------------------------------------------------    
    startPredict <- reactive({
      
      withProgress({
        setProgress(message = "Processing Health Data...")
        incProgress(0.1, detail = paste("Processing Symptoms"))
      
         # dtquestion <- paste(input$BodyName,input$Symptoms_text ,";" ,"Example: I have sinus infection.")
         dtquestion <- paste(input$BodyName,",",input$Symptoms_text)
         # dtquestion <- paste("  I am in depression")
        dtquestion(dtquestion)
        # print(c("DATA READ ", dtquestion))
        # dtquestion <- "depre; aire ; weer;infection ; dipression; sinus"
  
## Code for Recom. Start
#---------------------------Mongo Download        
#-----------------------------------

       
        if(mongo_run){ 
          #Connect winth Mongo
          fs_model <- gridfs(db = "MSDSProject5", url = "mongodb+srv://msds_user:msds@cluster0-bqyhe.gcp.mongodb.net/", prefix = "MOD",options = ssl_options())
          # create connection, database and collection
          Mongo_Train_Cond_Corpus = mongo(collection = "Nav_TrainC_Cond", db = "MSDSProject5", url = "mongodb+srv://msds_user:msds@cluster0-bqyhe.gcp.mongodb.net/") 
          
          # Store Question
          Mongo_Query = mongo(collection = "Predict_Query", db = "MSDSProject5", url = "mongodb+srv://msds_user:msds@cluster0-bqyhe.gcp.mongodb.net/") 
          
          
          # my_collection$insert(book)
          Mongo_Train_Cond_Corpus$count()
          Mongo_Train_Cond_Corpus$index()
          # print("Next CODE")
          # Read Freq words from Mongo 
          Mongo_Read_Freq <- Mongo_Train_Cond_Corpus$find()
          saveRDS(Mongo_Read_Freq,"www/Shiny_mongo/freq.rds")
         
          # DT::datatable(Mongo_Read_Freq)
          # Prepare for Test
          # Samed Model using saveRDS to Mongo
          # Remove Old Model and Upload Model to MOngo to Mongo
          # Downlaod Model from Mongo 
          fs_model$download("Nav_model.rds","www/Shiny_mongo/MongoNav.rds")
          # load the model
           Shiny_saved_model <- readRDS("www/Shiny_mongo/MongoNav.rds")
        }else{
          # load the model
          Shiny_saved_model <- readRDS("www/Shiny_mongo/MongoNav.rds")
          Mongo_Read_Freq <- readRDS("www/Shiny_mongo/freq.rds")
        }


#-----------------------------------
#---------------------------Mongo Download  End


#------------------------------------------------------------
        incProgress(0.3, detail = paste("Processing Symptoms"))
        

# dtquestion <- data.frame( str_split(c("I am in depression ;I have sinus ; I have throt infection"), pattern = ";"))

dtquestion <- data.frame( str_split(dtquestion, pattern = ";"))

names(dtquestion) <- c("text")


# Mongo_tm_dte_condition <- data.frame(doc_id = test$urlDrugName,text = test$condition)
Mongo_tm_dte_condition_User <- data.frame(doc_id = c("NoDRUG", "NoNEWDRUG",""),text =
                                            c("I am in depression",
                                              "I have sinus",
                                              "I have throt infection"))


# dtquestion <- Mongo_tm_dte_condition_User


Mongo_tm_dte_condition_User <- data.frame(doc_id = c(rep("QUESTION",1,length(dtquestion[,1]))),
                                          text = dtquestion$text )

if(mongo_run){
  Mongo_Query$insert(Mongo_tm_dte_condition_User)
  # Mongo_Query$find()
  # Mongo_Query$remove('{"doc_id":"QUESTION"}')
}
Mongo_tm_dte_condition_User <- add_row(Mongo_tm_dte_condition_User,doc_id ="XXX" , text="Example: I have sinus infection")


Mongo_corpus_condition_test_user <-VCorpus(DataframeSource(Mongo_tm_dte_condition_User))

# Create Document Term Matrix with only Freuest word as per train data  using Mongo_freq_words
as.list(Mongo_Read_Freq)

# Set model term varible 
# matr <- as.matrix(Mongo_Read_Freq$Mongo_freq_words)
dtModelTerm (Mongo_Read_Freq$Mongo_freq_words)

Mongo_dtm.test.nb <- DocumentTermMatrix(Mongo_corpus_condition_test_user, control=list(dictionary = 
                                                                                         Mongo_Read_Freq$Mongo_freq_words,
                                                                                       removePunctuation = 
                                                                                         TRUE,
                                                                                       tolower = TRUE,
                                                                                       stripWhitespace= TRUE,
                                                                                       removeNumbers = TRUE,
                                                                                       stopwords = TRUE))


dim(Mongo_dtm.test.nb)


# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_mongo <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

incProgress(0.5, detail = paste("Processing Symptoms"))
Mongo_testNB <- apply(Mongo_dtm.test.nb, 2, convert_mongo)

# Read Model from Mongo
# Do Predict

#------------------------------------------------------------



# Perform Test
Mongo_Pred_lap <- predict(Shiny_saved_model,Mongo_testNB)



fct_count(Mongo_Pred_lap,sort = TRUE)
predict_Sym_Result <- CrossTable(Mongo_tm_dte_condition_User$text,Mongo_Pred_lap,prop.chisq = FALSE, prop.t = FALSE, dnn = c("Problem","Drug"))

Mongo_tm_dte_condition_User$text[which(Mongo_tm_dte_condition_User$doc_id=="QUESTION")]
str_which(Mongo_tm_dte_condition_User$doc_id,"QUESTION")
length(Mongo_Pred_lap)

Model_Result <- cbind("doc_id"= Mongo_tm_dte_condition_User$doc_id,
                   "text" = Mongo_tm_dte_condition_User$text,
                   as.data.frame(Mongo_Pred_lap))

Model_Result <- Model_Result[which(Model_Result$doc_id=="QUESTION"),c(2,3)]

# CrossTable(Mongo_tm_dte_condition_User$text[which(Mongo_tm_dte_condition_User$doc_id=="QUESTION")],Mongo_Pred_lap,prop.chisq = FALSE, prop.t = FALSE, dnn = c("Problem","Drug"))


dtAnswer( as.character(Model_Result$Mongo_Pred_lap ))

# Make the wordcloud drawing during each check
dtTerm(Mongo_corpus_condition_test_user)
incProgress(0.7, detail = paste("Processing Condition"))
startSideInfo()

      }) # for  withProgress
})

#------------------------------------------------
  #Start Predict End
  
    
  #Start Side Info
#------------------------------------------------
  
  startSideInfo <- reactive({
 
    
    if(mongo_run){ 
      
      # Store Conditon 
      Cond_fs_model <- gridfs(db = "MSDSProject5", url = "mongodb+srv://msds_user:msds@cluster0-bqyhe.gcp.mongodb.net/", prefix = "MOD",options = ssl_options())
      Cond_Mongo_coll = mongo(collection = "Nav_TrainC_Condition", db = "MSDSProject5", url = "mongodb+srv://msds_user:msds@cluster0-bqyhe.gcp.mongodb.net/") 
      
      Cond_Mongo_DCon = mongo(collection = "Nav_TrainC_DCON", db = "MSDSProject5", url = "mongodb+srv://msds_user:msds@cluster0-bqyhe.gcp.mongodb.net/") 
      
      
      
      Cond_fs_model$find()
      Cond_Mongo_coll$count()
      Cond_Mongo_DCon$count()
      
      # Downlaod Model from Mongo 
      Cond_fs_model$download("Nav_model_cond.rds","www/Shiny_mongo/Nav_model_cond.rds")
      # load the model
      Shiny_saved_model_cond <- readRDS("www/Shiny_mongo/Nav_model_cond.rds")
      
      
      # Read Freq words from Mongo 
      Mongo_Read_Freq_cond <- Cond_Mongo_coll$find()
      # saveRDS(as.data.frame(freq_condition),"Shiny_mongo/freq_cond.rds")
      saveRDS(Mongo_Read_Freq_cond,"www/Shiny_mongo/freq_cond.rds")
      Cond_Mongo_DCon$count()
      
      
      
      #store conditon for offline test start
      #saveRDS(Diseases_condition,"Shiny_mongo/dis_cond.rds")
      #Output 
      # Diseases_condition[which(Diseases_condition$Diseases=="breast cancer"),c(1,2)]
      
      
      
      
    }else{
      # load the model
      Shiny_saved_model_cond <- readRDS("www/Shiny_mongo/Nav_model_cond.rds")
      Mongo_Read_Freq_cond <- readRDS("www/Shiny_mongo/freq_cond.rds")
      Diseases_cond <-  readRDS("www/Shiny_mongo/dis_cond.rds")
      # Diseases_condition[which(Diseases_condition$Diseases=="breast cancer"),c(1,2)]
    }
    # dtquestion_cond <- dtquestion
     #dtquestion_cond <- paste("  I am in depression")
    dtquestion_cond <- paste(input$BodyName,input$Symptoms_text)
    dtquestion_cond <- data.frame( str_split(dtquestion_cond, pattern = ";"))
    names(dtquestion_cond) <- c("text")
    Mongo_tm_dte_condition_User_cond <- data.frame(doc_id = c(rep("QUESTION",1,length(dtquestion_cond[,1]))),
                                                   text = dtquestion_cond$text )
    
    Mongo_tm_dte_condition_User_cond <- add_row(Mongo_tm_dte_condition_User_cond,doc_id ="XXX" , text="Example: I have sinus infection acne acid")
    
    Mongo_corpus_condition_test_user_cond <-VCorpus(DataframeSource(Mongo_tm_dte_condition_User_cond))
    Mongo_dtm.test.nb_cond <- DocumentTermMatrix(Mongo_corpus_condition_test_user_cond, control=list(dictionary =
                                                                                                       Mongo_Read_Freq_cond$freq_condition,
                                                                                                     removePunctuation = TRUE,
                                                                                                     tolower = TRUE,
                                                                                                     stripWhitespace= TRUE,
                                                                                                     removeNumbers = TRUE,
                                                                                                     stopwords = TRUE))
    
    
    
    
    
    # Function to convert the word frequencies to yes (presence) and no (absence) labels
    convert_mongo_cond <- function(x) {
      y <- ifelse(x > 0, 1,0)
      y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
      y
    }
   
    
    Mongo_testNB_cond <- apply(Mongo_dtm.test.nb_cond, 2, convert_mongo_cond)
    
    # output$Overview  <-   renderPrint({
    #   print(as.matrix(Mongo_testNB_cond)[,1:3] )
    #   })
    # 
    # output$Condition <- renderPrint({
    #   print(Mongo_tm_dte_condition_User_cond)
    # })
    # Perform Test
    
    Mongo_Pred_lap_cond <- predict(Shiny_saved_model_cond,Mongo_testNB_cond)

    Model_Result_cond <- cbind("doc_id"= Mongo_tm_dte_condition_User_cond$doc_id,
                               "text" = Mongo_tm_dte_condition_User_cond$text,
                               as.data.frame(Mongo_Pred_lap_cond))

    Model_Result_cond <- Model_Result_cond[which(Model_Result_cond$doc_id=="QUESTION"),c(2,3)]
    query_cond <- as.character(Model_Result_cond$Mongo_Pred_lap_cond[1])

    if(mongo_run){
       cond_over_dt <-Cond_Mongo_DCon$find(paste0('{"Diseases" :"',  query_cond,'" }') )
       }else{
         cond_over_dt <- NULL
       }
    # cond_over_dt$Dt_url
   output$Condition <- renderText({ paste(query_cond) })
   output$readMore <- renderText({ 
      paste0("<a href='",cond_over_dt$Dt_url ,"' target='_blank'>","Read More</a>")
     })
   output$Overview <- renderText({ 
                                         paste(cond_over_dt$Dt_overview)
    # "Basal cell carcinoma is a type of skin cancer that occurs in the basal cells of the epidermis - the outermost layer of the skin. Basal cells constantly divide to replace dead or damaged skin cells closer to the outer surface of the epidermis. However, these cells can become cancerous when exposed to ultraviolet (UV) radiation or other factors, triggering them to proliferate uncontrollably. Basal cell carcinoma is the most common type of cancer with nearly 2.8 million Americans affected every year. | Basal cell carcinoma is usually found on the head and neck, but can be found anywhere on the body, including the limbs. Unlike more aggressive cancers, basal cell carcinoma rarely spreads, or metastasizes, to other sites within the body. Timely treatment and removal are key interventions to avoiding deformity of the affected area, and spread into deeper tissues. This cancer is fully treatable if detected early, but the chance of reoccurrence in the same area or other areas of the body is high."
     
   })
    
  
  })  
#------------------------------------------------  
  # Start Side Info End
  
# DT::datatable(server_predict_for_user)
#### Render ouput on SERVER


  

output$fullResult_plot <- renderPrint({ 
  if(is.null(input$Symptoms_text)){
    return()
  }
 
  startPredict()
  
  
  
  })

output$Symptoms <-    renderText(paste("Symptoms:",dtquestion()))
  
debugText <- function(debug_text){
  print(paste("DebugText: ",debug_text))
}

output$result <-  renderText( 
  paste("Result:",dtAnswer())
  )

# Make the wordcloud drawing during each check

output$wordsym_plot <- renderPlot({
   # paste("Terms:",dtTerm())
  # wordcloud(Mongo_corpus_condition_test_user ,min.freq = 1, color= TRUE)
 
  wordcloud(dtTerm() ,min.freq = 1, fixed.asp = TRUE,scale = c(1,.8),random.color =  TRUE)
})

# Make the wordcloud for model Terms during each check

output$wordsym_Model_plot <- renderText({
 
  paste(dtModelTerm())
  
  # wordcloud(names(dtModelTerm()), dtModelTerm()    )
})

# output$plotDT <- DT::renderDataTable({
#   
#   DT::datatable(DT_predict[,c(1,2)],colnames = c("Problem","You may take!"))
# 
#   
#   # wordcloud(names(v), v, scale=c(2,0.5),
#   #           colors=brewer.pal(1, "Dark2"))
#   
# 
#   
# })

## Code for Recom. End

    
 

#----------------------------------------------------------------------------------#

#--------------------------Model Fetch And Apply Logic-----------------------------#

#----------------------------------------------------------------------------------#

  # 


} # End of Server Functiom 

