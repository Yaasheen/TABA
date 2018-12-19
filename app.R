#---------------------------------------------------------------------#
#        Shiny App for UDPipe NLP workflow                            #
#---------------------------------------------------------------------#

library("shiny")

ui <- shinyUI(
  fluidPage(
    
    titlePanel("Shiny App for UDPipe NLP workflow "),
    
    sidebarLayout( 
      
      sidebarPanel(  
        
        selectInput(inputId = "language",
                    label = "Choose a Language:",
                    choices = c("English", "Hindi", "Spanish")),
        
        fileInput("file1", "Upload data (txt file) for the language selected"),
        fileInput("file2", "Upload data (.udpipe format) for the language selected"),
        
        checkboxGroupInput(inputId = 'xpos',
                           label = h3('Select XPOS Part of speech for Co-occurrances and Annotated Data filtering'),
                           choices =list("adjective"= "JJ",
                                         "Noun" = "NN",
                                         "proper noun" = "NNP",
                                         "adverb"="RB","verb"= "VB"),
                           selected = c("JJ","NN","NNP"))
        
        
        
      ),
      
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Overview",
                             br(),
                             h4(p("Data Input Guidelines")),
                             p("This Shiny app supports only text documents (.txt) data file. ",align="justify"),
                             p("Kindly wait for few minutes after loading txt and udpipe file for output"),
                             p("Please refer to the link below for sample txt file."),
                             br(),
                             a(href="https://raw.githubusercontent.com/Yaasheen/TABA/master/Nokia_Lumia_reviews.txt"
                               ,"Sample input txt file"),   
                             br(),br(),
                             h4('How to use this App'),
                             p('To use this app, click on', 
                               span(strong("Upload data (Txt File)")),
                               'and upload the txt file. '),
                             p('To upload udpipe specific to the language, click on', 
                               span(strong("Upload data (Udpipe Format)")),
                               'and upload the udpipe file specific to the language '),
                             br(),
                             
                             h4('Details of Output'),
                             p('There are 4 Output Tabs, click on', 
                               span(strong("Second Page")),
                               'to see the Annotated Data and to Download the Data into csv file'),
                             p('Click on', 
                               span(strong("Third Page")),
                               'to see the word Clouds for Nouns and Verbs from the Annotated Data'),
                             p('Click on', 
                               span(strong("Fourth Page")),
                               'to see the Co-occurences for XPOS selected on the Check Boxes on the side pane.')),
                    
                    tabPanel("Table of Annotated documents", 
                             dataTableOutput('datatableOutput'),
                             br(),
                             downloadButton("downloadData", "Download Annotated Data")),
                    
                    tabPanel("Word Clouds",
                             h3("Nouns"),
                             plotOutput('wcplot1'),
                             h3("Verbs"),
                             plotOutput('wcplot2')),
                    
                    tabPanel("Co-Occurrences",
                             h3("Co-occurrences"),
                             plotOutput('coocplot'))
                    
        ) # end of tabsetPanel
      )# end of main panel
    ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI




# Define Server function
options(shiny.maxRequestSize=30*1024^2)

try(require(shiny) || install.packages("shiny"))
if (!require(udpipe)){install.packages("udpipe")}
if (!require(textrank)){install.packages("textrank")}
if (!require(lattice)){install.packages("lattice")}
if (!require(igraph)){install.packages("igraph")}
if (!require(ggraph)){install.packages("ggraph")}
if (!require(wordcloud)){install.packages("wordcloud")}
try(require("fmsb")||install.packages("fmsb"))

library("shiny")
library("fmsb")
library(udpipe)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)
require(stringr)
#getwd()
#setwd('D:/extraz/CBA ISB/Term 1/Residency 2 - Nov/Text Analytics/Assignment_TABA')

shinyServer(function(input, output) {
  
  LanguageInput <- reactive({
    switch(input$language,
           "English" = english,
           "Hindi" = Hindi,
           "Spanish" = Spanish)
  })
  
  Dataset <- reactive({
    
    if (is.null(input$file1)) {
      return(NULL) } 
    else{
      Data <- readLines(input$file1$datapath)
      Data  =  str_replace_all(Data, "<.*?>", "") # get rid of html junk 
      Data = Data[Data!= ""]
      str(Data)
      return(Data)
    }
  })
  
  english_model = reactive({
    if (is.null(input$file2)) {return(NULL)}
    else {
      english_model  = udpipe_load_model(input$file2$datapath)
    }
    # file_model only needed
    return(english_model)
  })
  
  #https://github.com/Yaasheen/TABA/blob/master/english-ud-2.0-170801.udpipe?raw=true
  #https://github.com/Yaasheen/TABA/blob/master/english-ud-2.0-170801.udpipe
  
  annot.obj = reactive({
    x <- udpipe_annotate(english_model(),x = Dataset())
    x <- as.data.frame(x)
    return(x)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      
      paste(input$language, ".csv", sep = "")
    },
    content = function(file){
      write.csv(annot.obj()[,-4],file,row.names = FALSE)
    }
  )
  
  output$datatableOutput = renderDataTable({
    if(is.null(input$file1)){return(NULL)}
    else{
      outp = annot.obj()[,-4]
      return(outp)
    }
  })
  
  output$wcplot1 = renderPlot({
    if(is.null(input$file1)){return(NULL)}
    else{
      all_nouns = annot.obj() %>% subset(., xpos %in% "NN") 
      top_nouns = txt_freq(all_nouns$lemma)  # txt_freq() calculates frequency of Nouns
      
      wordcloud(top_nouns$key,top_nouns$freq, min.freq = 3,colors = 1:10 )
    }
  })
  
  output$wcplot2 = renderPlot({
    if(is.null(input$file1)){return(NULL)}
    else{
      all_verbs = annot.obj() %>% subset(., xpos %in% "VB") 
      top_verbs = txt_freq(all_verbs$lemma)
      
      wordcloud(top_verbs$key,top_verbs$freq, min.freq = 3,colors = 1:10 )
    }
  })
  
  output$coocplot = renderPlot({
    if(is.null(input$file1)){return(NULL)}
    else{
      cooc <- cooccurrence(   	
        x = subset(annot.obj(), xpos %in% input$xpos), 
        term = "lemma", 
        group = c("doc_id", "paragraph_id", "sentence_id"))
      
      wordcooc <- head(cooc, 50)
      wordcooc <- igraph::graph_from_data_frame(wordcooc) # needs edgelist in first 2 colms.
      
      ggraph(wordcooc, layout = "fr") +  
        
        geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "green") +  
        geom_node_text(aes(label = name), col = "orange", size = 7) +
        
        theme_graph(base_family = "Arial Narrow") +  
        theme(legend.position = "none") +
        
        labs(title = "Cooccurrences Graph", subtitle = "Select the check boxes in the left pane")
    }
  })
})

shinyApp(ui = ui, server = server)