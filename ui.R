###########################################################################
# ID: 11810038  Yaasheen Sheikh                                           #
# ID: 11810106  Mahesh Patil                                              #
# ID: 11810091  Bhargav BR                                                #
###########################################################################


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
        
        checkboxGroupInput(inputId = 'upos',
                           label = h3('Select XPOS/UPOS Part of speech for Co-occurrances and Annotated Data filtering'),
                           choices =list("Adjective"= "ADJ",
                                         "Noun" = "NOUN",
                                         "Proper Noun" = "PROPN",
                                         "Adverb"="ADV",
                                         "Verb"= "VERB"),
                           selected = c("ADJ","NOUN","PROPN"))
        
        
        
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
                               span(strong("Tab of Annotated Documents")),
                               'to see the Annotated Data and to Download the Data into csv file'),
                             p('Click on', 
                               span(strong("Word Cloud")),
                               'to see the word Clouds for Nouns and Verbs from the Annotated Data'),
                             p('Click on', 
                               span(strong("Co-occurence for XPOS")),
                               'to see the Co-occurences for XPOS selected on the Check Boxes on the side pane.'),
                             br(),
                             
                             h4('Logical Flow'),
                             p('1. App takes First input as file in txt format based on the language selected in LOV.',br(),
                               '2. Second Input file is the udpipe model file based on the language selected in LOV.',br(),
                               '3. Based on the Language selected  if loops in server.R file reads input file1 and stores it in Dataset.',br(),
                               '4. Input file 2 - is stored as model . There is a check to ensure it is not null.',br(),
                               '5. annot.obj stores the object by applying the model to the input file collected. For Hindi Language, Devanagiri Font is loaded.',br(),
                               '6. output$downloadData stores the output for downloading data in csv format of the annotated file.output$datatableOutput object stores the output in table format for display.',br(),
                               '7. output$wcplot1 and output$wcplot2 objects store the word cloud for nouns and verbs. Code takes annotated object and filters on upos selected on the side panel.',br(),
                               '8. output$coocplot object uses renderplot and stores the output of co-occurence plot. The plot is created by filtering on the upos selected in side panel by user.')),
                             
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
