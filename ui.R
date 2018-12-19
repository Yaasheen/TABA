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
