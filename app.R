library(tidyverse)
library(lubridate)
library(extrafont)
library(showtext)
library(rmarkdown)
library(ragg)
library(ggimage)
library(png)
library(ggthemes)
library(ggrepel)
library(data.table)
library(sysfonts)
library(formattable)
library(shiny)
library(shinythemes)
library(shinyWidgets)

#utility functions
source("util_functions.R")

#load in data
data_path <- paste0(getwd(), "/output_data/data.csv")
df <- read.csv(data_path)

ui <- (fluidPage(theme = shinytheme("superhero"),
        setBackgroundColor("#27321b"),
        titlePanel(title = div(img(src="nps_icon.png", height = "190px", width = "205px"), HTML("<big>Biodiversity in U.S. National Parks</big>"))),
        #title ---
        navbarPage(title="", 
                ##MAIN PAGE##
                tabPanel(title=HTML("<big><b>MAIN</b></big>"), value=
                   
                   ##SECTION 0 - INTRO##
                   ##text blurb ---
                   fluidRow(p(HTML("<big>This data was made available by The National Park Service and contains animal and plant species identified in 
                              individual national parks and verified by evidence: observations, vouchers, or reports that document the presence
                              of a species in a park<sup>[1]</sup>. This app will allow you to explore this data via bar charts, tables, and generated 
                              statistics.</big>"), style = "text-align:justify;padding:15px,border-radius:10px")),
                   br(),
                   ##select and submit buttons ---
                   fluidRow(selectInput("parkChoice", label = "Select a park", choices = df$park_name), width=4),
                   fluidRow(submitButton(text = "Submit"), width=1),
                   
                   br(),
                   hr(style="border-color:lightgrey"),
                   
                   ##SECTION 1 - INFO##
                   ##park information ---
                   fluidRow(column(p(htmlOutput("infoPark")), style="text-align:center;background-color:#525b49", width=12)),    #all park info

                   hr(style="border-color:lightgrey"),
                   
                   ##SECTION 2 - ANIMALS##
                   ##animals heading ---
                   fluidRow(column(p(htmlOutput("animalsHead")), width=12)),
                   
                   hr(style="border-color:#7d8476"),
                   
                   ##vertebrates heading ---
                   fluidRow(column(h3("Vertebrates", style = "text-align:left;font-weight:bold;padding:1px"), width=12)),
                   ##vertebrates plot + rankings ---
                   fluidRow(
                           #plot
                           column(plotOutput(outputId="vertPlot"), width=7),
                           #empty column
                           column(width=1),
                           #rankings
                           column(p(htmlOutput("allRanks")), width=4)),
                   
                   hr(style="border-color:#7d8476"),
                   
                   #invertebrates heading ---
                   fluidRow(column(h3("Invertebrates", style = "text-align:left;font-weight:bold;padding:1px"), width=12)),
                   ##invertebrates table + definitions ---
                   fluidRow( #table
                             column(formattableOutput(outputId="invertTable"), width=5),
                             #empty column
                             column(width=3),
                             #definitions
                             column(htmlOutput("invertDefs"), width=4)),
                   
                   hr(style="border-color:lightgrey"),
                   
                   ##SECTION 3 - NON-ANIMALS##
                   #plants heading ---
                   fluidRow(column(h3("PLANTS", style = "text-align:left;font-weight:bold;padding:1px"), width=12)),
                   ##plants text + definitions ---
                   fluidRow(column(h4("Species of Vascular Plants", style="text-align:right;padding:20px"), width=3),    #no of species of vascular plants
                            column(h1(textOutput("plantVasc")), width=2),    # | number
                            column(width=1),
                            column(h4("Species of Non-Vascular Plants", style="text-align:right;padding:20px"), width=3),    #no of species of nonvascular plants
                            column(h1(textOutput("plantNonvasc")), width=3)),    # | number or 
                   
                   hr(style="border-color:lightgrey"),
                   
                   #algae/fungi heading ---
                   fluidRow(column(h3("ALGAE AND FUNGI", style = "text-align:left;font-weight:bold;padding:1px"), width=12)),
                   #algae/fungi text ---
                   fluidRow(column(width=1),
                            column(h4("Species of Algae", style="text-align:right;padding:20px"), width=2),    #no of species of vascular plants
                            column(h1(textOutput("algae")), width=2),    # | number or unknown
                            column(width=2),
                            column(h4("Species of Fungi", style="text-align:right;padding:20px"), width=2),    #no of species of nonvascular plants
                            column(h1(textOutput("fungi")), width=2)),    # | number or unknown
                   
                   
                   hr(style="border-color:lightgrey"),
                   
                   ##SECTION 4 - FOOTER##
                   #disclaimer ---
                   fluidRow(column(p(HTML("<em>***This website is not officially affiliated with The National Park Service</em>"), style="color:#7d8476;text-size:10px"), width=12))),
                   
                ##SECOND PAGE##
              tabPanel(HTML("<big><b>COMPARE TWO PARKS</b></big>"),
                       fluidRow(h1("Compare two parks")),
                       br(),
                       ##select and submit buttons ---
                       #pick two parks
                       fluidRow(column(width=1),
                                column(selectInput("parkChoice1", label = "Select a park", choices = df$park_name), width=4),
                                column(width=2),
                                column(selectInput("parkChoice2", label = "Select a second park", choices = df$park_name), width=4),
                                column(width=1)),
                       #submit button
                       fluidRow(column(width=1),
                                column(submitButton(text = "Submit"), width=2),
                                column(width=9)),
                       
                       br(),
                       br(),
                       
                       ##park information ---
                       ##park information ---
                       fluidRow(column(p(htmlOutput("infoPark1")), style="text-align:center;background-color:#525b49", width=6),    #park 1 info
                                column(p(htmlOutput("infoPark2")), style="text-align:center;background-color:#3d4732", width=6)),    #park 2 info
                       
                       hr(style="border-color:lightgrey"),
                       
                       ##animals heading ---
                       fluidRow(column(width=1),    #empty space
                                column(p(htmlOutput("animalsHead1")), width=3),
                                column(width=3),    #empty space
                                column(p(htmlOutput("animalsHead2")), width=3)),
                       
                       hr(style="border-color:#7d8476"),
                       
                       ##vertebrates heading ---
                       fluidRow(column(h3("Vertebrates", style = "text-align:center;font-weight:bold;padding:1px"), width=12)),
                       ##vertebrates plots ---
                       fluidRow(
                               #first park plot
                               column(plotOutput(outputId="vertPlot1"), width=6),
                               #second park plot
                               column(plotOutput(outputId="vertPlot2"), width=6)),
                       
                       hr(style="border-color:#7d8476"),
                       
                       #vertebrates rankings
                       fluidRow(column(width=1),
                                column(p(htmlOutput("allRanks1")), width=5),
                                column(width=1),
                                column(p(htmlOutput("allRanks2")), width=5)),
                       
                       
                       hr(style="border-color:#7d8476"),
                       
                       #invertebrates heading
                       fluidRow(column(h3("Invertebrates", style = "text-align:center;font-weight:bold;padding:1px"), width=12)),
                       ##invertebrates tables
                       fluidRow(
                               column(width=1),    #empty space
                               column(formattableOutput(outputId="invertTable1"), width=3), #first park table
                               column(width=3),    #empty space
                               column(formattableOutput(outputId="invertTable2"), width=3)), #second park table
                       
                       
                       hr(style="border-color:lightgrey"),
                       
                       ##SECTION 3 - NON-ANIMALS##
                       #plants heading ---
                       fluidRow(column(h1("PLANTS", style = "text-align:center;font-weight:bold;padding:1px"), width=12)),
                       ##vascular text --- 
                       fluidRow(column(width=1),
                                column(h3("Vascular", style="text-align:right;padding:10px"), width=2),    #no of species of vascular plants
                                column(h1(textOutput("plantVasc1")), width=2),    # | number
                                column(width=2),
                                column(h3("Vascular", style="text-align:right;padding:10px"), width=2),    #no of species of vascular plants
                                column(h1(textOutput("plantVasc2")), width=2)),
                       #non-vascular text ---
                       fluidRow(column(h3("Non-Vascular", style="text-align:right;padding:10px"), width=3),    #no of species of vascular plants
                                column(h1(textOutput("plantNonvasc1")), width=2),    # | number
                                column(width=1),
                                column(h3("Non-Vascular", style="text-align:right;padding:10px"), width=3),    #no of species of vascular plants
                                column(h1(textOutput("plantNonvasc2")), width=2)),
                       
                       
                       hr(style="border-color:lightgrey"),
                       
                       #algae/fungi heading ---
                       fluidRow(column(h1("ALGAE AND FUNGI", style = "text-align:center;font-weight:bold;padding:1px"), width=12)),
                       #algae text ---
                       fluidRow(column(width=1),
                                column(h3("Algae", style="text-align:right;padding:10px"), width=2),    #no of species of vascular plants
                                column(h1(textOutput("algae1")), width=2),    # | number or unknown
                                column(width=2),
                                column(h3("Algae", style="text-align:right;padding:10px"), width=2),    #no of species of nonvascular plants
                                column(h1(textOutput("algae2")), width=2)),    # | number or unknown
                       #fungi text ---
                       fluidRow(column(width=1),
                                column(h3("Fungi", style="text-align:right;padding:10px"), width=2),    #no of species of vascular plants
                                column(h1(textOutput("fungi1")), width=2),    # | number or unknown
                                column(width=2),
                                column(h3("Fungi", style="text-align:right;padding:10px"), width=2),    #no of species of nonvascular plants
                                column(h1(textOutput("fungi2")), width=2)),    # | number or unknown
                       
                       hr(style="border-color:lightgrey"),
                       
                       ##SECTION 4 - FOOTER##
                       #disclaimer ---
                       fluidRow(column(p(HTML("<em>***This website is not officially affiliated with The National Park Service</em>"), style="color:#7d8476;text-size:10px"), width=12))),
                ##FAQ PAGE##
              tabPanel(HTML("<big><b>FAQ</b></big>"),
                        fluidRow("content")),
                ##ABOUT PAGE##
              tabPanel(HTML("<big><b>ABOUT</b></big>"),
                       fluidRow("content")),
              navbarMenu(title=HTML("<big><b>OTHER VERSIONS</b></big>"),
                         tabPanel("PlainText", "content"),
                         tabPanel("en Espanol", "content"),
                         tabPanel("Plain text en Espanol", "content")
                         )
                   )))


server <- function(input, output){
        
        ###  MAIN PAGE  ###
        ##SECTION 1 - INFO##
        output$infoPark <- renderUI({get_park_info(input$parkChoice)})
        
        ##SECTION 2 - ANIMALS##
        output$animalsHead <- renderUI({get_animal_header(input$parkChoice)})
        #vertebrates bar plot
        output$vertPlot <- renderPlot(vert_plot(input$parkChoice))
        #rankings
        output$allRanks <- renderUI({get_all_ranks(input$parkChoice)})
        #invertebrates formattable
        output$invertTable <- renderFormattable({formattable(invert_tbl(input$parkChoice),list())})
        #invertebrates information
        output$invertDefs <- renderUI({get_invert_defs()})
        
        ##SECTION 3 - NON-ANIMALS##
        #plants
        output$plantVasc <- renderText(get_vasc(input$parkChoice))
        output$plantNonvasc <- renderText(get_nonvasc(input$parkChoice))
        #algae and fungi
        output$algae <- renderText(get_algae(input$parkChoice))
        output$fungi <- renderText(get_fungi(input$parkChoice))
        
        
        
        
        ###  COMPARE TWO PARKS PAGE  ###
        #park info
        output$infoPark1 <- renderText(get_park1_info(input$parkChoice1))
        output$infoPark2 <- renderText(get_park2_info(input$parkChoice2))
        
        ##SECTION 2 - ANIMALS##
        output$animalsHead1 <- renderUI({get_animal_header(input$parkChoice1)})
        output$animalsHead2 <- renderUI({get_animal_header(input$parkChoice2)})
        
        #vertebrates bar plots
        output$vertPlot1 <- renderPlot(vert_plot2(input$parkChoice1))
        output$vertPlot2 <- renderPlot(vert_plot2(input$parkChoice2))
        
        #rankings
        output$allRanks1 <- renderUI({get_all_ranks(input$parkChoice1)})
        output$allRanks2 <- renderUI({get_all_ranks(input$parkChoice2)})
        
        #invertebrates formattables
        output$invertTable1 <- renderFormattable({formattable(invert_tbl(input$parkChoice1),list())})
        output$invertTable2 <- renderFormattable({formattable(invert_tbl(input$parkChoice2),list())})
        
        ##SECTION 3 - NON-ANIMALS##
        #plants - vascular
        output$plantVasc1 <- renderText(get_vasc(input$parkChoice1))
        output$plantVasc2 <- renderText(get_vasc(input$parkChoice2))
        #plants - nonvascular
        output$plantNonvasc1 <- renderText(get_nonvasc(input$parkChoice1))
        output$plantNonvasc2 <- renderText(get_nonvasc(input$parkChoice2))
        #algae and fungi
        output$algae1 <- renderText(get_algae(input$parkChoice1))
        output$algae2 <- renderText(get_algae(input$parkChoice2))
        output$fungi1 <- renderText(get_fungi(input$parkChoice1))
        output$fungi2 <- renderText(get_fungi(input$parkChoice2))
        
}

shinyApp(ui = ui, server = server)




















