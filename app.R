library(tidyverse)
library(lubridate)
#library(extrafont)
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
library(rsconnect)
library(grDevices)
library(readr)

#utility functions
source("util_functions.R")

encoding="UTF-8"

#load in data
data_path <- paste0(getwd(), "/output_data/data.csv")
df <- read.csv(data_path)
#load in data en espanol
esp_data_path <- paste0(getwd(), "/output_data/parques.csv")
esp_df <- as.data.frame(read.table(esp_data_path, sep=",", header=T))

ui <- (fluidPage(theme = shinytheme("superhero"),
        setBackgroundColor("#27321b"),
        titlePanel(title = div(img(src="nps_icon.png", height = "190px", width = "205px"), HTML("<big>Biodiversity in U.S. National Parks</big>"))),
        #title ---
        navbarPage(title=HTML(" "), 
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
                         tabPanel(title=HTML("<big><b>PLAIN TEXT</b></big>"), value=
                                    br(),
                                  ### MAIN PAGE ### ---         
                                  fluidRow(column(h1("MAIN PAGE", style = "text-align:left;padding:1px"), width=12)), 
                                  br(),
                                  
                                  ##SECTION 0 - INTRO##
                                  ##text blurb ---
                                  fluidRow(p(HTML("This data was made available by The National Park Service and contains animal and plant species identified in 
                              individual national parks and verified by evidence: observations, vouchers, or reports that document the presence
                              of a species in a park<sup>[1]</sup>. This app will allow you to explore this data via bar charts, tables, and generated 
                              statistics."), style = "text-align:left;")),
                                  
                                  br(),
                                  br(),
                                  
                                  ##select and submit buttons ---
                                  fluidRow(column(h2("SELECT A PARK", style = "text-align:left;padding:1px"), width=12)),
                                  br(),
                                  fluidRow(selectInput("parkChoice", label = "Select a park", choices = df$park_name), width=4),
                                  fluidRow(submitButton(text = "Submit"), width=1),
                                  
                                  br(),
                                  br(),
                                  
                                  ##PLAIN TEXT SECTION 1 - INFO##
                                  fluidRow(column(h2("GENERAL PARK INFO", style = "text-align:left"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("parkInfoPlain"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  ##PLAIN TEXT SECTION 2 - ANIMALS##
                                  fluidRow(column(h2("ANIMALS", style = "text-align:left"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("animalInfoPlain"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  #VERTEBRATES
                                  fluidRow(column(h2("VERTEBRATES", style = "text-align:left"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("vertInfoPlain"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("rankInfoPlain"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  #INVERTEBRATES
                                  fluidRow(column(h2("INVERTEBRATES", style = "text-align:left"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("invertInfoPlain"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("invertDefsPlain"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  ##PLAIN TEXT SECTION 3 - NON-ANIMALS##
                                  #PLANTS
                                  fluidRow(column(h2("PLANTS", style = "text-align:left"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("plantVascPlain"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("plantNonvascPlain"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  #ALGAE AND FUNGI
                                  fluidRow(column(h2("ALGAE AND FUNGI", style = "text-align:left"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("algaePlain"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("fungiPlain"), width=12)),
                                  
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  
                                  ### PLAIN TEXT SECOND PAGE - COMPARE TWO PARKS ### ---         
                                  fluidRow(column(h1("SECOND PAGE - COMPARE TWO PARKS", style = "text-align:left;padding:1px"), width=12)),
                                  br(),
                                  
                                  ##select and submit buttons ---
                                  fluidRow(column(h2("SELECT TWO PARKS", style = "text-align:left;padding:1px"), width=12)),
                                  br(),
                                  fluidRow(selectInput("parkChoice1", label = "Select a park", choices = df$park_name), width=4),
                                  fluidRow(selectInput("parkChoice2", label = "Select a second park", choices = df$park_name), width=4),
                                  fluidRow(submitButton(text = "Submit"), width=1),
                                  
                                  br(),
                                  br(),
                                  
                                  ##PLAIN TEXT SECTION 1 - INFO##
                                  fluidRow(column(h2("GENERAL PARK INFO", style = "text-align:left"), width=12)),
                                  br(),
                                  #park1
                                  fluidRow(column(htmlOutput("parkInfoPlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("parkInfoPlain2"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  ##PLAIN TEXT SECTION 2 - ANIMALS##
                                  fluidRow(column(h2("ANIMALS", style = "text-align:left"), width=12)),
                                  br(),
                                  #park1
                                  fluidRow(column(htmlOutput("animalInfoPlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("animalInfoPlain2"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  #VERTEBRATES
                                  fluidRow(column(h2("VERTEBRATES", style = "text-align:left"), width=12)),
                                  br(),
                                  #park1
                                  fluidRow(column(htmlOutput("vertInfoPlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("vertInfoPlain2"), width=12)),
                                  br(),
                                  #park1
                                  fluidRow(column(htmlOutput("rankInfoPlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("rankInfoPlain2"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  #INVERTEBRATES
                                  fluidRow(column(h2("INVERTEBRATES", style = "text-align:left"), width=12)),
                                  br(),
                                  #park1
                                  fluidRow(column(htmlOutput("invertInfoPlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("invertInfoPlain2"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  ##PLAIN TEXT SECTION 3 - NON-ANIMALS##
                                  #PLANTS
                                  fluidRow(column(h2("PLANTS", style = "text-align:left"), width=12)),
                                  br(),
                                  #VASCULAR
                                  #park1
                                  fluidRow(column(htmlOutput("plantVascPlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("plantVascPlain2"), width=12)),
                                  br(),
                                  #NONVASCULAR
                                  #park1
                                  fluidRow(column(htmlOutput("plantNonvascPlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("plantNonvascPlain2"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  #ALGAE AND FUNGI
                                  fluidRow(column(h2("ALGAE AND FUNGI", style = "text-align:left"), width=12)),
                                  br(),
                                  #park1
                                  fluidRow(column(htmlOutput("algaePlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("algaePlain2"), width=12)),
                                  br(),
                                  #park1
                                  fluidRow(column(htmlOutput("fungiPlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("fungiPlain2"), width=12)),
                                  
                                  hr(style="border-color:#7d8476"),
                                  
                                  ##SECTION 4 - FOOTER##
                                  #disclaimer ---
                                  fluidRow(column(p(HTML("<em>***This website is not officially affiliated with The National Park Service</em>"), style="color:#7d8476;text-size:10px"), width=12))),
                         tabPanel(title=HTML("<big><b>EN ESPAÑOL</b></big>"), value=
                                    
                                    ##SECTION 0 - INTRO##
                                    ##text blurb ---
                                    fluidRow(p(HTML("<big>Esta información puestos a disposición por The National Park Service y contiene las especies de animales y plantas que 
                                   son identificados en los nacional parques and verificado por pruebas como: observaciones y informes que documentan el presencio 
                                   de una especie en un parque<sup>[1]</sup>. Esta app le permite explorar esta información vía gráficos de barras, tablas y 
                                   estadísticas.</big>"), style = "text-align:justify;padding:15px,border-radius:10px")),
                                  br(),
                                  ##select and submit buttons ---
                                  fluidRow(selectInput("esp_parkChoice", label = "Escoja un parque", choices = esp_df$park_name), width=4),
                                  fluidRow(submitButton(text = "Enviar"), width=1),
                                  
                                  br(),
                                  hr(style="border-color:lightgrey"),
                                  
                                  ##SECTION 1 - INFO##
                                  ##park information ---
                                  fluidRow(column(p(htmlOutput("esp_infoPark")), style="text-align:center;background-color:#525b49", width=12)),    #all park info
                                  
                                  hr(style="border-color:lightgrey"),
                                  
                                  ##SECTION 2 - ANIMALS##
                                  ##animals heading ---
                                  fluidRow(column(p(htmlOutput("esp_animalsHead")), width=12)),
                                  
                                  hr(style="border-color:#7d8476"),
                                  
                                  ##vertebrates heading ---
                                  fluidRow(column(h3("Vertebrados", style = "text-align:left;font-weight:bold;padding:1px"), width=12)),
                                  ##vertebrates plot + rankings ---
                                  fluidRow(
                                    #plot
                                    column(plotOutput(outputId="esp_vertPlot"), width=7),
                                    #empty column
                                    column(width=1),
                                    #rankings
                                    column(p(htmlOutput("esp_allRanks")), width=4)),
                                  
                                  hr(style="border-color:#7d8476"),
                                  
                                  #invertebrates heading ---
                                  fluidRow(column(h3("Invertebrados", style = "text-align:left;font-weight:bold;padding:1px"), width=12)),
                                  ##invertebrates table + definitions ---
                                  fluidRow( #table
                                    column(formattableOutput(outputId="esp_invertTable"), width=5),
                                    #empty column
                                    column(width=3),
                                    #definitions
                                    column(htmlOutput("esp_invertDefs"), width=4)),
                                  
                                  hr(style="border-color:lightgrey"),
                                  
                                  ##SECTION 3 - NON-ANIMALS##
                                  #plants heading ---
                                  fluidRow(column(h3("PLANTAS", style = "text-align:left;font-weight:bold;padding:1px"), width=12)),
                                  ##plants text + definitions ---
                                  fluidRow(column(h4("Especies de plantas vasculares", style="text-align:right;padding:20px"), width=3),    #no of species of vascular plants
                                           column(h1(textOutput("esp_plantVasc")), width=2),    # | number
                                           column(width=1),
                                           column(h4("Especies de plantas no vasculares", style="text-align:right;padding:20px"), width=3),    #no of species of nonvascular plants
                                           column(h1(textOutput("esp_plantNonvasc")), width=3)),    # | number or 
                                  
                                  hr(style="border-color:lightgrey"),
                                  
                                  #algae/fungi heading ---
                                  fluidRow(column(h3("ALGAS Y HONGOS", style = "text-align:left;font-weight:bold;padding:1px"), width=12)),
                                  #algae/fungi text ---
                                  fluidRow(column(width=1),
                                           column(h4("Especies de algas", style="text-align:right;padding:20px"), width=2),    #no of species of vascular plants
                                           column(h1(textOutput("esp_algae")), width=2),    # | number or unknown
                                           column(width=2),
                                           column(h4("Especies de hongos", style="text-align:right;padding:20px"), width=2),    #no of species of nonvascular plants
                                           column(h1(textOutput("esp_fungi")), width=2)),    # | number or unknown
                                  
                                  
                                  hr(style="border-color:lightgrey"),
                                  HTML("<big><b>PÁGINA SEGUNDA – COMPARA DOS PARQUES</b></big>"),
                                  br(),
                                  ##select and submit buttons ---
                                  #pick two parks
                                  fluidRow(column(width=1),
                                           column(selectInput("esp_parkChoice1", label = "Escoja un parque", choices = esp_df$park_name), width=4),
                                           column(width=2),
                                           column(selectInput("esp_parkChoice2", label = "Escoja el parque segundo", choices = esp_df$park_name), width=4),
                                           column(width=1)),
                                  #submit button
                                  fluidRow(column(width=1),
                                           column(submitButton(text = "Enviar"), width=2),
                                           column(width=9)),
                                  
                                  br(),
                                  br(),
                                  
                                  ##park information ---
                                  ##park information ---
                                  fluidRow(column(p(htmlOutput("esp_infoPark1")), style="text-align:center;background-color:#525b49", width=6),    #park 1 info
                                           column(p(htmlOutput("esp_infoPark2")), style="text-align:center;background-color:#3d4732", width=6)),    #park 2 info
                                  
                                  hr(style="border-color:lightgrey"),
                                  
                                  ##animals heading ---
                                  fluidRow(column(width=1),    #empty space
                                           column(p(htmlOutput("esp_animalsHead1")), width=3),
                                           column(width=3),    #empty space
                                           column(p(htmlOutput("esp_animalsHead2")), width=3)),
                                  
                                  hr(style="border-color:#7d8476"),
                                  
                                  ##vertebrates heading ---
                                  fluidRow(column(h3("Vertebrados", style = "text-align:center;font-weight:bold;padding:1px"), width=12)),
                                  ##vertebrates plots ---
                                  fluidRow(
                                    #first park plot
                                    column(plotOutput(outputId="esp_vertPlot1"), width=6),
                                    #second park plot
                                    column(plotOutput(outputId="esp_vertPlot2"), width=6)),
                                  
                                  hr(style="border-color:#7d8476"),
                                  
                                  #vertebrates rankings
                                  fluidRow(column(width=1),
                                           column(p(htmlOutput("esp_allRanks1")), width=5),
                                           column(width=1),
                                           column(p(htmlOutput("esp_allRanks2")), width=5)),
                                  
                                  
                                  hr(style="border-color:#7d8476"),
                                  
                                  #invertebrates heading
                                  fluidRow(column(h3("Invertebrados", style = "text-align:center;font-weight:bold;padding:1px"), width=12)),
                                  ##invertebrates tables
                                  fluidRow(
                                    column(width=1),    #empty space
                                    column(formattableOutput(outputId="esp_invertTable1"), width=3), #first park table
                                    column(width=3),    #empty space
                                    column(formattableOutput(outputId="esp_invertTable2"), width=3)), #second park table
                                  
                                  
                                  hr(style="border-color:lightgrey"),
                                  
                                  ##SECTION 3 - NON-ANIMALS##
                                  #plants heading ---
                                  fluidRow(column(h1("PLANTAS", style = "text-align:center;font-weight:bold;padding:1px"), width=12)),
                                  ##vascular text --- 
                                  fluidRow(column(width=1),
                                           column(h3("Vascular", style="text-align:right;padding:10px"), width=2),    #no of species of vascular plants
                                           column(h1(textOutput("esp_plantVasc1")), width=2),    # | number
                                           column(width=2),
                                           column(h3("Vascular", style="text-align:right;padding:10px"), width=2),    #no of species of vascular plants
                                           column(h1(textOutput("esp_plantVasc2")), width=2)),
                                  #non-vascular text ---
                                  fluidRow(column(h3("No Vascular", style="text-align:right;padding:10px"), width=3),    #no of species of vascular plants
                                           column(h1(textOutput("esp_plantNonvasc1")), width=2),    # | number
                                           column(width=1),
                                           column(h3("No Vascular", style="text-align:right;padding:10px"), width=3),    #no of species of vascular plants
                                           column(h1(textOutput("esp_plantNonvasc2")), width=2)),
                                  
                                  
                                  hr(style="border-color:lightgrey"),
                                  
                                  #algae/fungi heading ---
                                  fluidRow(column(h1("ALGAS Y HONGOS", style = "text-align:center;font-weight:bold;padding:1px"), width=12)),
                                  #algae text ---
                                  fluidRow(column(width=1),
                                           column(h3("Algas", style="text-align:right;padding:10px"), width=2),    #no of species of vascular plants
                                           column(h1(textOutput("esp_algae1")), width=2),    # | number or unknown
                                           column(width=2),
                                           column(h3("Algas", style="text-align:right;padding:10px"), width=2),    #no of species of nonvascular plants
                                           column(h1(textOutput("esp_algae2")), width=2)),    # | number or unknown
                                  #fungi text ---
                                  fluidRow(column(width=1),
                                           column(h3("Hongos", style="text-align:right;padding:10px"), width=2),    #no of species of vascular plants
                                           column(h1(textOutput("esp_fungi1")), width=2),    # | number or unknown
                                           column(width=2),
                                           column(h3("Hongos", style="text-align:right;padding:10px"), width=2),    #no of species of nonvascular plants
                                           column(h1(textOutput("esp_fungi2")), width=2)),    # | number or unknown
                                  
                                  hr(style="border-color:lightgrey"),
                                  
                                  ##SECTION 4 - FOOTER##
                                  #disclaimer ---
                                  fluidRow(column(p(HTML("<em>***Este sito no es oficialmente afiliada a The National Park Service</em>"), style="color:#7d8476;text-size:10px"), width=12))
                         ),
                         tabPanel(title=HTML("<big><b>EN ESPAÑOL - TEXTO PLANO</b></big>"), value=
                                    br(),
                                  ### MAIN PAGE ### ---         
                                  fluidRow(column(h1("PÁGINA INICIAL", style = "text-align:left;padding:1px"), width=12)), 
                                  br(),
                                  
                                  ##SECTION 0 - INTRO##
                                  ##text blurb ---
                                  fluidRow(p(HTML("Esta información puestos a disposición por The National Park Service y contiene las especies de animales y plantas que 
                                   son identificados en los nacional parques and verificado por pruebas como: observaciones y informes que documentan el presencio 
                                   de una especie en un parque<sup>[1]</sup>. Esta app le permite explorar esta información vía gráficos de barras, tablas y 
                                   estadísticas."), style = "text-align:left;")),
                                  
                                  br(),
                                  br(),
                                  
                                  ##select and submit buttons ---
                                  fluidRow(column(h2("ESCOJA UN PARQUE", style = "text-align:left;padding:1px"), width=12)),
                                  br(),
                                  fluidRow(selectInput("esp_parkChoice", label = "Escoja un parque", choices = esp_df$park_name), width=4),
                                  fluidRow(submitButton(text = "Enviar"), width=1),
                                  
                                  br(),
                                  br(),
                                  
                                  ##PLAIN TEXT SECTION 1 - INFO##
                                  fluidRow(column(h2("INFORMACIÓN GENERAL", style = "text-align:left"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("esp_parkInfoPlain"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  ##PLAIN TEXT SECTION 2 - ANIMALS##
                                  fluidRow(column(h2("ANIMALES", style = "text-align:left"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("esp_animalInfoPlain"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  #VERTEBRATES
                                  fluidRow(column(h2("VERTEBRADOS", style = "text-align:left"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("esp_vertInfoPlain"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("esp_rankInfoPlain"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  #INVERTEBRATES
                                  fluidRow(column(h2("INVERTEBRADOS", style = "text-align:left"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("esp_invertInfoPlain"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("esp_invertDefsPlain"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  ##PLAIN TEXT SECTION 3 - NON-ANIMALS##
                                  #PLANTS
                                  fluidRow(column(h2("PLANTAS", style = "text-align:left"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("esp_plantVascPlain"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("esp_plantNonvascPlain"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  #ALGAE AND FUNGI
                                  fluidRow(column(h2("ALGAS Y HONGOS", style = "text-align:left"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("esp_algaePlain"), width=12)),
                                  br(),
                                  fluidRow(column(htmlOutput("esp_fungiPlain"), width=12)),
                                  
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  
                                  ### PLAIN TEXT SECOND PAGE - COMPARE TWO PARKS ### ---         
                                  fluidRow(column(h1("PÁGINA SEGUNDA – COMPARA DOS PARQUES", style = "text-align:left;padding:1px"), width=12)),
                                  br(),
                                  
                                  ##select and submit buttons ---
                                  fluidRow(column(h2("ESCOJA DOS PARQUES", style = "text-align:left;padding:1px"), width=12)),
                                  br(),
                                  fluidRow(selectInput("esp_parkChoice1", label = "Escoja un parque", choices = df$park_name), width=4),
                                  fluidRow(selectInput("esp_parkChoice2", label = "Escoja un pargue segundo", choices = df$park_name), width=4),
                                  fluidRow(submitButton(text = "Enviar"), width=1),
                                  
                                  br(),
                                  br(),
                                  
                                  ##PLAIN TEXT SECTION 1 - INFO##
                                  fluidRow(column(h2("INFORMACIÓN GENERAL", style = "text-align:left"), width=12)),
                                  br(),
                                  #park1
                                  fluidRow(column(htmlOutput("esp_parkInfoPlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("esp_parkInfoPlain2"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  ##PLAIN TEXT SECTION 2 - ANIMALS##
                                  fluidRow(column(h2("ANIMALES", style = "text-align:left"), width=12)),
                                  br(),
                                  #park1
                                  fluidRow(column(htmlOutput("esp_animalInfoPlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("esp_animalInfoPlain2"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  #VERTEBRATES
                                  fluidRow(column(h2("VERTEBRADOS", style = "text-align:left"), width=12)),
                                  br(),
                                  #park1
                                  fluidRow(column(htmlOutput("esp_vertInfoPlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("esp_vertInfoPlain2"), width=12)),
                                  br(),
                                  #park1
                                  fluidRow(column(htmlOutput("esp_rankInfoPlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("esp_rankInfoPlain2"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  #INVERTEBRATES
                                  fluidRow(column(h2("INVERTEBRADOS", style = "text-align:left"), width=12)),
                                  br(),
                                  #park1
                                  fluidRow(column(htmlOutput("esp_invertInfoPlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("esp_invertInfoPlain2"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  ##PLAIN TEXT SECTION 3 - NON-ANIMALS##
                                  #PLANTS
                                  fluidRow(column(h2("PLANTAS", style = "text-align:left"), width=12)),
                                  br(),
                                  #VASCULAR
                                  #park1
                                  fluidRow(column(htmlOutput("esp_plantVascPlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("esp_plantVascPlain2"), width=12)),
                                  br(),
                                  #NONVASCULAR
                                  #park1
                                  fluidRow(column(htmlOutput("esp_plantNonvascPlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("esp_plantNonvascPlain2"), width=12)),
                                  
                                  br(),
                                  br(),
                                  
                                  #ALGAE AND FUNGI
                                  fluidRow(column(h2("ALGA Y HONGO", style = "text-align:left"), width=12)),
                                  br(),
                                  #park1
                                  fluidRow(column(htmlOutput("esp_algaePlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("esp_algaePlain2"), width=12)),
                                  br(),
                                  #park1
                                  fluidRow(column(htmlOutput("esp_fungiPlain1"), width=12)),
                                  br(),
                                  #park2
                                  fluidRow(column(htmlOutput("esp_fungiPlain2"), width=12)),
                                  
                                  hr(style="border-color:#7d8476"),
                                  
                                  ##SECTION 4 - FOOTER##
                                  #disclaimer ---
                                  fluidRow(column(p(HTML("<em>***Este sito no es oficialmente afiliada a The National Park Service</em>"), style="color:#7d8476;text-size:10px"), width=12)))
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
          ###  PLAIN TEXT MAIN PAGE  ###

  ##PLAIN TEXT SECTION 1 - INFO##
  output$parkInfoPlain <- renderUI({get_park_info(input$parkChoice)})

  ##PLAIN TEXT SECTION 2 - ANIMALS##
  output$animalInfoPlain <- renderUI({get_animal_header(input$parkChoice)})
  #vertebrates info
  output$vertInfoPlain <- renderUI({get_vert_info_plain(input$parkChoice)})
  #rankings
  output$rankInfoPlain <- renderUI({get_rank_info_plain(input$parkChoice)})
  #invertebrates info
  output$invertInfoPlain <- renderUI({get_invert_info_plain(input$parkChoice)})
  #invertebrates information
  output$invertDefsPlain <- renderUI({get_invert_defs_plain(input$parkChoice)})
  
  ##PLAIN TEXT SECTION 3 - NON-ANIMALS##
  #plants
  output$plantVascPlain <- renderText(get_vasc_plain(input$parkChoice))
  output$plantNonvascPlain <- renderText(get_nonvasc_plain(input$parkChoice))
  #algae and fungi
  output$algaePlain <- renderText(get_algae_plain(input$parkChoice))
  output$fungiPlain <- renderText(get_fungi_plain(input$parkChoice))
  
  
  ###  MAIN PAGE  ###
  ##SECTION 1 - INFO##
  output$esp_infoPark <- renderUI({esp_get_park_info(input$esp_parkChoice)})
  
  ##SECTION 2 - ANIMALS##
  output$esp_animalsHead <- renderUI({esp_get_animal_header(input$esp_parkChoice)})
  #vertebrates bar plot
  output$esp_vertPlot <- renderPlot(esp_vert_plot(input$esp_parkChoice))
  #rankings
  output$esp_allRanks <- renderUI({esp_get_all_ranks(input$esp_parkChoice)})
  #invertebrates formattable
  output$esp_invertTable <- renderFormattable({formattable(esp_invert_tbl(input$esp_parkChoice),list())})
  #invertebrates information
  output$esp_invertDefs <- renderUI({esp_get_invert_defs()})
  
  ##SECTION 3 - NON-ANIMALS##
  #plants
  output$esp_plantVasc <- renderText(esp_get_vasc(input$esp_parkChoice))
  output$esp_plantNonvasc <- renderText(esp_get_nonvasc(input$esp_parkChoice))
  #algae and fungi
  output$esp_algae <- renderText(esp_get_algae(input$esp_parkChoice))
  output$esp_fungi <- renderText(esp_get_fungi(input$esp_parkChoice))
  
  
  
  
  ###  COMPARE TWO PARKS PAGE  ###
  #park info
  output$esp_infoPark1 <- renderText(esp_get_park1_info(input$esp_parkChoice1))
  output$esp_infoPark2 <- renderText(esp_get_park2_info(input$esp_parkChoice2))
  
  ##SECTION 2 - ANIMALS##
  output$esp_animalsHead1 <- renderUI({esp_get_animal_header(input$esp_parkChoice1)})
  output$esp_animalsHead2 <- renderUI({esp_get_animal_header(input$esp_parkChoice2)})
  
  #vertebrates bar plots
  output$esp_vertPlot1 <- renderPlot(esp_vert_plot2(input$esp_parkChoice1))
  output$esp_vertPlot2 <- renderPlot(esp_vert_plot2(input$esp_parkChoice2))
  
  #rankings
  output$esp_allRanks1 <- renderUI({esp_get_all_ranks(input$esp_parkChoice1)})
  output$esp_allRanks2 <- renderUI({esp_get_all_ranks(input$esp_parkChoice2)})
  
  #invertebrates formattables
  output$esp_invertTable1 <- renderFormattable({formattable(esp_invert_tbl(input$esp_parkChoice1),list())})
  output$esp_invertTable2 <- renderFormattable({formattable(esp_invert_tbl(input$esp_parkChoice2),list())})
  
  ##SECTION 3 - NON-ANIMALS##
  #plants - vascular
  output$esp_plantVasc1 <- renderText(esp_get_vasc(input$esp_parkChoice1))
  output$esp_plantVasc2 <- renderText(esp_get_vasc(input$esp_parkChoice2))
  #plants - nonvascular
  output$esp_plantNonvasc1 <- renderText(esp_get_nonvasc(input$esp_parkChoice1))
  output$esp_plantNonvasc2 <- renderText(esp_get_nonvasc(input$esp_parkChoice2))
  #algae and fungi
  output$esp_algae1 <- renderText(esp_get_algae(input$esp_parkChoice1))
  output$esp_algae2 <- renderText(esp_get_algae(input$esp_parkChoice2))
  output$esp_fungi1 <- renderText(esp_get_fungi(input$esp_parkChoice1))
  output$esp_fungi2 <- renderText(esp_get_fungi(input$esp_parkChoice2))
  ###  PLAIN TEXT MAIN PAGE  ###
  
  ##PLAIN TEXT SECTION 1 - INFO##
  output$esp_parkInfoPlain <- renderUI({esp_get_park_info(input$esp_parkChoice)})
  
  ##PLAIN TEXT SECTION 2 - ANIMALS##
  output$esp_animalInfoPlain <- renderUI({esp_get_animal_header(input$esp_parkChoice)})
  #vertebrates info
  output$esp_vertInfoPlain <- renderUI({esp_get_vert_info_plain(input$esp_parkChoice)})
  #rankings
  output$esp_rankInfoPlain <- renderUI({esp_get_rank_info_plain(input$esp_parkChoice)})
  #invertebrates info
  output$esp_invertInfoPlain <- renderUI({esp_get_invert_info_plain(input$esp_parkChoice)})
  #invertebrates information
  output$esp_invertDefsPlain <- renderUI({esp_get_invert_defs_plain(input$esp_parkChoice)})
  
  ##PLAIN TEXT SECTION 3 - NON-ANIMALS##
  #plants
  output$esp_plantVascPlain <- renderText(esp_get_vasc_plain(input$esp_parkChoice))
  output$esp_plantNonvascPlain <- renderText(esp_get_nonvasc_plain(input$esp_parkChoice))
  #algae and fungi
  output$esp_algaePlain <- renderText(esp_get_algae_plain(input$esp_parkChoice))
  output$esp_fungiPlain <- renderText(esp_get_fungi_plain(input$esp_parkChoice))
  
  
  ###  PLAIN TEXT COMPARE TWO PARKS PAGE  ###
  #park info
  output$esp_parkInfoPlain1 <- renderText(esp_get_park1_info(input$esp_parkChoice1))
  output$esp_parkInfoPlain2 <- renderText(esp_get_park2_info(input$esp_parkChoice2))
  
  ##SECTION 2 - ANIMALS##
  output$esp_animalInfoPlain1 <- renderUI({esp_get_animal_header2(input$esp_parkChoice1)})
  output$esp_animalInfoPlain2 <- renderUI({esp_get_animal_header2(input$esp_parkChoice2)})
  #vertebrates info
  output$esp_vertInfoPlain1 <- renderUI({esp_get_vert_info_plain2(input$esp_parkChoice1)})
  output$esp_vertInfoPlain2 <- renderUI({esp_get_vert_info_plain2(input$esp_parkChoice2)})
  #rankings
  output$esp_rankInfoPlain1 <- renderUI({esp_get_rank_info_plain2(input$esp_parkChoice1)})
  output$esp_rankInfoPlain2 <- renderUI({esp_get_rank_info_plain2(input$esp_parkChoice2)})
  #invertebrates info
  output$esp_invertInfoPlain1 <- renderUI({esp_get_invert_info_plain2(input$esp_parkChoice1)})
  output$esp_invertInfoPlain2 <- renderUI({esp_get_invert_info_plain2(input$esp_parkChoice2)})
  
  ##SECTION 3 - NON-ANIMALS##
  #plants - vascular
  output$esp_plantVascPlain1 <- renderText(esp_get_vasc_plain2(input$esp_parkChoice1))
  output$esp_plantVascPlain2 <- renderText(esp_get_vasc_plain2(input$esp_parkChoice2))
  #plants - nonvascular
  output$esp_plantNonvascPlain1 <- renderText(esp_get_nonvasc_plain2(input$esp_parkChoice1))
  output$esp_plantNonvascPlain2 <- renderText(esp_get_nonvasc_plain2(input$esp_parkChoice2))
  #algae
  output$esp_algaePlain1 <- renderText(esp_get_algae_plain2(input$esp_parkChoice1))
  output$esp_algaePlain2 <- renderText(esp_get_algae_plain2(input$esp_parkChoice2))
  #fungi
  output$esp_fungiPlain1 <- renderText(esp_get_fungi_plain2(input$esp_parkChoice1))
  output$esp_fungiPlain2 <- renderText(esp_get_fungi_plain2(input$esp_parkChoice2))
  
  ###  PLAIN TEXT COMPARE TWO PARKS PAGE  ###
  #park info
  output$parkInfoPlain1 <- renderText(get_park1_info(input$parkChoice1))
  output$parkInfoPlain2 <- renderText(get_park2_info(input$parkChoice2))
  
  ##SECTION 2 - ANIMALS##
  output$animalInfoPlain1 <- renderUI({get_animal_header2(input$parkChoice1)})
  output$animalInfoPlain2 <- renderUI({get_animal_header2(input$parkChoice2)})
  #vertebrates info
  output$vertInfoPlain1 <- renderUI({get_vert_info_plain2(input$parkChoice1)})
  output$vertInfoPlain2 <- renderUI({get_vert_info_plain2(input$parkChoice2)})
  #rankings
  output$rankInfoPlain1 <- renderUI({get_rank_info_plain2(input$parkChoice1)})
  output$rankInfoPlain2 <- renderUI({get_rank_info_plain2(input$parkChoice2)})
  #invertebrates info
  output$invertInfoPlain1 <- renderUI({get_invert_info_plain2(input$parkChoice1)})
  output$invertInfoPlain2 <- renderUI({get_invert_info_plain2(input$parkChoice2)})

  ##SECTION 3 - NON-ANIMALS##
  #plants - vascular
  output$plantVascPlain1 <- renderText(get_vasc_plain2(input$parkChoice1))
  output$plantVascPlain2 <- renderText(get_vasc_plain2(input$parkChoice2))
  #plants - nonvascular
  output$plantNonvascPlain1 <- renderText(get_nonvasc_plain2(input$parkChoice1))
  output$plantNonvascPlain2 <- renderText(get_nonvasc_plain2(input$parkChoice2))
  #algae
  output$algaePlain1 <- renderText(get_algae_plain2(input$parkChoice1))
  output$algaePlain2 <- renderText(get_algae_plain2(input$parkChoice2))
  #fungi
  output$fungiPlain1 <- renderText(get_fungi_plain2(input$parkChoice1))
  output$fungiPlain2 <- renderText(get_fungi_plain2(input$parkChoice2))
}

shinyApp(ui = ui, server = server)
