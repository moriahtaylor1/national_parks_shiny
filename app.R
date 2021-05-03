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
        navbarPage("",     #title = div("Biodiversity in U.S. National Parks", img(src="nps_icon.png", height = "150px", width = "160px"))),
                ##MAIN PAGE##
                tabPanel(HTML("<big><b>MAIN</b></big>"),
                   
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
                             column(htmlOutput("invertInfo"), width=4)),
                   
                   hr(style="border-color:lightgrey"),
                   
                   ##SECTION 3 - NON-ANIMALS##
                   #plants heading ---
                   fluidRow(column(h3("PLANTS", style = "text-align:left;font-weight:bold;padding:1px"), width=12)),
                   ##plants text + definitions ---
                   fluidRow(column(h4("No. of Species of Vascular Plants", style="text-align:right"), width=2),    #no of species of vascular plants
                            column(h1(textOutput("plantVasc")), width=3),    # | number
                            column(h4("No. of Species of Non-Vascular Plants", style="text-align:right"), width=2),    #no of species of nonvascular plants
                            column(h1(textOutput("plantNonvasc")), width=3)),    # | number or 
                   
                   hr(style="border-color:lightgrey"),
                   
                   #algae/fungi heading ---
                   fluidRow(column(h3("ALGAE AND FUNGI", style = "text-align:left;font-weight:bold;padding:1px"), width=12)),
                   #algae/fungi text ---
                   fluidRow(column(h1(textOutput("algae", inline=TRUE)), width=3),    #number or unknown
                            column(h4(HTML("</br>Species of Algae"), style="text-align:left"), width=1),    #species of algae
                            column(width=1),    #empty column
                            column(h1(textOutput("fungi", inline=TRUE)), width=3),    #number or unknown
                            column(h4(HTML("</br>Species of Fungi"),  style="text-align:left"), width=1)),    #species of fungi
                   
                   hr(style="border-color:lightgrey"),
                   
                   ##SECTION 4 - FOOTER##
                   #disclaimer ---
                   fluidRow(column(p(HTML("<em>***This website is not officially affiliated with The National Park Service</em>"), style="color:#7d8476;text-size:10px"), width=12)),
                   
                ##SECOND PAGE##
              tabPanel("Compare two parks",
                       fluidRow("COMPARE TWO PARKS")),
                ##FAQ PAGE##
              tabPanel("FAQ",
                        fluidRow("FAQ")),
                ##ABOUT PAGE##
              tabPanel("About",
                       fluidRow("About")),
                   ))))


server <- function(input, output){
        
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
        output$invertInfo <- renderUI({get_invert_info()})
        
        ##SECTION 3 - NON-ANIMALS##
        #plants
        output$plantVasc <- renderText(get_vasc(input$parkChoice))
        output$plantNonvasc <- renderText(get_nonvasc(input$parkChoice))
        #algae and fungi
        output$algae <- renderText(get_algae(input$parkChoice))
        output$fungi <- renderText(get_fungi(input$parkChoice))
}

shinyApp(ui = ui, server = server)




















