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

#load in data
data_path <- paste0(getwd(), "/output_data/data.csv")
df <- read.csv(data_path)

shinyUI(fluidPage(theme = shinytheme("cerulean"),
        #title ---
        titlePanel(column(tags$img(src="nps_icon.png",width="100px",height="100px"),width=1),
                   column("Biodiversity in U.S. National Parks")),
        navbarPage(
                #main page ---
                tabPanel("Main"),
                   
                   
                   ##text blurb ---
                   fluidRow(
                   br(),
                   column(p("This data was made available by The National Park Service and contains animal and plant species identified in 
                              individual national parks and verified by evidence: observations, vouchers, or reports that document the presence
                              of a species in a park [1]. This app will allow you to explore this data via bar charts, tables, and generated 
                              statistics.", style = "text-align:justify;color:black;background-color:'#d79975';padding:15px,border-radius:10px"), width=12)),
                   br(),
                   
                   ##select and submit buttons ---
                   fluidRow(column(selectInput("parkChoice", label = "Select a park", choices = df$park_name), uiOutput("parkName"), width=3),
                            column(submitButton(text = "Submit"), width=1)),
                
                   ##park information ---
                   fluidRow(),    #park name ~ bold
                   fluidRow(),    #state ~ italics
                   fluidRow(),    #acres | meters sq
                   fluidRow(),    #location: (latitude, longitude)
                   
                   br(),
                   br(),
                   ##animals heading
                   fluidRow(),
                   ##vertebrates heading
                   fluidRow(),
                   ##vertebrates plot
                   fluidRow(
                           #plot
                           column(width=7),
                           #empty column
                           column(width=1),
                           #rankings
                           column(width=4)),
                   #invertebrates heading
                   fluidRow(),
                   ##invertebrates table + definitions
                   fluidRow( #table
                             column(width=5),
                             #empty column
                             column(width=3),
                             #definitions
                             column(width=4)),
                   #plants heading
                   fluidRow(),
                   ##plants plot + definitions
                   fluidRow(),
                   #algae/fungi heading
                   fluidRow(),
                   #algae/fungi plot
                   fluidRow(),
                   #disclaimer
                   fluidRow(),
                   
                   tabPanel("Compare two parks"),
                   fluidRow(),
                   tabPanel("FAQ"),
                   fluidRow(),
                   tabPanel("About"),
                   fluidRow())))


























ui <- fluidPage(
        #header
        headerPanel("Biodiversity in U.S. National Parks")
        #tabs
        fluidRow()
        #text blurb
        fluidRow()
        #user input (select and submit button)
        fluidRow(
          selectInput()
          submitButton()
        )
        #park information
        fluidRow()
        #animals heading + ranking
        fluidRow()
        #vertebrates heading + ranking
        fluidRow()
        ##vertebrates plot
        fluidRow()
        #invertebrates heading + ranking
        fluidRow()
        ##invertebrates plot + descriptions
        fluidRow()
        #plants heading + ranking
        fluidRow()
        ##plants plot + definitions
        fluidRow() 
        #algae/fungi heading + ranking
        fluidRow()
        #algae/fungi plot
        fluidRow()
        #disclaimer
        fluidRow()
)