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
library(tableHTML)

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
        includeCSS("www/stylize.css"),
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
                   fluidRow(column(h3("VERTEBRATES", style = "color:#c56c39;text-align:left;font-weight:bold;padding:1px"), width=12)),
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
                   fluidRow(column(h3("INVERTEBRATES", style = "color:#c56c39;text-align:left;font-weight:bold;padding:1px"), width=12)),
                   ##invertebrates table + definitions ---
                   fluidRow( #empty column
                             column(width=1),
                             #invertebrate counts
                             column(htmlOutput(outputId="invertTable"), width=5),
                             #empty column
                             column(width=2),
                             #definitions
                             column(htmlOutput("invertDefs"), width=4)),
                   
                   hr(style="border-color:lightgrey"),
                   
                   ##SECTION 3 - NON-ANIMALS##
                   #plants heading ---
                   fluidRow(column(h3("PLANTS", style = "color:#c56c39;text-align:left;font-weight:bold;padding:1px"), width=12)),
                   ##plants text + definitions ---
                   fluidRow(column(h4("Species of Vascular Plants", style="text-align:right;padding:20px"), width=3),    #no of species of vascular plants
                            column(h1(textOutput("plantVasc")), width=2),    # | number
                            column(width=1),
                            column(h4("Species of Non-Vascular Plants", style="text-align:right;padding:20px"), width=3),    #no of species of nonvascular plants
                            column(h1(textOutput("plantNonvasc")), width=3)),    # | number or 
                   
                   hr(style="border-color:lightgrey"),
                   
                   #algae/fungi heading ---
                   fluidRow(column(h3("ALGAE AND FUNGI", style = "color:#c56c39;text-align:left;font-weight:bold;padding:1px"), width=12)),
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
                       fluidRow(column(h3("VERTEBRATES", style = "color:#c56c39;text-align:center;font-weight:bold;padding:1px"), width=12)),
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
                       fluidRow(column(h3("INVERTEBRATES", style = "color:#c56c39;text-align:center;font-weight:bold;padding:1px"), width=12)),
                       ##invertebrates tables
                       fluidRow(
                               column(width=1),    #empty space
                               column(htmlOutput(outputId="invertTable1"), width=3), #first park table
                               column(width=3),    #empty space
                               column(htmlOutput(outputId="invertTable2"), width=3)), #second park table
                       
                       
                       hr(style="border-color:lightgrey"),
                       
                       ##SECTION 3 - NON-ANIMALS##
                       #plants heading ---
                       fluidRow(column(h1("PLANTS", style = "color:#c56c39;text-align:center;font-weight:bold;padding:1px"), width=12)),
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
                       fluidRow(column(h1("ALGAE AND FUNGI", style = "color:#c56c39;text-align:center;font-weight:bold;padding:1px"), width=12)),
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
              tabPanel(title=HTML("<big><b>FAQ</b></big>"), value=
                       br(),
                       #question 1
                       fluidRow(h2(HTML("Where does this data come from?"), style = "color:#c56c39")),
                       fluidRow(h4(HTML('This data was made available by the National Park Service and posted on Kaggle.com<sup>[3]</sup> (<a target="_blank" href="https://www.kaggle.com/nationalparkservice/park-biodiversity">link</a>)'))),
                       #question 2
                       fluidRow(h2(HTML("Is this site officially affiliated with the National Park Service?"), style = "color:#c56c39")),
                       fluidRow(h4(HTML('No, this site is not officially affiliated with the National Park Service. It was created independently as a submission to the <a target="_blank" href="https://blog.rstudio.com/2021/03/11/time-to-shiny/">2021 Shiny Contest</a>.'))),
                       #question 3
                       fluidRow(h2(HTML("Why are there zero or unknown numbers of species in certain categories?"), style = "color:#c56c39")),
                       fluidRow(h4(HTML('The following categories will often have missing or incomplete data: invertebrates, non-vascular plants, fungi, and algae. We can tell this data is missing 
                                        because the number of species listed in the raw data will say “zero” when, rest assured, there is an abundance of species in these categories found in 
                                        nearly all biomes. In fact, because of their sheer abundance and their lessened visibility (it is much easier to see antelope than fruit flies, yes?), it is 
                                        harder to keep accurate records of these species. Because of this, data is often missing for these categories.'))),
                       #question 4
                       fluidRow(h2(HTML("Why are there rankings for vertebrates, but not for other categories?"), style = "color:#c56c39")),
                       fluidRow(h4(HTML("There is complete data for each park in all of the vertebrate categories which allowed for accurate rankings to be determined. Due to the amount of missing 
                                        data in the other categories, it would not be statistically sound to assign rankings."))),
                       #question 5
                       fluidRow(h2(HTML("Why are there plain text and Spanish versions of this app?"), style = "color:#c56c39")),
                       fluidRow(h4(HTML("Accessibility was a big priority for the developer. Creating plain text versions of web pages allows them to be read more easily by screenreaders. Additionally,
                                         there are Spanish versions provided because there are many Spanish-speakers in the United States and the developer has a decent knowledge of the Spanish language, 
                                         so she was able to and motivated to create Spanish versions for those Spanish-speaking Americans that may want to learn more about their National Parks."))),
                       #question 6
                       fluidRow(h2(HTML("Who made the logo in the top left corner of the page?"), style = "color:#c56c39")),
                       fluidRow(h4(HTML("This app's logo was created by the developer herself. Utilizing the color palette from the official NPS logo, she created a logo in the shape of a hexagon as an 
                                        homage to R package logos and depicted a growing tree to symbolize how the NPS nurtures our parks and encourages growth both physically and culturally."))),
                       #question 2
                       #pregunta 1
                       fluidRow(h2(HTML("¿De dónde provienen esta información?"), style = "color:#81A85D")),
                       fluidRow(h4(HTML('Esta información fue proporcionados por el Servicio de Parques Nacionales y publicados en Kaggle.com<sup>[3]</sup> (<a target="_blank" href="https://www.kaggle.com/nationalparkservice/park-biodiversity">link</a>)'))),
                       #pregunta 2
                       fluidRow(h2(HTML("¿Este sitio está oficialmente afiliado al Servicio de Parques Nacionales?"), style = "color:#81A85D")),
                       fluidRow(h4(HTML('No, este sitio no está oficialmente afiliado al Servicio de Parques Nacionales. Fue creado independientemente como una presentación al <a target="_blank" href="https://blog.rstudio.com/2021/03/11/time-to-shiny/">Shiny Contest de 2021</a>.'))),
                       #pregunta 3
                       fluidRow(h2(HTML("¿Por qué hay un número cero o desconocido de especies en categorías específicas?"), style = "color:#81A85D")),
                       fluidRow(h4(HTML('Las siguientes categorías a menudo tienen datos incompletos o faltantes: Invertebrados, plantas no vasculares, hongos y algas. Sabemos que faltan estos datos porque el número de especies enumeradas en los datos brutos dirá “cero” cuando, de hecho, 
                                        hay una abundancia de especies en estas categorías que se encuentran en casi todos los biomas. De hecho, debido a su gran abundancia y a su menor visibilidad (es mucho más fácil ver el antílope que las moscas, sí?), es más difícil mantener registros 
                                        precisos de estas especies. Debido a esto, a menudo faltan información para estas categorías.'))),
                       #pregunta 4
                       fluidRow(h2(HTML("¿Por qué hay clasificaciones para vertebrados, pero no para otras categorías?"), style = "color:#81A85D")),
                       fluidRow(h4(HTML("Hay información completa para cada parque en todas las categorías de vertebrados que permiten determinar clasificaciones precisas. Debido a la cantidad de datos que faltan en las otras categorías, no sería estadísticamente correcto asignar clasificaciones."))),
                       #pregunta 5
                       fluidRow(h2(HTML("¿Por qué hay versiones en texto sin formato y español para esta aplicación?"), style = "color:#81A85D")),
                       fluidRow(h4(HTML("La accesibilidad era una gran prioridad para el desarrollador. La creación de versiones de texto sin formato de páginas web permite que sean leídos más fácilmente por los lectores de pantalla. Además, hay versiones en español proporcionadas porque hay muchos 
                                        hispanohablantes en los Estados Unidos y el desarrollador tiene un conocimiento decente de la lengua española, Así que ella fue capaz y motivada para crear versiones en español para aquellos estadounidenses de habla hispana que pueden querer aprender más sobre 
                                        sus Parques Nacionales."))),
                       #question 6
                       fluidRow(h2(HTML("¿Quién hizo el logotipo en la esquina superior izquierda de la página?"), style = "color:#81A85D")),
                       fluidRow(h4(HTML("El logotipo de esta aplicación fue creado por el propio desarrollador. Utilizando la paleta de colores del logotipo oficial de NPS, creó un logotipo en forma de hexágono como homenaje a los logotipos del paquete R y representó un árbol en crecimiento para 
                                        simbolizar cómo NPS nutre nuestros parques y fomenta el crecimiento tanto física como culturalmente.")))),
                ##ABOUT PAGE##
              tabPanel(title=HTML("<big><b>ABOUT</b></big>"), value=
                       br(),
                       fluidRow(h2(HTML("NATIONAL PARK SERVICE"))),
                       fluidRow(h4(HTML("The National Park Service (NPS) is a federal agency created in the year 1916 that manages all national parks, many national monuments, and other conservation and historical properties<sup>[2]</sup>. 
                                        Their mission is to preserve the natural and cultural resources, as well as the values of the National Park System for the enjoyment, education, and inspiration of this and future generations. 
                                        The Park Service cooperates with partners to extend the benefits of resource conservation and outdoor recreation throughout the United States and the world<sup>[1]</sup>. In 2018, the NPS employed 
                                        approximately 12,363 employees who oversaw 423 units, of which 63 were designated national parks<sup>[2]</sup>."))),
                       br(),
                       fluidRow(h2(HTML("RESOURCES"))),
                       fluidRow(h4(HTML('This app was created primarily using the <a target="_blank" href="https://shiny.rstudio.com/">R Shiny Package</a>. The code can be found on the developer`s GitHub repository 
                                        <a target = "_blank" href="https://github.com/moriahtaylor1/national_parks_shiny">here</a>.'))),
                       br(),
                       fluidRow(h2(HTML("SOURCES CITED"))),
                       fluidRow(h4(HTML('[1] <a target="_blank" href="https://www.nps.gov/index.htm">Official National Park Service website</a>'))),
                       fluidRow(h4(HTML('[2] <a target="_blank" href="https://en.wikipedia.org/wiki/National_Park_Service">National Park Service Wikipedia page</a>'))),
                       fluidRow(h4(HTML('[3] <a target="_blank" href="https://www.kaggle.com/nationalparkservice/park-biodiversity">Kaggle.com database</a>'))),
                       br(),
                       fluidRow(h2(HTML("MEET THE DEVELOPER"))),
                       #my photo
                       fluidRow(HTML('<img width="200" src="m3.jpg"/>')),
                       #caption
                       fluidRow(h4(HTML("Moriah Taylor<br>She/her"))),
                       br(),
                       fluidRow(h4(HTML("Moriah is an aspiring data scientist with a passion for programming. She has a wide range of interests including data visualization, machine learning, data journalism, education, geology, and video games. 
                                        She graduated in December 2019 from Duke University with a bachelor's degree in Statistical Science."))),
                       br(),
                       fluidRow(h4(HTML("Links:"))),
                       fluidRow(h4(HTML('<ul>
                                        <li><a target = "_blank" href="https://github.com/moriahtaylor1">GitHub</a></li>
                                        <li><a target = "_blank" href="https://twitter.com/moriah_taylor58">Twitter</a></li>
                                        <li><a target = "_blank" href="https://www.linkedin.com/in/moriah-taylor/">LinkedIn</a></li>
                                        <li><a target = "_blank" href="https://www.twitch.tv/moriah_streamr">Twitch</a></li>')))
                       ),
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
                                  fluidRow(selectInput("parkChoicePlain1", label = "Select a park", choices = df$park_name), width=4),
                                  fluidRow(selectInput("parkChoicePlain2", label = "Select a second park", choices = df$park_name), width=4),
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
                                  fluidRow(column(h3("VERTEBRADOS", style = "color:#c56c39;text-align:left;font-weight:bold;padding:1px"), width=12)),
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
                                  fluidRow(column(h3("INVERTEBRADOS", style = "color:#c56c39;text-align:left;font-weight:bold;padding:1px"), width=12)),
                                  ##invertebrates table + definitions ---
                                  fluidRow( #table
                                    column(htmlOutput(outputId="esp_invertTable"), width=5),
                                    #empty column
                                    column(width=3),
                                    #definitions
                                    column(htmlOutput("esp_invertDefs"), width=4)),
                                  
                                  hr(style="border-color:lightgrey"),
                                  
                                  ##SECTION 3 - NON-ANIMALS##
                                  #plants heading ---
                                  fluidRow(column(h3("PLANTAS", style = "color:#c56c39;text-align:left;font-weight:bold;padding:1px"), width=12)),
                                  ##plants text + definitions ---
                                  fluidRow(column(h4("Especies de plantas vasculares", style="text-align:right;padding:20px"), width=3),    #no of species of vascular plants
                                           column(h1(textOutput("esp_plantVasc")), width=2),    # | number
                                           column(width=1),
                                           column(h4("Especies de plantas no vasculares", style="text-align:right;padding:20px"), width=3),    #no of species of nonvascular plants
                                           column(h1(textOutput("esp_plantNonvasc")), width=3)),    # | number or 
                                  
                                  hr(style="border-color:lightgrey"),
                                  
                                  #algae/fungi heading ---
                                  fluidRow(column(h3("ALGAS Y HONGOS", style = "color:#c56c39;text-align:left;font-weight:bold;padding:1px"), width=12)),
                                  #algae/fungi text ---
                                  fluidRow(column(width=1),
                                           column(h4("Especies de algas", style="text-align:right;padding:20px"), width=2),    #no of species of vascular plants
                                           column(h1(textOutput("esp_algae")), width=2),    # | number or unknown
                                           column(width=2),
                                           column(h4("Especies de hongos", style="text-align:right;padding:20px"), width=2),    #no of species of nonvascular plants
                                           column(h1(textOutput("esp_fungi")), width=2)),    # | number or unknown
                                  
                                  
                                  hr(style="border-color:lightgrey"),
                                  HTML("<big><b>SEGUNDA PÁGINA – COMPARA DOS PARQUES</b></big>"),
                                  br(),
                                  ##select and submit buttons ---
                                  #pick two parks
                                  fluidRow(column(width=1),
                                           column(selectInput("esp_parkChoice1", label = "Escoja un parque", choices = esp_df$park_name), width=4),
                                           column(width=2),
                                           column(selectInput("esp_parkChoice2", label = "Escoja el segundo parque", choices = esp_df$park_name), width=4),
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
                                  fluidRow(column(h3("VERTEBRADOS", style = "color:#c56c39;text-align:center;font-weight:bold;padding:1px"), width=12)),
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
                                  fluidRow(column(h3("INVERTEBRADOS", style = "color:#c56c39;text-align:center;font-weight:bold;padding:1px"), width=12)),
                                  ##invertebrates tables
                                  fluidRow(
                                    column(width=1),    #empty space
                                    column(htmlOutput(outputId="esp_invertTable1"), width=3), #first park table
                                    column(width=3),    #empty space
                                    column(htmlOutput(outputId="esp_invertTable2"), width=3)), #second park table
                                  
                                  
                                  hr(style="border-color:lightgrey"),
                                  
                                  ##SECTION 3 - NON-ANIMALS##
                                  #plants heading ---
                                  fluidRow(column(h1("PLANTAS", style = "color:#c56c39;text-align:center;font-weight:bold;padding:1px"), width=12)),
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
                                  fluidRow(column(h1("ALGAS Y HONGOS", style = "color:#c56c39;text-align:center;font-weight:bold;padding:1px"), width=12)),
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
                                  fluidRow(selectInput("esp_parkChoicePlain", label = "Escoja un parque", choices = esp_df$park_name), width=4),
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
                                  fluidRow(column(h1("SEGUNDA PÁGINA – COMPARA DOS PARQUES", style = "text-align:left;padding:1px"), width=12)),
                                  br(),
                                  
                                  ##select and submit buttons ---
                                  fluidRow(column(h2("ESCOJA DOS PARQUES", style = "text-align:left;padding:1px"), width=12)),
                                  br(),
                                  fluidRow(selectInput("esp_parkChoicePlain1", label = "Escoja un parque", choices = esp_df$park_name), width=4),
                                  fluidRow(selectInput("esp_parkChoicePlain2", label = "Escoja el segundo parque", choices = esp_df$park_name), width=4),
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
        #invertebrates
        output$invertTable <- renderUI({invert_tbl(input$parkChoice)})
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
        
        #invertebrates
        output$invertTable1 <- renderUI({invert_tbl(input$parkChoice1)})
        output$invertTable2 <- renderUI({invert_tbl(input$parkChoice2)})
        
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
  #invertebrates
  output$esp_invertTable <- renderUI({esp_invert_tbl(input$esp_parkChoice)})
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
  
  #invertebrates
  output$esp_invertTable1 <- renderUI({esp_invert_tbl(input$esp_parkChoice1)})
  output$esp_invertTable2 <- renderUI({esp_invert_tbl(input$esp_parkChoice2)})
  
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
  output$esp_parkInfoPlain <- renderUI({esp_get_park_info(input$esp_parkChoicePlain)})
  
  ##PLAIN TEXT SECTION 2 - ANIMALS##
  output$esp_animalInfoPlain <- renderUI({esp_get_animal_header(input$esp_parkChoicePlain)})
  #vertebrates info
  output$esp_vertInfoPlain <- renderUI({esp_get_vert_info_plain(input$esp_parkChoicePlain)})
  #rankings
  output$esp_rankInfoPlain <- renderUI({esp_get_rank_info_plain(input$esp_parkChoicePlain)})
  #invertebrates info
  output$esp_invertInfoPlain <- renderUI({esp_get_invert_info_plain(input$esp_parkChoicePlain)})
  #invertebrates information
  output$esp_invertDefsPlain <- renderUI({esp_get_invert_defs_plain(input$esp_parkChoicePlain)})
  
  ##PLAIN TEXT SECTION 3 - NON-ANIMALS##
  #plants
  output$esp_plantVascPlain <- renderText(esp_get_vasc_plain(input$esp_parkChoicePlain))
  output$esp_plantNonvascPlain <- renderText(esp_get_nonvasc_plain(input$esp_parkChoicePlain))
  #algae and fungi
  output$esp_algaePlain <- renderText(esp_get_algae_plain(input$esp_parkChoicePlain))
  output$esp_fungiPlain <- renderText(esp_get_fungi_plain(input$esp_parkChoicePlain))
  
  
  ###  PLAIN TEXT COMPARE TWO PARKS PAGE  ###
  #park info
  output$esp_parkInfoPlain1 <- renderText(esp_get_park1_info(input$esp_parkChoicePlain1))
  output$esp_parkInfoPlain2 <- renderText(esp_get_park2_info(input$esp_parkChoicePlain2))
  
  ##SECTION 2 - ANIMALS##
  output$esp_animalInfoPlain1 <- renderUI({esp_get_animal_header2(input$esp_parkChoicePlain1)})
  output$esp_animalInfoPlain2 <- renderUI({esp_get_animal_header2(input$esp_parkChoicePlain2)})
  #vertebrates info
  output$esp_vertInfoPlain1 <- renderUI({esp_get_vert_info_plain2(input$esp_parkChoicePlain1)})
  output$esp_vertInfoPlain2 <- renderUI({esp_get_vert_info_plain2(input$esp_parkChoicePlain2)})
  #rankings
  output$esp_rankInfoPlain1 <- renderUI({esp_get_rank_info_plain2(input$esp_parkChoicePlain1)})
  output$esp_rankInfoPlain2 <- renderUI({esp_get_rank_info_plain2(input$esp_parkChoicePlain2)})
  #invertebrates info
  output$esp_invertInfoPlain1 <- renderUI({esp_get_invert_info_plain2(input$esp_parkChoicePlain1)})
  output$esp_invertInfoPlain2 <- renderUI({esp_get_invert_info_plain2(input$esp_parkChoicePlain2)})
  
  ##SECTION 3 - NON-ANIMALS##
  #plants - vascular
  output$esp_plantVascPlain1 <- renderText(esp_get_vasc_plain2(input$esp_parkChoicePlain1))
  output$esp_plantVascPlain2 <- renderText(esp_get_vasc_plain2(input$esp_parkChoicePlain2))
  #plants - nonvascular
  output$esp_plantNonvascPlain1 <- renderText(esp_get_nonvasc_plain2(input$esp_parkChoicePlain1))
  output$esp_plantNonvascPlain2 <- renderText(esp_get_nonvasc_plain2(input$esp_parkChoicePlain2))
  #algae
  output$esp_algaePlain1 <- renderText(esp_get_algae_plain2(input$esp_parkChoicePlain1))
  output$esp_algaePlain2 <- renderText(esp_get_algae_plain2(input$esp_parkChoicePlain2))
  #fungi
  output$esp_fungiPlain1 <- renderText(esp_get_fungi_plain2(input$esp_parkChoicePlain1))
  output$esp_fungiPlain2 <- renderText(esp_get_fungi_plain2(input$esp_parkChoicePlain2))
  
  ###  PLAIN TEXT COMPARE TWO PARKS PAGE  ###
  #park info
  output$parkInfoPlain1 <- renderText(get_park1_info(input$parkChoicePlain1))
  output$parkInfoPlain2 <- renderText(get_park2_info(input$parkChoicePlain2))
  
  ##SECTION 2 - ANIMALS##
  output$animalInfoPlain1 <- renderUI({get_animal_header2(input$parkChoicePlain1)})
  output$animalInfoPlain2 <- renderUI({get_animal_header2(input$parkChoicePlain2)})
  #vertebrates info
  output$vertInfoPlain1 <- renderUI({get_vert_info_plain2(input$parkChoicePlain1)})
  output$vertInfoPlain2 <- renderUI({get_vert_info_plain2(input$parkChoicePlain2)})
  #rankings
  output$rankInfoPlain1 <- renderUI({get_rank_info_plain2(input$parkChoicePlain1)})
  output$rankInfoPlain2 <- renderUI({get_rank_info_plain2(input$parkChoicePlain2)})
  #invertebrates info
  output$invertInfoPlain1 <- renderUI({get_invert_info_plain2(input$parkChoicePlain1)})
  output$invertInfoPlain2 <- renderUI({get_invert_info_plain2(input$parkChoicePlain2)})

  ##SECTION 3 - NON-ANIMALS##
  #plants - vascular
  output$plantVascPlain1 <- renderText(get_vasc_plain2(input$parkChoicePlain1))
  output$plantVascPlain2 <- renderText(get_vasc_plain2(input$parkChoicePlain2))
  #plants - nonvascular
  output$plantNonvascPlain1 <- renderText(get_nonvasc_plain2(input$parkChoicePlain1))
  output$plantNonvascPlain2 <- renderText(get_nonvasc_plain2(input$parkChoicePlain2))
  #algae
  output$algaePlain1 <- renderText(get_algae_plain2(input$parkChoicePlain1))
  output$algaePlain2 <- renderText(get_algae_plain2(input$parkChoicePlain2))
  #fungi
  output$fungiPlain1 <- renderText(get_fungi_plain2(input$parkChoicePlain1))
  output$fungiPlain2 <- renderText(get_fungi_plain2(input$parkChoicePlain2))
}

shinyApp(ui = ui, server = server)
