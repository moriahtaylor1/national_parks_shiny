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
park_stats <- read.csv(data_path)

###WRANGLING###
#get rid of x column
park_stats <- park_stats[,-1]
#single out parks with missing data in order to condition output functions
##no nonvascular plants
no_nonvasc_data <- park_stats %>% filter(count_nonvascular==0)
no_nonvasc_list <- no_nonvasc_data$park_name
##no algae
no_algae_data <- park_stats %>% filter(count_algae==0)
no_algae_list <- no_algae_data$park_name
##no fungi
no_fungi_data <- park_stats %>% filter(count_fungi==0)
no_fungi_list <- no_fungi_data$park_name

###PLOTTING LOGISTICS###
#add fonts
font_add(family = "title", "fonts/OrelegaOne-Regular.ttf")
font_add(family = "title2", "fonts/Merriweather-Regular.ttf")
showtext_auto()
loadfonts(device = "win")
#colors
vert_colors <- c("#AAD213", "#7b49a1", "#037FD0", "#E68200", "#036D50")
#themes
plot_theme <- theme(
  # no title, will be labeled on webpage
  plot.title = element_blank(),

  # panel and plot background
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "#3d4732", color="#1b2313", size=3),
  
  # axis
  axis.title.y = element_text(family="title", size=20, color="white"),
  axis.text.y = element_blank(),
  axis.text.x = element_text(family="title2", size=14, color="white"),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  
  #no legend
  legend.position = "none"
)
#theme for side-by-side comparison
plot_theme2 <- theme(
  # no title, will be labeled on webpage
  plot.title = element_blank(),
  
  # panel and plot background
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "#4e5d6c"),
  
  # axis
  axis.title.y = element_text(family="title", size=20, color="white"),
  axis.text.y = element_blank(),
  #rotate labels so they fit in side-by-side comparison
  axis.text.x = element_text(family="title2", size=14, color="white", angle=30),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  
  #no legend
  legend.position = "none"
)


##PLOTTING FUNCTIONS##
#VERTEBRATES
vert_plot <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  vert_counts <- park_data[1,8:12]
  #transpose columns to be rows
  vert_data <- transpose(vert_counts)
  vert_data$category <- c("Mammal", "Bird", "Reptile", "Amphibian", "Fish")
  colnames(vert_data) <- c("n", "category")
  plot_max <- max(vert_data$n)+50
  #ggplot
  ggplot(data=vert_data, aes(x=category, y=n, fill=category)) + 
    geom_bar(stat="identity") + ylim(0,plot_max) +
    geom_text(aes(label=n, family="title2"), vjust=-0.4, size=6, color="white") +
    scale_x_discrete(limits=c("Mammal", "Bird", "Reptile", "Amphibian", "Fish")) +
    scale_fill_manual(values=vert_colors) +
    labs(title="VERTEBRATES") +
    ylab("")+xlab("")+plot_theme
}

#ROTATE LABELS FOR SIDE-BY-SIDE COMPARISON
vert_plot2 <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  vert_counts <- park_data[1,8:12]
  #transpose columns to be rows
  vert_data <- transpose(vert_counts)
  vert_data$category <- c("Mammal", "Bird", "Reptile", "Amphibian", "Fish")
  colnames(vert_data) <- c("n", "category")
  plot_max <- max(vert_data$n)+50
  #ggplot
  ggplot(data=vert_data, aes(x=category, y=n, fill=category)) + 
    geom_bar(stat="identity") + ylim(0,plot_max) +
    geom_text(aes(label=n, family="title2"), vjust=-0.4, size=6, color="white") +
    scale_x_discrete(limits=c("Mammal", "Bird", "Reptile", "Amphibian", "Fish")) +
    scale_fill_manual(values=vert_colors) +
    labs(title="VERTEBRATES") +
    ylab("")+xlab("")+plot_theme2
}

##UNIT SCALE FUNCTION FROM R-BLOGGERS
unit.scale = function(x) (x - min(x)) / (max(x) - min(x))

#INVERTEBRATES
invert_tbl <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  invert_counts <- park_data[1,13:17]
  invert_data <- transpose(invert_counts)
  invert_data$category <- c("Insect", "Arachnid", "Gastropod", "Shellfish", "Other")
  colnames(invert_data) <- c("Count", "Category")
  #reorder columns
  invert_table <- invert_data[,c(2,1)]
  invert_table[invert_table == 0] <- "NA"
  #create formattable object
  formattable(invert_table,
              align =c("l","r"), 
              list(area(col=1:2) ~ formatter("span", style = x ~ style(font-size:"30px"))))
}

###GETTING INFO###
get_park_info <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  park_data$acres <- format(park_data$acres, big.mark=",")
  park_data$kilos_sq <- format(round(park_data$meters_sq/1000,2), big.mark=",")
  park_data$kilos_sq <- as.character(park_data$kilos_sq)
  park_data$latitude <- as.character(park_data$latitude)
  park_data$longitude <- as.character(park_data$longitude)
  line1 <- paste0("<big><big><big><big><b>", parkName, "</big></big></big></big></b>")
  line2 <- paste0("<big><big><big><em>", park_data$state_long, "</big></big></big></em>")
  line3 <- paste0("<big><big>", park_data$acres, " acres | ", park_data$kilos_sq, " sq kilometers", "</big></big>")
  line4 <- paste0("<big><big><b>Location: (", park_data$latitude, ", ", park_data$longitude, ")", "</b></big></big>")
  return(HTML(paste(line1, line2, line3, line4, sep="<br/>")))
}

get_park1_info <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  park_data$acres <- format(park_data$acres, big.mark=",")
  park_data$kilos_sq <- format(round(park_data$meters_sq/1000,2), big.mark=",")
  park_data$kilos_sq <- as.character(park_data$kilos_sq)
  park_data$latitude <- as.character(park_data$latitude)
  park_data$longitude <- as.character(park_data$longitude)
  line1 <- paste0("<big><big><big><big><b>Park 1: ", parkName, "</big></big></big></big></b>")
  line2 <- paste0("<big><big><big><em>", park_data$state_long, "</big></big></big></em>")
  line3 <- paste0("<big><big>", park_data$acres, " acres | ", park_data$kilos_sq, " sq kilometers", "</big></big>")
  line4 <- paste0("<big><big><b>Location: (", park_data$latitude, ", ", park_data$longitude, ")", "</b></big></big>")
  return(HTML(paste(line1, line2, line3, line4, sep="<br/>")))
}

get_park2_info <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  park_data$acres <- format(park_data$acres, big.mark=",")
  park_data$kilos_sq <- format(round(park_data$meters_sq/1000,2), big.mark=",")
  park_data$kilos_sq <- as.character(park_data$kilos_sq)
  park_data$latitude <- as.character(park_data$latitude)
  park_data$longitude <- as.character(park_data$longitude)
  line1 <- paste0("<big><big><big><big><b>Park 2: ", parkName, "</big></big></big></big></b>")
  line2 <- paste0("<big><big><big><em>", park_data$state_long, "</big></big></big></em>")
  line3 <- paste0("<big><big>", park_data$acres, " acres | ", park_data$kilos_sq, " sq kilometers", "</big></big>")
  line4 <- paste0("<big><big><b>Location: (", park_data$latitude, ", ", park_data$longitude, ")", "</b></big></big>")
  return(HTML(paste(line1, line2, line3, line4, sep="<br/>")))
}


##ANIMAL HEADING##
get_animal_header <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  park_data$animals_native_percent <- as.character(park_data$animals_native_percent)
  park_data$animals_endanger <- as.character(park_data$animals_endanger)
  line1 <- paste0("<big><big><big><big><b>", "ANIMALS", "</big></big></big></big></b>")
  line2 <- paste0("<big>", park_data$animals_native_percent, "% native species", "</big>")
  line3 <- paste0("<big>",park_data$animals_endanger, " threatened or endangered species", "</big>")
  return(HTML(paste(line1, line2, line3, sep="<br/>")))
}

##GET RANKS##
get_all_ranks <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  park_data$rank_mammal <- as.character(park_data$rank_mammal)
  park_data$rank_bird <- as.character(park_data$rank_bird)
  park_data$rank_reptile <- as.character(park_data$rank_reptile)
  park_data$rank_amphibian <- as.character(park_data$rank_amphibian)
  park_data$rank_fish <- as.character(park_data$rank_fish)
  line1 <- "<h3><b>RANKINGS</b></h3>"
  line2 <- paste0("<h3 style=color:#E68200>Mammals -- #", park_data$rank_mammal, "</h3>")
  line3 <- paste0("<h3 style=color:#7b49a1>Birds -- #", park_data$rank_bird, "</h3>")
  line4 <- paste0("<h3 style=color:#036D50>Reptiles -- #", park_data$rank_reptile, "</h3>")
  line5 <- paste0("<h3 style=color:#AAD213>Amphibians -- #", park_data$rank_amphibian, "</h3>")
  line6 <- paste0("<h3 style=color:#037FD0>Fish -- #", park_data$rank_fish, "</h3>")
  return(HTML(paste(line1, line2, line3, line4, line5, line6, sep="")))
}

##INVERTEBRATES INFO##
get_invert_defs <- function(){
  line1 <- "<h4 style=color:#93998d><b><em>In case you didn't know...</em></b></h4>"
  line2 <- "<h4 style=color:#7d8476><em>Arachnids include Spiders and Scorpions</em></h4>"
  line3 <- "<h4 style=color:#7d8476><em>Gastropods include Snails and Slugs</em></h4>"
  line4 <- "<h4 style=color:#7d8476><em>Shellfish include Crabs, Lobsters, and Shrimp</em></h4>"
  return(HTML(paste(line1, line2, line3, line4, sep="")))
}

##PLANTS NUMBERS##
get_vasc <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  park_data$count_vascular <- as.character(park_data$count_vascular)
  return(paste("  |", park_data$count_vascular, sep=" "))
}

get_nonvasc <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  if (park_data$count_nonvascular == 0){
    return("| Unknown")
  }
  else{
    park_data$count_nonvascular <- as.character(park_data$count_nonvascular)
    return(paste("| ", park_data$count_nonvascular, sep=" "))
  }
}

##ALGAE AND FUNGI NUMBERS##
get_algae <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  if (park_data$count_algae == 0){
    return("| Unknown")
  }
  else{
    park_data$count_algae <- as.character(park_data$count_algae)
    return(paste("| ", park_data$count_algae, sep=" "))
  }
}

get_fungi <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  if (park_data$count_fungi == 0){
    return("| Unknown")
  }
  else{
    park_data$count_fungi <- as.character(park_data$count_fungi)
    return(paste("| ", park_data$count_fungi, sep=" "))
  }
}

