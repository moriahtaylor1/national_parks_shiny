library(tidyverse)
library(lubridate)
#library(extrafont)
#library(showtext)
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
#load in data en espanol
esp_data_path <- paste0(getwd(), "/output_data/parques.csv")
esp_park_stats <- as.data.frame(read.table(esp_data_path, sep=",", header=T))

###WRANGLING###
#get rid of x column
park_stats <- park_stats[,-1]


###PLOTTING LOGISTICS###
#add fonts
font_add(family = "title", "fonts/OrelegaOne-Regular.ttf")
font_add(family = "title2", "fonts/Merriweather-Regular.ttf")
showtext_auto()
#loadfonts(device = "win")

#colors
vert_colors <- c("#AAD213", "#7b49a1", "#037FD0", "#E68200", "#036D50")
esp_vert_colors <- c("#AAD213", "#7b49a1", "#E68200", "#037FD0", "#036D50")
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

##PLAIN TEXT FUNCTIONS##
#in place of vertebrates plot
get_vert_info_plain <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  line1 <- "Number of Species"
  line2 <- paste("- Mammals: ", park_data$count_mammal, sep="")
  line3 <- paste("- Birds: ", park_data$count_bird, sep="")
  line4 <- paste("- Reptiles: ", park_data$count_reptile, sep="")
  line5 <- paste("- Amphibians: ", park_data$count_amphibian, sep="")
  line6 <- paste("- Fish: ", park_data$count_fish, sep="")
  return(HTML(paste(line1, line2, line3, line4, line5, line6, sep="<br/>")))
}
#rankings without formatting
get_rank_info_plain <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  line1 <- "Rankings"
  line2 <- paste("- Mammals: #", park_data$rank_mammal, sep="")
  line3 <- paste("- Birds: #", park_data$rank_bird, sep="")
  line4 <- paste("- Reptiles: #", park_data$rank_reptile, sep="")
  line5 <- paste("- Amphibians: #", park_data$rank_amphibian, sep="")
  line6 <- paste("- Fish: #", park_data$rank_fish, sep="")
  return(HTML(paste(line1, line2, line3, line4, line5, line6, sep="<br/>")))
}
#in place of invertebrate table
get_invert_info_plain <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  park_data[park_data == 0] <- "NA"
  line1 <- "Number of Species"
  line2 <- paste("- Insects: ", park_data$count_insect, sep="")
  line3 <- paste("- Arachnids: ", park_data$count_arachnid, sep="")
  line4 <- paste("- Shellfish: ", park_data$count_shellfish, sep="")
  line5 <- paste("- Gastropods: ", park_data$count_gastropod, sep="")
  line6 <- paste("- Other: ", park_data$count_other, sep="")
  return(HTML(paste(line1, line2, line3, line4, line5, line6, sep="<br/>")))
}
#invertebrate definitions without formatting
get_invert_defs_plain <- function(parkName){
  line1 <- "In case you didn't know..."
  line2 <- "Arachnids include Spiders and Scorpions"
  line3 <- "Gastropods include Snails and Slugs"
  line4 <- "Shellfish include Crabs, Lobsters, and Shrimp"
  return(HTML(paste(line1, line2, line3, line4, sep="<br/>")))
}
#vascular plants
get_vasc_plain <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  park_data$count_vascular <- as.character(park_data$count_vascular)
  return(paste(park_data$count_vascular, "species of vascular plants", sep=" "))
}
#nonvascular plants
get_nonvasc_plain <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  if (park_data$count_nonvascular == 0){
    return("Unknown number of species of non-vascular plants")
  }
  else{
    park_data$count_nonvascular <- as.character(park_data$count_nonvascular)
    return(paste(park_data$count_nonvascular, "species of non-vascular plants", sep=" "))
  }
}

#algae
get_algae_plain <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  if (park_data$count_algae == 0){
    return("- Algae: Unknown number of species")
  }
  else{
    park_data$count_algae <- as.character(park_data$count_algae)
    return(paste("- Algae:", park_data$count_algae, "species", sep=" "))
  }
}
#fungi
get_fungi_plain <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  if (park_data$count_fungi == 0){
    return("- Fungi: Unknown number of species")
  }
  else{
    park_data$count_fungi <- as.character(park_data$count_fungi)
    return(paste("- Fungi:", park_data$count_fungi, "species", sep=" "))
  }
}

##COMPARING TWO PARKS##
#want to include park name in beginning of each output
get_animal_header2 <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  park_data$animals_native_percent <- as.character(park_data$animals_native_percent)
  park_data$animals_endanger <- as.character(park_data$animals_endanger)
  line0 <- park_data$park_name
  line1 <- "ANIMALS"
  line2 <- paste0(park_data$animals_native_percent, "% native species")
  line3 <- paste0(park_data$animals_endanger, " threatened or endangered species")
  return(HTML(paste(line0, line1, line2, line3, sep="<br/>")))
}

#in place of vertebrates plot
get_vert_info_plain2 <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  line0 <- park_data$park_name
  line1 <- "Number of Species"
  line2 <- paste("- Mammals: ", park_data$count_mammal, sep="")
  line3 <- paste("- Birds: ", park_data$count_bird, sep="")
  line4 <- paste("- Reptiles: ", park_data$count_reptile, sep="")
  line5 <- paste("- Amphibians: ", park_data$count_amphibian, sep="")
  line6 <- paste("- Fish: ", park_data$count_fish, sep="")
  return(HTML(paste(line0, line1, line2, line3, line4, line5, line6, sep="<br/>")))
}
#rankings without formatting
get_rank_info_plain2 <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  line0 <- park_data$park_name
  line1 <- "Rankings"
  line2 <- paste("- Mammals: #", park_data$rank_mammal, sep="")
  line3 <- paste("- Birds: #", park_data$rank_bird, sep="")
  line4 <- paste("- Reptiles: #", park_data$rank_reptile, sep="")
  line5 <- paste("- Amphibians: #", park_data$rank_amphibian, sep="")
  line6 <- paste("- Fish: #", park_data$rank_fish, sep="")
  return(HTML(paste(line0, line1, line2, line3, line4, line5, line6, sep="<br/>")))
}
#in place of invertebrate table
get_invert_info_plain2 <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  park_data[park_data == 0] <- "NA"
  line0 <- park_data$park_name
  line1 <- "Number of Species"
  line2 <- paste("- Insects: ", park_data$count_insect, sep="")
  line3 <- paste("- Arachnids: ", park_data$count_arachnid, sep="")
  line4 <- paste("- Shellfish: ", park_data$count_shellfish, sep="")
  line5 <- paste("- Gastropods: ", park_data$count_gastropod, sep="")
  line6 <- paste("- Other: ", park_data$count_other, sep="")
  return(HTML(paste(line0, line1, line2, line3, line4, line5, line6, sep="<br/>")))
}

#vascular plants
get_vasc_plain2 <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  park_data$count_vascular <- as.character(park_data$count_vascular)
  return(paste(park_data$park_name, "has", park_data$count_vascular, "species of vascular plants", sep=" "))
}
#nonvascular plants
get_nonvasc_plain2 <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  if (park_data$count_nonvascular == 0){
    return(paste(park_data$park_name, "has an unknown number of species of non-vascular plants", sep=" "))
  }
  else{
    park_data$count_nonvascular <- as.character(park_data$count_nonvascular)
    return(paste(park_data$count_nonvascular, "species of non-vascular plants", sep=" "))
  }
}

#algae
get_algae_plain2 <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  if (park_data$count_algae == 0){
    return(paste(park_data$park_name, "has an unknown number of species of algae", sep=" "))
  }
  else{
    park_data$count_algae <- as.character(park_data$count_nonvascular)
    return(paste(park_data$park_name, "has", park_data$count_algae, "species of algae", sep=" "))
  }
}
#fungi
get_fungi_plain2 <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  if (park_data$count_fungi == 0){
    return(paste(park_data$park_name, "has an unknown number of species of fungi", sep=" "))
  }
  else{
    park_data$count_fungi <- as.character(park_data$count_nonvascular)
    return(paste(park_data$park_name, "has", park_data$count_fungi, "species of fungi", sep=" "))
  }
}

##PLOTTING FUNCTIONS EN ESPAÑOL##
#VERTEBRATES EN ESPAÑOL
esp_vert_plot <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  vert_counts <- park_data[1,8:12]
  #transpose columns to be rows
  vert_data <- transpose(vert_counts)
  vert_data$category <- c("Mamíferos", "Aves", "Reptiles", "Anfibios", "Peces")
  colnames(vert_data) <- c("n", "category")
  plot_max <- max(vert_data$n)+50
  #ggplot
  ggplot(data=vert_data, aes(x=category, y=n, fill=category)) + 
    geom_bar(stat="identity") + ylim(0,plot_max) +
    geom_text(aes(label=n, family="title2"), vjust=-0.4, size=6, color="white") +
    scale_x_discrete(limits=c("Mamíferos", "Aves", "Reptiles", "Anfibios", "Peces")) +
    scale_fill_manual(values=esp_vert_colors) +
    labs(title="VERTEBRATES") +
    ylab("")+xlab("")+plot_theme
}

#ROTATE LABELS FOR SIDE-BY-SIDE COMPARISON EN ESPAÑOL
esp_vert_plot2 <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  vert_counts <- park_data[1,8:12]
  #transpose columns to be rows
  vert_data <- transpose(vert_counts)
  vert_data$category <- c("Mamíferos", "Aves", "Reptiles", "Anfibios", "Peces")
  colnames(vert_data) <- c("n", "category")
  plot_max <- max(vert_data$n)+50
  #ggplot
  ggplot(data=vert_data, aes(x=category, y=n, fill=category)) + 
    geom_bar(stat="identity") + ylim(0,plot_max) +
    geom_text(aes(label=n, family="title2"), vjust=-0.4, size=6, color="white") +
    scale_x_discrete(limits=c("Mamíferos", "Aves", "Reptiles", "Anfibios", "Peces")) +
    scale_fill_manual(values=vert_colors) +
    labs(title="VERTEBRATES") +
    ylab("")+xlab("")+plot_theme2
}

#INVERTEBRATES EN ESPAÑOL
esp_invert_tbl <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  invert_counts <- park_data[1,13:17]
  invert_data <- transpose(invert_counts)
  invert_data$category <- c("Insectos", "Arácnidos", "Gasterópodos", "Crustáceos ", "Otros")
  colnames(invert_data) <- c("Cuenta", "CCategoría")
  #reorder columns
  invert_table <- invert_data[,c(2,1)]
  invert_table[invert_table == 0] <- "NA"
  #create formattable object
  formattable(invert_table,
              align =c("l","r"), 
              list(area(col=1:2) ~ formatter("span", style = x ~ style(font-size:"30px"))))
}

###GETTING INFO EN ESPAÑOL###
esp_get_park_info <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  park_data$acres <- format(park_data$acres, big.mark=" ")
  park_data$kilos_sq <- format(round(park_data$meters_sq/1000,2), big.mark=" ")
  park_data$kilos_sq <- as.character(park_data$kilos_sq)
  park_data$latitude <- as.character(park_data$latitude)
  park_data$longitude <- as.character(park_data$longitude)
  line1 <- paste0("<big><big><big><big><b>", parkName, "</big></big></big></big></b>")
  line2 <- paste0("<big><big><big><em>", park_data$state_long, "</big></big></big></em>")
  line3 <- paste0("<big><big>", park_data$acres, " acres | ", park_data$kilos_sq, " kilómetros cuadrados", "</big></big>")
  line4 <- paste0("<big><big><b>Localización: (", park_data$latitude, ", ", park_data$longitude, ")", "</b></big></big>")
  return(HTML(paste(line1, line2, line3, line4, sep="<br/>")))
}

esp_get_park1_info <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  park_data$acres <- format(park_data$acres, big.mark=" ")
  park_data$kilos_sq <- format(round(park_data$meters_sq/1000,2), big.mark=" ")
  park_data$kilos_sq <- as.character(park_data$kilos_sq)
  park_data$latitude <- as.character(park_data$latitude)
  park_data$longitude <- as.character(park_data$longitude)
  line1 <- paste0("<big><big><big><big><b>Parque 1:", parkName, "</big></big></big></big></b>")
  line2 <- paste0("<big><big><big><em>", park_data$state_long, "</big></big></big></em>")
  line3 <- paste0("<big><big>", park_data$acres, " acres | ", park_data$kilos_sq, " sq kilometers", "</big></big>")
  line4 <- paste0("<big><big><b>Location: (", park_data$latitude, ", ", park_data$longitude, ")", "</b></big></big>")
  return(HTML(paste(line1, line2, line3, line4, sep="<br/>")))
}

esp_get_park2_info <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  park_data$acres <- format(park_data$acres, big.mark=" ")
  park_data$kilos_sq <- format(round(park_data$meters_sq/1000,2), big.mark=" ")
  park_data$kilos_sq <- as.character(park_data$kilos_sq)
  park_data$latitude <- as.character(park_data$latitude)
  park_data$longitude <- as.character(park_data$longitude)
  line1 <- paste0("<big><big><big><big><b>Parque 2: ", parkName, "</big></big></big></big></b>")
  line2 <- paste0("<big><big><big><em>", park_data$state_long, "</big></big></big></em>")
  line3 <- paste0("<big><big>", park_data$acres, " acres | ", park_data$kilos_sq, " kilómetros cuadrados", "</big></big>")
  line4 <- paste0("<big><big><b>Localización: (", park_data$latitude, ", ", park_data$longitude, ")", "</b></big></big>")
  return(HTML(paste(line1, line2, line3, line4, sep="<br/>")))
}


##ANIMAL HEADING  EN ESPAÑOL##
esp_get_animal_header <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  park_data$animals_native_percent <- as.character(park_data$animals_native_percent)
  park_data$animals_endanger <- as.character(park_data$animals_endanger)
  line1 <- paste0("<big><big><big><big><b>", "ANIMALES", "</big></big></big></big></b>")
  line2 <- paste0("<big>", park_data$animals_native_percent, "% especies autóctonas", "</big>")
  line3 <- paste0("<big>",park_data$animals_endanger, " especies en peligro de extinción", "</big>")
  return(HTML(paste(line1, line2, line3, sep="<br/>")))
}

##GET RANKS  EN ESPAÑOL##
esp_get_all_ranks <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  park_data$rank_mammal <- as.character(park_data$rank_mammal)
  park_data$rank_bird <- as.character(park_data$rank_bird)
  park_data$rank_reptile <- as.character(park_data$rank_reptile)
  park_data$rank_amphibian <- as.character(park_data$rank_amphibian)
  park_data$rank_fish <- as.character(park_data$rank_fish)
  line1 <- "<h3><b>CLASIFICACIÓN</b></h3>"
  line2 <- paste0("<h3 style=color:#E68200>Mamíferos -- #", park_data$rank_mammal, "</h3>")
  line3 <- paste0("<h3 style=color:#7b49a1>Aves -- #", park_data$rank_bird, "</h3>")
  line4 <- paste0("<h3 style=color:#036D50>Reptiles -- #", park_data$rank_reptile, "</h3>")
  line5 <- paste0("<h3 style=color:#AAD213>Anfibios -- #", park_data$rank_amphibian, "</h3>")
  line6 <- paste0("<h3 style=color:#037FD0>Peces -- #", park_data$rank_fish, "</h3>")
  return(HTML(paste(line1, line2, line3, line4, line5, line6, sep="")))
}

##INVERTEBRATES INFO EN ESPAÑOL##
esp_get_invert_defs <- function(){
  line1 <- "<h4 style=color:#93998d><b><em>Para que lo sepas...</em></b></h4>"
  line2 <- "<h4 style=color:#7d8476><em>Arácnidos incluyen araños y escorpiones</em></h4>"
  line3 <- "<h4 style=color:#7d8476><em>Gasterópodos incluyen caracoles y babosas</em></h4>"
  line4 <- "<h4 style=color:#7d8476><em>Crustáceos incluyen cangrejos, langostas y camarones</em></h4>"
  return(HTML(paste(line1, line2, line3, line4, sep="")))
}

##PLANTS NUMBERS EN ESPAÑOL##
esp_get_vasc <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  park_data$count_vascular <- as.character(park_data$count_vascular)
  return(paste("  |", park_data$count_vascular, sep=" "))
}

esp_get_nonvasc <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  if (park_data$count_nonvascular == 0){
    return("| Desconocido")
  }
  else{
    park_data$count_nonvascular <- as.character(park_data$count_nonvascular)
    return(paste("| ", park_data$count_nonvascular, sep=" "))
  }
}

##ALGAE AND FUNGI NUMBERS EN ESPAÑOL##
esp_get_algae <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  if (park_data$count_algae == 0){
    return("| Desconcido")
  }
  else{
    park_data$count_algae <- as.character(park_data$count_algae)
    return(paste("| ", park_data$count_algae, sep=" "))
  }
}

esp_get_fungi <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  if (park_data$count_fungi == 0){
    return("| Desconcido")
  }
  else{
    park_data$count_fungi <- as.character(park_data$count_fungi)
    return(paste("| ", park_data$count_fungi, sep=" "))
  }
}

##PLAIN TEXT FUNCTIONS EN ESPAÑOL##
#in place of vertebrates plot  EN ESPAÑOL
esp_get_vert_info_plain <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  line1 <- "Número de especies"
  line2 <- paste("- Mamíferos: ", park_data$count_mammal, sep="")
  line3 <- paste("- Aves: ", park_data$count_bird, sep="")
  line4 <- paste("- Reptiles: ", park_data$count_reptile, sep="")
  line5 <- paste("- Anfibios: ", park_data$count_amphibian, sep="")
  line6 <- paste("- Peces: ", park_data$count_fish, sep="")
  return(HTML(paste(line1, line2, line3, line4, line5, line6, sep="<br/>")))
}
#rankings without formatting  EN ESPAÑOL
esp_get_rank_info_plain <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  line1 <- "Clasificación"
  line2 <- paste("- Mamíferos: #", park_data$rank_mammal, sep="")
  line3 <- paste("- Aves: #", park_data$rank_bird, sep="")
  line4 <- paste("- Reptiles: #", park_data$rank_reptile, sep="")
  line5 <- paste("- Anfibios: #", park_data$rank_amphibian, sep="")
  line6 <- paste("- Peces: #", park_data$rank_fish, sep="")
  return(HTML(paste(line1, line2, line3, line4, line5, line6, sep="<br/>")))
}
#in place of invertebrate table  EN ESPAÑOL
esp_get_invert_info_plain <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  park_data[park_data == 0] <- "NA"
  line1 <- "Número de especies"
  line2 <- paste("- Insectos: ", park_data$count_insect, sep="")
  line3 <- paste("- Arácnidos: ", park_data$count_arachnid, sep="")
  line4 <- paste("- Crustáceos : ", park_data$count_shellfish, sep="")
  line5 <- paste("- Gasterópodos: ", park_data$count_gastropod, sep="")
  line6 <- paste("- Otros: ", park_data$count_other, sep="")
  return(HTML(paste(line1, line2, line3, line4, line5, line6, sep="<br/>")))
}
#invertebrate definitions without formatting  EN ESPAÑOL
esp_get_invert_defs_plain <- function(parkName){
  line1 <- "Para que lo sepas..."
  line2 <- "Arácnidos incluyen araños y escorpiones"
  line3 <- "Gasterópodos incluyen caracoles y babosas"
  line4 <- "Crustáceos incluyen cangrejos, langostas y camarones "
  return(HTML(paste(line1, line2, line3, line4, sep="<br/>")))
}
#vascular plants  EN ESPAÑOL
esp_get_vasc_plain <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  park_data$count_vascular <- as.character(park_data$count_vascular)
  return(paste(park_data$count_vascular, "especies de plantas vasculares", sep=" "))
}
#nonvascular plants EN ESPAÑOL
esp_get_nonvasc_plain <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  if (park_data$count_nonvascular == 0){
    return("Un número desconocido de especies de plantas no vasculares")
  }
  else{
    park_data$count_nonvascular <- as.character(park_data$count_nonvascular)
    return(paste(park_data$count_nonvascular, "especies de plantas no vasculares", sep=" "))
  }
}

#algae EN ESPAÑOL
esp_get_algae_plain <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  if (park_data$count_algae == 0){
    return("- Algas: Un número desconocido de especies")
  }
  else{
    park_data$count_algae <- as.character(park_data$count_algae)
    return(paste("- Algas:", park_data$count_algae, "especies", sep=" "))
  }
}
#fungi EN ESPAÑOL
esp_get_fungi_plain <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  if (park_data$count_fungi == 0){
    return("- Hongos: Un número desconocido de especies")
  }
  else{
    park_data$count_fungi <- as.character(park_data$count_fungi)
    return(paste("- Hongos:", park_data$count_fungi, "especies", sep=" "))
  }
}

##COMPARING TWO PARKS  EN ESPAÑOL##
#want to include park name in beginning of each output
esp_get_animal_header2 <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  park_data$animals_native_percent <- as.character(park_data$animals_native_percent)
  park_data$animals_endanger <- as.character(park_data$animals_endanger)
  line0 <- park_data$park_name
  line1 <- "ANIMALES"
  line2 <- paste0(park_data$animals_native_percent, "% especies autóctonas")
  line3 <- paste0(park_data$animals_endanger, " especies en peligro de extinción")
  return(HTML(paste(line0, line1, line2, line3, sep="<br/>")))
}

#in place of vertebrates plot  EN ESPAÑOL
esp_get_vert_info_plain2 <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  line0 <- park_data$park_name
  line1 <- "Número de especies"
  line2 <- paste("- Mamíferos: ", park_data$count_mammal, sep="")
  line3 <- paste("- Aves: ", park_data$count_bird, sep="")
  line4 <- paste("- Reptiles: ", park_data$count_reptile, sep="")
  line5 <- paste("- Anfibios: ", park_data$count_amphibian, sep="")
  line6 <- paste("- Peces: ", park_data$count_fish, sep="")
  return(HTML(paste(line0, line1, line2, line3, line4, line5, line6, sep="<br/>")))
}
#rankings without formatting EN ESPAÑOL
esp_get_rank_info_plain2 <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  line0 <- park_data$park_name
  line1 <- "Clasificación"
  line2 <- paste("- Mamíferos: #", park_data$rank_mammal, sep="")
  line3 <- paste("- Aves: #", park_data$rank_bird, sep="")
  line4 <- paste("- Reptiles: #", park_data$rank_reptile, sep="")
  line5 <- paste("- Anfibios: #", park_data$rank_amphibian, sep="")
  line6 <- paste("- Peces: #", park_data$rank_fish, sep="")
  return(HTML(paste(line0, line1, line2, line3, line4, line5, line6, sep="<br/>")))
}
#in place of invertebrate table EN ESPAÑOL
esp_get_invert_info_plain2 <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  park_data[park_data == 0] <- "NA"
  line0 <- park_data$park_name
  line1 <- "Número de especies"
  line2 <- paste("- Insectos: ", park_data$count_insect, sep="")
  line3 <- paste("- Arácnidos: ", park_data$count_arachnid, sep="")
  line4 <- paste("- Crustáceos: ", park_data$count_shellfish, sep="")
  line5 <- paste("- Gasterópodos: ", park_data$count_gastropod, sep="")
  line6 <- paste("- Otros: ", park_data$count_other, sep="")
  return(HTML(paste(line0, line1, line2, line3, line4, line5, line6, sep="<br/>")))
}

#vascular plants EN ESPAÑOL
esp_get_vasc_plain2 <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  park_data$count_vascular <- as.character(park_data$count_vascular)
  return(paste(park_data$park_name, "tiene", park_data$count_vascular, "especies de plantas vasculares", sep=" "))
}
#nonvascular plants EN ESPAÑOL
esp_get_nonvasc_plain2 <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  if (park_data$count_nonvascular == 0){
    return(paste(park_data$park_name, "tiene un número desconocido de plantas vasculares", sep=" "))
  }
  else{
    park_data$count_nonvascular <- as.character(park_data$count_nonvascular)
    return(paste(park_data$count_nonvascular, "especies de plantas no vasculares", sep=" "))
  }
}

#algae EN ESPAÑOL
esp_get_algae_plain2 <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  if (park_data$count_algae == 0){
    return(paste(park_data$park_name, "tiene un número desconocido de algas", sep=" "))
  }
  else{
    park_data$count_algae <- as.character(park_data$count_nonvascular)
    return(paste(park_data$park_name, "tiene", park_data$count_algae, "especies de algas", sep=" "))
  }
}
#fungi EN ESPAÑOL
esp_get_fungi_plain2 <- function(parkName){
  park_data <- esp_park_stats %>% filter(park_name==parkName)
  if (park_data$count_fungi == 0){
    return(paste(park_data$park_name, "tiene un número desconocido de hongos", sep=" "))
  }
  else{
    park_data$count_fungi <- as.character(park_data$count_nonvascular)
    return(paste(park_data$park_name, "tiene", park_data$count_fungi, "especies de hongos", sep=" "))
  }
}