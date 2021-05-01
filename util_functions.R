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
#single out parks with missing data in order to condition plot functions
##no invertebrates
no_inverts_data <- park_stats %>% filter(count_invert==0)
no_inverts_list <- no_inverts_data$park_name
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
vert_colors <- c("#AAD213", "#5A1B8A", "#037FD0", "#E68200", "#036D50")
invert_colors <- c("#92d800", "#fc7400", "#d7004b", "#0372bb", "#d67dd6")
#theme
plot_theme <- theme(
  # no title, will be labeled on webpage
  plot.title = element_blank(),

  # panel and plot background
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "#cccccc"),
  
  # axis
  axis.title.y = element_text(family="title", size=20, color="black"),
  axis.text.y = element_blank(),
  axis.text.x = element_text(family="title2", size=14, color="black"),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  
  #no legend
  legend.position = "none"
)


###PLOTTING FUNCTIONS###
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
    geom_text(aes(label=n, family="title2"), vjust=-0.4, size=6, color="black") +
    scale_x_discrete(limits=c("Mammal", "Bird", "Reptile", "Amphibian", "Fish")) +
    scale_fill_manual(values=vert_colors) +
    labs(title="VERTEBRATES") +
    ylab("")+xlab("")+plot_theme
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
              list(`Category` = formatter("span", 
                                          style = x ~ style(color = "black",font.weight = "bold")),
                   `Count` = color_bar("#cccccc"), fun = unit.scale))
}


###RANK FUNCTIONS###
#mammal rank
get_rank_m <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  return(paste0("#", park_data$rank_mammal))
}

#bird rank
get_rank_b <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  return(paste0("#", park_data$rank_bird))
}

#reptile rank
get_rank_r <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  return(paste0("#", park_data$rank_reptile))
}

#amphibian rank
get_rank_a <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  return(paste0("#", park_data$rank_amphibian))
}

#fish_rank
get_rank_f <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  return(paste0("#", park_data$rank_fish))
}

##OTHER STATS FUNCTIONS##
##return strings of stats##

#percentage of native species
get_nativeness <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  return(paste0(str(park_data$animals_native_percent), "%"))
}

#number of endangered or threatened species
get_endangered <- function(parkName){
  park_data <- park_stats %>% filter(park_name==parkName)
  return(str(park_data$animals_endanger))
}

  
