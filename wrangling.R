#packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(rlist)
#utility function
'%!in%' <- function(x,y)!('%in%'(x,y))


#load in data
parks_path <- paste0(getwd(), "/source_data/parks.csv")
parks <- read.csv(parks_path)
species_path <- paste0(getwd(), "/source_data/species.csv")
species <- read.csv(species_path)

#rename columns
species_colnames <- c("species_id", "park_name", "category", "order", "family", "scientific_name", "common_names", "record_status", "occurrence", "nativeness", "abundance", "seasonality", "conservation_status", "X")
parks_colnames <- c("park_code", "park_name", "state", "acres", "latitude", "longitude")
names(species) <- species_colnames
names(parks) <- parks_colnames

#create short names for parks in both dfs
parks$park_name_short <- str_remove(parks$park_name, " National Park")
parks$park_name_short <- str_remove(parks$park_name_short, " and Preserve")

#take care of special cases
parks[48,7] <- "Sequoia | Kings Canyon"
parks[53,7] <- "Wrangell-St.Elias"

#create long names for states
for (i in 1:56){
  if (parks[i,3]=="ME"){
    parks$state_long[i]<-"Maine"
  }
  else if (parks[i,3]=="UT"){
    parks$state_long[i]<-"Utah"
  }
  else if (parks[i,3]=="SD"){
    parks$state_long[i]<-"South Dakota"
  }
  else if (parks[i,3]=="TX"){
    parks$state_long[i]<-"Texas"
  }
  else if (parks[i,3]=="FL"){
    parks$state_long[i]<-"Florida"
  }
  else if (parks[i,3]=="CO"){
    parks$state_long[i]<-"Colorado"
  }
  else if (parks[i,3]=="NM"){
    parks$state_long[i]<-"New Mexico"
  }
  else if (parks[i,3]=="CA"){
    parks$state_long[i]<-"California"
  }
  else if (parks[i,3]=="SC"){
    parks$state_long[i]<-"South Carolina"
  }
  else if (parks[i,3]=="OR"){
    parks$state_long[i]<-"Oregon"
  }
  else if (parks[i,3]=="OH"){
    parks$state_long[i]<-"Ohio"
  }
  else if (parks[i,3]=="AK"){
    parks$state_long[i]<-"Alaska"
  }
  else if (parks[i,3]=="CA, NV"){
    parks$state_long[i]<-"California | Nevada"
  }
  else if (parks[i,3]=="MT"){
    parks$state_long[i]<-"Montana"
  }
  else if (parks[i,3]=="NV"){
    parks$state_long[i]<-"Nevada"
  }
  else if (parks[i,3]=="TN, NC"){
    parks$state_long[i]<-"Tennessee | North Carolina"
  }
  else if (parks[i,3]=="AZ"){
    parks$state_long[i]<-"Arizona"
  }
  else if (parks[i,3]=="WY"){
    parks$state_long[i]<-"Wyoming"
  }
  else if (parks[i,3]=="HI"){
    parks$state_long[i]<-"Hawaii"
  }
  else if (parks[i,3]=="AR"){
    parks$state_long[i]<-"Arkansas"
  }
  else if (parks[i,3]=="MI"){
    parks$state_long[i]<-"Michigan"
  }
  else if (parks[i,3]=="KY"){
    parks$state_long[i]<-"Kentucky"
  }
  else if (parks[i,3]=="WA"){
    parks$state_long[i]<-"Washington"
  }
  else if (parks[i,3]=="VA"){
    parks$state_long[i]<-"Virginia"
  }
  else if (parks[i,3]=="ND"){
    parks$state_long[i]<-"North Dakota"
  }
  else if (parks[i,3]=="MN"){
    parks$state_long[i]<-"Minnesota"
  }
  else if (parks[i,3]=="WY, MT, ID"){
    parks$state_long[i]<-"Wyoming | Montana | Idaho"
  }
}
#add meters square
parks$meters_sq <- parks$acres * 4046.86

#reduce species df to columns of interest
reduced_species <- subset(species, select=c(park_name, category, scientific_name, nativeness, conservation_status))

#rename some categories
reduced_species$category[reduced_species$category=='Spider/Scorpion'] <- "Arachnid"
reduced_species$category[reduced_species$category=='Crab/Lobster/Shrimp'] <- "Shellfish"
reduced_species$category[reduced_species$category=='Slug/Snail'] <- "Gastropod"
reduced_species$category[reduced_species$category=='Invertebrate'] <- "Other"
reduced_species$category <- str_remove(reduced_species$category, " Plant")    #clean for future plots

#initialize stats dataframe with parks dataset as base
park_stats <- parks[,-c(1,3)]

#get counts
counts_df <- reduced_species %>% group_by(park_name) %>% count(category)
park_row <- 1
for (park in unique(counts_df$park_name)){
  sub <- subset(counts_df, park_name == park)
  #count_mammal
  if ("Mammal" %!in% sub$category){
    park_stats$count_mammal[park_row] <- 0
  }
  else{
    park_stats$count_mammal[park_row] <- sub$n[sub$category=="Mammal"]
  }
  #count_bird
  if ("Bird" %!in% sub$category){
    park_stats$count_bird[park_row] <- 0
  }
  else{
    park_stats$count_bird[park_row] <- sub$n[sub$category=="Bird"]
  }
  #count_reptile
  if ("Reptile" %!in% sub$category){
    park_stats$count_reptile[park_row] <- 0
  }
  else{
    park_stats$count_reptile[park_row] <- sub$n[sub$category=="Reptile"]
  }
  #count_amphibian
  if ("Amphibian" %!in% sub$category){
    park_stats$count_amphibian[park_row] <- 0
  }
  else{
    park_stats$count_amphibian[park_row] <- sub$n[sub$category=="Amphibian"]
  }
  #count_fish
  if ("Fish" %!in% sub$category){
    park_stats$count_fish[park_row] <- 0
  }
  else{
    park_stats$count_fish[park_row] <- sub$n[sub$category=="Fish"]
  }
  #count_insect
  if ("Insect" %!in% sub$category){
    park_stats$count_insect[park_row] <- 0
  }
  else{
    park_stats$count_insect[park_row] <- sub$n[sub$category=="Insect"]
  }
  #count_arachnid
  if ("Arachnid" %!in% sub$category){
    park_stats$count_arachnid[park_row] <- 0
  }
  else{
    park_stats$count_arachnid[park_row] <- sub$n[sub$category=="Arachnid"]
  }
  #count_gastropod
  if ("Gastropod" %!in% sub$category){
    park_stats$count_gastropod[park_row] <- 0
  }
  else{
    park_stats$count_gastropod[park_row] <- sub$n[sub$category=="Gastropod"]
  }
  #count_shellfish
  if ("Shellfish" %!in% sub$category){
    park_stats$count_shellfish[park_row] <- 0
  }
  else{
    park_stats$count_shellfish[park_row] <- sub$n[sub$category=="Shellfish"]
  }
  #count_other
  if ("Other" %!in% sub$category){
    park_stats$count_other[park_row] <- 0
  }
  else{
    park_stats$count_other[park_row] <- sub$n[sub$category=="Other"]
  }
  #count_vascular
  if ("Vascular" %!in% sub$category){
    park_stats$count_vascular[park_row] <- 0
  }
  else{
    park_stats$count_vascular[park_row] <- sub$n[sub$category=="Vascular"]
  }
  #count_nonvascular
  if ("Nonvascular" %!in% sub$category){
    park_stats$count_nonvascular[park_row] <- 0
  }
  else{
    park_stats$count_nonvascular[park_row] <- sub$n[sub$category=="Nonvascular"]
  }
  #count_algae
  if ("Algae" %!in% sub$category){
    park_stats$count_algae[park_row] <- 0
  }
  else{
    park_stats$count_algae[park_row] <- sub$n[sub$category=="Algae"]
  }
  #count_fungi
  if ("Fungi" %!in% sub$category){
    park_stats$count_fungi[park_row] <- 0
  }
  else{
    park_stats$count_fungi[park_row] <- sub$n[sub$category=="Fungi"]
  }
  park_row <- park_row + 1
}

#create counts by using RowSums
park_stats$count_animals <- rowSums(park_stats[, c(8:17)])
park_stats$count_vert <- rowSums(park_stats[, c(8:12)])
park_stats$count_invert <- rowSums(park_stats[, c(13:17)])
park_stats$count_plants <- rowSums(park_stats[, c(18,19)])
park_stats$count_fungi <- rowSums(park_stats[, c(20,21)])

#create variables for number of endangered species and percentage of native species
#by looping through data
park_row <- 1
animals <- c("Mammal", "Bird", "Reptile", "Amphibian", "Fish", "Arachnid", "Insect", "Other", "Shellfish", "Gastropod")
sub2 <- subset(reduced_species, category %in% animals)
for (park in unique(park_stats$park_name)){
  nativeness <- sub2$nativeness[sub2$park_name==park]
  conservation_status <- sub2$conservation_status[sub2$park_name==park]
  total_animals <- park_stats$count_animals[park_stats$park_name==park]

  native_sum <- sum(nativeness=="Native")
  
  native_perc <- native_sum/total_animals*100
  
  endangered_sum <- sum(conservation_status=="Endangered")
  
  threatened_sum <- sum(conservation_status=="Threatened")
  
  park_stats$animals_native_total[park_row] <- native_sum
  
  park_stats$animals_native_percent[park_row] <- round(native_perc, digits=1)
  
  park_stats$animals_endanger[park_row] <- endangered_sum + threatened_sum
  
  park_row <- park_row + 1
}

#get rankings of vertebrates and assign to variables
park_stats <- park_stats %>% mutate(rank_mammal = dense_rank(desc(count_mammal)),
                                    rank_bird = dense_rank(desc(count_bird)),
                                    rank_reptile = dense_rank(desc(count_reptile)),
                                    rank_amphibian = dense_rank(desc(count_amphibian)),
                                    rank_fish = dense_rank(desc(count_fish)))




file_name <- paste0(getwd(), "/output_data/data.csv")
write.csv(park_stats, file_name)