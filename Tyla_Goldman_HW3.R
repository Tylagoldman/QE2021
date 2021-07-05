#Tyla Noelle Goldman(3837300)
#Doubs river species data homework 
#Due 5/07/2021

#librarying in new packages 
library(vegan)
library(ggplot2)


library(tidyverse)
library(betapart)
library(vegan)
library(gridExtra)
library(BiodiversityR)
library(grid)
library(gridBase)
library(tidyr)

Doubs_species <- read_csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
view(Doubs_species)
head(Doubs_species)
tail(Doubs_species)

#1.
#The type of data being dealt with is abundance data as its greater than 1.
#Taking note that its species data, the x-plane shows the type of species observed 
#within the study of Doubs river. While the y-plane is the various sites that were sampled 
#along the Doubs river region. Although an unnecessary column, 'X1' exists.

Doubs_species <- dplyr::select(Doubs_species, -1) #removal of the column seen unnecessary 
Doubs_species
#we always need a data set with sites as rows and species as columns 

dim(Doubs_species) #analyzing dimensions of data 

#2.
#Bray-curtis because we are dealing with abundance data as its greater than 1 in certain cases. 

#3
braycurtis_doubs <-  vegdist(Doubs_species, method = "bray") #specifying dissimilarity matrix using bray-curtis 
braycurtis_doubs <- round(as.matrix(braycurtis_doubs), 4) #generating a matrix 
braycurtis_doubs
dim(braycurtis_doubs) 

#4.
#The results no longer hold the species ID but rather the dissimilarity metric. 
#The diagonal within the results square matrix displays zero disimilarity as identical species occur at the same sites. 
#The principle of dissimilarity matrix, is for higher dissimilarity among species data to be displayed by values 
#nearing 1. Therefore, ecological difference among species seen to increase across sites as dissimilarity values are increasing towards 1
#therefore new species are present across sites/ alpha diversity of species across sites is increasing.
#The species presence is changing across sites due to changing environmental conditions, where conditions at various sites act optimally
#for different species(habitat heterogeneity). Biodiversity is changing across sites. 
#In context: New fish species is observed across a river at different sites as the dissimilarity among fish species is increasing.
#Due to dissimilarity nearing 1. This is related to environmental conditions changing across the sites of the rivers. 
#For example, increasing altitude higher up on a mountain can affect oxygen availability in river water
#and differences in pH, conditions less favorable than that at lower regions of the river thus meaning, fish species adapted to these type 
#will be different to fish species at lower parts of the river and this influences the biodiversity in 
#fish species to be changing as seen in the species dissimilarity matrix.  
#The data also showed sites present with missing species data thus providing a high dissimilarity value to species of 1

#5.
#Dissimilarity metric is increasing as values are nearing 1 however outliers occur within the pattern as 
#decreasing spikes occur in the data. 

#6.
braycurtis_doubs_subset <- data.frame(braycurtis_doubs[1:1 , 1:30])
braycurtis_doubs_subset

library(ggthemes)

ggplot(data = braycurtis_doubs_subset, (aes(x = 1:30, y = braycurtis_doubs.1.1..1.30.))) +
  geom_col() + 
  geom_line(col = "red", size = 3) +
  labs(x = "Doubs river sections(km)", y = "Species disimilarity", 
       subtitle = "Bray-courtis index") +
  ggtitle(~underline("Doubs river species dissimilarity matrix" )) + #underline allows for a heading that follows the rules of biological graphs 
  theme_classic() + #changed theme 
  theme(plot.title = element_text(size = 20, face = "bold")) 

#7.
#Dissimilarity metric is increasing as values are nearing 1 where different fish species are arising along the river sections as environmental distance increase (detailed explained  Q4) 
#however outliers occur within the pattern as decreasing spikes occur in the data. 
#This can relate to a mountain not displaying a stable plateau causing regions of noise in data and noise is too observed where sites lack species data. 
#For a section between 20- 30km of the river, the graph plateus which relates to similar species composition over that region 
#influenced by similar environmental conditions over the region 

#8.
#converting abundance data into presence-absence data using decostand()
pres_abs_doubs <- (decostand(Doubs_species, method = "pa"))
view(pres_abs_doubs)
dim(pres_abs_doubs)

#sorrensens indices to dissimilarity matrix of species data usig vegdist()
sor_doubs <- vegdist(pres_abs_doubs, binary = TRUE) # binary = TRUE sets to presence/absence data
sor_doubs_df <- round(as.matrix(sor_doubs), 4)

dim(sor_doubs_df) #analyzing he dimensions of the data 

#9.
#generating the appropriate data frame of data for graphing 
sor_doubs_subset <- data.frame(sor_doubs_df[1:1 , 1:30])
sor_doubs_subset 

#Gererating the graph 
library(ggthemes)

ggplot(data = sor_doubs_subset, (aes(x = 1:30, y = sor_doubs_df.1.1..1.30.))) +
  geom_col() +
  geom_line(col = "red", size = 3) +
  labs(x = "Doubs river sections", y = "Species disimilarity", 
       subtitle = "Sorrensens index") +
  ggtitle(~underline("Doubs river species dissimilarity matrix" )) + #underline allows for a heading that follows the rules of biological graphs 
  theme_classic() + #changed theme 
  theme(plot.title = element_text(size = 20, face = "bold")) 

#The graph displays a similar shape to that created above using species abundance data.
#This is understandable as it is the exact same data in use.
#The dissimilarity metric to ranges between 0-1 with the same principle of 1 representing high disimilarity. 
#However, the troughs in the graph is more strongly defined 
#This can be related to presence-absence data placing stronger weight on noise within data. 

#The reasons for its trend in repeated bellow however, its too detailedly explained above. 
#Dissimilarity in species is increasing across the river.
#hence there is an absence of species seen at lower regions of the river, compared to the higher regions of the river.
#unique species present at regions. 
#thus 'presence/ absence data'. 
#As the dissimilarity among species is increasing, the environmental conditions over the sections of river is differing.
#the graph plateus over the further sites, representing similar environmental conditions over the portion of river.
