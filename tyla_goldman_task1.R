#Tyla Noelle Goldman(3837300)
#Quantitative ecology
#Seaweed data homework 1

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

#reading in seaweed data 
spp <- read_csv("diversity/SeaweedsSpp.csv")
view(spp) #familiarizing self with the data 
head(spp)
tail(spp)
str(spp)
summary(spp)

spp <- dplyr::select(spp, -1) #removal of the column seen unnecessary 
#we always need a data set with sites as rows and species as columns 

dim(spp)#to see the dimensions of the final working dataset 

spp[1:5, 1:5] 
spp[(nrow(spp) - 5):nrow(spp), (ncol(spp) - 5):ncol(spp)]
#the above codes show the first five and last 5 rows and columns respectively 

#ALPHA DIVERSITY 

#1. species richness using diversityresult function in BiodiversityR package 
spp_richness <- diversityresult(spp, index = 'richness', method = 'each site')
spp_richness #see total number of species per sight 

ggplot(data = spp_richness, (aes(x = 1:58, y = richness))) +
  geom_line() + xlab("Coastal section, west to east") + ylab("Species richness")

#alternative method to calculate species richness with vegan in a case BiodiversityR fails 
specnumber(spp, MARGIN = 1)

#2. Univariate diversity indicies 
light <- read_csv("Quantitative_Ecology-main/Quantitative_Ecology-main/exercises/diversity/light_levels.csv")
view(light)
head(light)
tail(light)
str(light)
summary(light)

dim(light)

#species richness + shannons indices + simpsons indices 
light_div <- data.frame(
  site = c("low_light", "mid_light", "high_light"),
  richness = specnumber(light[, 2:7], MARGIN = 1),
  shannon = round(diversity(light[, 2:7], MARGIN = 1, index = "shannon"), 2),
  simpson = round(diversity(light[, 2:7], MARGIN = 1, index = "simpson"), 2)
)
light_div

#3. Dissimilarity indicies 
#NOTES: pairwise comparison 
# 0 = no dissimilarity 
# 1 = high dissimilarty 
#Bray-curtis & Jaccard dissimilarity indices- require abundance data 
#sorensen dissimilarity- use presence-absence data 

sor <- vegdist(spp, binary = TRUE) # binary = TRUE sets to presence/absence data
sor_df <- round(as.matrix(sor), 4)
sor_df[1:20, 1:20] # the first 20 rows and columns

#ANSWERS TO QUESTIONS 

#1.
#The dissimilarity indicies is set to calculate dissimilarity of each site over every other site 
#with placing all sites on a y-plane and x-plane
#(therefore, the number of row & columns is dependent on the number of sites)
#thus the code run to calculate dissimilarity will do a calculation at each location at which sites meet
#as there is equal amounts of columns to rows, the square matrix shaped end product generated. 

#2.
#The diagonal acts as the 'line of symmetry' for the square matrix, 
#formed by the point at which each site is compared to itself, thus identical sites being compared
#causing the diagonal to be identified by 0 dissimilarity output values 
#within the disimilarity matrix 

#3.
#The non-diagonals is a measure of dissimilarity among a site on a x-plane to its corresponding site
#on the y-plane. Dependent on species present over the two sites.
#The dissimilarity measurement output increases over the columns and rows as 
#sites being compared are geographically located further from each other. 

#4. 
spp_dissimilarity <- data.frame(sor_df[1:1, 1:58]) #subset of data containing information of the first row of dissimilarity index 
spp_dissimilarity #needed to make a data frame for graph suitability 
colnames(spp_dissimilarity) #seeing column names to understand what to place into graph code 

library(ggthemes)

ggplot(data = spp_dissimilarity, (aes(x = 1:58, y = sor_df.1.1..1.58.))) +
  geom_line(linetype = "longdash") + 
  labs(x = "Coastal section, west to east(km)", y = "Dissimilarity of species", 
       subtitle = "Dissimilarity indices performed using sorrenson") +
  ggtitle(~underline("Dissimilarity of species across a coastal section" )) + #underline allows for a heading that follows the rules of biological graphs 
  theme_tufte() + #changed theme 
  theme(plot.title = element_text(size = 20, face = "bold")) 

#5.
#The temperature across the coastal section, or simply the environmental distance across sites plotted on x-axis 
#is increasing. Possible environmental gradient(thermal gradient) where environmental conditions differing across the sites 
#This causes an increase in biodiversity across the sites/ coastal section; simply a change in biodiversity.
#because species all have their unique optimum environmental conditions. 
#it is represented by the increase in dissimilarity of species across the the sites/ coastal sections 
#as more species , not present in prior sites will be recorded and an increase in dissimilarity over the sites develops. 
#increasing dissimilarity seen by graphically displayed increasing line. 

#GAMMA DIVERSITY 

# the number of columns gives the total number of species in this example:
ncol(spp)

#an alternative method for gamma diversity 
diversityresult(spp, index = 'richness', method = 'pooled')

#the two methods for measuring gamma diversity provides a result with 
#a difference of 1 

#QUESTIONS 

#1.
#The ncol() function calculates the sum of all the columns 
#this may be the column including sites too 
#while the diversityresult() function acts more biologically correct by calculating 
#the sum of columns only designated to species related data 

#2.
#gamma diversity measured with diversityfunction() is correct therefore total of 846
