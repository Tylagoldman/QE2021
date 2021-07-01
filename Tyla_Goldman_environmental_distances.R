#Environmental distances script 
#Tyla NOelle Goldman(3837300)
#30/06/2021

library(vegan)
library(ggplot2)
library(geodist) # for calculating geographical distances between lats/lons
library(ggpubr) # to arrange the multipanel graphs

library(readr)

xyz <- read.csv("https://raw.githubusercontent.com/ajsmit/Quantitative_Ecology/main/exercises/diversity/Euclidian_distance_demo_data_xyz.csv")
dim(xyz) #observing number of rows and columns in data 

xyz #seeing actual data 

#the vegdist() function in vegan package to calculate euclidean distances 
#it applies pythagoras theorem 
#'upper = FALSE' eliminates the display of upper triangle in square matrix 
#'diag = TRUE' shows the calculation over the same sites causing a zero diagonal over the matrix 
xyz_euc <- round(vegdist(xyz[, 2:4], method = "euclidian", upper = FALSE, diag = TRUE), 4) # select only cols 2, 3 and 4
xyz_euc 
#[, 2:4] - take note how ratio is after the comma - refers to column selection - sites column removed 

xyz_df <- as.data.frame(round(as.matrix(xyz_euc), 4))
xyz_df #presenting matrix as a data frame 

#The above is fictional xyz data to simply get an idea as to how the euclidean distances work 

#DIFFERENCE BETWEEN SPECIES DISSIMALIRITY & ENVIRONMENTAL DISIMILARITY 
#species dissimilarity performed on presence-absence data therefore its matrix ranges in values from 0 to 1
#environmental dissimilarity works on actual environmental distances 
#hence euclidian distance formula 
#therefore matrix can contain values greater than 1 where lower values 
#represents more similarity aming environments- closer environments 
#bigger values mean greater dissimilarity- environments further away.

#NB NB NB 
#every location at which species data is takn - environment must also be taken. 
#as species resent needs to relate to the environment to identify biodiversity 
#pattern in accordance to environmental gradients 

#ANALYZING SEAWEED ENVIRONMENTAL DATA

load("Quantitative_Ecology-main/exercises/diversity/SeaweedEnv.RData")
dim(env)

round(env[1:5, 1:5], 4)
round(env[(nrow(env) - 5):nrow(env), (ncol(env) - 5):ncol(env)], 4)
#viewing first five and last five columns
#see that rows contain the sites and columns contain environmental data 

colnames(env)
#viewing column manes to see the type of environmental variables that were measured and recorded 

#18 columns - highly multivariate - not like the simple 3 variables used previously 
#need to observe data statistically 

#only selecting the environmental that we interested in 
env1 <- dplyr::select(env, febMean, febRange, febSD, augMean,
                      augRange, augSD, annMean, annRange, annSD)

#as environmental data variables can each have different ranges of measure 
#for example pH only measure till 14 while temperature till 100- temperature will always be considered larger 
#tihis results in inaccurate dissimilarities being calculated 
#therefore standardize the data 
#the unique value off all data is lost 
E1 <- round(decostand(env1, method = "standardize"), 4)
E1[1:5, 1:5]

#calculating euclidian distances to for environmental distance dissimilarity matrix
#matrix preented as a data frame 
E1_euc <- round(vegdist(E1, method = "euclidian", upper = TRUE), 4)
E1_df <- as.data.frame(as.matrix(E1_euc))
E1_df[1:10, 1:10] # the first 10 rows and columns

ggplot(data = E1_df, (aes(x = 1:58, y = `1`))) +
  ggtitle("Graph showing environmental dissimilarity") +
  theme(plot.title = element_text( colour = "forestgreen", hjust = 0.5, face = "bold")) +
  geom_line(colour = "forestgreen") + xlab("Coastal section, west to east") + ylab("Environmental distance")

#Euclidean distances of geographical data 

geo <- read.csv("Quantitative_Ecology-main/exercises/diversity/sites.csv")
dim(geo) #looking at dimensions 
head(geo)

dists <- geodist(geo, paired = TRUE, measure = "geodesic")
dists_df <- as.data.frame(as.matrix(dists))
colnames(dists_df) <- seq(1:58) #dataframe to allow for graphic displays 
dists_df[1:5, 1:5]

plt1 <- ggplot(data = dists_df, (aes(x = 1:58, y = `1`/1000))) +
  ggtitle("Graph showing actual distance across geographic points") +
  theme(plot.title = element_text( colour = "deeppink1", hjust = 0.5, face = "bold")) +
  geom_line( colour = "deeppink1") + xlab("Coastal section, west to east") + ylab("Distance (km)")
plt1

dists_euc <- vegdist(geo, method = "euclidian")
dists_euc_df <- round(as.data.frame(as.matrix(dists_euc)), 4)
dists_euc_df[1:5, 1:5]
library(ggthemes)
plt2 <- ggplot(data = dists_euc_df, (aes(x = 1:58, y = `1`))) +
  ggtitle("Graph showing euclidian distance across geographic points") +
  theme(plot.title = element_text( colour = "lawngreen", hjust = 0.5, face = "bold")) +
  geom_line(colour = "lawngreen") + xlab("Coastal section, west to east(km)") + ylab("Euclidian distance")
plt2
ggarrange(plt1, plt2, nrow = 2) #showing both graphs 
