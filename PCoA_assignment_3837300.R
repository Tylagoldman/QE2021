#Tyla Noelle Goldman(3837300) 
#PCoA - integrative assignment 

#PCoA

#Loading all the doubs data 
doubs.spe <- read.csv ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/DoubsSpe.csv', row.names = 1)
view(doubs.spe) #species abundance data 
doubs.env <- read.csv ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/DoubsEnv.csv', row.names = 1)
view(doubs.env)
doubs.spa <- read.csv ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/DoubsSpa.csv', row.names = 1)
view(doubs.spa)

library(tidyverse)
library(vegan)

doubs.spe <- read.csv ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/DoubsSpe.csv', row.names = 1)
doubs.spe <- dplyr::slice(doubs.spe, -8)

spe_bray <- vegdist(doubs.spe)

# spe_pcoa <- cmdscale(spe_bray, k = nrow(spe) - 1, eig = TRUE)
spe_pcoa <- capscale(spe_bray ~ 1)
spe_pcoa

summary(spe_pcoa) #species scores not presnet due to diddimilarity matrix 

spe_pcoa <- capscale(doubs.spe ~ 1, distance = "bray")
spe_pcoa #allowing for presence of species scores 

summary(spe_pcoa)

round(sum(spe_pcoa$CA$eig[1:3]) / sum(spe_pcoa$CA$eig) * 100, 2)

plot(spe_pcoa, scaling = 1, main = "PCoA fish abundances - biplot scaling 1")
plot(spe_pcoa, scaling = 2, main = "PCoA fish abundances - biplot scaling 2")

par(mfrow = c(1, 2))

pl1 <- ordiplot(spe_pcoa, type = "none", scaling = 1, main = "PCoA fish abundances - biplot scaling 1")
points(pl1, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl1, "species", col = "blue4", cex = 0.9)
text(pl1, "sites", col = "red4", cex = 0.9)

pl2 <- ordiplot(spe_pcoa, type = "none", scaling = 2, main = "PCoA fish abundances - biplot scaling 2")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(1, 2))
with(doubs.spe, tmp <- ordisurf(spe_pcoa ~ TRU, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "TRU"))
abline(h = 0, v = 0, lty = 3)
with(doubs.spe, tmp <- ordisurf(spe_pcoa ~ ABL, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "ABL"))
abline(h = 0, v = 0, lty = 3)

doubs.env <- read.csv ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/DoubsEnv.csv', row.names = 1)
doubs.env
doubs.env <- dplyr::slice(doubs.env, -8)

(spe_pcoa_env <- envfit(spe_pcoa, doubs.env, scaling = 2)) 
plot(spe_pcoa_env, col = "grey40")
plot(spe_pcoa_env, p.max = 0.05, col = "red")

#notes on scaling of PCoA ordination
#Ordination plots that have CA1 and CA2 axes, hold points plotted and coordinated according to 
#species and site scores. Where scaling 1 explains correspondence among sites strongly with sites closer together being similar 
#with reference to the relative species frequencies present. Ordination plots according to scaling 2 strongly explains correspondence
#among species where species point that are in close proximity have relatively similar frequencies across sites. 
#The information provided from scaling one and two ordination plots are relatively similar.

#performing a preliminary rda to observe colinearallity 
library(tidyverse)
library(betapart)
library(vegan)
library(gridExtra)
library(grid)
library(gridBase)
library(tidyr)
#loading in of species data 
doubs.spe <- read.csv ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/DoubsSpe.csv', row.names = 1)
view(doubs.spe)

apply(doubs.spe, 1, sum)
#above command shows that row 8 is a problem as no species observed 

doubs.spe2 <- doubs.spe[rowSums(doubs.spe) > 0, ]
head(doubs.spe2, 8) #row 8 is omitted by commanding for the cancellation of any row summing less than 1

#loading of environmental data
doubs.env1 <- read.csv ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/DoubsEnv.csv', row.names = 1)
head(env)
doubs.env.sub1 <- dplyr::slice(doubs.env1, -8) #row 8 removed of this data too so that environmental and spcies data can relate 

#placing species data into a dissimilarity matrix to allow for rda  
braycurtis_doubs <-  vegdist(doubs.spe2, diag = TRUE) #specifying dissimilarity matrix using bray-curtis 

#standardizing the environmental data as environmental factors measured differently  
snd.env <- decostand(doubs.env.sub1, method = "standardize")

rda_full <- capscale(braycurtis_doubs ~., snd.env, distance = "bray")
rda_full

#checking for collinearity using using varience inflation factors that allow a subset of non-collinear variables to be produced. 
vif.cca(rda_full)

E2 <- snd.env %>% select(-das)

rda_2 <- capscale(braycurtis_doubs~., E2, distance = "bray")
vif.cca(rda_2)

E3 <- E2 %>% select(-amm)
rda_3 <- capscale(braycurtis_doubs ~., E3, distance = "bray")
vif.cca(rda_3)

E4 <- E3 %>% select(-pho)
rda_4 <- capscale(braycurtis_doubs~., E4, distance = "bray")
vif.cca(rda_4)

#clusterung 

#ordinations aim to display patterns and gradients among data.
#clustering tries to place our samples into a certain number of discrete units or clusters

library(tidyverse) 
library(GGally)
library(cluster)
library(dendextend)
library(ggcorrplot)
library(factoextra)
library(gridExtra)
library(vegan)

#loading in of environmental data for correlation plot - to confirm variables selected above  
doubs.env.extra <- read.csv ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/DoubsEnv.csv', row.names = 1)
head(doubs.env.extra)
dim(doubs.env.extra)

# Computing a correlation matrix
corr <- round(cor(doubs.env.extra), 1)

# Visualization of our correlation matrix
ggcorrplot(corr, type = 'upper', outline.col = "white",
           colors = c("#1679a1", "white", "#f8766d"),
           lab = TRUE)

library(ggthemes)

# variables selected with non collinearity as seen above at rda 
E4_std <- decostand(E4, method = "standardize") 
euc <- vegdist(E4_std, method = "euclidean")

E4_km <- kmeans(E4_std, centers = 4) #applying the kmeans format of clustering analysis 

fviz_cluster(E4_km, data = E4_std,  #graphical display of clustering 
             geom = "text",
             ellipse.type = "confidence",
             main = "FFigure 2: Cluster plot of Doubs river sites",
             ggtheme = theme_classic(),
)

#PCA 

#librarying packages 
library(tidyverse)
library(vegan)
library(readr)

#introducing the environmental data to the script 
doubs.env2 <- read.csv ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/DoubsEnv.csv', row.names = 1)
view(doubs.env2)
dim(doubs.env2)
#observing the first section of data
head(doubs.env2) #see the different environmental factors that were observed 

#command for the pca using rda() ----- details regarding the command 
env_pca <- rda(doubs.env2, scale = TRUE) #no restrictions has been coded therefore its a pca  
env_pca #if restrictions are coded into rda(), it will be a redundancy analysis 
#'scale = TRUE' automatically standardized the environmental data 
#this is a correlation matrix 

#details regarding whats happening in the actual pca: 
#PCA applied to environmental data - works with correlations among scaled variables 
#PCA of cource retains euclidian distance - as the priciple of pca is to display relatiinships in accordance to distnace 
#therefore transformations needed if using species data - better yet- species data rarely used 

#discussion of the output: 
#unconstrained/ inertia shows how many environmental variables were observed 
#its also the amount of diagonal values in correlation matrix- where same sites were compared 
#HOWEVER 
#in covarience matrix- inertia is the sum of variances of the variables 

#PCA analysis is UNCONSTRAINED as its not limited by some environmental variables 

#Eigenvalues for unconstrained axes shows the relative importance of the resultant reduced axes
#used to determine the proportion of the total inertia (sum of the eigenvalues) captured by any one of the axes
#first eigenvalue(PC1) has most important info
#explains the most variation (the largest fraction), and each subsequent one explains the largest proportion of the remaining variance.
#Eigenvalues measures of the importance (variance) of the axes. 
#axes are orthogonal and ranked in decreasing order of importance - each orthogonal axe shows the importance in the remaining variation 
#sum of all eigenvalue is the total inertia, so collectively they theoretically can explain all of the variation in the dataset 

#extracting the first eigen value 
round(env_pca$CA$eig[1], 3)

#total inertia - essentially the number of environmental variables 
sum(env_pca$CA$eig)

round(env_pca$CA$eig[1] / sum(env_pca$CA$eig) * 100, 1) # result in %
#see the porion of the first variation from the total variation 

barplot(as.vector(env_pca$CA$eig)/sum(env_pca$CA$eig)) #used to see the change in eigenvalues over principal components 
#can see which principal components to use in ordination plots 

#Principal component values become smaller as the value of information it carries decreases 
#Because the first few principle components show the variables displaying the greatest variation among sites 
#as seen in the species scores. By adding another principle component, its data will alter the 
#graphical display of important variation seen in the earlier principal components- this changes how 
#one interprets information from the graphical display- a more accurate reading can be taken when only 
#involving principle components with the greatest impact on variation among sites.  

summary(env_pca)

#Kaiser-Guttman criterion
(ev <- env_pca$CA$eig) 
ev[ev > mean(ev)] 

#GRAPHICAL REPRESENTATIONS OF ORDINATIONS 
#a biplot 
#plots sites as points and environmental variables as vectors 

biplot(env_pca, scaling = 1, main = "Figure 1: PCA biplot of Doubs river environmental data (scaling 1)", choices = c(1, 2))
biplot(env_pca, scaling = 2, main = "scaling 2", choices = c(1, 2))

par(mfrow = c(1, 2))
# we need to load the function first from its R file:
source("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(env_pca, scaling = 1)
cleanplot.pca(env_pca, scaling = 2)

#NOTES ON SCALING: 
#"Scaling" refers to the way ordination results are projected in the reduced space for graphical display.
#trying to show all variables is impossible, i can take away from seeing impactful variables therefore
#scale what is graphically displayed 
#Scaling 1=distance biplot: the eigenvectors are scaled to unit length. 
#(1) Distances among objects in the biplot are approximations of their Euclidean distances in multidimensional space. 
#(2) The angles among descriptor vectors are meaningless

#Scaling 2=correlation biplot: each eigenvector is scaled to the square root of its eigenvalue.
#(1) Distances among objects in the biplot are not approximations of their Euclidean distances in multidimensional space.
#(2) The angles between descriptors in the biplot reflect their correlations

#Bottom line: if the main interest of the analysis is to interpret the relationships 
#among objects, choose scaling 1. If the main interest focuses on the relationships among descriptors, 
#choose scaling 2

#objects are represented as points and variables are displayed as arrows.

#Species scores: coordinates of the arrow heads of the variables. For historical 
#reasons, response variables are always called "species" in vegan, no matter 
#what they represent

#species scores also - indicate the strength of contribution of the original environmental variables 
#to the new variables, the Principal Components (PC1, PC2, etc.). 
# larger (more positive) and smaller (more negative) values indicate a greater contribution, albeit in opposite directions.

#Site scores: coordinates of the sites in the ordination diagram. Objects are always called "Sites" in vegan output files.
#scaled and rotated coordinates of the objects (sites or samples, one for each row of the raw data table). 
# used to plot the position of the sites in 2D or 3D ordination space. 
#Sites spread further apart from others in this space differ much in terms of the environmental conditions.
#How far they spread apart depends on the major environmental gradients indicated by the species scores

#attempted extra method- DID NOT WORK OUT
?PCNM
doubs.spa <- read.csv ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/DoubsSpa.csv', row.names = 1)
doubs.env3 <- read.csv ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/DoubsEnv.csv', row.names = 1)
PCNM_quick <- quickPCNM(doubs.env3, doubs.spa)
summary(PCNM_quick)
PCNM_quick[[2]]
PCNM_quick[[3]]
?vegan
