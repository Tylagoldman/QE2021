#Tyla Noelle Goldman 3837300
#Homework 7 - nMDS plots 

#introducing packages into the script 
library(tidyverse)
library(betapart)
library(vegan)
library(gridExtra)
library(grid)
library(gridBase)
library(tidyr)
library(readr)

#loading the data 
spe <- read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
view(spe)
spe <- dplyr::select(spe, -1)#unnecessary column 
spe <- dplyr::slice(spe, -8) #row has all zeros 

spe_nmds <- metaMDS(spe, distance = "bray") #bray-curtis used due to the type of data 
spe_nmds #can see stress value here too 
spe_nmds$stress #specifically shos stress value 
#stress value shows how reliable the ordination is 
#if larger than 0.2 then ordination a bit dodgy 
#is come across the above- maybe transform data, remover outliers or use a other ordination 

summary(spe_nmds) # not useful

#ordination plots 
# par(mfrow = c(2, 2))
stressplot(spe_nmds, main = "Shepard plot") 
ordiplot(spe_nmds, type = "t", cex = 1.5, main = paste0("nMDS stress = ", round(spe_nmds$stress, 2)))
gof = goodness(spe_nmds)
plot(spe_nmds, type = "t", main = "Goodness of fit")
points(spe_nmds, display = "sites", cex = gof * 200) # bigger bubbles indicate a worse fit
#SHEPARDS PLOT: shows observed dissimilarities- showing species with relation to rank- line going up
#observed and ordination rather small- causes a low stress and scatter of points away from line on shepards plot is low(shepards plot) 

#GOODNESS OF FIT: bubble sizes display how much confidence we have in seeing a fair and accurate ordination over the 2D space 

#regurlar ordinatio plot is interpreted as usual 

#last plot shows direction in which species are influencing how sites are plotted - one side will be more strongly influenced 
#by a set of species compared to other side therefore sites are different 

#build ordination plots from scratch to suit specific needs

pl <- ordiplot(spe_nmds, type = "none", main = "nMDS fish abundances ")
points(pl, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl, "species", col = "blue4", cex = 0.9)
text(pl, "sites", col = "red4", cex = 0.9)

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_nmds ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_nmds ~ Scer, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Scer"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_nmds ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_nmds ~ Cogo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cogo"))
abline(h = 0, v = 0, lty = 3)

env <- read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsEnv.csv")
view(env)
env <- dplyr::select(env, -1)
env <- dplyr::slice(env, -8)

(spe_nmds_env <- envfit(spe_nmds, env)) 
plot(spe_nmds_env, col = "grey40")
plot(spe_nmds_env, p.max = 0.05, col = "red")
# see what environmnetal factors are at play for the distribution of speicific species 
#doing this four-plot is a manner of highlighting data not seen by the overplotting 

#QUESTIONS 

#1.

#PCA

#librarying packages 
library(tidyverse)
library(vegan)

#introducing the environmental data to the script 
data("mite.env")
view(mite.env)

#observing the first section of data
head(mite.env) #see the different environmental factors that were observed 
mite.env <- dplyr::select(mite.env, -3, -4, -5) #removing factorial environmental factors 
head(mite.env)

#command for the pca using rda() ----- details regarding the command 
mite_env_pca <- rda(mite.env, scale = TRUE) #no restrictions has been coded therefore its a pca  
mite_env_pca #if restrictions are coded into rda(), it will be a redundancy analysis 
#'scale = TRUE' automatically standardized the environmental data 
#this is a correlation matrix 

#extracting the first eigen value 
round(mite_env_pca$CA$eig[1], 3)

#total inertia - essentially the number of environmental variables 
sum(mite_env_pca$CA$eig)

round(mite_env_pca$CA$eig[1] / sum(mite_env_pca$CA$eig) * 100, 1) # result in %
#see the porion of the first variation from the total variation 

summary(mite_env_pca)

#GRAPHICAL REPRESENTATIONS OF ORDINATIONS 
#a biplot 
#plots sites as points and environmental variables as vectors 

par(mfrow = c(1, 1))

biplot(mite_env_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(mite_env_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

# we need to load the function first from its R file:
source("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(mite_env_pca, scaling = 1)
cleanplot.pca(mite_env_pca, scaling = 2)

#The total inertial of the PCA shows that only 2 environmental variables were used, a low number of variables used as the bulk were recorded as factors therefore unable to use in 
#PCA. PC1 and PC2 used. Summary of PCA shows that the two principal components, cumulatively explain about 100% of variation across sites. 
#The small amount of information recieved from the species scores show how both environmental factors, water content of the substrate and substrate density will strongly influence 
#variation across sites in accordance to its euclidean distance. 
#Firstly none of the 2 environmental factors display vectors outside of the circle of equilibrium contribution therefore, one variable will not overide the other regrading its influence in 
#variation across sites. However, in simply observing the scaling 1 ordination plot, which in accordance to scaling one displays the euclidean distance among sites, It is seen that sites
#on the righ side of plot, withing positive PC1 region such as 67, 44, 39, 59 ect. will experience high water content within the substrate and relatively high substrate densities 
#while sites positioned oppositely will experience the inverse environmental conditions. The clustering of sites at the origon represents that a stable form of environmental 
#conditions exist over those sites allowing similar species to exist over the sites causing low variation influences by sites in that area(referring to strong clust of sites at the origin)
#The speciefics in environmental conditions by sites, influences the environmental gradient. 

#As PC1 was discussed above its good to note that a positive correlation is observed from this with reference to species scores observed in the PCA summary and the small angle displayed 
#between vectors within scaling two emphasis a positive correlation. Meaning as Water content is high in the substrate , the density of the substrate will too increase. 

#CA

library(vegan)
data("mite")
head(mite)
view(mite)

#bringing in needed packages 
library(tidyverse)

mite_spe_ca <- cca(mite) #the cca() function is for correspondence analysis and constrained correspondence analysis 
mite_spe_ca    #no constraints were specified in code therefore its a simple correspondence analysis 

summary(mite_spe_ca) #shows proportion of each eigenvalue to the total inertia. when looking at the cumulative - show how much correspondence explained 
#again observe species scores 
#can see in CA1- Lcil and trimalc2 the highest eigenvectors 
#on an environmental gradient - these species most impacting 
#influences community differences among sites

#sites scores - highest positive or negative loadings indicate sites that are dispersed far apart on the biplot (in ordination space).
#large differences in species community compositions 

#Kaiser-Guttman criterion
(ev <- mite_spe_ca$CA$eig) 
ev[ev > mean(ev)] 

#the following looks at various proportions of variations

round(sum(mite_spe_ca$CA$eig), 5)

round(mite_spe_ca$CA$eig[1], 5) #CA1 - inertia(total variation) for first axes 

round(sum(mite_spe_ca$CA$eig[1:2]), 5) #inertia CA1 and CA2

#fraction of varience explained by CA1 and CA2
round(sum(mite_spe_ca$CA$eig[1:2]) / sum(mite_spe_ca$CA$eig) * 100, 2) # result in %

plot(mite_spe_ca, scaling = 1, main = "CA mite abundances - biplot scaling 1")
plot(mite_spe_ca, scaling = 2, main = "CA mite abundances - biplot scaling 2")

#individual- most abundant species plotted 
#bubbles are the abundances 
#contours of location 
require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(mite, tmp <- ordisurf(mite_spe_ca ~ LCIL, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "LCIL"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(mite_spe_ca ~ Trimalc2, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Trimalc2"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(mite_spe_ca ~ Protopl, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Protopl"))
abline(h = 0, v = 0, lty = 3)
with(mite, tmp <- ordisurf(mite_spe_ca ~ Lepidzts, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Lepidzts"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
data("mite.env")
head(mite.env)
mite.env <- dplyr::select(mite.env, -3,-4, -5)
# the last plot produced (CA scaling 2) must be active
# scaling 2 is default
(mite_ca_env <- envfit(mite_spe_ca, mite.env, scaling = 2))
plot(mite_ca_env, col = "grey40")
plot(mite_ca_env, p.max = 0.05, col = "red") # plot significant variables with a different colour
#coded for significant variables to pplot in red 

?mite 

#through the correspondence analysis, the mite community data provides a total inertia of 1.696
#where reduced axes in cumulative proportion of correspondence to total inertia is about 44% for CA1 and CA2 with eigenvalues
#0.5251 and 0.2273 respectively. 

#This is relatively low proportion of correspondence displayed therefore Kaiser-guttman criterion test was applied. 
#This showed that there is value in observing up until CA8 therefore an ordination biplot 
#was developed for CA1 and CA2 as by means of ordinations the first few reduced axes show the greatest impacts towards the correspondence among sites and species. 
#In relation to that, another biplot with CA1 and CA18 was produced, it doesn't provide the most feasible information. 

#Ordination plots that have CA1 and CA2 axes, hold points plotted and coordinated according to 
#species and site scores. Where scaling 1 explains correspondence among sites strongly with sites closer together being similar 
#with reference to the relative species frequencies present. Ordination plots according to scaling 2 strongly explains correspondence
#among species where species point that are in close proximity have relatively similar frequencies across sites. 
#The information provided from scaling one and two ordination plots are relatively similar.
#Therefore in scaling 1 ordination plot of mite communities its seen that the sites 44,67 and 69 will be similar as they are in close proximity(right side of plot)
#their similarity is influenced by similar relative frequencies of mite species- LCIL, Trimalc2 ect. - which is also the highest influencers within CA1- with observing 
#clustering of sites, they influenced by different species in their proximity seen on ordination plot, for example, cluster of sites 28, 32, 34, 35 and the rest within region 
#are associated as mite species, SUCT, NPRA, Eupelops and Brachy to mention a few are present within the sites, making sites similar- they correspond more highly.   
#Differences among sites as species are unevenly distributed shows an environmental gradient exists. This influences the species community among sites. 
#Species difference across sites clearly seen as the highest species score, LCIL, exists on the far right side of plot thus influencing correspondence across sites strongly.
#To more discuss the association of species at particular sites, or in context, at positions over the soil sites, we look at scaling 2 ordination plot. 
#It shows its very unlikely for LCIL and Protopl to be at the same sight as they are positioned very far apart on opposite regions of the graph. This relates to their completely different 
#environmental conditions underwhich they function. However, mite species, Protopl, TVEL, Minigimn, SLAT, PPEL ect. which exists in close proximity under similar environmental conditions 
#as they are position close to each other on the ordination plot. Same is obvious for group of mite species- SUCT, NPRA, Eupelops and Brachy. This only discusses some of the clusters seen 
#to show the idea. To see species abundances more strongly relative to environmental factors to determine environmental gradients 

#the two highest species scores and two lowest species scores were plotted. At quick glance its reiterated as explained above that the strongly negative 
#and strongly positive species scores representing the species abundances will exist independently at different extremes of environmental conditions like LCIL and Protopl 
#these two species will not coexist. They decrease correspondence among sites. They occupy different niches due to adaptations to environmental conditions.  
#Trimalc2 and Lepidzt will also not strongly exist together as as they found in abundances at different regions as seen on the fourplot alothough they occupy more areas of sites thus they adapted to more of a range of environmental coditions. 
#Due to the lack of numerical environmental factors, an indirect gradient analysis using environmental data will not be performed as its not diversely showing environmental drivers. Although contours shown among the abundance plotting of species 
#show how these species influence a gradient over space. 

#2
#LOOK AT ABOVE NMDS FOR INSTRUCTIONS OF WHAT WAS DONE IN CODE 
library(vegan)
data("dune") #species data 
head(dune)
view(dune)

dune_spe_nmds <- metaMDS(dune, distance = "bray") #bray-curtis used due to abundance data  
dune_spe_nmds 
dune_spe_nmds$stress

summary(dune_spe_nmds) # not useful

par(mfrow = c(1, 1))
stressplot(dune_spe_nmds, main = "Shepard plot")
gof = goodness(dune_spe_nmds)
plot(dune_spe_nmds, type = "t", main = "Goodness of fit")
points(dune_spe_nmds, display = "sites", cex = gof * 200) 
ordiplot(dune_spe_nmds, type = "t", cex = 1.5, main = paste0("nMDS stress = ", round(dune_spe_nmds$stress, 2)))

pl <- ordiplot(dune_spe_nmds, type = "none", main = "nMDS dune data abundances ")
points(pl, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl, "species", col = "blue4", cex = 0.9)
text(pl, "sites", col = "red4", cex = 0.9)

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(dune, tmp <- ordisurf(dune_spe_nmds ~ Airaprae, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Airaprae"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(dune_spe_nmds ~ Hyporadi, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Hyporadi"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(dune_spe_nmds ~ Trifrepe, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Trifrepe"))
abline(h = 0, v = 0, lty = 3)
with(dune, tmp <- ordisurf(dune_spe_nmds ~ Cirsarve, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cirsarve"))
abline(h = 0, v = 0, lty = 3)

data("dune.env")
view(dune.env)

(dune_nmds_env <- envfit(dune_spe_nmds, dune.env)) 
plot(dune_nmds_env, col = "grey40")
plot(dune_nmds_env, p.max = 0.05, col = "red")

?Dune

#The nMDS from which we draw conclusions through an ordination plot, works on the basis of exact distances not being retained
#among objects within the ordination plot, objects/sites are rather ranked in a small and specified number of axes therefore ordination 
#is within a low dimensional space which causes stress and influences the level of confidence that the positioning of objects and values 
#relative to one another. From running the nMDS on the dunes species data the output stress value was lower than 0.2 , it was 0.12; therefore confidence 
#in the presentation of data in the ordination plot is seen. To graphically display stress, a shepards plot is constructed as stress is 
#"the scatter of observed dissimilarities against an expected monotone regression"while the shepards plot placed distances developed through
#the ordination which is influenced by rank, against original distances to a linear fit line. This aids in observing the relationships among 
#objects and values thats shown by nMDS ordination plot, hence the points being highly scattered from the line, decreases the confidence in the
#ordination plot while another fit statistic produced, being the 'goodness of fit' plot also shows the level of confidence in the fairness and 
#accuracy of the the objects placements in ordination over 2D space, where the larger bubbles seen, show objects/sites that are less accurately 
#placed, essentially a worse fit. 

#In observing the actual nMDS ordination plots.If sites are close to origin then species are evenly distributed across gradients/sites.
#However the more increased arrow lengths associated with Airaprae, Empenigr and Hyporadi signify that they strongly influence the dissimilarity 
#within sites 17 and 19. Or Cirsarve is likely to be found at site 3 and 4. Over the sites with an even distribution of species, the species can withstand
#a variety of environmental conditions Display of particular species in relation to environmental vectors show, evenly distributed species
# according to Trifrepe, positioned at the origion, is seen in abundance over the entire area according to the fourplot developed.
#The species is seen in abundance according to large bubble size, in regions with different extremities of an environmental factor. Suchas low and high  
#soil thickness in A1 horizon or moisture level 1 and moisture level 5. with contours thus these species carry the gradient across space with its changes 
#in abundances, thus influencing the dissimilarity among sites. While species with specific codition subjected to a a particular point in the environmental gradient 
#such as Cirsarve,falls in a region influenced by low soil thickness in A1 horizon and region experiences manure level 1, 2 & 4; providing a moisture level of 1.  
#Habitat heterogeneity is shown across space for areas hoasting particular species in abundance. This is also displayed by Airaprae and Hyporadi, their location with 
#large bubble sized on the fourplot supports how regular ordination plots associated them with sites 17 and 19. Although with this said, the fit statistics cause observations 
#to be questionable. Although stress value showed reliability in ordination, this may be due to the stress value strongly nearing 0.2. To combat these uncertainties, data can be 
#transformed or outliers removed. 

#PCoA

library(tidyverse)
library(vegan)

data("dune") #species data 
head(dune)
view(dune)
dune_spe_bray <- vegdist(dune) #getting the data into a dissimilarity matrix with bray-curtis method 
#because it is abundance data 

#spe_pcoa <- cmdscale(dune_spe_bray, k = nrow(dune) - 1, eig = TRUE)
dune_spe_pcoa <- capscale(dune_spe_bray ~ 1)
dune_spe_pcoa #can see the unconstrained value, typically the inertia  and overall eigenvalues 

summary(dune_spe_pcoa)

dune2_spe_pcoa <- capscale(dune ~ 1, distance = "bray") #"1 is for PCoA without constraints"
dune2_spe_pcoa

summary(dune2_spe_pcoa) #unable to see species names at sites and species scores due to the species information lost as data converted 
#into a dissimilarity matrix  

round(sum(dune2_spe_pcoa$CA$eig[1:3]) / sum(dune2_spe_pcoa$CA$eig) * 100, 2)

par(mfrow = c(1, 1))

plot(dune2_spe_pcoa, scaling = 1, main = "PCoA dune abundances - biplot scaling 1")
plot(dune2_spe_pcoa, scaling = 2, main = "PCoA dune abundances - biplot scaling 2")

pl1 <- ordiplot(dune2_spe_pcoa, type = "none", scaling = 1, main = "PCoA dune abundances - biplot scaling 1")
points(pl1, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl1, "species", col = "blue4", cex = 0.9)
text(pl1, "sites", col = "red4", cex = 0.9)

pl2 <- ordiplot(dune2_spe_pcoa, type = "none", scaling = 2, main = "PCoA dune abundances - biplot scaling 2")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)


#The PCoA is being run on the dune data as it too in an instance in which a dissimilarity matrix is used instead of raw data however the distances among objects is retained within this 
#ordination therefore, concerns regarding the reliability of results gathered is not as great. 
#PCoA is strongly interpreted with the same principles of CA expalined above. Although species scores are available, in light of the use of a dissimilarity matrix, not raw data. 
#However, the lack of credible environmental data display within indirect gradient analysis means the species scores will not largly be used. Rather, observations will be 
#drawn from ordination plots as in directed for the nMDS. Therefore PCoa ordination plot according to scalling  shows how for instance, sites 20, 15 and 14 will be similar due to 
#existence of Callcusp, Juncarti and Raruflam is similar frequencies across the sites. Hence these sites are ordinated closer together therefore they have smaller dissimilarities 
#compared to site that are ordinated further apart such as the above cluster of sites discussed compared to sites 10, 5 and 7 which will be more strong dominated by Achimil, Trifrepe and Rumeacet.
#Hence they will be highly different as their associated species are different. This is what influences an environmental gradient. Observing the arrowed ordination plot, one can see the extremeties 
#of dissimilarities among species where species associated with long arrows such as Poatriv will strongly influence its associated sites to be highly different from, for example the sclusters of sites 
#discussed. 

#scaling 2 PCoA ordination plot again shows the same principle discussed above where species are clustered around the origin therefore they experience even distribution over many sites, commonly found together.
#However, again the longer arrows in the scalling 2 plot shows which species strongly frays from the idea of being evenly distributed over many sites. These species like Poatriv again is more strongly adapted to 
#specific dune characteristics therefore influence the dissimilarity among sites. Influencing environmental gradient within dunes. 
