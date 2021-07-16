#Tyla Noelle Goldman(3837300)
#HW 6- Correspondance analysis 
#Due date - 16/07/2021

#measures degree of correspondance among rows and columns(sites and species)
#works with species abundance & presence-absense data 
#maximizes correspondance among species scores and site scores 
#PCA maximizes varience 
#unimodal structure of speces seen down coulms across sites in rows. 
#CA influences rare species more 
#ordination diagrams - species and sites are also represented by points as per usual convention. The relative positions of these points
#(species vs sites, species vs. other species, or sites vs. other sites) indicate how strongly they 'correspond' to one another.
# "Which sites do my species prefer?" or "Which sites to my species correspond to?"

#bringing in needed packages 
library(tidyverse)
library(vegan)

spe <- read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
spe
spe <- dplyr::select(spe, -1) #unnecessary column needed to be removed 
head(spe, 8) #only seeing all species over the first 8 sites 

spe_ca <- cca(spe) #the cca() function is for correspondence analysis and constrained correspondence analysis 
spe_ca    #no constraints were specified in code therefore its a simple correspondence analysis 
#command caused an error message as rows must be >0 meaning there must be a species accounted for in every site 
#therefore we need to identify which row has none of the species 
apply(spe, 1, sum)
#above command shows that row 8 is the problem 

spe <- spe[rowSums(spe) > 0, ]
head(spe, 8) #row 8 is omitted by commanding for the cancellation of any row summing less than 1

#correspondance analysis can be run again without errors 
spe_ca <- cca(spe) 
spe_ca #show sum of all the eigenvalues na dtotal inertia 

summary(spe_ca) #shows proportion of each eigenvalue to the total inertia. when looking at the cumulative - show how much correspondence explained 
#again observe species scores 
#can see in CA1- congo and satr the highest eigenvectors 
#on an environmental gradient - these species most affected 
#influences community differences among sites

#sites scores - highest positive or negative loadings indicate sites that are dispersed far apart on the biplot (in ordination space).
#large differences in species community compositions 

#the following looks at varius poportions of variations 

round(sum(spe_ca$CA$eig), 5)

round(spe_ca$CA$eig[1], 5) #CA1 - inertia(total variation) for first axes 

round(sum(spe_ca$CA$eig[1:2]), 5) #inertia CA1 and CA2

#fraction of varience explained by CA1 and CA2
round(sum(spe_ca$CA$eig[1:2]) / sum(spe_ca$CA$eig) * 100, 2) # result in %

plot(spe_ca, scaling = 1, main = "CA fish abundances - biplot scaling 1")
plot(spe_ca, scaling = 2, main = "CA fish abundances - biplot scaling 2")

#individual- most abundant species plotted 
#bubbles are the abundances 
#contours of location 
require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_ca ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Scer, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Scer"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Cogo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cogo"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
env <- read.csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsEnv.csv")
env <- dplyr::select(env, -1)

# we removed the 8th row in spe, so do it here too
env <- dplyr::slice(env, -8)

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default
(spe_ca_env <- envfit(spe_ca, env, scaling = 2))
plot(spe_ca_env, col = "grey40")
plot(spe_ca_env, p.max = 0.05, col = "red") # plot significant variables with a different colour
#coded for significant variables to pplot in red 

#QUESTIONS 

#CODE ANSWERS 

#BIRD DATA 

library(readr)
ybirds.spe <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_spe.txt', row.names = 1)
view(ybirds.spe)
#No unnecessary columns present 

birds_ca <- cca(ybirds.spe)
birds_ca

summary(birds_ca)

round(sum(birds_ca$CA$eig), 5)

round(birds_ca$CA$eig[1], 5) #CA1 - inertia(total variation) for first axes 

round(sum(birds_ca$CA$eig[1:2]), 5) #inertia CA1 and CA2

#fraction of varience explained by CA1 and CA2
round(sum(birds_ca$CA$eig[1:2]) / sum(birds_ca$CA$eig) * 100, 2) # result in %

par(mfrow = c(1,1))

plot(birds_ca, scaling = 1, main = "CA bird abundances - biplot scaling 1")
plot(birds_ca, scaling = 2, main = "CA bird abundances - biplot scaling 2")

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(ybirds.spe, tmp <- ordisurf(birds_ca ~ SWP, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "SWP"))
abline(h = 0, v = 0, lty = 3)
with(ybirds.spe, tmp <- ordisurf(birds_ca ~ ILT, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "ILT"))
abline(h = 0, v = 0, lty = 3)
with(ybirds.spe, tmp <- ordisurf(birds_ca ~ ALA, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "ALA"))
abline(h = 0, v = 0, lty = 3)
with(ybirds.spe, tmp <- ordisurf(birds_ca ~ WRN, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "WRN"))
abline(h = 0, v = 0, lty = 3)

ybirds.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt', row.names = 1)
view(ybirds.env)
birds_env_edit <- dplyr::select(ybirds.env, -1, -2) #there are two columns of qualitative data that needed to be removed 
head(birds_env_edit)


(birds_ca_env <- envfit(birds_ca, birds_env_edit, scaling = 2))
plot(birds_ca_env, col = "grey40")
plot(birds_ca_env, p.max = 0.05, col = "red")

#ALPINE DATA 

aravo_sp <- read_csv("aravo_sp.csv")
view(aravo_sp)
aravo_spe_edit <- dplyr::select(aravo_sp, -1)
view(aravo_spe_edit)

alpine_ca <- cca(aravo_spe_edit)
alpine_ca

summary(alpine_ca)

#Kaiser-Guttman criterion
(ev <- alpine_ca$CA$eig) 
ev[ev > mean(ev)] 

round(sum(alpine_ca$CA$eig), 5)

round(alpine_ca$CA$eig[1], 5) #CA1 - inertia(total variation) for first axes 

round(sum(alpine_ca$CA$eig[1:2]), 5) #inertia CA1 and CA2

#fraction of varience explained by CA1 and CA2
round(sum(alpine_ca$CA$eig[1:2]) / sum(alpine_ca$CA$eig) * 100, 2) # result in %

par(mfrow = c(1,1))

plot(alpine_ca, scaling = 1, main = "CA Alpine abundances - biplot scaling 1", choices = c(1, 2))
plot(alpine_ca, scaling = 1, main = "CA Alpine abundances - biplot scaling 1", choices = c(1, 18))

plot(alpine_ca, scaling = 2, main = "CA Alpine abundances - biplot scaling 2")

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(aravo_spe_edit, tmp <- ordisurf(alpine_ca ~ Bart.alpi, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Bart.alpi"))
abline(h = 0, v = 0, lty = 3)
with(aravo_spe_edit, tmp <- ordisurf(alpine_ca ~ Sali.retu, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Sali.retu"))
abline(h = 0, v = 0, lty = 3)
with(aravo_spe_edit, tmp <- ordisurf(alpine_ca ~ Alch.pent, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Alch.pent"))
abline(h = 0, v = 0, lty = 3)
with(aravo_spe_edit, tmp <- ordisurf(alpine_ca ~ Poa.supi, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Poa.supi"))
abline(h = 0, v = 0, lty = 3)

aravo_env <- read_csv("aravo_env.csv")
view(aravo_env)
aravo_env_edit <- dplyr::select(aravo_env, -1, -6) #there are two columns of qualitative data that needed to be removed 
head(aravo_env_edit)


(alpine_ca_env <- envfit(alpine_ca, aravo_env_edit, scaling = 2))
plot(alpine_ca_env, col = "grey40")
plot(alpine_ca_env, p.max = 0.05, col = "red")


#TEXT ANSWERS

#1.
#The Satr fish species is strongly abundant or heavily dominates sites on the right hand side of plot. Due to Satr's particulate presence at particular site, it emphasizes how 
#it contributes to the uneven distribution of of species over sites which is influenced by an environmental gradient which is graphically shown through the contours on the Satr plot. 
#Scer abundances do not have a distinct pattern or distinct abundance at a region of sites. There is one distinct region its heavily abundent seen by large bubble in ordination plot, 
#while species is very rare at other sites. Teso and Cogo fish species also show distinct regions where strongly abundant but more importantly the similarity in placments of bubbles 
#among the two plots show how the two species will covary and coexitst at the same regions in the river, therefore ones very likely to find Teso and Cogo together as they a species community. 
#tHe indirect gradient analysis shows the environmental conditions that each of the species prefer. Cogo and Teso exist in parts of the river where oxygen presence is high. Their absence in regions 
#with low oxygen suggest they are sensitive to oxygen concentrations. Satr exists in low oxygen region therefore biological oxygen demand is low, well adapted to harsh conditions. 
#Scer exists at a high altitude where the flow is high, conditions may be too harsh for good existance of sish communities. Therefore an 
#environmental gradient exists due to particular environmental conditions associated to species where abundant.  

#2.
#through the correspondence analysis, the bird community data provides a total inertia of 2.008
#where reduced axes in cumulative proportion of correspondence to total inertia is about 54% for CA1 and CA2 with eigenvalues
#0.748 and 0.334 respectively. Thus the ordination plots have CA1 and CA2 axes which hold points plotted and coordinated according to 
#species and site scores. Where scaling 1 explains correspondence among sites strongly with sites closer together being similar 
#with reference to the relative species frequencies present. Ordination plots according to scaling 2 strongly explains correspondence
#among species where species point that are in close proximity have relatively similar frequencies across sites. 
#The information provided from scaling one and two ordination plots are relatively similar.
#Therefore in scaling 1 ordination plot of bird communities its seen that the sites 50,48 and 49 will be similar as they are in close proximity(left side of plot)
#their similarity is influenced by similar relative frequencies of bird species- ALA, WRN ect. - with observing shape of the graph, the sites are progressing 
#relative to sampling over a mountain slope and in proximity to sites, they influenced by different species in their proximity seen on ordination plot 
#causing differences among sites as species are unevenly distributed therefore an environmental gradient exists. This influences the species community among sites. 
#Species difference across sites clearly seen as the highest species score, SWP, exists on the far right side of plot thus influencing correspondence across sites strongly.
#To more strongly discuss the association of species at particular sites, or in context, at positions over the mountain slope, we look at scaling 2 ordination plot. 
#It shows its very unlikely for ALA and SWP to be at the same sight as they are positioned very far apart on oposite regions of teh graph. This relates to their completely diffreent 
#environmental conditions underwhich they function. However, bird species, FLT and JBR with exist in close proximity under similar environmental conditions as they are position close
#to each other on the ordination plot. Same is obvious for group of bird species- YBW, WBB and BGW. 
#To see species abundances more strongly relative to environmental factors to determine environmental gradients 
#the two highest species scores and two lowest species scores were plotted in relation to environmental data. At quick glance its reiterated as explained above that the strongly negative 
#and strongly positive species scores representing the species abundances will exist independently at different extremes of environmental conditions like SWP and ALA 
#they two species will not coexist. One bird species heavily dominates the to of the mountain while the other at the lower part., they may be occupying the different niches due to adaptations to food 
#for example, where SWP may feed on fruit from big trees as its at the top of the mountain while ALA feeds on seeds from shrubs at lower regions of the mountain. 
#Slight covarience may exist for SWP and ILT at first few sites at right hand side of graph and a slightly stronger covarience as ALA and WRN coexist towards last few sites on left hand side of plot. 
#To see environmental drivers influencing the species locations along sites, an indirect gradient analysis performed; shows that WRN and ALA exist in sites at lower regions of the lountain as low elevation exists,
#with low ground cover. The species are very scarve at higher evelvations with high ground cover. Whereas SWP and ILT exist abundantly in regions with relatively mature trees as trees with high mean diameters at 
#breast height exist. The species will be scarce at regions with young trees. Therefore environmental gradient exixts. 

#With regards to interpresting ordinations of alpine communities. Through the correspondence analysis, the bird community data provides a total inertia of 4.214
#where reduced axes in cumulative proportion of correspondence to total inertia is about 26% for CA1 and CA2 with eigenvalues proportion 0.157 and 0.099 respectively. 
#This is relatively low therefore Kaiser-guttman criterion test was applied. This showed that there is value in observing up until CA18 therefore an ordination biplot 
#was developed for CA1 and CA2 as by means of ordinations the first few reduced axes show the greatest impacts towards the correspondence among sites and species. 
#In relation to that, another biplot with CA1 and CA18 was produced, it doesnt provide the most feasible information 
#As labling is not present on plot due to data magnitde, scalling one and two can receive quick analysis showing the reduced horseshoe shape 
#which emphasizes that the results of this correspondence analysis is not constrained by linearality in data. The biplot with axes CA1 & 2 shows complexity that may 
#influence correspondence at the optimum point of bell shaped. This is seen more clearly with biplot containing CA1 & CA18 axes. With knowing the black points are sites 
#and red points are species, there is a higher variety in species in sites on the right side of plot compared to left 
#can represent regions on the right holding more favorable conditions. This can more specifically be seen in four plot showing species with highest species scores and lowest
#species scores. Although scaling two shows that there is a good amount of groups of species that coexist across the sites. No species is strongy subjected to a site. 
#This may represent that the habitat heterogeneity across sites is gradual. The four plot shows that Bart.alpi and Sali.retu are found at high slpoes. 
#These regions tend to have unique harsh conditions to which salpine species needs to be well adapted therefore not abundant at lower slopes. 
#Alch. pent and Poa.supi are found in sites where dramatic environmental factors are not strongly acting therefoe it does not have a speciefied region
#of abundance as environmnetal conditions not as strongly presuring. Can find these species over multiple sites. 

 