#Tyla Noelle Goldman(3837300)
#HW5 - Principle Component Ananlysis 
#Due date - 12/07/2021

#librarying packages 
library(tidyverse)
library(vegan)

#introducing the environmental data to the script 
env <- read_csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsEnv.csv")

#observing the first section of data
head(env, 1) #see the different environmental factors that were observed 
#and an unnecessary column 'X1'exists 
# drop the first column
env <- dplyr::select(env, -1)
head(env)

#command for the pca using rda() ----- details regarding the command 
env_pca <- rda(env, scale = TRUE) #no restrictions has been coded therefore its a pca  
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

#QUESTIONS A

#Principal component values become smaller as the value of information it carries decreases 
#Because the first few principle components show the variables displaying the greatest variation among sites 
#as seen in the species scores. By adding another principle component, its data will alter the 
#graphical display of important variation seen in the earlier principal components- this changes how 
#one interprets information from the graphical display- a more accurate reading can be taken when only 
#involving principle components with the greatest impact on variation among sites.  

summary(env_pca)

#GRAPHICAL REPRESENTATIONS OF ORDINATIONS 
#a biplot 
#plots sites as points and environmental variables as vectors 

biplot(env_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(env_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

# we need to load the function first from its R file:
source("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(env_pca, scaling = 1)
cleanplot.pca(env_pca, scaling = 2)

biplot(env_pca, type = c("text", "points"), col = c("black", "black"))
ordisurf(env_pca ~ bod, env, add = TRUE, col = "turquoise", knots = 1)
ordisurf(env_pca ~ alt, env, add = TRUE, col = "salmon", knots = 1)

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

#QUESTION B 

#SEE INSTRUCTINS FOR WHATS DONE AT EACH STEP OF CODE IN NOTEES FOR THE ABOVE CODE 

#environmental data of bird communities 
ybirds.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt', row.names = 1)
view(ybirds.env)
head(ybirds.env, 1)

birds_env_edit <- dplyr::select(ybirds.env, -1, -2) #there are two columns of qualitative data that needed to be removed 
head(birds_env_edit)

birds_env_pca <- rda(birds_env_edit, scale = TRUE)
birds_env_pca

#extracting the first eigen value 
round(birds_env_pca$CA$eig[1], 3)  #see its 9.486

#total inertia - essentially the number of environmental variables 
sum(birds_env_pca$CA$eig) #18 environmental variabes being observed in pca 

round(birds_env_pca$CA$eig[1] / sum(birds_env_pca$CA$eig) * 100, 1) # result in %
#see the porion of the first variation from the total variation 

summary(birds_env_pca)

biplot(birds_env_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(birds_env_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

source("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(birds_env_pca, scaling = 1)
cleanplot.pca(birds_env_pca, scaling = 2)

biplot(birds_env_pca, type = c("text", "points"), col = c("black", "black"))
ordisurf(birds_env_pca ~ GC, birds_env_edit, add = TRUE, col = "turquoise", knots = 1)
ordisurf(birds_env_pca ~ T2C, birds_env_edit, add = TRUE, col = "salmon", knots = 1)

#explaining whats seen: 
#A
#MOST IMPACTFUL VARIABLES 
#PC1 and PC2 were only used as according to the summary of PCA its seen that the two principal components 
#explain the bulk of variation across sites, cumulatively explaining 71% of variation while PC3 only explain about 11% of variation
#in PC1 ground cover has the highest positive while secondary tree cover has the highest negative seen in summary-species scores 
#the pca ordination display according to scaling 1 shows how ground cover and secondary tree cover strongly impacts the 
#variation in environmental conditions across sites in accordance to its euclidean distance. 
#Therefore sites in the region of higher/positive PC1 axes are defined by higher ground cover 
#while sites in the region of lower/negative PC1 axes are defined by low secondary tree cover(such as sire 2, 6, 9 ect. )
#in PC2 foliage height diversity index has the highest negative while tree species diversity has the highest positive seen in summary species scores
#therefore on the pca ordination display according to scaling 1, sites in the region of higher/ positive PC2 axes are defined by high tree species diversity(such as site 12 and 22) 
#and sites in the region of lower/negative PC2 axes are defined by low foliage height diversity 
#Due to variables, elevation and foliage height diversity index vectors being outside the radius of the circloe of equilibrium contribution of the ordination display 
#they can strongly direct one towards patterns where ground cover is high when elevation is high 
#or low secondary tree cover and tree species diversity can relate to low foliage height diversity   

#B
#Firstly the high elevations is supported by high exposures as it relates to peaks 
#With relation to elevation, the scaling one graph shows how higher elevation on the PC1 axes is related to sites with Higher ground cover 
#mechanistically this is true as the dynamic way in which forests function is that higher elevations allow for greater sun exposure 
#Therefore more plant growth is present and ground cover is high 
#Similarly in a region of low elevation, secondary tree cover, tree species diversity and foliage high diversity index is low 
#as higher sunlight received at higher elevations which promote productivity are the factors that stimulate the greater diversity development
#which is not seen at lower elevations. 

#C
#According to the variables mainly influencing variation, negative correlation is seen among secondary tree cover and ground cover, 
#this correlation is graphically seen in scaling two as angle among variable vectors are greater than 90 degrees 
#ground cover is high when secondary tree cover is low as more sunlight is reaching the ground allowing for more shrub like plants to grow 
#conversely when there is high secondary tree cover, the higher trees maximize the sunlight, only allowing trees on the ground to grow in regions
#where remaining sunlight reaches 

#Another negative correlation exists where tree species diversity is low when foliage height diversity index is high. 
#this relates to the higher trees in the canopy being a result of secondary/tertiary succession therefore they are the stronger species 
#that reproduce over space much faster causing a lower species diversity as theyve colonized a region well
#and they will alter the conditions for lower growing trees causing some species unfit to grow at lower regions thus the tree species 
#diversity is low with multiple canopies deemed by a high foliage high diversity index

#exposure and elevation was matched above and their positive correlation is supported by the small angle among variable vectors in scalling two graph 

#To touch on another impactful environmental variable among sites with regatds to its vector reaching out of the radius in scaling one
#the tree basal are is positively correlated to foliage height diversity index as a small angle exists among vector. This exists mechanistically as 
#the higher the basal area of tree becomes, the more established the larger tree is thus influencing higher canopies in foresty area
#resultng in higher foliage height diversity index 

#alpine plant communities environmental data 

library(readr)
alpine_env <- read_csv("aravo_env.csv")
view(alpine_env)

alpine_env_edit <- select(alpine_env, -1, -6) #there are two columns of qualitative data that needed to be removed 
head(alpine_env_edit)

alpine_env_pca <- rda(alpine_env_edit, scale = TRUE)
alpine_env_pca

#extracting the first eigen value 
round(alpine_env_pca$CA$eig[1], 3)

#total inertia - essentially the number of environmental variables 
sum(alpine_env_pca$CA$eig)

round(alpine_env_pca$CA$eig[1] / sum(alpine_env_pca$CA$eig) * 100, 1) # result in %
#see the porion of the first variation from the total variation 

summary(alpine_env_pca)

#Kaiser-Guttman criterion
(ev <- alpine_env_pca$CA$eig) 
ev[ev > mean(ev)] #only PC1 and PC2 useful in strongly displaying variation across sites  

biplot(alpine_env_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))

biplot(alpine_env_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

source("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/cleanplot.pca.R")
cleanplot.pca(alpine_env_pca, scaling = 1)
cleanplot.pca(alpine_env_pca, scaling = 2)

biplot(alpine_env_pca, type = c("text", "points"), col = c("black", "black"))
ordisurf(alpine_env_pca ~ GC, birds_env_edit, add = TRUE, col = "turquoise", knots = 1)
ordisurf(alpine_env_pca ~ T2C, birds_env_edit, add = TRUE, col = "salmon", knots = 1)

#explaining results: 

#A
#MOST IMPACTFUL VARIABLES 
#PC1 and PC2 were used as according to the summary of PCA its seen that the two principal components 
#cumulatively explaining 63% of variation across sites, this rather a low amount of variation accounted for in an ordination diagram 
#therefore PC3, explain about 19% of remaining variation allowing for cumulative 83% of variation to be explained was considered upon observation. 

#However to confirm the needed principal components for appropriate ordination display - the Kaiser - Guttman criterion was applied 
#only PC1 and PC2 neded in ordination display 

#in PC1 form has the highest positive while slope has the highest negative seen in summary-species scores 
#the pca ordination display according to scaling 1 shows how slope and form strongly impacts the 
#variation in environmental conditions across sites in accordance to its euclidean distance. 
#Therefore sites in the region of higher/positive PC1 axes are defined by higher form(such as site 53 & 45 ect.) 
#while sites in the region of lower/negative PC1 axes are defined by low slope(such as site 25 & 48 ect.)
#in PC2 snow has the highest negative while form has the highest positive seen in summary species scores
#therefore on the pca ordination display according to scaling 1, sites in the region of higher/ positive PC2 axes are defined by high form (such as site 12 and 22) 
#and sites in the region of lower/negative PC2 axes are defined by low snow (such as site 59 & 60 ect.)
#Due to variables, form and slope vectors being outside the radius of the ordination display 
#they can strongly direct one towards patterns where form is high when slope is low 
#or low slopes influence the high snow. 
#high form influences high snowmelt 

#B
#In accordance to the general shape of elevated regions the areas where the slop is lower, the form is higher as elevated landforms begin to take 
#concave shape. Low slope impacts low physical disturbance as solifluction will be low due to low movement of wet soil and other material down a 
#slope at a low elevation.

#C.
#In accordance to impactful variables identified above, there is a negative correlation by lower slope influencing higher form(common landform shapes in a elevated region)
#this influences the positive correlation where high form has higher snowmelt as it shape will act as a catchment region for snow and its 
#located at the lower region of the slope - seen by small angling, less that 90 degrees in scaling two among snow and form variable vectors 
#aspect and slope is strongly positivly correlated seen in scaling two - this is as the slope inflences the magnitude of directionality of land 
#towards the south simply through the influence of how the landform has shaped overtime through varius environmnetal processes 