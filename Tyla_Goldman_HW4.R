#Tyla Noelle Goldman(3837300) 
#HW4 - Correlation and association 

#librarying in the needed packages 
library(tidyverse)
library(vegan)
library(Hmisc) # for rcorr()
library(readr)

#introducing the environmental data to the script 
env <- read_csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsEnv.csv")

#observing the first section of data
head(env, 1) #see the different environmental factors that were observed 
#and an unnecessary column 'X1'exists 
# drop the first column
env <- dplyr::select(env, -1)
head(env)

#SUMMARY ON CORRELATION: 
#Allows one to observe potential relationship among variables of the same sample 
#one variable doesnt explain the other 

#pearsons correlation between environmental variables 
env_cor <- round(cor(env), 2)

env_cor_matrix <- rcorr(as.matrix(env))
view(env_cor_matrix)

#QUESTIONS A 
#1.
library(corrplot)
corrplot(env_cor, method = "square",
         title = "Correlation among environmental variables",
         mar = c(0, 0, 4, 0))

#2.
#negatives:
#altitude and distance from source in km 
#minimum speed/ velocity of the water and altitude 

#positives
#minimum speed/velocity of the water and source in kilometers 
#ammonia and phosphorus 

#3.
#The negative correlation among altitude and distance from source is representative of the higher the 
#altitude is, the lower the distance from the source of the river is. This geographically sound as rivers begin at 
#higher elevations of a landscape such as mountains and hills which experience higher altitude. 
#The above concept influences the positive correlation seen by mean minimum discharge and distance from source 
#where at the higher altitude being the lower distance from source of river, the flow is of the river essentially the discharge is low
#as velocity of the river flow will increase over area. This supports the negative correlation seen among the minimum discharge and altitude 
#where at the lower altitude, reached after the water flow has gained velocity, the mean discharge will be higher. 
#A positive correlation is seen for phosphate and ammonia concentration. These are both chemical outputs of plant decay, 
#or nutrients picked up as water travels across landscapes or anthropogenic processes
#thus if these nutrient concentration influencing processes increase, the concentartions of these nutrients increase.    

#Association between species 
spp <- read_csv("Quantitative_Ecology-main/Num_Ecol_R_book_ed1/DoubsSpe.csv")
head(spp)
spp <- dplyr::select(spp, -1)
head(spp)

#data is being transposed 
spp_t <- t(spp)
spp_t #viewing transposed species table 

#QUESTIONS B
#1.
#The data needs to be transposed to change the structure of the original species data 
#where species in positioned on the x-plane and sites on the y-plane 
#as the correlation command is set to compare variables on the y-plane to each other 
#therefore its important to change the species to the y-plane so that the correlation being commanded 
#can be an analysis of an relationship among the species rater than sites(sites would be correlated 
#if the data is not transposed- which is not desirable as we are not looking for environmental data potential relationships)

#2.
#The transposed species table has the species on the y-plane and the sites on the x-plane. 
#A completely reversed structure to traditional species tables. 
#The need for this reversed structure is explained above. 

#calculating the association matrix 
spp_assoc1 <- vegdist(spp_t, method = "jaccard")
as.matrix((spp_assoc1))[1:10, 1:10] # display only a portion of the data

spp_assoc2 <- vegdist(spp_t, method = "jaccard", binary = TRUE)
as.matrix((spp_assoc2))[1:10, 1:10] # display only a portion of the data

#QUESTIONS C
#1.
#The association matrix generates outputs of association coefficients that may be binary coefficients or quantitative coefficients 
#The outputs are strongly similar to species dissimilarity matrix and correlation as it hold the multivarience of pairs of 
#objects or descriptors, they too remain square matrices with row numbers equaling column numbers. However, apart from similarities in outputs
#association matricies are unique as it function under a Q-mode which looks at dissimilarity/ similarity between pairs of objects and an R-mode
#that displays dependence among variables through correlation and covarience. Assocation matricies perform both mechanisms that correlation singularly
#performs with relation to the R-mode and species disimilarity matricies perform with relation to the Q-mode. 
#Association may provide a more informative output for analyszing biodiversity along posible gradients or simply in relation to environmental conditions 

#2.
#The second association makes use of 'binary=TRUE' which causes the association matrix using jaccard to be based on presence- absence data compared to the first association 
#matrix where Jaccard was applied to to species abundance data. Due to the use of prenece absence data the coefficiens give more weight to rara species or simply noise 
#within the data set .It is for this reason that the coeffiecients are larger. 

#3.
#We are able to gain information on dependence among variables with inclusion of the similarity/dissimilarity among variables. Therefore 
#better relationships among the environment and species can be gained. as explained above. 