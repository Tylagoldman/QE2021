#Tyla Noelle Goldman (3837300)
#HW2 - 02/07/2021 - beta diversity 

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

dim(spp)

#WHITTAKER"S CONCEPT OF BETA DIVERSITY 

#ncol() represents gamma diversity 
#specnumber() represents alpha diversity 

#true beta diversity 
#gamma diversity/alpha-diversity   ---------- "kind of see how much change there is"
true_beta <- data.frame(
  beta = ncol(spp) / specnumber(spp, MARGIN = 1),
  section_no = c(1:58)
)
true_beta

ggplot(data = true_beta, (aes(x = section_no, y = beta))) +
  geom_line() + xlab("Coastal section, west to east") + ylab("True beta-diversity")

#absolute species turnover 
abs_beta <- data.frame(
  beta = ncol(spp) - specnumber(spp, MARGIN = 1),
  section_no = c(1:58)
)
abs_beta

ggplot(data = abs_beta, (aes(x = section_no, y = beta))) +
  geom_line() + xlab("Coastal section, west to east") + ylab("Absolute beta-diversity")

#these two beta-diversity components don't say much about species patterns over gradients 

#CONTEMPORY DEFENITIONS OF BETA_DIVERSITY 

# Decompose total Sørensen dissimilarity into turnover and nestedness-resultant components:
Y.core <- betapart.core(spp)
Y.pair <- beta.pair(Y.core, index.family = "sor")

# Let Y1 be the turnover component (beta-sim):
Y1 <- as.matrix(Y.pair$beta.sim)
Y1 #over the 58 sites we see how species are changing from one section to another 

# Let Y2 be the nestedness-resultant component (beta-sne):
Y2 <- as.matrix(Y.pair$beta.sne)
Y2 #over the 58 sites we see how species have chamged over a large scale 

round(Y1[1:10, 1:10], 4)
round(Y2[1:10, 1:10], 4)

#QUESTIONS 

#1.

Y1_subset <- data.frame(Y1[1:1 , 1:58])
Y1_subset 

library(ggthemes)

ggplot(data = Y1_subset, (aes(x = 1:58, y = Y1.1.1..1.58.))) +
  geom_line(linetype = "longdash") + 
  labs(x = "Coastal section, west to east(km)", y = "Turnover of species(Bsim)", 
       subtitle = "Beta- diversity graphical displays") +
  ggtitle(~underline("Species turnover across a coastal section" )) + #underline allows for a heading that follows the rules of biological graphs 
  theme_tufte() + #changed theme 
  theme(plot.title = element_text(size = 20, face = "bold")) 
#explaining graph: 
#the graph displays an increasing curve,  
#thus trend means species replaced as one makes observations across the coast
#species richness remaining stable although species composition is changing
#this supports the definition of species turnover suggested in Smit et al.(2017)
#where species are replaced independently of differences in species richness, 
#the process where community differentiation is caused by species gain/ loss 
#dissimilarity among species is increasing across the coast 
#relating this to environmental conditions -
#sites close together however environmental conditions are changing over the landscape (partially heterogeneous)
#this influencing the dissimilarity matrix among environment and species 
#therefore suggesting an environmental gradient exists across the coastal section understudy 
#according to Smit et al.(2017) a thermal gradient exists caused by the Aghulas current  
#which will result in pattern of small species difference across a small environmental distance 
#and large species difference across a large environmental distance. 

#2.
#Nestedness-result according to Smit et al(2017), is the difference in species richness across sites  
#it can consider species interactions 
#staying within the marine example proposed by Smit et al(2017) - observed sites from the south to north coast
#it was found that a stable species turnover is not seen as the pattern of small species difference across a small environmental distance 
#and large species difference across a large environmental distance is absent 
#therefore the nestedness-resultant needed to be analyzed. 
#as more simply speaking, nestedness resultant shows what sub-groups of biota exists in the large scale of study hence, the biodiversity(Meyer, 2017)
#therefore one can identify where unique biota communities lie and develop biological reasoning for the lack of a pattern in species
#In the answer to the above question, species turnover is seen influenced by a high thermal gradient caused by aghulus current
#although in observing sites over the opposite direction, influenced by the benguela current - a weak thermal gradient exists 
#therefore a high nestedness resultant is displayed. Bioregion boundaries can even be seen through beta-diversity peaks. 
#this variation in environmental gradient in two regions has formed the Benguela Marine Province and Aghulus Marine Province
#biological reasoning is accounted to historic factors of upwelling influencing temperature gradient disruption 
#alternatively High habitat heterogeneity can exist across the sites that causes completely different species communities to be present
#or habitat homogeneity causing the absence of species variation over areas 
#this can be influenced by dispersal limitation or other stochastic influences. 
#overall it was a historic event that has caused the Atlantic & Indian Ocean of South africa to experience 
#completely different community structuring mechanisms causing different  environmental conditions resulting in different species presence
#this variation and environmental gradient absence  isseen through  ANALYSES OF NESTEDNESS RESULTANT 


#extra literature: 
#Meyer, K.S., 2017. Islands in a sea of mud: Insights from terrestrial island theory 
#for community assembly on insular marine substrata. Adv. Mar. Biol. 76, 1-40.
