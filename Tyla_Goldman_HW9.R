#Tyla Noelle Goldman(3837300)
#Topic 13 - cluster analysis 

#loading all needed packages  
library(tidyverse) 
library(cluster)
library(ggcorrplot)
library(factoextra)
library(vegan)
library(ggpubr)

#loading the data 
SDGs <- read_csv("Quantitative_Ecology-main//exercises/WHO/SDG_complete.csv")
#viewing the stipulated set of data 
SDGs[1:5, 1:8]

#viewing all the categories within a particular column 
unique(SDGs$ParentLocation)

#number of categories within a column 
length(SDGs$Location)

# a correalation matrix - only use non collinear variables in cluster 
corr <- round(cor(SDGs[3:ncol(SDGs)]), 1)  #what data is being correlated and that its output must be rounded off 
ggcorrplot(corr, type = 'upper', outline.col = "white", #graphical display of correlation output with in a matrix 
           colors = c("navy", "white", "#FC4E07"),  #coded only for the upper triangle of matrix to show with aestetic of white outline and blue and orange blocks 
           lab = TRUE)

#the data is big therefore difficult to see non-colliearaty easily in the correlation plot 
#therefore doing a PCA to see variables that load the strongest on a reduced axes.

#data first standardized 
SDGs_std <- decostand(SDGs[3:ncol(SDGs)], method = "standardize") #data standardized as variables are measured differently herefore some data may falsely dominate the other 
# SDGs_euc <- vegdist(SDGs_std, method = "euclidian")
rownames(SDGs_std) <- SDGs$Location # carry location names into output

#cluster algorithm tend to defaultly choose how many clusters to develop 
#although may want to stipulate number of clusterd 
#bellow will show methods to develop output graphs with suggestive numvbers of clusters for the data 

# using silhouette analysis
plt1 <- fviz_nbclust(SDGs_std, cluster::pam, method = "silhouette") + theme_grey()

# total within cluster sum of square / elbow analysis
plt2 <- fviz_nbclust(SDGs_std, cluster::pam, method = "wss") + theme_grey()

# gap statistics
plt3 <- fviz_nbclust(SDGs_std, cluster::pam, method = "gap_stat") + theme_grey()

ggarrange(plt1, plt2, plt3, nrow = 3)

#each plot provides different result regarding suggestive numbers of clusters 
#therefore using ones own specuation according to what your investigation is aiming to uncover is the best guidline to number of clusters 

SDGs_pam <- pam(SDGs_std, metric = "euclidean", k = 2) #changed it to two clusterd according to hypothesis discussed below. 

fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex", palette = c("#FC4E07", "violetred3", "deepskyblue3"), ellipse.alpha = 0.05) +
  geom_text(aes(label = SDGs$Location), size = 2.5)

# scale SA bigger for plotting
SDGs <- SDGs |> 
  mutate(col_vec = ifelse(Location == "South Africa", "black", "grey50"),
         scale_vec = ifelse(Location == "South Africa", 3.5, 2.5))

fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex",
             palette = c("#FC4E07", "violetred3", "deepskyblue3"),
             ellipse.alpha = 0.05, pointsize = 2.0) +
  geom_text(aes(label = SDGs$Location), size = SDGs$scale_vec, col = SDGs$col_vec)

#a star plot
fviz_cluster(SDGs_pam, palette = c("#FC4E07", "violetred3", "deepskyblue3"), ellipse.type = "euclid", 
             star.plot = TRUE, repel = TRUE, pointsize = SDGs$scale_vec * 0.8) + # SA, no 147, plotted slightly bigger
  theme_grey()

#silhouette analysis to check cluster fidelity:
fviz_silhouette(SDGs_pam, palette = c("#FC4E07", "violetred3", "deepskyblue3"), ggtheme = theme_grey())

#median value for each cluster
SDGs_centroids <- SDGs |> 
  mutate(cluster = SDGs_pam$clustering) |> 
  group_by(cluster) |> 
  summarise_at(vars(other_1:SDG3.b_5), median, na.rm = TRUE)
SDGs_centroids

#the most representative example countries of each cluster - by pam()
#the values inside are very different from that produced when we calculated the medians because medoids report the standardised data:
SDGs_pam$medoids

#a coloured pairwise scatterplot to check data details
pairs(SDGs[, 3:10], col = c("#FC4E07", "violetred3", "deepskyblue3")[SDGs_pam$clustering])

#QUESTIONS 

#1.
#Simply increasing the number of clusters can lead to error warning as the cluster analysis work on the basis of association among data where each unit of data must belong to a singular subset, along with units of data that its best 
#associated with. Therefore, the desired number of clusters cant blindly be increased as data may not be able to gether sufficient associations over the commanded number of clusters, thus preventing the grouping of data. It is for this 
#reason as above, method exist to observe the optimum number of clusters for data and this can be used as a reference point towards the number of clustered to be made. 

#2.
#Data undergoing a cluster analysis, loses its labeling, the above code merely commanded for location names to be retained to accommodate comprehensive graphical display of the cluster analysis output. Therefore, it is not ideal to 
#base the number of clusters on the manner in which raw data is organized. Rather, my suggestive number of clusters will be TWO- on the basis of my hypothesis towards sustainable development goal 3. The hypothesis is that countries in 
#the northern region of the globe will be closer to achieving the SDG as they are deemed the more financially stable countries, where with the previous assignment(assignment answers attached below) it was explained 
#that financial stability in accordance to countries governmental structure can in most cases be the driving force in the factors that determine poverty stability in a country. While lots of research exists that discuss the reasons for 
#northern regions being more developed than southern regions- hence seperating the data into two clusterd to observe whether the hemispherical countries are strongly associated. 

#3.
#Code for k-means

#loading of needed packages 
library(tidyverse) 
library(GGally)
library(cluster)
library(dendextend)
library(ggcorrplot)
library(factoextra)
library(gridExtra)
library(vegan)
library(dplyr)


km <- kmeans(SDGs_std, centers = 2) #applying the kmeans format of clustering analysis with stipulated 2 clusters  

fviz_cluster(km, data = SDGs_std,  #graphical display of clustering 
             geom = "text",
             ellipse.type = "confidence",
             main = "Cluster plot displaying ",
             ggtheme = theme_classic(),
)

# scale SA bigger for plotting
SDGs <- SDGs |> 
  mutate(col_vec = ifelse(Location == "South Africa", "black", "grey50"),
         scale_vec = ifelse(Location == "South Africa", 3.5, 2.5))

fviz_cluster(km, data = SDGs_std, geom = "point", ellipse.type = "convex",
             palette = c("#FC4E07", "violetred3", "deepskyblue3"),
             ellipse.alpha = 0.05, pointsize = 2.0) +
  geom_text(aes(label = SDGs$Location), size = SDGs$scale_vec, col = SDGs$col_vec)

# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)


#Kmeans will be preferred as it has the same objective as pam where it makes use of the mediod- where objects with smallest dissimilarity to other objects are clustered together; its know as a notion of centrality. 
#Therefore, the graphical outputs of kmeans and pam are highly similar. And, hclust has a large amount of details that needs to be command for compared to k-means. Such as appropriate distance needs to be selected 
#as euclidean distance can over emphasize dissimilarity due to the dynamic of its mathematical calculation and the correct version of Ward's heiracheal clustering algorithm must be used. The specifications attached
#commands of hclust can be considered as improved accuracy. However, cluster analyses is not considered a pure statistical approach upon data therefore, it is not used to support/ deny a hypothesis but merely to get 
#the 'ball-rolling' on a hypothesis or as a quick confirmation of another analysis' results. For example, in relation to the hypothesis stated above, the cluster analysis merely shows that countries do tend to cluster 
#together in somewhat accordance to their development status and this intel will then be used experimentally further on a more statistical method therefore, the accuracy of hlust may not be as required in this context. 
#As an ecologist, the partitioning allowed for by kmeans allows for quick biological interpretations to be made from graphical outputs.  

#4.
#To reiterate what was said above, the k-means cluster analysis will be a first step in methodology of observing countries progress towards meeting SDG3, where there will be a distinct difference in developed and developing
#countries possibly related to geographical placement as some research claims according to environmental determinism concept of Jared Diamond. This can contribute to the concept of firstly groupings of countries, operating differently 
#according to economical class and every country being an independent system therefore although the current study looks at progress towards sustainable development goal 3 on the basis of hemispherical scale and economical difference; 
#a future studies can 'dig deeper' on a country scale and the cluster analysis can  be the starting-point. Back to the idea around the current study, te cluster analysis will be the large-scale confirmation that there is value in studying 
#the data on a smaller scale. As stated in previous assignment discussion(see below), the factors that were highly influential towards the variation in for example, poverty across countries, were strongly linked back to the column dealing 
#with governmental contributions as it was stated that governance is a major roleplayer in the operational level of a country. Therefore, for example, the small scale, more statistical approach that can be taken on the data may be a linear 
#regression statistical analysis to observe relationship on government to other focal factords towards the SDG and from here, the analysis can be taken on an even smaller scale - get the picture. 

#PREVIOUS ASSIGNMENT ANSWER 
#Firstly a PCA displays variation therefore, the following discussion aims to show how particular factors of SDG 3 observed per country(essentially making the gamma diversity in simple terms the globe), cause the greatest difference in poverty across the world.   
#For starters the PCA output shows a total inertia of 38 exists. The summary of the PCA which provides more detailed information, shows how the ordination plots developed with the first two principle components displays at least about 57% of the global variation 
#in SDG 3. Species scores show how according to pc 1 where about 38% of variation is explained, shows that the greatest influences of variation are the life expectancy at birth and the adolescent birth rate as they the highest positive and negative respectively. 
#In analyzing the major plot its seen how observations from Africa have points clustered on the left side of the graph, while the other locations - america, eastern mediteranean, europe, south east asia and western pacific - are strongly clusetred on the right 
#hand side of the graph. Therefore, africa is a strong influence in the variation of poverty across the globe and its mainly associated with high adolescent birth rates which was one of the factors that strongly influenced variation across the data as a whole. 
#Other variables strongly influencing africas variation to other locations are high rates of malaria, hepititas B, turbuculosis, road traffic death and unintentional poisoning. These are all negative impacts towards poverty; the factors of SDG3 mentioned that 
#influence africa are all positively correlated to ones another as seen thrugh the small angling among vectors on a scaling 2 plot. This mechanistically makes sense as to discuss briefly, infections are related where terbuculosis causes high inmunne system depression
#that influences multiple secondary infections. Alot of the observed factors influencing Africa's poverty can be mechanistically tied to current issues in Africa such as slow COVID-19 vaccine rollout, where individuals possibly do not get vaccinated in all 
#african regions in the past. The high road tarrifs may be related to poor security personnel presence which can again be related to current events such as past uprise events in Zimbabwe and the recent looting in South Africa resultant of #Zumaprotests where
#government had afterwards admitted that security personnel was low and more importantly it was 'a government fault', this brings one to the next point in discussion where its shown that government is the highest figure per country; therefore, its their actions 
#that strongly influence the state of a countries  poverty. In looking at factors of SDG3 associated with locations clustered on the right hand side of plot, its seen that a variable with high magnitude is associated to governance, the domestic general government 
#health expenditure. Of course the ability to spend large amounts of money comes from high GDP therefore reiterating that the locations on the right side of the graph are wealthier, possibly more developed countries. This has allowed for high measures of SDG3 factors 
#observed such as the life expectancy at birth being high, one of the main influences on total variation of the data. This is representative of good health care and stability to maintain a healthy lifestyle during pregnancies which again roots to government providing 
#to citizens. And in relation to Afric a major problem of life expectancy at birth is feutal alcohol syndromme although the plot shows that the more developed countries experience higher alcohol consumption levels. However, this shows that the consumption of pleasure 
#items is performed responsibly, which is shown by the low road traffic deaths for these locations as governmnet enstils their power proffessionally. This again contribute to effective givernance from the more developed countries as pleasure tax is one of the highest 
#financial incomers for some countries therefore its important to not let things like alcohol ect. become negative aspect towards the country. Apart from locations showing high alcohol/ high smoking, they still reprent the higher health workforce, which can be resultant 
#of good health care systems that are well funded. The most contrastingly to Africa is the high vaccine statistics for locations on the right of graph, such as HPV vaccine and pneumococcal conjugate vaccine. These precautions stressed by the governmnet helps decrease pressure
#on health systems. Healthy individuals allow for health workforce, healthy workforce allows for financial gain which reduces poverty. Reduction of poverty solves a maginitude of issues for example crime rates can reduce as theft for financia gain will reduce, this can make 
#locations more appatising for tourism which thus increases locations GDP. There is much more advantages that can arise from reduced poverty.These variable on the right side of graph according to their angling on scalling 2 is positively correlated. As a citizen of South Africa that hpes to help poverty in my future endevours, I believe the gateway out of poverty is education, 
#to learn how to live a progressive lifestyle. For eample, being deermined to be a working citizen or simply behaviours such as asian countries understanding of mask-wearing for years during fu season to prevent a decreased workforce, this comes from a place of education 
#Apart from workforce, health system is also less stressed. Therefore peoples behaviour in locations also impact statistics. However again, education is dependent on the umbrella group being governance.  
#In observation of my home country, South Africa, it is compared to a country with one of the highest GDP's being Germany, as the above discussion shows how governance and its work with a countries finances influences a countries poverty status. The plot comparing the two show 
#that the african country is plotted in relation to factors of the SDG3 that negatively influence povert while germany is strongly related to factors of SDG3 that positively influences poverty. The two countries will not share similar conditions. Referring back to the main plot, 
#the factors of SDG3 situated around the origin with short arrows not strongly pertaining to a particular location can relate to a lack of observations across the data. 

#5.
#As mentioned in the prevous assignmnet, Africa is commonly associated poverty however, for Mauritius, a place in the African domain, however affiliated with the knowingly successfull eropiann countries in the cluster analysis. This can be tunnelled back to governance and the 
#financial stability the country gains through it high tourism. South Africa is a country that isn't as financially strained as other African countries, in the context of Maurtius, South Africa is also a country with high tourism, such as hoe to the big five, or the best beaches 
#the best archeological discoveries ect. However, with their relative financial success, the money is being mismanagged by corruption, a prime example iis South Africa former president, Jacob Zuma, currently behind bars. Therefore South Africa is falling short of its full potential 
#towards being a developed country/ or more successful - essentially successful enough for its SDG to be more achievable and have them in the other cluster, associated to the wealthier countries.  


