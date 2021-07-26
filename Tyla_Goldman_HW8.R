#Tyla Noelle Goldman(3837300) 
#PCA of World Health Organization data on progress towards attaining SDG's

#Loading all needed packages for the PCA
library(tidyverse)
library(vegan)
library(missMDA) # to impute missing values
library(ggcorrplot) # for the correlations

#loading in of data 
#For the next few paragraphs of code, the following applied: 
#1 - the csv data file, separated by commas hence, 'read.csv' used, is loaded in with data.
#2 - filter function used to retrieve a portion of information from a stipulated year in period column of data.  
#    At a stage, sex too filtered for 'both sexes' to ensure that data analysis was not influenced by individual sex 
#3 - select function used to retrieve a stipulated column in data.
#4 - mutate function used to develop new column onto the data , which labels each row with the data name. 
#A new edited data set from the original is formed. 

#Global scale data capturing have their own dynamics where not all countries can record every particular factor 
#being measured hence the use of the package to accommodate missing data as every country have their own circumstances 
#thus the year for which data used is stimulated as all countries may not have collected statistics over a set period. 
#Data of various natures were used such as demographics, health care, financial ect. as a good conclusion around the progress
#towards good health and well-being sustainable development goal requires a an analysis of multidimentsional pieces of data 
#as health related information is not the only influence- for example finances needed for effective healthcare services. 

# Domestic general government health expenditure (GGHE-D) as percentage of general government expenditure (GGE) (%)
SDG1.a <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG1.a_domestic_health_expenditure.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG1.a")

# Maternal mortality ratio (per 100 000 live births))
SDG3.1_1 <- read.csv("Quantitative_Ecology-main//exercises/WHO/WHO_SDG3.1_maternal_mort.csv") %>%
  filter(Period == 2016,
         Indicator == "Maternal mortality ratio (per 100 000 live births)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.1_1")

# Births attended by skilled health personnel (%))
SDG3.1_2 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.1_skilled_births.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.1_2")

# Number of neonatal deaths (Child mortality)
SDG3.2_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.2_neonatal_deaths.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_1")

#  Number of under-five deaths (Child mortality)
SDG3.2_2 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.2_under_5_deaths.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_2")

#  Number of infant deaths (Child mortality)
SDG3.2_3 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.2_infant_deaths.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_3")

# New HIV infections (per 1000 uninfected population))
SDG3.3_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.3_new_HIV_infections.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_1")

# Incidence of tuberculosis (per 100 000 population per year))
SDG3.3_2 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.3_TB.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_2")

#Malaria incidence (per 1 000 population at risk))
SDG3.3_3 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.3_malaria.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_3")

# Hepatitis B surface antigen (HBsAg) prevalence among children under 5 years-prevalence-among-children-under-5-years)
SDG3.3_4 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.3_hepatitis_B.csv") %>%
  filter(Period == 2015) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_4")

#Reported number of people requiring interventions against NTDs
SDG3.3_5 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.3_NCD_interventions.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_5")

# Adult mortality rate (probability of dying between 15 and 60 years per 1000 population))
SDG3.4_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.4_adult_death_prob.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_1")

# Number of deaths attributed to non-communicable diseases, by type of disease and sex
SDG3.4_2 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Diabetes mellitus") %>%
  mutate(Indicator = Dim2) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_2")

SDG3.4_3 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Cardiovascular diseases") %>%
  mutate(Indicator = Dim2) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_3")

SDG3.4_4 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Respiratory diseases") %>%
  mutate(Indicator = Dim2) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_4")

#Crude suicide rates (per 100 000 population) (SDG 3.4.2))
SDG3.4_5 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.4_suicides.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_5")

#Total NCD Deaths (in thousands)
SDG3.4_6 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.4_NCD_data_total.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_6")

# Alcohol, total per capita (15+) consumption (in litres of pure alcohol) (SDG Indicator 3.5.2)-alcohol-per-capita-(15-)-consumption)
SDG3.5 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.5_alcohol_consumption.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.5")

# Estimated road traffic death rate (per 100 000 population))
SDG3.6 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.6_traffic_deaths_prop.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.6")

# Adolescent birth rate (per 1000 women aged 15-19 years))
SDG3.7 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.7_adolescent_births.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.7")

#UHC Index of service coverage (SCI)
SDG3.8_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.8_UHC_data_availability.csv") %>%
  filter(Period == "2013-2017") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.8_1")

#Data availability for UHC index of essential service coverage (%))
SDG3.8_2 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.8_UHC_index_of_service_coverage.csv") %>%
  filter(Period == 2017) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.8_2")

#Poison control and unintentional poisoning
SDG3.9_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.9_unintentional_poisoning_prop.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.9_1")

# Mortality rate attributed to exposure to unsafe WASH services (per 100 000 population) (SDG 3.9.2)-(sdg-3-9-2))
SDG3.9_3 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.9_WASH_mortalities.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.9_3")

#Estimates of rate of homicides (per 100 000 population)
SDG16.1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG16.1_homicides.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG16.1")

# Prevalence of current tobacco use among persons aged 15 years and older (age-standardized rate)
SDG3.a <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.a_tobacco_control.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.a")

# Total net official development assistance to medical research and basic health sectors per capita (US$), by recipient country
SDG3.b_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.b_dev_assistence_for_med_research.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_1")

#Measles-containing-vaccine second-dose (MCV2) immunization coverage by the nationally recommended age (%)-immunization-coverage-by-the-nationally-recommended-age-(-))
SDG3.b_2 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.b_measles_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_2")

# Diphtheria tetanus toxoid and pertussis (DTP3) immunization coverage among 1-year-olds (%)-immunization-coverage-among-1-year-olds-(-))
SDG3.b_3 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.b_diphtheria_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_3")

# Pneumococcal conjugate vaccines (PCV3) immunization coverage among 1-year-olds (%)-immunization-coverage-among-1-year-olds-(-))
SDG3.b_4 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.b_pneumococcal_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_4")

# Girls aged 15 years old that received the recommended doses of HPV vaccine
SDG3.b_5 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.b_HPV_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_5")

# SDG Target 3.c | Health workforce: Substantially increase health financing and the recruitment, development, training and retention of the health workforce in developing 
#countries, especially in least developed countries and small island developing States
SDG3.c_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Medical doctors (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_1")

SDG3.c_2 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Nursing and midwifery personnel (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_2")

SDG3.c_3 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Dentists (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_3")

SDG3.c_4 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Pharmacists  (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_4")

# Average of 13 International Health Regulations core capacity scores, SPAR version
SDG3.d_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.d_health_risks.csv")  %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.d_1")

# Life expectancy at birth (years))
other_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_Other_life_expectancy.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes",
         Indicator == "Life expectancy at birth (years)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "other_1")

# Life expectancy at age 60 (years))
other_2 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_Other_life_expectancy.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes",
         Indicator == "Life expectancy at age 60 (years)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "other_2")

#rbinding the data
#Firstly a set of data is being developed by the name of 'health' and this set of data is a combined form of all the above loaded data 
#using the rbind function to essentially 'combine' the above loaded data sets row by row. However the rbind function is somewhat commanded by the 'do.call' function
#which is a function that places other functions in motion, in this case it is, 'rbind' and 'lapply' thus the do.call functions strength is in allowing 
#multiple functions to be commanded for in one line of code. The lapply ensures that the action commanded for is applied to each element of the list and these lists
#are fetched by the 'get'function - again another finction within one line allowed by the 'do.call' function.
#The complexity in this line of code to simply join data lies in the magnitude of the data as it'd be tedious to apply functions for every part of listing 
#within the data. Simple manner of getting the list of dataframes into 1.  
#Thereafter, the new 'health' data frame generated is observed using the head function where the first 6 rows of data show within the console. 

health <- do.call("rbind", lapply(ls(),get))
head(health)

#create list of SDG's used 
#All the SDG's used are found together in the newly generated 'health' data. Therefore, 'unique' function used to seect particular column wanting to be observed in the data.
#In this case it is the SDG and indicator column which are column 5 and 1. The column specifications followed a comma as any specifications placed before the column would command 
#towards particular rows within the data. The list of SDG's is generated for one to observe what ones working with, to effectivly perform the next section of code. 
unique(health[, c(5, 1)])

#pivot wider 
#Developing a new data set called 'health_wide' as a subset from the 'health' data set. The data set is arranged according to locations. Thereafter the indicator column
#is removed from the data. 'Pivot_wider' is used to spread the data so that column information can strongly correspond to observations within rows. This acts similarly to the 
#'spread' function. Data is widened by giving each SDG its own column by which the measure for each SDG is paired with it from the FactValueNumeric column. This information is 
#too inline with its location, thus the arrangement previously. essentially data is tidyed which facilitates better summary of data as each SDG can be read for each country.
#The data also needed to be tidied due to the functionality of R where each variable must have its own column and each value measured must have its own cell. More than one value 
#in a column can not pertain to a single observation in a row; otherwise functions commanded upon data will not work well. Hence, the tidying of data, the set of commands is closed 
#with the 'as_tibble' function allowing for tidy data to present in a neat table.
#Therefore, the above code of observing SDG's was performed, to help direct the formulation of the tidy data. 
health_wide <- health %>%
  arrange(Location) %>%
  select(-Indicator) %>%
  pivot_wider(names_from = SDG, values_from = FactValueNumeric) %>%
  as_tibble()

#adding world population data 
#In the following set of code, data regarding the world population is added to the 'health_wide' data set that exists at this point. Firstly the population data is read into the script 
#and thereafter its columns named 'Country' and 'population' is renamed with the 'rename' function to 'location' and 'popl_size' respectively. The values within the 'popl_size' is edited 
#to be in an appropriate numerical form with changes of comma removals.
#The edited population size data is attached to the main data using the left join function ; hence, the renaing of the columns allows for this joining function to effectively join the data 
#through observations that match. 
popl <- read_csv("Quantitative_Ecology-main/exercises/WHO/WHO_population.csv") %>%
  filter(Year == 2016) %>%
  rename(popl_size = `Population (in thousands) total`,
         Location = Country) %>%
  select(Location, popl_size) %>%
  mutate(popl_size = as.numeric(gsub("[[:space:]]", "", popl_size)) * 1000)

health_wide <- health_wide %>%
  left_join(popl)

#expressing some variable sto unit of population size 
#The mutate function which aids data changes in a column is used to get population size by a unit of 'per 100000'. This aids in controlling noise within a dataset of global capacity. 
#The data set remains named, 'health_wide'. 
health_wide <- health_wide %>%
  mutate(SDG3.4_4 = SDG3.4_4 / popl_size * 100000,
         SDG3.4_3 = SDG3.4_3 / popl_size * 100000,
         SDG3.4_2 = SDG3.4_2 / popl_size * 100000,
         SDG3.4_6 = SDG3.4_6 / 100,
         SDG3.2_2 = SDG3.2_2 / popl_size * 100000,
         SDG3.2_3 = SDG3.2_3 / popl_size * 100000,
         SDG3.2_1 = SDG3.2_1 / popl_size * 100000)

#histogram of missing values and correlations 
#Firstly developing a data set called 'health_wide$na_count' representative that it will display the number of missing values in columns. the total of missing values is calculated using an 'apply' funtion 
#that commands what actions need to be applied to produce missing value totals - the sum feature of 'is.na.' which identifies as the missing values in data sets is used. This is a self stipulated function thats
#presented by the 'apply' function. Thereafter, a histogram is developed to graphically show the number of missing values in each column as its a big data set thus graphics will more conveniently display the needed 
#information.The data set for missing values is used for the histogram and the command confirms the plotting of the histogram.  
health_wide$na_count <- apply(health_wide[, 3:(ncol(health_wide) - 1)], 1, function(x) sum(is.na(x)))
hist(health_wide$na_count, breaks = 14, plot = TRUE)

# remove rows where there are more than 10 NAs
#'na_count' column within data was merely used for identification of parts of data where missing values are high this once particular NA values removed using filter , the na_count column can also be removed from data. 
#using select function.The purspose of removing columns with excessive missing values is to somewhat prevent some factors from be over observed compared to others where they may have more values assocated to then. So a 
#particular SDG with more measurements may overpower an other SDG that was under sampled however it doesnt truthfully respect that the particular SDG factor is being achieved more strongly.  
health_wide <- health_wide %>%
  filter(na_count <= 10) %>%
  select(-na_count)

# calculate pairwise correlations
#command calculates the correlation among 'health_wide' data 
corr <- round(cor(health_wide[, 3:(ncol(health_wide) - 1)]), 1)

# visualization of the correlation matrix
#correlation plot developed with command stating the presence of labels, particular colors codes to show correlation and a grey outline to the plot. The correlation output is
#a matrix, thus 'upper' selected so that information not duplicated. Diagonal absent. the ggcorplot function used.   
ggcorrplot(corr, type = 'upper', outline.col = "grey60",
           colors = c("#1679a1", "white", "#f8766d"),
           lab = TRUE)

#Impute remaining NA's 
#As a PCA will be performed on this data, the 'imputePCA' is a built in preliminary function to allow missing data of mixed data - as this data is composed of the list of different data read in above - 
#to be filled so that the actual PCA method can be applied smoothly. 
health_wide_complete <- imputePCA(health_wide[, 3:(ncol(health_wide) - 1)])$completeObs

#scale and center the data and do PCA 
#The data is standardized as its specified within the 'decostand' function. thereafter the PCA is simply performed by placing the final data set called 'health_wide_complete_std' into the 'rda' function. 
#The pca output is named 'health_pca' and it can be placed in the summary function to attain information regarding the inertia and different eigen values for the principal components. Species scores can 
#also be seen, thus better understandings can be drawn to the ordination plot afetr analysing the summary output. 
health_wide_complete_std <- decostand(health_wide_complete, method = "standardize")
health_pca <- rda(health_wide_complete_std)
health_pca
summary(health_pca)

#graphical display
#ordination plots where scaling one shows the relation of objects to valyues where closer ordinated points are similar while scaling two allows for nagles among vectors to give an idea of correlation among variables 
biplot(health_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(health_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

#plots using vegan component functions 
#The above biplots displayed with more aesthetics to show the actual variables and their related sites. This allows for easier observations. 
pl1 <- ordiplot(health_pca, type = "none", scaling = 1, main = "PCA WHO/SDG")
points(pl1, "sites", pch = 21, cex = 1.0, col = "grey20", bg = "grey80")
points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl1, "species", col = "blue4", cex = 0.9)
text(pl1, "sites", col = "red4", cex = 0.9)

pl2 <- ordiplot(health_pca, type = "none", scaling = 2, main = "PCA WHO/SDG")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)

#another way to make ordination plot 
site_scores <- tibble(ParentLocation = health_wide$ParentLocation,
                      Location = health_wide$Location)
site_scores <- tibble(cbind(site_scores, scores(health_pca, display = "sites", choices = c(1:7))))
species_scores <- data.frame(scores(health_pca, display = "species", choices = c(1:7)))
species_scores$species <- rownames(species_scores)
species_scores <- tibble(species_scores)

ggplot(data = site_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col = ParentLocation)) +
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"), 
               color = "lightseagreen", alpha = 1, size = 0.3) +
  geom_text(data = species_scores, 
            aes(x = PC1, y = PC2, label = species),
            color = "black") +
  xlab("PC1") + ylab("PC2") + 
  ggtitle("WHO SDGs, Scaling 2")

#SOUTH AFRICA SECTION 
view(site_scores)
site_scores <- site_scores %>% 
  filter(Location %in% c("South Africa", "Germany"))

SA <- ggplot(data = site_scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col = Location)) +
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"), 
               color = "lightseagreen", alpha = 1, size = 0.3) +
  geom_text(data = species_scores, 
            aes(x = PC1, y = PC2, label = species),
            color = "black") +
  xlab("PC1") + ylab("PC2") + 
  ggtitle("WHO SDGs, Scaling 2")
SA
#2
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

#3.
#Discourages that may exist on a regional scale is the above discussed lmanner of lifestyles that may be unique per country therefore standardizing apporaches across the globe is not  true reflection in statistics. Countries may also not all have measures all of these stats therefore complete data is not available globally. 
#such as from past experience working with global dat, the United Arab Emirates had low data on rural communities. This study is also done with data from many years ago therefore it is not related to current issues such as the COVID-19 pandemic. 
