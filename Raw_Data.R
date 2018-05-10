### Nicole Keefner
### Sponge and Coral Data


## Set Working Directory


## Load packages
library(ggplot2)
library(tidyverse)
library(AICcmodavg)
require(gridExtra)

## Read in Data
# Note that the data is formatted in wide form for both datasets
# Import original sponge dataset
Sponges.start <- read.csv("ORIGINAL_Guana_Sponge_data_for_analysis.csv", header=T)
# Only select rows and columns for which there are values entered (Sponges.start has extraneous rows and columns)
Sponges.original <- Sponges.start[1:557,1:65]
# Put datast into long form
# key = title of new column, value = numbers being moved around because of the key, 
# ":" specifies which columns should be included in key
Sponges.long <- Sponges.original %>%
  gather(key = "Taxonomic_Group", value = "Count", 
         Agelas.clathrodes..Agelas.citrina.or.Clathria.faviformis:Black..spiny..purple.exudate.but.not.slimy)
# Double-check
summary(Sponges.long)
# Taxonomic_Group is being read as a character, but it should be a factor
# overwrite column so it is a factor
Sponges.long$Taxonomic_Group <- as.factor(Sponges.long$Taxonomic_Group)
# Over the years, people have entered in sites by different names. The following code
# corrects these entry mistakes, so that only 3 site names are retained.
temp = which(Sponges.long$Site == "Bigelow-South")
Sponges.long$Site[temp] = "Bigelow"
temp = which(Sponges.long$Site == "Long Point")
Sponges.long$Site[temp] = "Muskmelon"
temp = which(Sponges.long$Site == "Monkey Pt area")
Sponges.long$Site[temp] = "Monkey Pt"
temp = which(Sponges.long$Site == "Pelican")
Sponges.long$Site[temp] = "Pelican Ghut"
temp = which(Sponges.long$Site == "White Bay-alt")
Sponges.long$Site[temp] = "White Bay"
temp = which(Sponges.long$Site == "White Bay E")
Sponges.long$Site[temp] = "White Bay"
# Take this dataset and calculate the mean count for each site for each year 
# (note that some sites have more transects than others, so sampling effort varies)
Sponges.long.means <- Sponges.long %>% group_by(Year, Site) %>% summarize(Total_Mean_Count = mean(Count))
# Overwrite this dataset to remove NAs
Sponges.long.means <- Sponges.long.means %>% filter(!is.na(Total_Mean_Count))
# IF I WANT A DATASET WITHOUT ZEROES
Sponges.long.nozero <- Sponges.long %>% filter(Sponges.long$Count != 0)

# Import cleaned original dataset of corals and benthic means
Corals.original <- read.csv(file = "benthicmeans_clean_all.csv", header = T)
# Subset data to only include live hard corals and put into long form
Corals.long <- subset(Corals.original, select=c(Site:po.massive)) %>%
  gather(key = "Taxonomic_Group", value = "Mean_Percent_Cover", 
         acpa:po.massive)
# Double-check
summary(Corals.long)
# Taxonomic_Group is being read as a character, but it should be a factor
# overwrite column so it is a factor
Corals.long$Taxonomic_Group <- as.factor(Corals.long$Taxonomic_Group)
# Mean percent covers of less than 0 should be read in as zeroes
#######QUESTION FOR GRAHAM - WHY DO WE GET NEGATIVES?
Corals.long$Mean_Percent_Cover[which(Corals.long$Mean_Percent_Cover < 0)] <- 0
# Take this dataset and calculate the mean % cover for each site for each year
Corals.long.means <- Corals.long %>% group_by(Year, Site) %>% summarize(Total_Mean_Cover = mean(Mean_Percent_Cover))  
# Overwrite this dataset to remove NAs
Corals.long.means <- Corals.long.means %>% filter(!is.na(Total_Mean_Cover))

##############################################################################################

## Coral Analysis

# Null model
model.0 = lm(Total_Mean_Cover~1, data = Corals.long.means)
summary(model.0)

# Year model
model.year = lm(Total_Mean_Cover~Year, data = Corals.long.means)
summary(model.year)

# Site model
model.site = lm(Total_Mean_Cover~Site, data = Corals.long.means)
summary(model.site)

# Site+Year model
model.site.year = lm(Total_Mean_Cover~Site+Year, data = Corals.long.means)
summary(model.site.year)

# Site*Year model
model.siteyear = lm(Total_Mean_Cover~Site*Year, data = Corals.long.means)
summary(model.siteyear)

# Consolidate all models into a list
models_coral <- list(model.0, model.year, model.site, model.site.year, model.siteyear)

# Add model names in the order they are listed in models_coral
models_coral_names <- c("Null Model", "Year Model", "Site Model", "Site+Year Model", "Site*Year Model")

# Construct AIC table
aic_table_coral = aictab(models_coral, models_coral_names, digits=4)
aic_table_coral

# Each model produces predictions of the probability of detection for each observation and covariate value
all.predictions=cbind(predict(model.0,type="response"),
                      predict(model.year,type="response"),
                      predict(model.site,type="response"),
                      predict(model.site.year,type="response"),
                      predict(model.siteyear,type="response"))
# Name the prediction columns by model name
colnames(all.predictions) <- models_coral_names
head(all.predictions)
# Add predictions to dataset
Corals.long.means.pred <- data.frame(Corals.long.means, all.predictions)

##############################################################################################

## Sponge Analysis

# Null model
smodel.0 = lm(Total_Mean_Count~1, data = Sponges.long.means)
summary(smodel.0)

# Year model
smodel.year = lm(Total_Mean_Count~Year, data = Sponges.long.means)
summary(smodel.year)

# Site model
smodel.site = lm(Total_Mean_Count~Site, data = Sponges.long.means)
summary(smodel.site)

# Site+Year model
smodel.site.year = lm(Total_Mean_Count~Site+Year, data = Sponges.long.means)
summary(smodel.site.year)

# Site*Year model
smodel.siteyear = lm(Total_Mean_Count~Site*Year, data = Sponges.long.means)
summary(smodel.siteyear)

# Consolidate all models into a list
models_sponge <- list(smodel.0, smodel.year, smodel.site, smodel.site.year, smodel.siteyear)

# Add model names in the order they are listed in models_sponge
models_sponge_names <- c("Null Model", "Year Model", "Site Model", "Site+Year Model", "Site*Year Model")

# Construct AIC table
aic_table_sponge = aictab(models_sponge, models_sponge_names, digits=4)
aic_table_sponge

# Each model produces predictions of the probability of detection for each observation and covariate value
all.predictions.s=cbind(predict(smodel.0,type="response"),
                        predict(smodel.year,type="response"),
                        predict(smodel.site,type="response"),
                        predict(smodel.site.year,type="response"),
                        predict(smodel.siteyear,type="response"))
# Name the prediction columns by model name
colnames(all.predictions.s) <- models_sponge_names
head(all.predictions.s)
# Add predictions to dataset
Sponges.long.means.pred <- data.frame(Sponges.long.means, all.predictions.s)

##############################################################################################

## Summary Plots
# Take the average count/cover for each year regardless of site
Sponges.long.yearmeans <- Sponges.long.means.pred %>% group_by(Year) %>% summarize(Total_Mean_Count = mean(Total_Mean_Count))
Corals.long.yearmeans <- Corals.long.means.pred %>% group_by(Year) %>% summarize(Total_Mean_Cover = mean(Total_Mean_Cover))
# Scatterplots of count/cover for each year
figure1_top <- ggplot(data = Sponges.long.yearmeans, aes(x = Year, y = Total_Mean_Count)) + 
  geom_point()+ geom_line() +
  scale_y_continuous("Total Mean Sponge Counts")
figure1_bottom <- ggplot(data = Corals.long.yearmeans, aes(x = Year, y = Total_Mean_Cover)) + 
  geom_point()+ geom_line() +
  scale_y_continuous("Total Mean Percent Coral Cover")
# Boxplots of count/cover for each site
figure2_top <- ggplot(data = Sponges.long.means.pred, aes(x = Site, y = Total_Mean_Count)) + 
  geom_boxplot() +
  scale_y_continuous("Total Mean Sponge Counts")
figure2_bottom <- ggplot(data = Corals.long.means.pred, aes(x = Site, y = Total_Mean_Cover)) + 
  geom_boxplot() +
  scale_y_continuous("Total Mean Percent Coral Cover")


## Prediction Plots
# Combine 4 plots that show how the CORAL models compare to the null model
plot1 <- ggplot(Corals.long.means.pred, aes(x = Year, y = Year.Model, color="")) +
  labs(color = "") + 
  scale_color_manual(labels = c("Pooled Sites"), values = "red") +
  geom_line(size = 1) + geom_line(aes(x = Year, y = Null.Model), color = "black", linetype = 3, size = 1) +
  scale_y_continuous("Total Mean Percent Coral Cover", limits = c(0, 2.7)) +
  ggtitle("(A) Year Model")

plot2 <- ggplot(Corals.long.means.pred, aes(x = Year, y = Site.Model, color = Site)) +
  labs(color = "") + 
  geom_line(size = 1) + geom_line(aes(x = Year, y = Null.Model),  color = "black", linetype = 3, size = 1) +
  scale_y_continuous("Total Mean Percent Coral Cover", limits = c(0, 2.7)) +
  ggtitle("(B) Site Model")

plot3 <- ggplot(Corals.long.means.pred, aes(x = Year, y = Site.Year.Model, color = Site)) +
  labs(color = "") + 
  geom_line(size = 1) + geom_line(aes(x = Year, y = Null.Model), color = "black", linetype = 3, size = 1) +
  scale_y_continuous("Total Mean Percent Coral Cover", limits = c(0, 2.7)) +
  ggtitle("(C) Site + Year Model")

plot4 <- ggplot(Corals.long.means.pred, aes(x = Year, y = Site.Year.Model.1, color = Site)) +
  labs(color = "") + 
  geom_line(size = 1) + geom_line(aes(x = Year, y = Null.Model), color = "black", linetype = 3, size = 1) +
  scale_y_continuous("Total Mean Percent Coral Cover", limits = c(0, 2.7)) +
  ggtitle("(D) Site * Year Model")
figure3 <- grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow=2)

# Combine 4 plots that show how the SPONGE models compare to the null model
plot1 <- ggplot(Sponges.long.means.pred, aes(x = Year, y = Year.Model, color="")) +
  labs(color = "") + 
  scale_color_manual(labels = c("Pooled Sites"), values = "red") +
  geom_line(size = 1) + geom_line(aes(x = Year, y = Null.Model), color = "black", linetype = 3, size = 1) +
  scale_y_continuous("Total Mean Sponge Counts", limits = c(0.3, 1.3)) +
  ggtitle("(A) Year Model")

plot2 <- ggplot(Sponges.long.means.pred, aes(x = Year, y = Site.Model, color = Site)) +
  labs(color = "") + 
  geom_line(size = 1) + geom_line(aes(x = Year, y = Null.Model),  color = "black", linetype = 3, size = 1) +
  scale_y_continuous("Total Mean Sponge Counts", limits = c(0.3, 1.3)) +
  ggtitle("(B) Site Model")

plot3 <- ggplot(Sponges.long.means.pred, aes(x = Year, y = Site.Year.Model, color = Site)) +
  labs(color = "") + 
  geom_line(size = 1) + geom_line(aes(x = Year, y = Null.Model), color = "black", linetype = 3, size = 1) +
  scale_y_continuous("Total Mean Sponge Counts", limits = c(0.3, 1.3)) +
  ggtitle("(C) Site + Year Model")

plot4 <- ggplot(Sponges.long.means.pred, aes(x = Year, y = Site.Year.Model.1, color = Site)) +
  labs(color = "") + 
  geom_line(size = 1) + geom_line(aes(x = Year, y = Null.Model), color = "black", linetype = 3, size = 1) +
  scale_y_continuous("Total Mean Sponge Counts", limits = c(0.3, 1.3)) +
  ggtitle("(D) Site * Year Model")
figure4 <- grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow=2)