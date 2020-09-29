# RLS data wrangling course
# CJ Brown 
# 2020-07-30
# Description: 

library(readr)

dat <- read_csv("data-raw/Rottnest-Island-UVC.csv") 

head(dat)
names(dat)
nrow(dat)
ncol(dat)
unique(dat$SurveyID)
length(unique(dat$SurveyID))
unique(dat$Sizeclass)
table(dat$Sizeclass)
summary(dat)

dat$Sizeclass <- as.factor(dat$Sizeclass)
dat$Sizeclass
class(dat$Sizeclass)

#
# Data Wrangling 
#

library(dplyr)

datscarus <- filter(dat, CURRENT_TAXONOMIC_NAME == "Scarus ghobban")

datscarus
length(unique(datscarus$SpeciesID))
unique(dat$Method)

datsub <- select(datscarus, -Method)
datsub2 <- select(datscarus, 
                  SurveyID:Sizeclass, 
                  Abundance:TempTrop_23cutoff)
names(datsub2)

#
# Plots 
#

library(ggplot2)

range(datscarus$Abundance)

ggplot(datscarus) + 
  aes(x = Abundance) + 
  geom_histogram()

ggplot(datscarus) + 
  aes(x = Sizeclass, y = Abundance) + 
  geom_point()

ggplot(datscarus) + 
  aes(x = Sizeclass, y = Abundance, color = Sizeclass) + 
  geom_point()

ggplot(datscarus, aes(x = Sizeclass, y = Abundance)) + 
  geom_point(aes(color = Sizeclass))

class(datscarus$Sizeclass)

ggplot(datscarus) + 
  aes(x = Sizeclass, y = Abundance, color = Sizeclass) + 
  geom_point() + 
  scale_color_brewer(palette = "Dark2")

datscarus$Sizeclass <- 
  as.numeric(as.character(datscarus$Sizeclass))

ggplot(datscarus) + 
  aes(x = Sizeclass, y = Abundance, color = Sizeclass) + 
  geom_point() + 
  #See colorbrewer2.org for brewer palette names
  #for continuous palettes in brewer use: 
  scale_color_distiller(palette = "Reds")
  #for discrete palettes in brewer use:
  #scale_color_brewer(palette = "Reds")
  # scale_color_viridis_c()
   #scale_color_gradient(low = "pink", high = "red")

#
# Joins 
#

survdat <- read_csv("data-raw/Rottnest-Island-surveys.csv")
datscarus2 <- left_join(datscarus, survdat, by = "SurveyID")
nrow(survdat) #number of rows of survey data
unique(survdat$SurveyID)#unique survey IDs
length(unique(survdat$SurveyID)) #number of unique survey IDs
#there are more rows in survdat than unique survey IDs... 
nrow(datscarus) #before the join
nrow(datscarus2) #after the join

library(lubridate)
class(datscarus2$SurveyDate)

datscarus2$year <- year(datscarus2$SurveyDate)

ggplot(datscarus2) + 
  aes(x = year, y = Abundance) + 
  # geom_point() + 
  stat_smooth()

#
# Errors in joins
# 

dat2 <- left_join(dat, survdat, by = "SurveyID")
nrow(dat)
nrow(dat2) #oops, how can there be more UVC data after the join?
length(unique(dat$SurveyID))
length(unique(dat2$SurveyID))

dups <- which(duplicated(survdat$SurveyID))
survdat$SurveyID[dups] #gives us the exact ID that is duplicated

#Now we select for rows that are duplicated
filter(survdat, SurveyID == survdat$SurveyID[dups])

survdat2 <- distinct(survdat)
nrow(survdat2)

anyDuplicated(survdat2$SurveyID)

#Try again!
dat2 <- left_join(dat, survdat2, by = "SurveyID")
nrow(dat)
nrow(dat2)

sdat <- read_csv("data-raw/Rottnest-Island-sites.csv")
length(unique(sdat$SiteCode))
nrow(sdat)
dat3 <- left_join(dat2, sdat, by = "SiteCode")
names(dat3)
dat3$year <- year(dat3$SurveyDate)

#
# Summarizing by groups 
#


sum(dat3$Abundance)
sum(dat3$Abundance, na.rm = TRUE)

summarize(dat3, sum(Abundance, na.rm = TRUE))
dat3g <- group_by(dat3, SurveyID)
dat3g

datA <- summarize(dat3g, 
                  sum_abund = sum(Abundance, na.rm = TRUE),
                  mean_abund = mean(Abundance, na.rm = TRUE),
                  med_abund = median(Abundance, na.rm = TRUE))
?summarize
head(datA)
median(datA$sum_abund)
ggplot(datA) + 
  aes(x = sum_abund, color = NULL) + 
  geom_histogram()

#Grouping by multiple variables 

dat3 <- filter(dat3, !is.na(TempTrop_23cutoff))
# ! means 'NOT", so NOT the NAs
nrow(dat3)
dat3g <- group_by(dat3, SurveyID, SiteCode, year, 
                  TempTrop_23cutoff)
dat4 <- summarize(dat3g, 
                  sum_abund = sum(Abundance, na.rm = TRUE))
head(dat4)

gp1 <- ggplot(dat4) + 
  aes(x = year, y = sum_abund, color = TempTrop_23cutoff) + 
  geom_point() +
  stat_smooth() + 
  facet_wrap(~TempTrop_23cutoff, scales = "free")
ggsave(gp1, file = "temptrop-plot.jpg",
       width = 10, height = 5) #to change width and height :) 

#
# Creating new variables 
#

library(tidyr)

#make separate columns for temp and tropical
#abundances 
dat5 <- pivot_wider(dat4, names_from = TempTrop_23cutoff,
                    values_from = sum_abund,
                    values_fill = list(sum_abund = 0))
dat5

dat5 <- mutate(dat5, prop_trop = tropical/(temperate + tropical))
dat5
dat6 <- pivot_longer(dat5, temperate:tropical, 
                     names_to = "TempTrop_23cutoff",
                     values_to = "sum_abund")
#back to what we had before, but now we have prop_trop too!
dat6
head(data.frame(dat6))

ggplot(dat5) + 
  aes(x = year, y = prop_trop) + 
  geom_point(aes(color = SiteCode)) + 
  #Question: why did I put site code aesthetic here, not at top?
  stat_smooth()

ggplot(dat5) + 
  aes(x = year, y = prop_trop) + 
  geom_boxplot(aes(group = year)) + 
  stat_smooth()

#another fun one!

ggplot(dat5) + 
  aes(x = year, y = prop_trop) + 
  geom_violin() + 
  stat_smooth()

#
# How to make plots for papers! 
# 

panelA <- ggplot(dat5) + 
  aes(x = year, y = prop_trop) + 
  geom_boxplot(aes(group = year)) + 
  stat_smooth(color = "black") + 
  theme_classic() +
  xlab("Year") + 
  ylab("Proportion tropical fish")
panelA

panelB <- ggplot(dat5) + 
  aes(x = prop_trop) + 
  geom_density(fill = "red") + 
  theme_classic() + 
  xlab("Proportion tropical fish")

library(cowplot)
pg <- plot_grid(panelA, panelB, labels = c("(a)", "(b)"))
pg
ggsave2(pg, file = "gridplot.png", width = 7, height = 4)

library(patchwork)
panelA | panelB #same as above, but looks pretty code

#
# Adding zeros back into a dataframe 
#

#
nrow(dat)
table(dat$SpeciesID)
dat_comp <- complete(dat, SurveyID, SpeciesID, Sizeclass)
nrow(dat_comp)
table(dat_comp$SpeciesID)

