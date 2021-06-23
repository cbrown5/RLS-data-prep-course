# RLS data wrangling course
# CJ Brown 
# 2020-10-22

library(readr)

# Read the data in 
dat <- read_csv("data-raw/Rottnest-Island-UVC.csv")

head(dat)
tail(dat)
nrow(dat)
ncol(dat)

length(unique(dat$SurveyID))
class(unique(dat$SurveyID))
class(dat)
length(unique(dat$SpeciesID))
class(unique(dat$CURRENT_TAXONOMIC_NAME))
table(dat$Sizeclass)
summary(dat)

dat$Sizeclass
dat$Sizeclass <- as.factor(dat$Sizeclass)
dat$Sizeclass
levels(dat$Sizeclass)

dat$Sizeclass <- as.numeric(as.character(dat$Sizeclass))

# ---------------- 
# Data wrangling 
# ---------------- 

library(dplyr)

datscarus <- filter(dat, CURRENT_TAXONOMIC_NAME == "Scarus ghobban")
unique(datscarus$SpeciesID)
length(unique(datscarus$SpeciesID))

unique(dat$Method)

datsub <- select(dat, -Method)
names(datsub)
names(dat)

datsub2 <- select(dat, SurveyID:Sizeclass, Abundance:TempTrop_23cutoff)
names(datsub2)

?select

# ---------- 
# Plotting 
# --------- 

library(ggplot2)

ggplot(datscarus) + 
  aes(x = Abundance) +
  geom_histogram(bins = 3)

ggplot(datscarus) + 
  aes(x = Sizeclass, y = Abundance, color = Sizeclass) + 
  geom_point() + 
  theme_classic() + 
  # scale_color_viridis_c() 
   scale_colour_gradient(low = "hotpink", high = "purple")

ggplot(dat)+ 
  aes(x=TempTrop_23cutoff, y=Abundance, color = Sizeclass)+
  geom_point()+
  geom_jitter() + 
  theme_classic()

ggplot(dat)+ 
  # add x and y axis
  aes(x=TempTrop_23cutoff, y=Abundance, color = Sizeclass)+
  # make a boxplot 
  geom_boxplot() + 
  #scale y axis on log scale
  scale_y_log10() + 
  theme_classic()

#
# Joining 
#

survdat <- read_csv("data-raw/Rottnest-Island-surveys.csv")
head(datscarus)
?left_join
datscarus2 <- left_join(datscarus, survdat, by = "SurveyID")

#Check number of rows stays same
nrow(datscarus)
nrow(datscarus2)

#check for dupblications
nrow(survdat)
length(unique(survdat$SurveyID))

# Dates
library(lubridate)
#Find years from the survey dates
datscarus2$year <- year(datscarus2$SurveyDate)

#plot abundance by years
ggplot(datscarus2) + 
  aes(x = year, y = Abundance) + 
  #geom_point() + 
  stat_smooth()
?stat_smooth()

#
# Errors in joins!
#

head(dat)
head(survdat)

#This will join all the fish observations
# to the survey level data
dat2 <- left_join(dat, survdat, by = "SurveyID")
nrow(dat) #number of rows in original dataframe
nrow(dat2) #number of rows after we join
#notice we have more data after the join!

#Check the number of unique survey IDs
# stays teh same after the join (ie have we lost any?)
length(unique(dat$SurveyID))
length(unique(dat2$SurveyID))
length(unique(survdat$SurveyID))

#Identify duplicates
#finds the row number that is duplicated surveyID
dups <- which(duplicated(survdat$SurveyID))
#shows us which surveyID name was duplicated
survdat$SurveyID[dups]

#print out the rows that are duplicated
filter(survdat, SurveyID == survdat$SurveyID[dups])
#above is same as:
filter(survdat, SurveyID == 6002035)

#Find the 'distinct' rows and remove teh duplicates
survdat2 <- distinct(survdat)
nrow(survdat2)
nrow(survdat)
#Hooray! we got rid of the duplicates in survdat2

#Now repeat the join, but using survey data 
# WITHOUT duplicates
dat2 <- left_join(dat, survdat2, by = "SurveyID")
#Check that there aren't duplicates
nrow(dat2)
nrow(dat)

#Add the site level information 
sdat <- read_csv("data-raw/Rottnest-Island-sites.csv")
length(unique(sdat$SiteCode))
nrow(sdat)
dat3 <- left_join(dat2, sdat, by= "SiteCode")
nrow(dat3)
names(dat3)

#
# Summarizing by groups
#

sum(dat3$Abundance)
#Try again, but ignore missing numbers
sum(dat3$Abundance, na.rm = TRUE)

#this does the same as above, but is a step on the way
# to more complex summaries
summarize(dat3, sum(Abundance, na.rm = TRUE))

#Tell R what to group by, in this case surveys
dat3g <- group_by(dat3, SurveyID)
datA <- summarize(dat3g, 
          sum_abund = sum(Abundance, na.rm = TRUE))
datA

# Histogram of survey level abundances
ggplot(datA) + 
  aes(x = sum_abund, color = NULL) + 
  geom_histogram()

#histogram of all fish schools
ggplot(dat) + 
  aes(x = Abundance, color = NULL) +
  geom_histogram()

#
#Grouping by multiple variables 
#
unique(dat3$TempTrop_23cutoff)

#REMOVE species with NA temp/trop

#is.na - find the NA rows for temptrop
# ! means NOT
# so !is.na() find the NOT NAs (so the rows that have
# a value)
#now use that to filter out all the NAs:
dat3 <- filter(dat3, !is.na(TempTrop_23cutoff))

#Group by multple variables 
dat3$year <- year(dat3$SurveyDate)
#
dat3g <- group_by(dat3, SurveyID, SiteCode,
                  year, TempTrop_23cutoff)
#Summarize by unique combos of 
#year, surveyid, sitecode, temptrop
dat4 <- summarize(dat3g, 
                  sum_abund = sum(Abundance, na.rm = TRUE))
dat4

#plot abundance trends by year for tropical and temperate spp. 
ggplot(dat4) + 
  aes(x = year, y = sum_abund, color = TempTrop_23cutoff) +
  geom_point() + 
  stat_smooth() + 
  #Facets splits it into two panels, one for each 
  # of the names in 'temptrop'
  facet_wrap(~TempTrop_23cutoff) + 
  scale_y_log10()

#
# Combining plots and saving them to a file 
#
library(patchwork)

# Histogram of survey level abundances
a <- ggplot(datA) + 
  aes(x = sum_abund, color = NULL) + 
  geom_histogram() + 
  theme_classic()

b <- ggplot(dat4) + 
  aes(x = year, y = sum_abund, color = TempTrop_23cutoff) +
  geom_point() + 
  stat_smooth() + 
  scale_y_log10() + 
  xlab("Year") + 
  ylab("Abundance") + 
  theme_classic()
#we created the plot, but saved it in 'b'
#now to see 'b', just type 'b':
b

c <- a + b
c
c_stacked <- a/b
c_stacked

ggsave("fig-1.png", c_stacked) #for png
ggsave("fig-1.pdf", c_stacked) # for pdf

