
## test to see whether github desktop worked....###

#...15/11/2023
#...Dummy analysis..##

#  READING SPEICIFC COLUMNS AND ROWS....#
fishdat <- read.csv('dat.csv', header = TRUE)
fishdat [1, 3]
fishdat [, 2]
fishdat [1,]
fishdat [, c(1, 3)]

fishdat$site
fishdat$transect
fishdat$fishing
fishdat$abundance

head(fishdat)
head(fishdat, 10)

fishdat [4]
fishdat [1]
fishdat [2]
fishdat [1, 2]
nrow(fishdat)
mean(fishdat$abundance)
sd(fishdat$abundance)
table(fishdat$site)
table(fishdat$site)
table(fishdat$transect)
View(fishdat)


install.packages('ggplot2')
install.packages('dplyr')
install.packages('tidyr')
library(dplyr)
library(tidyr)
library(ggplot2)


fishdat$site == 'A'
fishdat$site == 'B'
fishdat$site == 'b'


which(fishdat$site != 'A')
which(fishdat$fishing > 6.01)
which(fishdat$fishing >= 6.01)


#Correcting mistakes in data frames

ierror <- which(fishdat$site == 'b')

fishdat$site[ierror]
fishdat$site[ierror] <- 'B'
View(fishdat)


#Function to drop level from the actual table in hte dataframe
table(fishdat$site)
fishdat$site <- droplevels(fishdat$site)
table(fishdat$site)

fishdat$site == 'b'


#sIMPLE BOXPLOTS GRAPHS 

ggplot(fishdat, aes(x = site, y = abundance)) + geom_boxplot()

#Saving your new and/or corrected dataframe 
write.csv(fishdat, 'data_corrected.csv', row.names = FALSE)

#putting box plots with specific binwidth
ggplot(fishdat, aes(x = abundance)) + geom_histogram(binwidth = 3)

ggplot(fishdat, aes(x = abundance)) + geom_histogram(binwidth = 4)
ggplot(fishdat, aes(x = abundance)) + geom_histogram(binwidth = 1)

ggplot(fishdat, aes(x = abundance)) + geom_histogram(binwidth = 2)



#putting geom_points 
ggplot(fishdat, aes(x = fishing, y = abundance)) + geom_point()


#....Command to change graph appearacens 
ggplot(fishdat, aes(x = fishing, y = abundance)) + geom_point(size = 4) +
  xlab('Fishing pressure.index') +
  ylab('Fish abundance')

ggplot(fishdat, aes(x = fishing, y = abundance)) + geom_point(size = 5) +
  ylab('Fish abundance') + 
  xlab('jFishing pressure index') +
  labs(colour = 'Sampling sites')


ggplot(fishdat, aes(x = fishing, y = abundance)) + geom_point(size = 5) +
  ylab('Fish abundance') + 
  xlab('Fishing pressure index') + 
  labs(Colour = 'sampling sites')


#....LOADING DATA WRANGLING PACKAGES....#
fish_A <- filter(fishdat, site == 'A')
fish_A
ggplot(fish_A, aes(x = fishing, y = abundance)) + geom_point(size = 5) + 
  ylab('FIsh abundance') + 
  xlab('Fishing pressure index')


#Summaries using dplr
grps <- group_by(fishdat, site)
View(grps)
grps

#Simple summary of the mean fishing at every site
summarise(grps, mean_fn = mean(fishing))

#Simple summary of the mean fishing and mean abundance at every site
summarise(grps, mean_f = mean(fishing), mean_abund = mean(abundance))

#Simple summary of the standard deviation of fishing and standard deviation of abundance at every site
summarise(grps, sd_f = sd(fishing), sd_abund = sd(abundance))


#Storing the the summary as an object 
datsum <- summarise(grps, meanF = mean(fishing), mean_abund = mean(abundance), 
                    sdF = sd(fishing), sdabund = sd(abundance))

datsum


p <- ggplot(datsum, aes(x = meanF, y = mean_abund, color = site)) + 
  geom_point(size = 4) +
  ylab('Fish abundance') + 
  xlab('Fishing pressure index')

p

#With the base plot created as an object P, we can add more function   

p + geom_errorbar(aes(ymin = mean_abund - sdabund, ymax = mean_abund + sdabund))

p + geom_errorbar(aes(ymin = mean_abund - sdabund, ymax = mean_abund + sdabund)) +
  geom_errorbar(aes(xmin = meanF - sdF, xmax = meanF + sdF))

#Aesthetics in multi-line coding: utilizing the pipe function %>%
summarise(group_by(fishdat, site), meanF = mean(fishing))
grp <- group_by(fishdat, site)
summarise(grp, meanF = mean(fishing))

fishdat%>%group_by(site)%>%summarise(meanF=mean(fishing))


#Joining dataframes
sitedat <- read.csv('Sitesdat.csv', header = TRUE)
View(sitedat)
head(sitedat, 10)
head(sitedat, 5)

#joining dataframe using the innder_join() function

datnew <- inner_join(fishdat, sitedat, by = 'site')
datnew
fishdat
sitedat
write.csv(datnew, 'data_combined.csv', row.names = FALSE)

#Ploting fishing pressure against distance to pot 
ggplot(datnew, aes(x = distance, y = fishing, colour = site)) +
  geom_point(size = 3)

P2 <- ggplot(datnew, aes(x = distance, y = fishing, colour = site)) +
  geom_point(size = 3)

datsum
datnew

datdis <- inner_join(datsum, datnew, by = 'site')
datdis

ggplot(datdis,  aes(x = distance, y = fishing, colour = site)) +
  geom_point(size = 3) + geom_errorbar(aes(ymin = mean_abund - sdabund, ymax = mean_abund + sdabund))


ggplot(datdis,  aes(x = distance, y = fishing, colour = site)) +
  geom_point(size = 3) + geom_errorbar(aes(ymin = mean_abund - sdabund, ymax = mean_abund + sdabund))

datdis

ggplot(datdis,  aes(x = distance, y = fishing, colour = site)) +
  geom_point(size = 3) + geom_errorbar(aes(ymin = meanF - sdF, ymax = meanF + sdF))

#fromal analysis using liner models 

lm(fishing ~ distance, data = datnew)

#SAVING THE LINEAR MODEL AS AN OBJECT AND PROVIDING A SUMMARY

modl <- lm(fishing ~ distance, data = datnew) 

summary(modl)
modl$coefficients
modl$effects
modl$rank
modl$fitted.values
modl$assign


#adding the line of best fit
P2 + geom_abline(intercept = modl$coefficients[1], slope = modl$coefficients[2])

datnew

#fitting line of best fit awithout table summary 
P2 + stat_smooth(method = 'lm', se = T, aes(group = 1))





