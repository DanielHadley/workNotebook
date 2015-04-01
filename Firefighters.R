library(dplyr)
library(truncnorm)


setwd("C:/Users/dhadley/Documents/GitHub/workNotebook/")
allData <- read.csv("./FirefightersVacation.csv")


# FY15 is not complete, so I will drop that
d <- allData %>%
  select(-FY15Missed, -FY15Cost) %>%
  mutate(date = as.Date(Day, format="%m/%d/%Y"))

d[is.na(d)] <- 0

d$costPerShift13 <- ifelse(d$FY13Cost == 0, 0, d$FY13Cost / d$FY13Missed)
d$costPerShift14 <- ifelse(d$FY14Cost == 0, 0, d$FY14Cost / d$FY14Missed)

hist(d$costPerShift13)
hist(d$costPerShift14)

# Most time we are not paying much overtime for missed shifts ^^^


# Now I will group by the number of missed shifts to try and come up 
# with an average value per missed shift
# The problem, of course, is that there are other factors
# like sick shifts that come into play

byMissed <- d %>%
  group_by(FY13Missed) %>%
  summarise(avg = mean(costPerShift13),
            min = min(costPerShift13),
            max = max(costPerShift13),
            stDev = sd(costPerShift13))

# simple regression model to see what an average number of missed shifts will cost
reg1 <- lm(byMissed$avg~byMissed$FY13Missed)
plot(byMissed$avg~byMissed$FY13Missed)
abline(reg1)

byMissed$Predicted <- predict(reg1)



# Now for 14
byMissed14 <- d %>%
  group_by(FY14Missed) %>%
  summarise(avg = mean(costPerShift14),
            min = min(costPerShift14),
            max = max(costPerShift14),
            stDev = sd(costPerShift14))

# simple regression model to see what an average number of missed shifts will cost
byMissed14[1,] <- 0

reg1 <- lm(byMissed14$avg~byMissed14$FY14Missed)
plot(byMissed14$avg~byMissed14$FY14Missed)
abline(reg1)

byMissed14$Predicted <- predict(reg1)





# Now we will go back to d and try to model a year of shifting shifts
d <- mutate(d, avgMissed = (FY13Missed + FY14Missed)/2)

d$OffPeak <- ifelse(d$avgMissed > 5, "No", "Yes")



### A simulation of Off Peak
# First see what the actual dist looks like
OffPeak <- d %>%
  filter(OffPeak == "Yes")

summary(OffPeak$avgMissed)
sd(OffPeak$avgMissed)
hist(OffPeak$avgMissed)

# Now simulate it
OffPeakSim <- rtruncnorm(n=178, a=0.5, b=5, m=3.25, sd=1.25) 
hist(OffPeakSim)

# I round so that I can combine with averages later
OffPeakSim <- round(OffPeakSim)


### A simulation of Peak
# First see what the actual dist looks like
Peak <- d %>%
  filter(OffPeak == "No")

summary(Peak$avgMissed)
sd(Peak$avgMissed)
hist(Peak$avgMissed)

# Now simulate it
PeakSim <- rtruncnorm(n=178, a=5, b=20, m=10.25, sd=3.5) 
hist(PeakSim)

# I round so that I can combine with averages later
PeakSim <- round(PeakSim)



OffPeakSim <- OffPeakSim  %>% as.data.frame() 
PeakSim <- PeakSim  %>% as.data.frame() 

OneYear <- rbind(OffPeakSim, PeakSim)
names(OneYear)[1] <- "FY14Missed"

test <- merge(OneYear, byMissed14)