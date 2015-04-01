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


#### This first section is to model the average OT cost per shift given how many shifts must be filled ####
# The problem, of course, is that there are other factors
# like sick shifts that come into play

# First combine the data to get averages and start the model
Thirteen <- d %>%
  select(FY13Missed, Day, costPerShift13) %>%
  rename(Missed = FY13Missed, cost = costPerShift13)

Fourteen <- d %>%
  select(FY14Missed, Day, costPerShift14) %>%
  rename(Missed = FY14Missed, cost = costPerShift14)

CostPerShift <- union(Thirteen, Fourteen)
rm(Thirteen, Fourteen)


# simple regression model to see what an average number of missed shifts will cost
byMissed <- CostPerShift %>%
  group_by(Missed) %>%
  summarise(avg = mean(cost),
            min = min(cost),
            max = max(cost),
            stDev = sd(cost))

byMissed[1,] <- 0

reg1 <- lm(byMissed$avg~byMissed$Missed)
plot(byMissed$avg~byMissed$Missed)
abline(reg1)

byMissed$Predicted <- predict(reg1)


## new is the key for predicting cost per shift
x <- byMissed$Missed
y <- x + byMissed$avg
predict(lm(y ~ x))
new <- data.frame(x = seq(0,27))
reg2 <- predict(lm(y ~ x), new, se.fit = TRUE)

new$CostPerMissed <- reg2$fit
names(new)[1] <- "Missed"




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


### A simulation of Peak
# First see what the actual dist looks like
Peak <- d %>%
  filter(OffPeak == "No")

summary(Peak$avgMissed)
sd(Peak$avgMissed)
hist(Peak$avgMissed)

# Now simulate it
RegularYearSim <- function(nsims){
  for (i in 1:nsims) {
    OffPeakSim <- rtruncnorm(n=178, a=0.5, b=5, m=3.25, sd=1.25) 
    # I round so that I can combine with averages later
    OffPeakSim <- round(OffPeakSim)
    
    PeakSim <- rtruncnorm(n=187, a=5, b=20, m=10.25, sd=3.5) 
    # I round so that I can combine with averages later
    PeakSim <- round(PeakSim)
    
    
    OffPeakSim <- OffPeakSim  %>% as.data.frame() 
    PeakSim <- PeakSim  %>% as.data.frame() 
    
    OneYear <- rbind(OffPeakSim, PeakSim)
    names(OneYear)[1] <- "Missed"
    
    OneYearFinal <- merge(OneYear, new)
    OneYearFinal$TotalCost <- OneYearFinal$Missed * OneYearFinal$CostPerMissed
    i = sum(OneYearFinal$TotalCost)
    print(i)
  }
}


ShiftedYearSim <- function(nsims, avgPeak, avgOffPeak){
  for (i in 1:nsims) {
    OffPeakSim <- rtruncnorm(n=178, a=0.5, b=5, m=avgOffPeak, sd=1.25) 
    # I round so that I can combine with averages later
    OffPeakSim <- round(OffPeakSim)
    
    PeakSim <- rtruncnorm(n=187, a=5, b=20, m=avgPeak, sd=3.5) 
    # I round so that I can combine with averages later
    PeakSim <- round(PeakSim)
    
    
    OffPeakSim <- OffPeakSim  %>% as.data.frame() 
    PeakSim <- PeakSim  %>% as.data.frame() 
    
    OneYear <- rbind(OffPeakSim, PeakSim)
    names(OneYear)[1] <- "Missed"
    
    OneYearFinal <- merge(OneYear, new)
    OneYearFinal$TotalCost <- OneYearFinal$Missed * OneYearFinal$CostPerMissed
    i = sum(OneYearFinal$TotalCost)
    print(i)
  }
}
