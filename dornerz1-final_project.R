# set working directory
setwd("/courses/STA145/dornerz1")

# import data
library(readr)
data <- read_csv("data.csv")


# descriptive statistics for table 1
# years in office
summary(data$years_in_office)
sd(data$years_in_office)

#gender
table(data$gender)

# descriptive stats using describe
library(psych)
summary(data$party)
summary(data$elections_run)
table(data$party)
table(data$elections_run)
boxplot(data$party)
boxplot(data$elections_run~ data$party)
t.test(data$elections_run~ data$party)
table(data$elections_run, data$party)
chisq.test(table(data$elections_run, data$party))
lot(data$party, data$elections_run)
model <- lm(party ~ years_in_office, data = data)
summary(model)
abline(model)
plot(data$years_in_office, residuals(model))
abline(h = 0)