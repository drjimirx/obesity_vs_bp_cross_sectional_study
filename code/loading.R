install.packages(c("readxl", "dplyr", "ggplot2", "skimr"))

library(readxl)
library(dplyr)
library(ggplot2)
library(skimr)

data <- read_excel("data/raw_data.xlsx")

dim(data)

names(data)

str(data)

summary(data)

skim(data)

#missing data
colSums(is.na(data))

sapply(data, function(x) sum(duplicated(x)))


range(data$Age, na.rm = TRUE)

range(data$SBP, na.rm = TRUE)

range(data$DBP, na.rm = TRUE)

range(data$WC, na.rm = TRUE)

range(data$HC, na.rm = TRUE)

range(data$weight, na.rm = TRUE)

range(data$height, na.rm = TRUE)


table(data$Gender, useNA = "ifany")

table(data$Alcoholconsumption, useNA = "ifany")

table(data$physicalActivity, useNA = "ifany")
