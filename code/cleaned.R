analysis_data <- data

#calculate bmi
#also height is in CM.
analysis_data <- analysis_data %>%
  mutate(
    height_m = height / 100,
    BMI = weight / (height_m^2)
  )

#calculate waist to hip ratio
analysis_data <- analysis_data %>%
  mutate(
    WHR = WC / HC
  )

#check result
summary(analysis_data$BMI)
summary(analysis_data$WHR)
head(
  analysis_data %>%
    select(weight, height, BMI, WC, HC, WHR)
)


#checking raw categorical value
unique(analysis_data$Gender)

unique(analysis_data$Alcoholconsumption)

unique(analysis_data$physicalActivity)


#checking distributions
hist(analysis_data$BMI,
     main = "Distribution of BMI",
     xlab = "BMI")

hist(analysis_data$WHR,
     main = "Distribution of Waist-Hip Ratio",
     xlab = "WHR")

hist(analysis_data$SBP,
     main = "Distribution of Systolic Blood Pressure",
     xlab = "SBP")

hist(analysis_data$DBP,
     main = "Distribution of Diastolic Blood Pressure",
     xlab = "DBP")

