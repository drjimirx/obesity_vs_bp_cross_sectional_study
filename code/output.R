library(dplyr)

table1_continuous <- analysis_data %>%
  group_by(central_obesity) %>%
  summarise(
    n = n(),
    
    Age_mean = mean(Age),
    Age_sd = sd(Age),
    
    SBP_mean = mean(SBP),
    SBP_sd = sd(SBP),
    
    DBP_mean = mean(DBP),
    DBP_sd = sd(DBP),
    
    WC_mean = mean(WC),
    WC_sd = sd(WC),
    
    BMI_mean = mean(BMI),
    BMI_sd = sd(BMI)
  )

table1_continuous
table(analysis_data$Gender, analysis_data$central_obesity)
table(analysis_data$Alcoholconsumption,
      analysis_data$central_obesity)
table(analysis_data$physicalActivity,
      analysis_data$central_obesity)
table(analysis_data$elevated_BP,
    analysis_data$central_obesity)

prop.table(
  table(analysis_data$Gender, analysis_data$central_obesity),
  margin = 2
) * 100

prop.table(
  table(analysis_data$Alcoholconsumption,
        analysis_data$central_obesity),
  margin = 2
) * 100

prop.table(
  table(analysis_data$physicalActivity,
        analysis_data$central_obesity),
  margin = 2
) * 100
prop.table(
  table(analysis_data$elevated_BP,
        analysis_data$central_obesity),
  margin = 2
) * 100


overall_summary <- analysis_data %>%
  summarise(
    n = n(),
    
    Age_mean = mean(Age),
    Age_sd = sd(Age),
    
    SBP_mean = mean(SBP),
    SBP_sd = sd(SBP),
    
    DBP_mean = mean(DBP),
    DBP_sd = sd(DBP),
    
    WC_mean = mean(WC),
    WC_sd = sd(WC),
    
    BMI_mean = mean(BMI),
    BMI_sd = sd(BMI)
  )

overall_summary
table(analysis_data$Gender)
prop.table(table(analysis_data$Gender)) * 100

table(analysis_data$Alcoholconsumption)
prop.table(table(analysis_data$Alcoholconsumption)) * 100

table(analysis_data$physicalActivity)
prop.table(table(analysis_data$physicalActivity)) * 100

table(analysis_data$elevated_BP)
prop.table(table(analysis_data$elevated_BP)) * 100
