analysis_data <- analysis_data %>%
  mutate(
    
    # Central obesity based on South Asian waist circumference cut-offs
    central_obesity = case_when(
      Gender == "Male" & WC >= 90 ~ "Yes",
      Gender == "Female" & WC >= 80 ~ "Yes",
      TRUE ~ "No"
    ),
    
    # Central obesity based on WHR
    central_obesity_whr = case_when(
      Gender == "Male" & WHR >= 0.90 ~ "Yes",
      Gender == "Female" & WHR >= 0.85 ~ "Yes",
      TRUE ~ "No"
    ),
    
    # BMI classification
    BMI_category = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI < 25 ~ "Normal",
      BMI < 30 ~ "Overweight",
      BMI >= 30 ~ "Obesity"
    ),
    
    # Elevated BP based on study measurement
    elevated_BP = case_when(
      SBP >= 140 | DBP >= 90 ~ "Yes",
      TRUE ~ "No"
    )
  )

#making category
analysis_data <- analysis_data %>%
  mutate(
    Gender = factor(Gender),
    Alcoholconsumption = factor(Alcoholconsumption),
    physicalActivity = factor(physicalActivity),
    central_obesity = factor(central_obesity),
    central_obesity_whr = factor(central_obesity_whr),
    BMI_category = factor(
      BMI_category,
      levels = c("Underweight", "Normal", "Overweight", "Obesity")
    ),
    elevated_BP = factor(elevated_BP)
  )
