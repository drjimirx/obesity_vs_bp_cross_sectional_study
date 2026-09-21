t.test(
  Age ~ central_obesity,
  data = analysis_data
)

table(
  analysis_data$Gender,
  analysis_data$central_obesity
)

chisq.test(
  table(
    analysis_data$Gender,
    analysis_data$central_obesity
  )
)

table(
  analysis_data$Alcoholconsumption,
  analysis_data$central_obesity
)

chisq.test(
  table(
    analysis_data$Alcoholconsumption,
    analysis_data$central_obesity
  )
)

table(
  analysis_data$physicalActivity,
  analysis_data$central_obesity
)

chisq.test(
  table(
    analysis_data$physicalActivity,
    analysis_data$central_obesity
  )
)

#get a table
analysis_data %>%
  group_by(central_obesity) %>%
  summarise(
    n = n(),
    mean_age = mean(Age),
    sd_age = sd(Age),
    mean_SBP = mean(SBP),
    sd_SBP = sd(SBP),
    mean_DBP = mean(DBP),
    sd_DBP = sd(DBP),
    mean_BMI = mean(BMI),
    sd_BMI = sd(BMI),
    mean_WC = mean(WC),
    sd_WC = sd(WC)
  )


t.test(
  Age ~ central_obesity,
  data = analysis_data
)
