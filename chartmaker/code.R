install.packages(c(
  "dplyr",
  "ggplot2",
  "broom",
  "sandwich",
  "lmtest",
  "car",
  "flextable",
  "officer",
  "writexl"
))

library(dplyr)
library(ggplot2)
library(broom)
library(sandwich)
library(lmtest)
library(car)
library(flextable)
library(officer)
library(writexl)

model_SBP_adj <- lm(
  SBP ~ central_obesity + Age + Gender +
    Alcoholconsumption + physicalActivity,
  data = analysis_data
)

model_DBP_adj <- lm(
  DBP ~ central_obesity + Age + Gender +
    Alcoholconsumption + physicalActivity,
  data = analysis_data
)


analysis_data <- analysis_data %>%
  mutate(
    elevated_BP_num = ifelse(elevated_BP == "Yes", 1, 0)
  )

model_PR <- glm(
  elevated_BP_num ~ central_obesity + Age + Gender +
    Alcoholconsumption + physicalActivity,
  family = poisson(link = "log"),
  data = analysis_data
)

robust_PR <- coeftest(
  model_PR,
  vcov = vcovHC(model_PR, type = "HC0")
)


#create table 1 from data
table1 <- analysis_data %>%
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

table1
table(analysis_data$central_obesity, analysis_data$Gender)

table(analysis_data$central_obesity,
      analysis_data$Alcoholconsumption)

table(analysis_data$central_obesity,
      analysis_data$physicalActivity)

table(analysis_data$central_obesity,
      analysis_data$elevated_BP)

install.packages("gtsummary")
library(gtsummary)
table1_gtsummary <- analysis_data %>%
  select(
    central_obesity,
    Age,
    Gender,
    SBP,
    DBP,
    WC,
    BMI,
    Alcoholconsumption,
    physicalActivity,
    elevated_BP
  ) %>%
  tbl_summary(
    by = central_obesity,
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    label = list(
      Age ~ "Age, years",
      Gender ~ "Gender",
      SBP ~ "Systolic blood pressure, mmHg",
      DBP ~ "Diastolic blood pressure, mmHg",
      WC ~ "Waist circumference, cm",
      BMI ~ "BMI, kg/m²",
      Alcoholconsumption ~ "Alcohol consumption",
      physicalActivity ~ "Physical activity",
      elevated_BP ~ "Elevated BP"
    )
  ) %>%
  modify_header(
    label ~ "**Characteristic**",
    stat_1 ~ "**No central obesity**",
    stat_2 ~ "**Central obesity**"
  ) %>%
  bold_labels()

table1_gtsummary


#Create Table 2 from your actual regression models
SBP_results <- tidy(
  model_SBP_adj,
  conf.int = TRUE
)

SBP_results
DBP_results <- tidy(
  model_DBP_adj,
  conf.int = TRUE
)

DBP_results
SBP_central <- SBP_results %>%
  filter(term == "central_obesityYes")

DBP_central <- DBP_results %>%
  filter(term == "central_obesityYes")

SBP_central
DBP_central

table2 <- data.frame(
  Outcome = c(
    "SBP (mmHg)",
    "DBP (mmHg)",
    "Elevated BP"
  ),
  
  Crude_effect = c(
    "β = 3.98",
    "β = 3.22",
    "PR = 3.96"
  ),
  
  Adjusted_effect = c(
    paste0(
      "β = ",
      round(SBP_central$estimate, 2)
    ),
    paste0(
      "β = ",
      round(DBP_central$estimate, 2)
    ),
    paste0(
      "PR = ",
      round(PR_results["central_obesityYes", "PR"], 2)
    )
  ),
  
  CI_95 = c(
    paste0(
      round(SBP_central$conf.low, 2),
      " to ",
      round(SBP_central$conf.high, 2)
    ),
    paste0(
      round(DBP_central$conf.low, 2),
      " to ",
      round(DBP_central$conf.high, 2)
    ),
    paste0(
      round(PR_results["central_obesityYes", "Lower_95CI"], 2),
      " to ",
      round(PR_results["central_obesityYes", "Upper_95CI"], 2)
    )
  ),
  
  P_value = c(
    format.pval(SBP_central$p.value, digits = 3),
    format.pval(DBP_central$p.value, digits = 3),
    format.pval(
      PR_results["central_obesityYes", "P_value"],
      digits = 3
    )
  )
)

table2
install.packages("gt")
library(gt)
table2_ft <- gt(table2)

table2 %>%
  gt() %>%
  tab_header(
    title = "Table 2. Association of Central Obesity with Blood Pressure Outcomes"
  ) %>%
  gtsave("result/Table_2_Central_Obesity_BP.html")



#Create Figure 1: elevated BP prevalence
plot_data <- analysis_data %>%
  group_by(central_obesity) %>%
  summarise(
    n = n(),
    elevated_n = sum(elevated_BP == "Yes"),
    prevalence = elevated_n / n * 100
  )

plot_data
fig1 <- ggplot(
  plot_data,
  aes(
    x = central_obesity,
    y = prevalence
  )
) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(prevalence, 1), "%")),
    vjust = -0.4,
    size = 5
  ) +
  labs(
    title = "Prevalence of Elevated Blood Pressure by Central Obesity",
    x = "Central obesity",
    y = "Participants with elevated BP (%)"
  ) +
  ylim(0, 30) +
  theme_classic(base_size = 13)

fig1
ggsave(
  "result/Figure_1_Elevated_BP_by_Central_Obesity.png",
  fig1,
  width = 7,
  height = 5,
  dpi = 300
)


#Create Figure 2: adjusted regression effects
forest_data <- data.frame(
  Outcome = c("SBP", "DBP"),
  
  Estimate = c(
    SBP_central$estimate,
    DBP_central$estimate
  ),
  
  Lower = c(
    SBP_central$conf.low,
    DBP_central$conf.low
  ),
  
  Upper = c(
    SBP_central$conf.high,
    DBP_central$conf.high
  )
)

forest_data
fig2 <- ggplot(
  forest_data,
  aes(
    x = Estimate,
    y = Outcome
  )
) +
  geom_errorbar(
    aes(
      xmin = Lower,
      xmax = Upper
    ),
    orientation = "y",
    height = 0.15
  ) +
  geom_point(size = 3) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Adjusted Association of Central Obesity With Blood Pressure",
    x = "Adjusted difference in blood pressure (mmHg)",
    y = NULL
  ) +
  theme_classic(base_size = 13)

fig2
ggsave(
  "result/Figure_2_Adjusted_Central_Obesity_BP.png",
  fig2,
  width = 7,
  height = 5,
  dpi = 300
)

#Create a proper regression table containing all covariates
model_table <- tbl_regression(
  model_SBP_adj,
  intercept = FALSE,
  exponentiate = FALSE
) %>%
  bold_labels()

model_table
model_table_DBP <- tbl_regression(
  model_DBP_adj,
  intercept = FALSE,
  exponentiate = FALSE
) %>%
  bold_labels()

model_table_DBP
combined_models <- tbl_merge(
  tbls = list(
    model_table,
    model_table_DBP
  ),
  tab_spanner = c(
    "**SBP model**",
    "**DBP model**"
  )
)

table3<-combined_models


#Create the modified Poisson table
PR_table <- data.frame(
  Predictor = rownames(PR_results),
  PR = PR_results$PR,
  Lower_95CI = PR_results$Lower_95CI,
  Upper_95CI = PR_results$Upper_95CI,
  P_value = PR_results$P_value
)

PR_table
PR_table <- PR_table %>%
  filter(Predictor != "(Intercept)")
PR_table$Predictor <- c(
  "Central obesity",
  "Age",
  "Male gender",
  "Alcohol consumption",
  "Sedentary physical activity"
)
PR_table <- PR_table %>%
  mutate(
    `Adjusted PR (95% CI)` = paste0(
      round(PR, 2),
      " (",
      round(Lower_95CI, 2),
      "–",
      round(Upper_95CI, 2),
      ")"
    ),
    `P-value` = format.pval(P_value, digits = 3)
  ) %>%
  select(
    Predictor,
    `Adjusted PR (95% CI)`,
    `P-value`
  )

PR_table
table3_excel <- as.data.frame(table3)
class(table1)
class(table2)
class(table3)
class(PR_table)

library(writexl)

write_xlsx(
  list(
    `Table 1` = as.data.frame(table1),
    `Table 2` = table2,
    `Table 3` = table3,
    `Table 4` = PR_table
  ),
  "Thesis_Analysis_Tables.xlsx"
)
