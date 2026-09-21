#waist circumference as a continuous variable
model_SBP_WC <- lm(
  SBP ~ WC +
    Age +
    Gender +
    Alcoholconsumption +
    physicalActivity,
  data = analysis_data
)

summary(model_SBP_WC)
confint(model_SBP_WC)

model_DBP_WC <- lm(
  DBP ~ WC +
    Age +
    Gender +
    Alcoholconsumption +
    physicalActivity,
  data = analysis_data
)

summary(model_DBP_WC)
confint(model_DBP_WC)


plot(
  analysis_data$WC,
  analysis_data$SBP,
  xlab = "Waist circumference (cm)",
  ylab = "Systolic blood pressure (mmHg)",
  main = "SBP vs Waist Circumference"
)

abline(
  lm(SBP ~ WC, data = analysis_data),
  lwd = 2
)

plot(
  analysis_data$WC,
  analysis_data$DBP,
  xlab = "Waist circumference (cm)",
  ylab = "Diastolic blood pressure (mmHg)",
  main = "DBP vs Waist Circumference"
)

abline(
  lm(DBP ~ WC, data = analysis_data),
  lwd = 2
)

par(mfrow = c(2, 2))
plot(model_SBP_WC)

par(mfrow = c(2, 2))
plot(model_DBP_WC)

par(mfrow = c(1, 1))

plot(analysis_data$Age, analysis_data$SBP,
     xlab = "Age (years)",
     ylab = "Systolic blood pressure (mmHg)",
     main = "SBP vs Age")

abline(lm(SBP ~ Age, data = analysis_data), lwd = 2)

plot(analysis_data$Age, analysis_data$DBP,
     xlab = "Age (years)",
     ylab = "Diastolic blood pressure (mmHg)",
     main = "DBP vs Age")

abline(lm(DBP ~ Age, data = analysis_data), lwd = 2)

analysis_data <- analysis_data %>%
  mutate(
    elevated_BP_num = ifelse(elevated_BP == "Yes", 1, 0)
  )
table(analysis_data$elevated_BP, analysis_data$elevated_BP_num)


model_PR <- glm(
  elevated_BP_num ~ central_obesity + Age + Gender +
    Alcoholconsumption + physicalActivity,
  family = poisson(link = "log"),
  data = analysis_data
)

summary(model_PR)

library(sandwich)
library(lmtest)


coeftest(
  model_PR,
  vcov = vcovHC(model_PR, type = "HC0")
)




robust_se <- sqrt(diag(vcovHC(model_PR, type = "HC0")))

PR_results <- data.frame(
  PR = exp(coef(model_PR)),
  Lower_95CI = exp(coef(model_PR) - 1.96 * robust_se),
  Upper_95CI = exp(coef(model_PR) + 1.96 * robust_se),
  P_value = 2 * pnorm(
    abs(coef(model_PR) / robust_se),
    lower.tail = FALSE
  )
)

PR_results

table(
  analysis_data$central_obesity,
  analysis_data$elevated_BP
)
with(
  analysis_data,
  prop.table(table(central_obesity, elevated_BP), margin = 1)
)
