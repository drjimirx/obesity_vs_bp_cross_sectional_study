model_SBP_crude <- lm(
  SBP ~ central_obesity,
  data = analysis_data
)

summary(model_SBP_crude)
confint(model_SBP_crude)

model_DBP_crude <- lm(
  DBP ~ central_obesity,
  data = analysis_data
)

summary(model_DBP_crude)
confint(model_DBP_crude)


#adjusted
model_SBP_adj <- lm(
  SBP ~ central_obesity +
    Age +
    Gender +
    Alcoholconsumption +
    physicalActivity,
  data = analysis_data
)

summary(model_SBP_adj)
confint(model_SBP_adj)

model_DBP_adj <- lm(
  DBP ~ central_obesity +
    Age +
    Gender +
    Alcoholconsumption +
    physicalActivity,
  data = analysis_data
)

summary(model_DBP_adj)
confint(model_DBP_adj)

par(mfrow = c(2, 2))
plot(model_SBP_adj)

par(mfrow = c(2, 2))
plot(model_DBP_adj)


#multicolinearity
install.packages("car")
library(car)

vif(model_SBP_adj)
vif(model_DBP_adj)

bptest(model_SBP_adj)
bptest(model_DBP_adj)


#Third: identify influential observations
influence.measures(model_SBP_adj)
influence.measures(model_DBP_adj)
cooks.distance(model_SBP_adj)
cooks.distance(model_DBP_adj)
sort(cooks.distance(model_SBP_adj), decreasing = TRUE)[1:10]
sort(cooks.distance(model_DBP_adj), decreasing = TRUE)[1:10]


vif(model_SBP_adj)
vif(model_DBP_adj)

install.packages("lmtest")
library(lmtest)
bptest(model_SBP_adj)
bptest(model_DBP_adj)

sort(cooks.distance(model_SBP_adj), decreasing = TRUE)[1:10]
sort(cooks.distance(model_DBP_adj), decreasing = TRUE)[1:10]

analysis_data[c(7, 121, 12, 127, 75, 154), ]
analysis_data[c(7, 121, 12, 127, 75, 154),
              c("Age", "Gender", "SBP", "DBP", "WC", "HC",
                "BMI", "central_obesity",
                "Alcoholconsumption", "physicalActivity")]


model_SBP_no7 <- lm(
  SBP ~ central_obesity +
    Age +
    Gender +
    Alcoholconsumption +
    physicalActivity,
  data = analysis_data[-7, ]
)

summary(model_SBP_no7)
confint(model_SBP_no7)

model_DBP_no7 <- lm(
  DBP ~ central_obesity +
    Age +
    Gender +
    Alcoholconsumption +
    physicalActivity,
  data = analysis_data[-7, ]
)

summary(model_DBP_no7)
confint(model_DBP_no7)

