table(analysis_data$central_obesity)
prop.table(table(analysis_data$central_obesity)) * 100

table(analysis_data$central_obesity_whr)
prop.table(table(analysis_data$central_obesity_whr)) * 100

table(analysis_data$BMI_category)
prop.table(table(analysis_data$BMI_category)) * 100

table(analysis_data$elevated_BP)
prop.table(table(analysis_data$elevated_BP)) * 100


#Look at central obesity by sex
table(
  analysis_data$Gender,
  analysis_data$central_obesity
)
prop.table(
  table(
    analysis_data$Gender,
    analysis_data$central_obesity
  ),
  margin = 1
) * 100


#Look at elevated BP by central obesity
table(
  analysis_data$central_obesity,
  analysis_data$elevated_BP
)

prop.table(
  table(
    analysis_data$central_obesity,
    analysis_data$elevated_BP
  ),
  margin = 1
) * 100

#chi square test
chisq.test(
  table(
    analysis_data$central_obesity,
    analysis_data$elevated_BP
  )
)

#compare actual sbp and dbp
t.test(
  SBP ~ central_obesity,
  data = analysis_data
)

t.test(
  DBP ~ central_obesity,
  data = analysis_data
)

#calculate crude prevalance ratio
# 2 x 2 table
install.packages("epitools")
library(epitools)
tab <- table(
  analysis_data$central_obesity,
  analysis_data$elevated_BP
)

tab
riskratio(tab)
