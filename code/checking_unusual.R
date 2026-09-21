analysis_data %>%
  arrange(BMI) %>%
  select(Age, Gender, weight, height, BMI, WC, HC, WHR) %>%
  head(10)

analysis_data %>%
  arrange(desc(BMI)) %>%
  select(Age, Gender, weight, height, BMI, WC, HC, WHR) %>%
  head(10)

analysis_data %>%
  arrange(desc(WHR)) %>%
  select(Age, Gender, weight, height, BMI, WC, HC, WHR) %>%
  head(10)

analysis_data %>%
  arrange(desc(SBP)) %>%
  select(Age, Gender, SBP, DBP, WC, HC, BMI, WHR) %>%
  head(10)

analysis_data %>%
  arrange(desc(DBP)) %>%
  select(Age, Gender, SBP, DBP, WC, HC, BMI, WHR) %>%
  head(10)

analysis_data %>%
  filter(HC < 60 | WC < 50) %>%
  select(Age, Gender, weight, height, BMI, WC, HC, WHR)
analysis_data %>%
  filter(WHR > 1.20) %>%
  select(Age, Gender, weight, height, BMI, WC, HC, WHR)
