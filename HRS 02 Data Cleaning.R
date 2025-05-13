library(ggplot2)
library(dplyr)
library(mice)
library(writexl)
library(tidyr)
library(nnet)


df <- reggplot2df <- read.csv('D:\\R project\\Digital\\HRS\\hrs.csv')
df <- df[,-1]

#define sample size n = 18165
df <- df[df$inw6 == 1,]

df <- df[complete.cases(df$age),]
df <- df[df$age >= 50, ]

#mortality n = 11503 death or dropout
df <- df[df$death_year > 2010 | is.na(df$death_year), ]
df <- df[df$inw10 == 1,]

# missingness pattern
selected_columns <- df[, c("w6","w7","w8","w9","w10")]
# Show the missing data pattern
md_data <- md.pattern(selected_columns,plot = TRUE, rotate.names = TRUE)
md.pattern(selected_columns,plot = TRUE, rotate.names = TRUE)

#1. without imputation (complete cases) n = 9882
df_complete <- df[rowSums(is.na(df[, c("w6","w7","w8","w9","w10")])) < 1, ]
df$c_w6 <- df$w6
df$c_w7 <- df$w7
df$c_w8 <- df$w8
df$c_w9 <- df$w9
df$c_w10 <- df$w10
#2. with imputation n = 11077
# delete rows na > 3 in each row
df <- df[rowSums(is.na(df[, c("w6","w7","w8","w9","w10")])) < 3, ]
selected_columns <- df[, c("w6","w7","w8","w9","w10")]
md.pattern(selected_columns,plot = TRUE, rotate.names = TRUE)
# imputation on specific situation
# w6
df$w6 <- ifelse(is.na(df$w6), 0, df$w6)

#w7-w10
fill_na_with_nearest <- function(df, cols) {
  for (i in cols) {
   
    df[, i] <- ifelse(is.na(df[, i]), lag(df[, i]), df[, i])
    
    df[, i] <- ifelse(is.na(df[, i]), lead(df[, i]), df[, i])
  }
  
  return(df)
}

selected_columns <- c("w6", "w7", "w8", "w9", "w10")
df <- fill_na_with_nearest(df, selected_columns)
print(df)

## sample size with imputation 11697

#construct digital usage
#1. complete cases
df_complete$int_5 <- apply(df_complete[, c("w6","w7","w8","w9","w10")], 
                       1, function(x) paste(x, collapse = ""))
table(df_complete$int_5)
library(tableone)
table_one <- CreateTableOne(vars = "int_5", data = df_complete)
print(table_one)

#2. imputation data
#combined 5 interviews
df$int_5 <- apply(df[, c("w6","w7","w8","w9","w10")], 
                       1, function(x) paste(x, collapse = ""))
table(df$int_5)
library(tableone)
table_one <- CreateTableOne(vars = "int_5", data = df)
print(table_one)

#r6/r10
ggplot(df, aes(x = factor(int_5))) +
  geom_bar(fill = "#0072B2", color = "black") +
  labs(title = "Sample size by category of Digital usage (2002-2010) after imputation.", 
       x = "Categories of Digital Usage", 
       y = "Sample Size") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45))

ggplot(df[df$int_5 != "11111" & df$int_5 != "00000", ], aes(x = factor(int_5))) +
  geom_bar(fill = "#0072B2", color = "black") +
  labs(title = "Sample Size per Category of Internet Usage Except 11111 & 00000 (2002-2010)", 
       x = "Categories of Internet Usage", 
       y = "Sample Size") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45))

#2.with imputation
df$digital_usage <- ifelse(df$int_5 == '00000',"Never users",
                                ifelse(df$int_5 == '11111','Consistent users',
                                       ifelse(df$int_5 == '00001'|df$int_5 == '00011'|
                                                df$int_5 == '00111'|df$int_5 == '01111' ,"Adopters", ifelse(df$int_5 == '00010'|df$int_5 == '00100'|df$int_5 == '00110'|df$int_5 == '01000'|df$int_5 == '01100'|df$int_5 == '01110'|df$int_5 == '10000'|df$int_5 == '11000'|df$int_5 == '11100'|df$int_5 == '11110',"Dropouts","Intermittent Users"))))


table(df$digital_usage)


######################## data cleaning
#gender
df$gender <- as.factor(ifelse(df$gender == 1 , "Men", "Women"))

#Race
#Generate a combined race: 0 non-hisp white, 1 non-hisp black, 2 hispanic, 3 other 
df$Race <- ifelse(df$race == 1 & 
                         df$ethnicity == 0, 0,
                       ifelse(df$race == 2 & 
                                df$ethnicity == 0, 1,
                              ifelse(df$ethnicity == 1, 2, 3)))

df$Race <- ifelse(df$Race == 0, "Non-Hispanic White" , 
                       ifelse(df$Race == 1, "Non-Hispanic Black",
                              ifelse(df$Race == 2, "Hispanic","Other")))
#df$Race <- factor(df$Race,levels = c("Non-Hispanic White" ,"Non-Hispanic Black","Hispanic","Other"))

#df$Race <- ifelse(df$Race == "Non-Hispanic White", "Non-Hispanic White", ifelse(df$Race == "Non-Hispanic Black", "Non-Hispanic Black", "Non-Hispanic African American/Hispanic/Other"))
table(df$Race)

df$Race <- ifelse(df$Race == "Non-Hispanic White","Non-Hispanic White", "Other")
df$Race <- factor(df$Race,levels = c("Non-Hispanic White" ,"Other"))
df$Race <- relevel(df$Race, ref = "Non-Hispanic White")
table(df$Race)

#highest_degree (range 0~8)
df <- df %>%
  mutate(highest_degree = case_when(
    highest_degree == 0 ~ "No degree",
    highest_degree == 1 ~ "GED",
    highest_degree == 2 ~ "HS",
    highest_degree == 3 ~ "HS/GED",
    highest_degree == 4 ~ "AA/ Lt BA",
    highest_degree == 5 ~ "BA",
    highest_degree == 6 ~ "MA/MBA",
    highest_degree == 7 ~ "Law/MD/PhD",
    highest_degree == 8 ~ "Other",
    TRUE ~ NA_character_  # NA
  )) 
df$highest_degree <- factor(df$highest_degree,level = c("No degree", "GED", "HS", "HS/GED", 
                                                                  "AA/ Lt BA", "BA", "MA/MBA", 
                                                                  "Law/MD/PhD", "Other"))

# edcation categories
df <- df %>%
  mutate(education_cate = case_when(
    education_cate == 1 ~ "Lt High-school",  
    education_cate == 2 ~ "GED",
    education_cate == 3 ~ "High-school graduate",
    education_cate == 4 ~ "Some college",
    education_cate == 5 ~ "College and above"
  ))
table(df$education_cate)
df$education_cate <- factor(df$education_cate,level = c("Lt High-school","GED","High-school graduate","Some college","College and above"))


#marital_status
##1.Married 2.Married4.Separated 5.Divorced 6.null 7.Widowed 8.Never married 9.null
df <- df %>%
  mutate(marital_status = case_when(
    marital_status %in% c(1, 2) ~ "Married/Parterned",  # Married
    marital_status %in% c(4, 5, 7, 8) ~ "Other",      # Separated, Divorced, Widowed, Never married
    TRUE ~ NA_character_  # NA for any other values
  ))
table(df$marital_status)
df$marital_status <- factor(df$marital_status,level = c("Married/Parterned","Other"))

#employment_status
##1.Works FT 2.Works PT 3.Unemployed 4.Partly retired 5.Retired 6.Disabled 7.Not in LbrF 
#recode 1=employed, 2=retired, 3=other
df$employment_status <- ifelse(df$employment_status==1, "employed","other")
table(df$employment_status)
df$employment_status <- factor(df$employment_status,level = c("employed","retired", "other"))

#residence_region
#1.Northeast 2.Midwest 3.South 4.West 5.Other
df <- df %>%
  mutate(residence_region = case_when(
    residence_region == 1 ~ "Northeast",
    residence_region == 2 ~ "Midwest",
    residence_region == 3 ~ "South",
    residence_region == 4 ~ "West",
    residence_region == 5 ~ "Other",
    TRUE ~ NA_character_  # NA
  ))
table(df$residence_region)
df$residence_region <- factor(df$residence_region)

#rural
df <- df %>%
  mutate(residence_rural = case_when(
    residence_rural == 0 ~ "urban",
    residence_rural == 1 ~ "rural",
    TRUE ~ NA_character_  # NA
  ))
table(df$residence_rural)
df$residence_rural <- factor(df$residence_rural)

#health_condition
table(df$r10conds)

#living_with_others
df$living_with_others <- as.numeric(df$living_with_others)

#equivalized_wealth
df$equivalized_wealth <- df$wealth/sqrt(df$living_with_others)
df$equivalized_wealth <- round(df$equivalized_wealth/1000, 2)
summary(df$equivalized_wealth)
#df$equivalized_wealth <- log(df$equivalized_wealth + 188.09 + 1)
df$equivalized_wealth <- as.factor(ifelse(df$equivalized_wealth >= 128.91, "Upper wealth", "Lower wealth"))

#education
summary(df$H_education)
df <- df %>%
  mutate(H_education = case_when(
    H_education == 1 ~ "less than upper secondary",
    H_education == 2 ~ "upper secondary and vocat",
    H_education == 3 ~ "tertiary",
    TRUE ~ NA_character_
  ))
df$H_education <- ifelse(df$H_education == "less than upper secondary", "Less than upper secondary","Upper secondary and above")
table(df$H_education)

####exposure digital usage####
df$digital_usage <- factor(df$digital_usage, levels = c("Never users", "Dropouts", "Intermittent Users", "Adopters", "Consistent users"))
df$digital_usage <- relevel(df$digital_usage, ref = "Never users")


#imputation
df_converted <- df[, c("age","gender","education_year","highest_degree","H_education",
                       "marital_status","employment_status","residence_region","equivalized_wealth",
                       "living_with_others","education_mother_year","education_father_year",
                       "residence_rural","Race")]
classes <- sapply(df_converted, class)
labelled_vars <- names(classes[classes == "labelled"])
df_converted <- df_converted %>%
  mutate_if(names(.) %in% c("gender","highest_degree","H_education",
                            "marital_status","employment_status","residence_region","health_condition",
                            "living_with_others",
                            "residence_rural","Race"), as.factor)
set.seed(1005)
mice_mod <- mice(df_converted, method = "cart", m =1, maxit = 1)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(df), names(imputed_data))
df[common_cols] <- imputed_data[common_cols]
write.csv(df,"D:\\R project\\Digital\\HRS\\hrs_imputation.csv")


####iptw####
iptw_m <- multinom(digital_usage ~ age  + gender + Race 
                   + H_education + marital_status , 
                   data = df)

#predict propensity score
df$propensity_scores <- predict(iptw_m, type = "probs")  
summary(df$propensity_scores)

# calculate weights：
#exposure a, weight =  1 / P(exposure == a)
df$digital_usage <- factor(df$digital_usage, 
                                     levels = c("Never users", "Dropouts", "Intermittent Users", "Adopters", "Consistent users"))
levels(df$digital_usage)
for (i in 1:5) {  
  df$iptw_weight[df$digital_usage == levels(df$digital_usage)[i]] <- 
    1 / df$propensity_scores[df$digital_usage == levels(df$digital_usage)[i], i]
}
summary(df$iptw_weight)
hist(df$iptw_weight)

#replace values greater than the 0.99 quantile with the 0.99 quantile value
quantile_99 <- quantile(df$iptw_weight, 0.99)
df$iptw_weight <- ifelse(df$iptw_weight > quantile_99, quantile_99, df$iptw_weight)
summary(df$iptw_weight)

####ipaw####
library(dplyr)
library(tidyr)
library(glm2)

# Step 1: 
df <- df %>%
  mutate(

    death_inw11 = ifelse(inw11 == 5, 1, ifelse(inw11 < 5, 0, NA)),
    death_inw12 = ifelse(inw12 == 5, 1, ifelse(inw12 < 5, 0, NA)),
    death_inw13 = ifelse(inw13 == 5, 1, ifelse(inw13 < 5, 0, NA)),
    death_inw14 = ifelse(inw14 == 5, 1, ifelse(inw14 < 5, 0, NA)),
    
  
    lost_inw11 = ifelse(inw11 == 7 | inw11 == 4, 1, ifelse(inw11 == 1, 0, NA)),
    lost_inw12 = ifelse(inw12 == 7 | inw12 == 4, 1, ifelse(inw12 == 1, 0, NA)),
    lost_inw13 = ifelse(inw13 == 7 | inw13 == 4, 1, ifelse(inw13 == 1, 0, NA)),
    lost_inw14 = ifelse(inw14 == 7 | inw14 == 4, 1, ifelse(inw14 == 1, 0, NA))
  )

library(dplyr)

# Step 2: 
for (wave in 11:14) {

  death_var <- paste0("death_inw", wave)
  lost_var <- paste0("lost_inw", wave)

  model_death <- glm(as.formula(paste(death_var, "~ age + gender + Race + H_education + marital_status")),
                     data = df, 
                     family = binomial, 
                     control = glm.control(maxit = 1000), 
                     na.action = na.exclude)  
  
  model_lost <- glm(as.formula(paste(lost_var, "~ age + gender + Race + H_education + marital_status")),
                    data = df, 
                    family = binomial, 
                    control = glm.control(maxit = 1000),
                    na.action = na.exclude)  
  
  pred_death <- predict(model_death, type = "response")
  pred_lost <- predict(model_lost, type = "response")
  
  # Check if both prediction vectors are the same length as df
  if (length(pred_death) == nrow(df) && length(pred_lost) == nrow(df)) {
    df <- df %>%
      mutate(!!paste0("prob_alive_inw", wave) := 
               (1 - pred_death) * (1 - pred_lost))
  } else {
    warning("not match", wave)
  }
}

# Step 3: 
for (wave in 11:14) {
  prob_var <- paste0("prob_alive_inw", wave)
  ipcw_var <- paste0("ipcw_inw", wave)
  
  df <- df %>%
    mutate(!!ipcw_var := 1 / get(prob_var))
}

# Step 4: 
df <- df %>%
  mutate(
    ipcw_cum_inw11 = ipcw_inw11,
    ipcw_cum_inw12 = ifelse(is.na(ipcw_inw11), NA, ipcw_inw12 * ipcw_inw11),
    ipcw_cum_inw13 = ifelse(is.na(ipcw_cum_inw12), NA, ipcw_inw13 * ipcw_cum_inw12),
    ipcw_cum_inw14 = ifelse(is.na(ipcw_cum_inw13), NA, ipcw_inw14 * ipcw_cum_inw13)
  )

df %>%
  select(ipcw_cum_inw11, ipcw_cum_inw12, ipcw_cum_inw13, ipcw_cum_inw14) %>%
  summary()

write.csv(df, "D:\\R project\\Digital\\HRS\\hrs_imputation.csv")

# wave <- 11  
# death_var <- paste0("lost_inw", wave)
# table(df[[death_var]], df$Race)

####2.complete###########################################################################################################################
#gender
df_complete$gender <- as.factor(ifelse(df_complete$gender == 1 , "Men", "Women"))

#Race
#Generate a combined race: 0 non-hisp white, 1 non-hisp black, 2 hispanic, 3 other 
df_complete$Race <- ifelse(df_complete$race == 1 & 
                    df_complete$ethnicity == 0, 0,
                  ifelse(df_complete$race == 2 & 
                           df_complete$ethnicity == 0, 1,
                         ifelse(df_complete$ethnicity == 1, 2, 3)))

df_complete$Race <- ifelse(df_complete$Race == 0, "Non-Hispanic White" , 
                  ifelse(df_complete$Race == 1, "Non-Hispanic Black",
                         ifelse(df_complete$Race == 2, "Hispanic","Other")))

df_complete$Race <- ifelse(df_complete$Race == "Non-Hispanic White","Non-Hispanic White", "Other")
df_complete$Race <- factor(df_complete$Race,levels = c("Non-Hispanic White" ,"Other"))
df_complete$Race <- relevel(df_complete$Race, ref = "Non-Hispanic White")
table(df_complete$Race)


#highest_degree (range 0~8)
df_complete <- df_complete %>%
  mutate(highest_degree = case_when(
    highest_degree == 0 ~ "No degree",
    highest_degree == 1 ~ "GED",
    highest_degree == 2 ~ "HS",
    highest_degree == 3 ~ "HS/GED",
    highest_degree == 4 ~ "AA/ Lt BA",
    highest_degree == 5 ~ "BA",
    highest_degree == 6 ~ "MA/MBA",
    highest_degree == 7 ~ "Law/MD/PhD",
    highest_degree == 8 ~ "Other",
    TRUE ~ NA_character_  # NA
  )) 
df_complete$highest_degree <- factor(df_complete$highest_degree,level = c("No degree", "GED", "HS", "HS/GED", 
                                                        "AA/ Lt BA", "BA", "MA/MBA", 
                                                        "Law/MD/PhD", "Other"))

# edcation categories
df_complete <- df_complete %>%
  mutate(education_cate = case_when(
    education_cate == 1 ~ "Lt High-school",  
    education_cate == 2 ~ "GED",
    education_cate == 3 ~ "High-school graduate",
    education_cate == 4 ~ "Some college",
    education_cate == 5 ~ "College and above"
  ))
table(df_complete$education_cate)
df_complete$education_cate <- factor(df_complete$education_cate,level = c("Lt High-school","GED","High-school graduate","Some college","College and above"))


#marital_status
##1.Married 2.Married4.Separated 5.Divorced 6.null 7.Widowed 8.Never married 9.null
df_complete <- df_complete %>%
  mutate(marital_status = case_when(
    marital_status %in% c(1, 2) ~ "Married/Parterned",  # Married
    marital_status %in% c(4, 5, 7, 8) ~ "Other",      # Separated, Divorced, Widowed, Never married
    TRUE ~ NA_character_  # NA for any other values
  ))
table(df_complete$marital_status)
df_complete$marital_status <- factor(df_complete$marital_status,level = c("Married/Parterned","Other"))

#employment_status
##1.Works FT 2.Works PT 3.Unemployed 4.Partly retired 5.Retired 6.Disabled 7.Not in LbrF 
#recode 1=employed, 2=retired, 3=other
df_complete$employment_status <- ifelse(df_complete$employment_status==1|df_complete$employment_status==2|
                                 df_complete$employment_status==4, "employed", 
                               ifelse(df_complete$employment_status==5, "retired", "other"))
table(df_complete$employment_status)
df_complete$employment_status <- factor(df_complete$employment_status,level = c("employed","retired", "other"))

#residence_region
#1.Northeast 2.Midwest 3.South 4.West 5.Other
df_complete <- df_complete %>%
  mutate(residence_region = case_when(
    residence_region == 1 ~ "Northeast",
    residence_region == 2 ~ "Midwest",
    residence_region == 3 ~ "South",
    residence_region == 4 ~ "West",
    residence_region == 5 ~ "Other",
    TRUE ~ NA_character_  # NA
  ))
table(df_complete$residence_region)
df_complete$residence_region <- factor(df_complete$residence_region)

#rural
df_complete <- df_complete %>%
  mutate(residence_rural = case_when(
    residence_rural == 0 ~ "urban",
    residence_rural == 1 ~ "rural",
    TRUE ~ NA_character_  # NA
  ))
table(df_complete$residence_rural)
df_complete$residence_rural <- factor(df_complete$residence_rural)

#health_condition
df_complete$health_condition <- factor(df_complete$health_condition)

#living_with_others
df_complete$living_with_others <- as.numeric(df_complete$living_with_others)

df_complete$equivalized_wealth <- df_complete$wealth/sqrt(df_complete$living_with_others)
df_complete$equivalized_wealth <- round(df_complete$equivalized_wealth/1000, 2)
summary(df_complete$equivalized_wealth)
#df_complete$equivalized_wealth <- log(df_complete$equivalized_wealth + 188.09 + 1)
df_complete$equivalized_wealth <- as.factor(ifelse(df_complete$equivalized_wealth >= 132.97, "Upper wealth", "Lower wealth"))

## tertial wealth
df_complete$equivalized_wealth_tertile <- df_complete$wealth/sqrt(df_complete$living_with_others)
quantiles_w <- quantile(df_complete$equivalized_wealth_tertile, probs = c(0.25, 0.75))

df_complete$equivalized_wealth_tertile <- cut(df_complete$equivalized_wealth_tertile,
                                      breaks = c(-Inf, quantiles_w, Inf),
                                      labels = c("Low wealth", "Intermediate wealth", "High wealth"),
                                      right = TRUE)
table(df_complete$equivalized_wealth_tertile)

#education
summary(df_complete$H_education)
df_complete <- df_complete %>%
  mutate(H_education = case_when(
    H_education == 1 ~ "less than upper secondary",
    H_education == 2 ~ "upper secondary and vocat",
    H_education == 3 ~ "tertiary",
    TRUE ~ NA_character_
  ))
df_complete$H_education <- factor(df_complete$H_education, levels = c("less than upper secondary","upper secondary and vocat","tertiary"))
df_complete$H_education <- ifelse(df_complete$H_education == "less than upper secondary","Less than upper secondary","Upper secondary and above")
df_complete$H_education <- factor(df_complete$H_education, levels = c("Less than upper secondary","Upper secondary and above"))
df_complete$H_education <- relevel(df_complete$H_education, ref = "Less than upper secondary")

#exposure
df_complete$digital_usage <- factor(df_complete$digital_usage, levels = c("Never users", "Dropouts", "Intermittent Users", "Adopters", "Consistent users"))
df_complete$digital_usage <- relevel(df_complete$digital_usage, ref = "Never users")
print(levels(df_complete$digital_usage))
print(unique(df_complete$digital_usage))



write.csv(df_complete,"D:\\R project\\Digital\\HRS\\sensitivity analysis\\unimputed data\\hrs_complete.csv")















