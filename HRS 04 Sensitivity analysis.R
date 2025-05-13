library(dplyr)
library(survival)
library(ggplot2)
library(broom)
library(survcomp)
library(survMisc)

df_complete <- read.csv("D:\\R project\\Digital\\HRS\\sensitivity analysis\\unimputed data\\hrs_complete.csv")
df_imputed <- read.csv("D:\\R project\\Digital\\HRS\\hrs_inputed_mortality.csv")

#1.processing complete cases###########
df_complete <- df_complete[,-1]
df_complete_death <- read.csv('D:\\R project\\Digital\\HRS\\hrs.csv')
df_complete_death <- df_complete_death[,-1]
#survival analysis
df_complete <- merge(df_complete, df_complete_death[, c("hhid", "hhidpn", "death_year", "death_month")], 
            by = c("hhid", "hhidpn"), all.x = TRUE)

df_complete <- subset(df_complete, select = -c(death_year.x, death_month.x))
names(df_complete)[names(df_complete) == "death_year.y"] <- "death_year"
names(df_complete)[names(df_complete) == "death_month.y"] <- "death_month"

## survival status
df_complete$death_year[df_complete$death_year >= 2019] <- NA
df_complete$survival_status <- NA
df_complete$survival_status <- ifelse(is.na(df_complete$death_date),0,1)

## survival time
df_complete <- df_complete %>%
  mutate(
    # 如果 inv_date14 是 NA，使用中位数填充
    inv_date14 = ifelse(is.na(inv_date14), median(df_complete$inv_date14, na.rm = TRUE), inv_date14), 
    
    # 确保日期列是 Date 类型
    inv_date10 = as.Date(inv_date10),
    inv_date14 = as.Date(inv_date14),
    death_date = as.Date(death_date),
    exit_date = as.Date(exit_date),
    
    # 计算 survival_time
    survival_time = case_when(
      survival_status == 0 ~ (year(inv_date14) - year(inv_date10)) * 12 + (month(inv_date14) - month(inv_date10)),  # Calculate months between inv_date14 and inv_date10
      !is.na(death_date) ~ (year(death_date) - year(inv_date10)) * 12 + (month(death_date) - month(inv_date10)),    # Calculate months between death_date and inv_date10
      is.na(death_date) ~ (year(exit_date) - year(inv_date10)) * 12 + (month(exit_date) - month(inv_date10))       # Calculate months between exit_date and inv_date10
    )
  )
#reference group
df_complete$digital_usage <- factor(df_complete$digital_usage, 
                           levels = c("Never users", "Dropouts", "Intermittent Users", "Adopters", "Consistent users"))
df_complete$digital_usage <- relevel(df_complete$digital_usage, ref = "Never users")
df_complete$Race <- factor(df_complete$Race,level = c("Non-Hispanic White","Other") )
df_complete$Race <- relevel(df_complete$Race,ref = "Non-Hispanic White" )
# df_complete$education_cate <- factor(df_complete$education_cate,level = c("Lt High-school","GED","High-school graduate","Some college","College and above"))
# df_complete$education_cate <- relevel(df_complete$education_cate, ref = "Lt High-school")
df_complete$H_education <- factor(df_complete$H_education)
df_complete$H_education <- relevel(df_complete$H_education, ref = "Less than upper secondary")
df_complete$health_condition <- as.numeric(df_complete$health_condition)

#reference group
# df_imputed$digital_usage <- factor(df_imputed$digital_usage, 
#                                     levels = c("Never users", "Dropouts", "Intermittent Users", "Adopters", "Consistent users"))
# df_imputed$digital_usage <- relevel(df_imputed$digital_usage, ref = "Never users")
# df_imputed$Race <- factor(df_imputed$Race,level = c("Non-Hispanic White","Other") )
# df_imputed$Race <- relevel(df_imputed$Race,ref = "Non-Hispanic White" )
# # df_imputed$education_cate <- factor(df_imputed$education_cate,level = c("Lt High-school","GED","High-school graduate","Some college","College and above"))
# # df_imputed$education_cate <- relevel(df_imputed$education_cate, ref = "Lt High-school")
# df_imputed$H_education <- factor(df_imputed$H_education)
# df_imputed$H_education <- relevel(df_imputed$H_education, ref = "Less than upper secondary")
# df_imputed$health_condition <- as.numeric(df_imputed$health_condition)

#1. Comparison of Complete Cases vs. Imputed Data Model Results
##complete cases
df_complete$age_sq <- (df_complete$age)^2
model1_complete <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + age_sq + gender + Race 
                        + H_education + marital_status + health_condition, data = df_complete)
summary(model1_complete)

##comparing result
library(ggplot2)
library(survival)
library(survminer)
### complete
base_surv <- survfit(Surv(survival_time, survival_status) ~ digital_usage,data = df_complete)
ggsurvplot(base_surv, data = df_complete,legend.title = "Digital Usage",
           legend.labs = levels(as.factor(df_complete$digital_usage)),
           xlab = "Survival Months",  
           ylab = "Suevival Probability",
           pval = TRUE)

#2. interaction#####
## digital_usage * gender
model_interaction_1 <- coxph(Surv(survival_time, survival_status) ~ digital_usage * gender  + age+ age^2 +  Race 
                             + H_education + marital_status + health_condition, data = df_imputed)
summary(model_interaction_1)

## digital_usage * Race
model_interaction_2 <- coxph(Surv(survival_time, survival_status) ~ digital_usage * Race  + age+ age^2 + gender 
                             + H_education + marital_status + health_condition, data = df_imputed)
summary(model_interaction_2)

## digital_usage * education
model_interaction_3 <- coxph(Surv(survival_time, survival_status) ~ digital_usage *H_education + Race  + age+ age^2 + gender 
                             + marital_status + health_condition, data = df_imputed)
summary(model_interaction_3)

## digital_usage * residence
model_interaction_4 <- coxph(Surv(survival_time, survival_status) ~ digital_usage * residence_rural + Race  + age+ age^2 + gender 
                             +H_education+ marital_status + health_condition, data = df_imputed)
summary(model_interaction_4)

## digital_usage * health_condition
model_interaction_5 <- coxph(Surv(survival_time, survival_status) ~ digital_usage * health_condition + Race  + age+ age^2 + gender 
                             +H_education+ marital_status + health_condition, data = df_imputed)
summary(model_interaction_5)



## digital_usage * living_with_others
cramers_v <- CramerV(df_imputed$marital_status, df_imputed$living_with_others)
print(cramers_v)

model_interaction_6 <- coxph(Surv(survival_time, survival_status) ~ digital_usage * living_with_others + Race + age + age^2 + gender + H_education  + health_condition, data = df_imputed)
summary(model_interaction_6)
vif(model_interaction_6)

## digital_usage * wealth
model_interaction_7 <- coxph(Surv(survival_time, survival_status) ~ digital_usage *equivalized_wealth +marital_status + Race + age + age^2 + gender + H_education  + health_condition, data = df_imputed)
summary(model_interaction_7)

#3. Sequential Inclusion of Covariates#####
# Model 1
model_i_1 <- coxph(Surv(survival_time, survival_status) ~ digital_usage, data = df_imputed)
summary(model_i_1)
rsq_1 <- survMisc::rsq(model_i_1)
print(rsq_1)
c_index_1 <- concordance.index(predict(model_i_1, type = "risk"), 
                               df_imputed$survival_time, 
                               df_imputed$survival_status)
print(c_index_1$c.index)

# Model 2
model_i_2 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2), data = df_imputed)
summary(model_i_2)
rsq_2 <- survMisc::rsq(model_i_2)
print(rsq_2)
c_index_2 <- concordance.index(predict(model_i_2, type = "risk"), 
                               df_imputed$survival_time, 
                               df_imputed$survival_status)
print(c_index_2$c.index)

# Model 3
model_i_3 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2) + gender, data = df_imputed)
summary(model_i_3)
rsq_3 <- survMisc::rsq(model_i_3)
print(rsq_3)
c_index_3 <- concordance.index(predict(model_i_3, type = "risk"), 
                               df_imputed$survival_time, 
                               df_imputed$survival_status)
print(c_index_3$c.index)

# Model 4
model_i_4 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2) + gender + Race, data = df_imputed)
summary(model_i_4)
rsq_4 <- survMisc::rsq(model_i_4)
print(rsq_4)
c_index_4 <- concordance.index(predict(model_i_4, type = "risk"), 
                               df_imputed$survival_time, 
                               df_imputed$survival_status)
print(c_index_4$c.index)

# Model 5
model_i_5 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2) + gender + Race + marital_status, data = df_imputed)
summary(model_i_5)
rsq_5 <- survMisc::rsq(model_i_5)
print(rsq_5)
c_index_5 <- concordance.index(predict(model_i_5, type = "risk"), 
                               df_imputed$survival_time, 
                               df_imputed$survival_status)
print(c_index_5$c.index)

# Model 6
model_i_6 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2) + gender + Race + marital_status + health_condition, data = df_imputed)
summary(model_i_6)
rsq_6 <- survMisc::rsq(model_i_6)
print(rsq_6)
c_index_6 <- concordance.index(predict(model_i_6, type = "risk"), 
                               df_imputed$survival_time, 
                               df_imputed$survival_status)
print(c_index_6$c.index)

# Model 7
model_i_7 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2) + gender + Race + marital_status + health_condition + residence_rural, data = df_imputed)
summary(model_i_7)
rsq_7 <- survMisc::rsq(model_i_7)
print(rsq_7)
c_index_7 <- concordance.index(predict(model_i_7, type = "risk"), 
                               df_imputed$survival_time, 
                               df_imputed$survival_status)
print(c_index_7$c.index)

# Model 8
model_i_8 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2) + gender + Race + marital_status + health_condition + residence_rural + residence_region, data = df_imputed)
summary(model_i_8)
rsq_8 <- survMisc::rsq(model_i_8)
print(rsq_8)
c_index_8 <- concordance.index(predict(model_i_8, type = "risk"), 
                               df_imputed$survival_time, 
                               df_imputed$survival_status)
print(c_index_8$c.index)

# Model 9
model_i_9 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2) + gender + Race + marital_status + health_condition + residence_rural + residence_region + employment_status, data = df_imputed)
summary(model_i_9)
rsq_9 <- survMisc::rsq(model_i_9)
print(rsq_9)
c_index_9 <- concordance.index(predict(model_i_9, type = "risk"), 
                               df_imputed$survival_time, 
                               df_imputed$survival_status)
print(c_index_9$c.index)

# Model 10
model_i_10 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2) + gender + Race + marital_status + health_condition + residence_rural + residence_region + employment_status + equivalized_wealth, data = df_imputed)
summary(model_i_10)
rsq_10 <- survMisc::rsq(model_i_10)
print(rsq_10)
c_index_10 <- concordance.index(predict(model_i_10, type = "risk"), 
                                df_imputed$survival_time, 
                                df_imputed$survival_status)
print(c_index_10$c.index)


# r-squared
library(survMisc)

# Model 1
model_i_1 <- coxph(Surv(survival_time, survival_status) ~ digital_usage, data = df_imputed)
summary(model_i_1)
rsq_1 <- survMisc::rsq(model_i_1)
print(rsq_1)

# Model 2
model_i_2 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2), data = df_imputed)
summary(model_i_2)
rsq_2 <- survMisc::rsq(model_i_2)
print(rsq_2)

# Model 3
model_i_3 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2) + gender, data = df_imputed)
summary(model_i_3)
rsq_3 <- survMisc::rsq(model_i_3)
print(rsq_3)

# Model 4
model_i_4 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2) + gender + Race, data = df_imputed)
summary(model_i_4)
rsq_4 <- survMisc::rsq(model_i_4)
print(rsq_4)

# Model 5
model_i_5 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2) + gender + Race + marital_status, data = df_imputed)
summary(model_i_5)
rsq_5 <- survMisc::rsq(model_i_5)
print(rsq_5)

# Model 6
model_i_6 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2) + gender + Race + marital_status + health_condition, data = df_imputed)
summary(model_i_6)
rsq_6 <- survMisc::rsq(model_i_6)
print(rsq_6)

# Model 7
model_i_7 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2) + gender + Race + marital_status + health_condition + residence_rural, data = df_imputed)
summary(model_i_7)
rsq_7 <- survMisc::rsq(model_i_7)
print(rsq_7)

# Model 8
model_i_8 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2) + gender + Race + marital_status + health_condition + residence_rural + residence_region, data = df_imputed)
summary(model_i_8)
rsq_8 <- survMisc::rsq(model_i_8)
print(rsq_8)

# Model 9
model_i_9 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2) + gender + Race + marital_status + health_condition + residence_rural + residence_region + employment_status, data = df_imputed)
summary(model_i_9)
rsq_9 <- survMisc::rsq(model_i_9)
print(rsq_9)

# Model 10
model_i_10 <- coxph(Surv(survival_time, survival_status) ~ digital_usage + age + I(age^2) + gender + Race + marital_status + health_condition + residence_rural + residence_region + employment_status + equivalized_wealth, data = df_imputed)
summary(model_i_10)
rsq_10 <- survMisc::rsq(model_i_10)
print(rsq_10)


