library(dplyr)
library(survival)
library(ggplot2)
library(broom)
library(dagitty)
library(ggdag)
library(lubridate)
library(nnet)
library(sjPlot)

df <- read.csv("D:\\R project\\Digital\\HRS\\hrs_imputation.csv")
df <- df[,-1]

####Outcome mortality####
df_death <- read.csv('D:\\R project\\Digital\\HRS\\hrs.csv')
df_death <- df_death[,-1]
df$age_sq <- (df$age)^2

#survival analysis
df_mortality <- merge(df, df_death[, c("hhid", "hhidpn", "death_year", "death_month")], 
            by = c("hhid", "hhidpn"), all.x = TRUE)

df_mortality <- subset(df_mortality, select = -c(death_year.x, death_month.x))
names(df_mortality)[names(df_mortality) == "death_year.y"] <- "death_year"
names(df_mortality)[names(df_mortality) == "death_month.y"] <- "death_month"

## survival status
#df_mortality <- df_mortality[as.Date(df_mortality$death_date, origin = "1960-01-01") < '2019-01-01',]

df_mortality$survival_status <- ifelse(!is.na(df_mortality$death_date) & as.Date(df_mortality$death_date, origin = "1960-01-01") < '2019-01-01',1, 0)
df_mortality$survival_status <- ifelse(!is.na(df_mortality$death_date) & as.Date(df_mortality$death_date, origin = "1960-01-01") < '2019-01-01',1, 0)

df_mortality <- df_mortality %>%
  mutate(
    min_inv_date14 = min(inv_date14, na.rm = TRUE),
    median_inv_date14 = median(inv_date14, na.rm = TRUE),
    survival_date = case_when(
      survival_status == 0 & !is.na(inv_date14) & is.na(death_date)~ as.Date(inv_date14, origin = "1960-01-01"),  
      survival_status == 0 & is.na(inv_date14) & is.na(death_date) ~ as.Date(median_inv_date14, origin = "1960-01-01"), 
      survival_status == 1 ~ as.Date(death_date, origin = "1960-01-01"), 
      TRUE ~ as.Date(NA)  
    )
  ) %>%
  select(-min_inv_date14, -median_inv_date14)

df_mortality <- df_mortality %>%
  mutate(
    survival_time = as.numeric(difftime(survival_date, as.Date(inv_date10, origin = "1960-01-01"), units = "days")) / 30.44
  )

#reference group
df_mortality$digital_usage <- factor(df_mortality$digital_usage, 
                           levels = c("Never users", "Dropouts", "Intermittent Users", "Adopters", "Consistent users"))
df_mortality$digital_usage <- relevel(df_mortality$digital_usage, ref = "Never users")
df_mortality$Race <- factor(df_mortality$Race,levels = c("Non-Hispanic White" ,"Other"))
df_mortality$Race <- relevel(df_mortality$Race,ref = "Non-Hispanic White" )
df_mortality$H_education <- factor(df_mortality$H_education)
df_mortality$H_education <- relevel(df_mortality$H_education, ref = "Less than upper secondary")
#df_mortality$health_condition <- as.numeric(df_mortality$health_condition)
df_mortality$residence_rural <- factor(df_mortality$residence_rural)
df_mortality$residence_rural <- relevel(df_mortality$residence_rural, ref = "rural")
df_mortality$gender <- relevel(factor(df_mortality$gender), ref = "Men")
df_mortality$equivalized_wealth <- relevel(factor(df_mortality$equivalized_wealth), ref = "Lower wealth")

# write.csv(df,"D:\\R project\\Digital\\HRS\\hrs_inputed_mortality.csv")
# df <- read.csv("D:\\R project\\Digital\\HRS\\hrs_inputed_mortality.csv")

####mortality model####
library(survival)
library(survminer)
library(ggplot2)

model_mortality <- coxph(Surv(survival_time, survival_status) ~ 
                           digital_usage + age + age_sq + gender + Race + 
                           residence_rural + H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_mortality, weights = iptw_weight)
summary(model_mortality)
#tab_model(model_mortality)
exp(confint(model_mortality))

model_mortality_unadjusted <- coxph(Surv(survival_time, survival_status) ~ 
                           digital_usage,
                         data = df_mortality, weights = iptw_weight)
summary(model_mortality_unadjusted)


model_mortality_nw <- coxph(Surv(survival_time, survival_status) ~ 
                           digital_usage + age + age_sq + gender + Race + 
                           residence_rural + H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_mortality)
summary(model_mortality_nw)

mean_age <- mean(df_mortality$age, na.rm = TRUE)
mean_age_sq <- mean(df_mortality$age_sq, na.rm = TRUE)
#mean_health_condition <- mean(df_mortality$health_condition, na.rm = TRUE)
mode_gender <- factor('Men')
mode_race <- factor("Non-Hispanic White")
mode_rural <- factor("rural")
mode_education <- factor("Less than upper secondary")
mode_marital <- factor("Married/Parterned")
mode_wealth <- factor("Lower wealth")

newdata <- data.frame(
  digital_usage = factor(levels(df_mortality$digital_usage), levels = levels(df_mortality$digital_usage)),
  age = mean_age,
  age_sq = mean_age_sq,
  gender = rep(mode_gender, length(levels(df_mortality$digital_usage))),
  Race = rep(mode_race, length(levels(df_mortality$digital_usage))),
  residence_rural = rep(mode_rural, length(levels(df_mortality$digital_usage))),
  H_education = rep(mode_education, length(levels(df_mortality$digital_usage))),
  marital_status = rep(mode_marital, length(levels(df_mortality$digital_usage))),
  equivalized_wealth = rep(mode_wealth, length(levels(df_mortality$digital_usage)))
)

fit <- survfit(model_mortality_unadjusted, newdata = newdata)
ggsurv_obj <- ggsurvplot(fit,
                         data = df_mortality,
                         fun = "event",
                         conf.int = FALSE,
                         risk.table = FALSE,
                         palette = "Set1",
                         xlab = "Time (Years)",
                         ylab = "Cumulative Incidence (%)",
                         legend.title = "Digital Usage",
                         legend.labs = c("Never users", "Dropouts", "Intermittent Users", "Adopters", "Consistent users"),
                         risk.table.height = 0.25,
                         title = "A.All-cause mortality",
                         break.time.by = 12,
                         xscale = "d_y",
                         censor = FALSE,    
                         size = 0.7)         

p_final <- ggsurv_obj$plot + 
  theme_minimal() +  
  scale_x_continuous(
    breaks = seq(0, 106, by = 12),
    labels = function(x) paste0(round(x / 12))  
  ) +
  scale_color_manual(
    values = c("Never users" = "#8b0000", "Dropouts" = "#f06d6d", "Intermittent Users" = "#f8c8c8", "Adopters" = "#66cdaa", "Consistent users" = "#006400"),
    labels = c("Never Users", "Dropouts", "Intermittent Users", "Adopters", "Consistent Users") 
  ) +
  theme(
    text = element_text(size = 14),  
    legend.position = c(0.2, 0.8),  
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12),  
    legend.key.size = unit(0.5, "cm"),  
    legend.background = element_rect(color = "black", size = 1, linetype = "solid")  
  ) +
  guides(color = guide_legend(
    title = "Internet use",
    title.position = "top",  
    title.hjust = 0.5, 
    label.position = "right",  
    order = 1)) +
  coord_cartesian(xlim = c(0, 106), ylim = c(0,1))  

#extract plot
png(file = 'D:\\R project\\Digital\\HRS\\plot\\mortality.png',
    width = 1800,
    height = 1200,
    units = "px",
    res = 200)
print(p_final)
dev.off()

p_final_b <- ggsurv_obj$plot + 
  theme_minimal() +  
  scale_x_continuous(
    breaks = seq(0, 106, by = 12),
    labels = function(x) paste0(round(x / 12))  
  ) +
  scale_color_manual(
    values = c("Never users" = "#8b0000", "Dropouts" = "#f06d6d", "Intermittent Users" = "#f8c8c8", "Adopters" = "#66cdaa", "Consistent users" = "#006400"),
    labels = c("Never Users", "Dropouts", "Intermittent Users", "Adopters", "Consistent Users") 
  )  +
  theme(
    text = element_text(size = 14),  
    legend.position = c(0.2, 0.8),  
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12),  
    legend.key.size = unit(0.5, "cm"),  
    legend.background = element_rect(color = "black", size = 1, linetype = "solid"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()   
  ) +
  guides(color = guide_legend(
    title = "Internet use",
    title.position = "top",  
    title.hjust = 0.5, 
    label.position = "right",  
    order = 1)) +
  coord_cartesian(xlim = c(0, 106), ylim = c(0,1))  

#extract plot
png(file = 'D:\\R project\\Digital\\HRS\\plot\\mortality_b.png',
    width = 1800,
    height = 1200,
    units = "px",
    res = 200)
print(p_final_b)
dev.off()

####interaction####
#base model
model_mortality <- coxph(Surv(survival_time, survival_status) ~ 
                                        digital_usage + gender  + age + age_sq + Race + 
                                        residence_rural + H_education + marital_status + 
                                        equivalized_wealth, 
                                      data = df_mortality, weights = iptw_weight)
tab_model(model_mortality)
conf_int <- confint(model_mortality)
round(exp(conf_int), 2)


#gender interaction
model_mortality_gender_inter <- coxph(Surv(survival_time, survival_status) ~ 
                                        digital_usage*gender  + age + age_sq + Race + 
                                        residence_rural + H_education + marital_status + 
                                        equivalized_wealth, 
                                      data = df_mortality, weights = iptw_weight)
tab_model(model_mortality_gender_inter)
conf_int <- confint(model_mortality_gender_inter)
round(exp(conf_int), 2)

#race interaction
model_mortality_race_inter <- coxph(Surv(survival_time, survival_status) ~ 
                                        digital_usage * Race + gender  + age + age_sq + 
                                        residence_rural + H_education + marital_status + 
                                        equivalized_wealth, 
                                      data = df_mortality, weights = iptw_weight)
tab_model(model_mortality_race_inter)
conf_int <- confint(model_mortality_race_inter)
round(exp(conf_int), 2)

#urbanicity interaction
model_mortality_rural_inter <- coxph(Surv(survival_time, survival_status) ~ 
                                      digital_usage * residence_rural + Race + gender  + age + age_sq + 
                                      residence_rural + H_education + marital_status + 
                                      equivalized_wealth, 
                                    data = df_mortality, weights = iptw_weight)
tab_model(model_mortality_rural_inter)
conf_int <- confint(model_mortality_rural_inter)
round(exp(conf_int), 2)

#wealth interaction
model_mortality_wealth_inter <- coxph(Surv(survival_time, survival_status) ~ 
                                       digital_usage * equivalized_wealth + residence_rural + Race + gender  + age + age_sq + 
                                       residence_rural + H_education + marital_status, 
                                     data = df_mortality, weights = iptw_weight)
tab_model(model_mortality_wealth_inter)
conf_int <- confint(model_mortality_wealth_inter)
round(exp(conf_int), 2)

#likelihood ratio test
library(lmtest)
lrt_1 <- lrtest(model_mortality_inter, model_mortality_gender_inter)
lrt_2 <- lrtest(model_mortality_inter, model_mortality_race_inter)
lrt_3 <- lrtest(model_mortality_inter, model_mortality_rural_inter)
lrt_4 <- lrtest(model_mortality_inter, model_mortality_wealth_inter)

lrt_1
lrt_2
lrt_3
lrt_4

####stratified analysis####
#gender
##men
model_mortality_men <- coxph(Surv(survival_time, survival_status) ~ 
                                        digital_usage + age + age_sq + Race + 
                                        residence_rural + H_education + marital_status + 
                                        equivalized_wealth, 
                                      data = df_mortality[df_mortality$gender == "Men",], weights = iptw_weight)
summary(model_mortality_men)
conf_int <- confint(model_mortality_men)
round(exp(conf_int), 2)

##women
model_mortality_women <- coxph(Surv(survival_time, survival_status) ~ 
                               digital_usage  + age + age_sq + Race + 
                               residence_rural + H_education + marital_status + 
                               equivalized_wealth, 
                             data = df_mortality[df_mortality$gender == "Women",], weights = iptw_weight)
summary(model_mortality_women)
conf_int <- confint(model_mortality_women)
round(exp(conf_int), 2)
tab_model(model_mortality_men,model_mortality_women)

#race
##white
model_mortality_white <- coxph(Surv(survival_time, survival_status) ~ 
                               digital_usage + age + age_sq  + 
                               residence_rural + H_education + marital_status + 
                               equivalized_wealth, 
                             data = df_mortality[df_mortality$Race == "Non-Hispanic White",], weights = iptw_weight)
summary(model_mortality_white)
conf_int <- confint(model_mortality_white)
round(exp(conf_int), 2)


##other
model_mortality_other <- coxph(Surv(survival_time, survival_status) ~ 
                                 digital_usage  + age + age_sq + 
                                 residence_rural + H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_mortality[df_mortality$Race == "Other",], weights = iptw_weight)
summary(model_mortality_other)
conf_int <- confint(model_mortality_other)
round(exp(conf_int), 2)
tab_model(model_mortality_white,model_mortality_other)

#rural
##rural
model_mortality_rural <- coxph(Surv(survival_time, survival_status) ~ 
                                 digital_usage + age + age_sq  + 
                                  H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_mortality[df_mortality$residence_rural == "rural",], weights = iptw_weight)
summary(model_mortality_rural)
confint(model_mortality_rural)
conf_int <- confint(model_mortality_rural)
round(exp(conf_int), 2)

##other
model_mortality_urban <- coxph(Surv(survival_time, survival_status) ~ 
                                 digital_usage  + age + age_sq + 
                                  H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_mortality[df_mortality$residence_rural == "urban",], weights = iptw_weight)
summary(model_mortality_urban)
conf_int <- confint(model_mortality_urban)
round(exp(conf_int), 2)
tab_model(model_mortality_rural,model_mortality_urban)

#wealth
##lower
model_mortality_low <- coxph(Surv(survival_time, survival_status) ~ 
                                 digital_usage + age + age_sq  + 
                                 H_education + marital_status + residence_rural, 
                               data = df_mortality[df_mortality$equivalized_wealth == "Lower wealth",], weights = iptw_weight)
summary(model_mortality_low)
conf_int <- confint(model_mortality_low)
round(exp(conf_int), 2)

##upper
model_mortality_high <- coxph(Surv(survival_time, survival_status) ~ 
                                 digital_usage  + age + age_sq + 
                                 H_education + marital_status + 
                                residence_rural, 
                               data = df_mortality[df_mortality$equivalized_wealth == "Upper wealth",], weights = iptw_weight)
summary(model_mortality_high)
conf_int <- confint(model_mortality_high)
round(exp(conf_int), 2)
tab_model(model_mortality_low,model_mortality_high)

####sensitivity analysis####
#lagged analysis
df_mortality_lag <- df_mortality[df_mortality$inw11 == 1,]
df_mortality_lag <- df_mortality[df_mortality_lag$death_year > 2012 | is.na(df_mortality_lag$death_year), ]

model_mortality_lag <- coxph(Surv(survival_time, survival_status) ~ 
                           digital_usage + age + age_sq + gender + Race + 
                           residence_rural + H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_mortality_lag, weights = iptw_weight)
tab_model(model_mortality_lag)
conf_int <- confint(model_mortality_lag)
round(exp(conf_int), 2)

logLik_model_mortality <- as.numeric(logLik(model_mortality))
logLik_model_mortality_lag <- as.numeric(logLik(model_mortality_lag))

lr_stat <- 2 * (logLik_model_mortality_lag - logLik_model_mortality)
df_diff <- df.residual(model_mortality_lag) - df.residual(model_mortality)
p_value <- 1 - pchisq(lr_stat, df_diff)
lr_stat
p_value

#complete information n = 9891
df_complete <- df_mortality[rowSums(is.na(df_mortality[, c("c_w6","c_w7","c_w8","c_w9","c_w10")])) < 1, ]
model_mortality_complete <- coxph(Surv(survival_time, survival_status) ~ 
                               digital_usage + age + age_sq + gender + Race + 
                               residence_rural + H_education + marital_status + 
                               equivalized_wealth, 
                             data = df_complete, weights = iptw_weight)
tab_model(model_mortality_complete)
conf_int <- confint(model_mortality_complete)
round(exp(conf_int), 2)

logLik_model_mortality <- as.numeric(logLik(model_mortality))
logLik_model_mortality_complete <- as.numeric(logLik(model_mortality_complete))

lr_stat <- 2 * (logLik_model_mortality_complete - logLik_model_mortality)
df_diff <- df.residual(model_mortality_complete) - df.residual(model_mortality)
p_value <- 1 - pchisq(lr_stat, df_diff)
lr_stat
p_value

#sampling weight
df_mortality$WEIGHT <- as.numeric(df_mortality$r6weight)
df_mortality$iptw_weight[df_mortality$iptw_weight <= 0] <- 1e-6
df_mortality$WEIGHT[df_mortality$WEIGHT <= 0] <- 1e-6
model_mortality_sample <- coxph(Surv(survival_time, survival_status) ~ 
                                    digital_usage + age + age_sq + gender + Race + 
                                    residence_rural + H_education + marital_status + 
                                    equivalized_wealth, 
                                  data = df_mortality, weights = iptw_weight * WEIGHT)
tab_model(model_mortality_sample)
conf_int <- confint(model_mortality_sample)
round(exp(conf_int), 2)
round(exp(confint(model_mortality_sample)),2)

logLik_model_mortality <- as.numeric(logLik(model_mortality))
logLik_model_mortality_complete <- as.numeric(logLik(model_mortality_sample))

lr_stat <- 2 * (logLik_model_mortality_complete - logLik_model_mortality)
df_diff <- df.residual(model_mortality_complete) - df.residual(model_mortality)
p_value <- 1 - pchisq(lr_stat, df_diff)
lr_stat
p_value

#iptw < 10
df_mortality_iptw5 <- filter(df_mortality, df_mortality$iptw_weight < 10)
model_mortality_iptw5 <- coxph(Surv(survival_time, survival_status) ~ 
                                  digital_usage + age + age_sq + gender + Race + 
                                  residence_rural + H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_mortality_iptw5, weights = iptw_weight * WEIGHT)
tab_model(model_mortality_iptw5)
conf_int <- confint(model_mortality_iptw5)
round(exp(conf_int), 2)

logLik_model_mortality <- as.numeric(logLik(model_mortality))
logLik_model_mortality_iptw5 <- as.numeric(logLik(model_mortality_iptw5))

lr_stat <- 2 * (logLik_model_mortality_iptw5 - logLik_model_mortality)
df_diff <- df.residual(model_mortality_iptw5) - df.residual(model_mortality)
p_value <- 1 - pchisq(lr_stat, df_diff)
lr_stat
p_value

#ipcw
df_mortality$ipcw_cum_inw14[df_mortality$ipcw_cum_inw14 <= 0] <- 1e-6
df_mortality$iptw_weight[df_mortality$iptw_weight <= 0] <- 1e-6
model_mortality_sample <- coxph(Surv(survival_time, survival_status) ~ 
                                  digital_usage + age + age_sq + gender + Race + 
                                  residence_rural + H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_mortality, weights = iptw_weight * ipcw_cum_inw14)
tab_model(model_mortality_sample)
conf_int <- confint(model_mortality_sample)
round(exp(conf_int), 2)

logLik_model_mortality <- as.numeric(logLik(model_mortality))
logLik_model_mortality_sample <- as.numeric(logLik(model_mortality_sample))

lr_stat <- 2 * (logLik_model_mortality_sample - logLik_model_mortality)
df_diff <- df.residual(model_mortality_sample) - df.residual(model_mortality)
p_value <- 1 - pchisq(lr_stat, df_diff)
lr_stat
p_value

#Pairwise Comparisons
emmeans_results <- emmeans(model_mortality, ~ digital_usage)
comparisons <- contrast(emmeans_results, method = "revpairwise")
summary_result <- summary(comparisons, 
                          type = "response", 
                          infer = c(TRUE, TRUE))

summary_result$ratio <- as.numeric(summary_result$ratio)
summary_result$p.value <- as.numeric(summary_result$p.value)

summary_result$lower.CL <- exp(log(summary_result$ratio) - 1.96 * summary_result$SE)
summary_result$upper.CL <- exp(log(summary_result$ratio) + 1.96 * summary_result$SE)

summary_result$ratio <- sprintf("%.2f", round(summary_result$ratio, 2))
summary_result$lower.CL <- sprintf("%.2f", round(summary_result$lower.CL, 2))
summary_result$upper.CL <- sprintf("%.2f", round(summary_result$upper.CL, 2))
summary_result$p.value <- sprintf("%.2f", round(summary_result$p.value, 2))

final_output <- summary_result[, c("contrast", "ratio", "lower.CL", "upper.CL", "p.value")]
print(final_output)

#E-value
#extract rr
##define function
calc_e_value <- function(rr) {
  if (rr < 1) rr <- 1 / rr  # if rr < 1, take the reciprocal
  return(rr + rr * (rr - 1))
}

rr <- exp(coef(model_mortality))
variable_names <- names(rr)

e_values <- sapply(rr, calc_e_value)
results <- data.frame(Variable = variable_names, RiskRatio = rr, EValue = e_values)
print(results)

####table####
library(tableone)
library(kableExtra)
result <- df_mortality %>%
  filter(death_year %in% c(2012, 2014, 2016, 2018), !is.na(death_year)) %>%
  group_by(digital_usage, death_year) %>%
  summarise(death_count = n(), .groups = "drop") %>%
  arrange(digital_usage, death_year)

cumulative_result <- result %>%
  group_by(digital_usage) %>%
  mutate(cumulative_death_count = cumsum(death_count)) %>%
  ungroup()

print(cumulative_result)

table(df_mortality$digital_usage)
