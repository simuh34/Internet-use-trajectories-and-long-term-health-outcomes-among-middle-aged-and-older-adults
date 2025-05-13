df <- read.csv("D:\\R project\\Digital\\HRS\\hrs_imputation.csv")

df$age_sq <- df$age^2
####cardiovascular model####
df %>%
  select(r10hearte, r11hearte, r12hearte, r13hearte, r14hearte) %>%
  summary()
df %>%
  select(r10stroke, r11stroke, r12stroke, r13stroke, r14stroke) %>%
  summary()
#delete participants had heart problem in 2010 
df_heart <- subset(df, r10hearte != 1)
df_cvd <- subset(df_heart, r10stroke != 1)
#construct outcome cvd event
for (i in 11:14) {
  df_cvd[[paste0("cvd_", i)]] <- ifelse(df_cvd[[paste0("r", i, "hearte")]] + df_cvd[[paste0("r", i, "stroke")]] > 0, 1, 0)
}
#events_status
df_cvd <- df_cvd %>%
  mutate(
    event_status = case_when(
      (cvd_11 == 1 | cvd_12 == 1 | cvd_13 == 1 | cvd_14 == 1) ~ 1,
      is.na(cvd_11) & is.na(cvd_12) & is.na(cvd_13) & is.na(cvd_14) ~ NA_real_,
      TRUE ~ 0  
    )
  )
#drop rows do not have any chronic disease records
df_cvd <- df_cvd[!is.na(df_cvd$event_status),]


#NOTE:The logic behind calculating event_date is as follows: For event_status == 0 (no event), if inv_date14 is not missing, the event_date is set to inv_date14. If inv_date14 is missing but death_date exists and is earlier than the minimum of inv_date14, the event_date is set to death_date. If inv_date14 is missing and the death_date condition does not apply, the event_date is imputed using the median of inv_date14 from available non-missing values. If neither condition applies, the event_date is set to NA. For event_status == 1 (event occurred), if cvd_11 == 1, the event_date is set to inv_date11; if cvd_12 == 1, the event_date is set to inv_date12; if cvd_13 == 1, the event_date is set to inv_date13; and if cvd_14 == 1, the event_date is set to inv_date14. If none of these conditions apply, the event_date is set to NA. This approach ensures the event_date is logically determined based on available data and handles missing values appropriately. Temporary columns used for the calculation of the minimum and median of inv_date14 are removed at the end.
df_cvd <- df_cvd %>%
  mutate(
    min_inv_date14 = min(inv_date14, na.rm = TRUE),
    median_inv_date14 = median(inv_date14, na.rm = TRUE),
    event_date = case_when(
      event_status == 0 & !is.na(inv_date14) ~ as.Date(inv_date14, origin = "1960-01-01"),  
      event_status == 0 & !is.na(death_date) & death_date < min_inv_date14 ~ as.Date(death_date, origin = "1960-01-01"),  
      event_status == 0 & is.na(inv_date14) ~ as.Date(median_inv_date14, origin = "1960-01-01"),  
      event_status == 0 ~ as.Date(NA),  
      event_status == 1 ~ as.Date(
        coalesce(
          ifelse(cvd_11 == 1, inv_date11, NA), 
          ifelse(cvd_12 == 1, inv_date12, NA), 
          ifelse(cvd_13 == 1, inv_date13, NA), 
          ifelse(cvd_14 == 1, inv_date14, NA)
        ), origin = "1960-01-01"
      ),
      TRUE ~ as.Date(NA)
    )
  ) %>%
  select(-min_inv_date14, -median_inv_date14)

df_cvd <- df_cvd %>%
  mutate(
    event_time = as.numeric(difftime(event_date, as.Date(inv_date10, origin = "1960-01-01"), units = "days")) / 30.44
  )


#reference group
df_cvd$digital_usage <- factor(df_cvd$digital_usage, 
                                     levels = c("Never users", "Dropouts", "Intermittent Users", "Adopters", "Consistent users"))
df_cvd$digital_usage <- relevel(df_cvd$digital_usage, ref = "Never users")
df_cvd$Race <- factor(df_cvd$Race,levels = c("Non-Hispanic White" ,"Other"))
df_cvd$Race <- relevel(df_cvd$Race,ref = "Non-Hispanic White" )
df_cvd$H_education <- factor(df_cvd$H_education)
df_cvd$H_education <- relevel(df_cvd$H_education, ref = "Less than upper secondary")
#df_cvd$health_condition <- as.numeric(df_cvd$health_condition)
df_cvd$residence_rural <- factor(df_cvd$residence_rural)
df_cvd$residence_rural <- relevel(df_cvd$residence_rural, ref = "rural")
df_cvd$gender <- relevel(factor(df_cvd$gender), ref = "Men")
df_cvd$equivalized_wealth <- relevel(factor(df_cvd$equivalized_wealth), ref = "Lower wealth")

library(survival)
surv_obj <- Surv(time = df_cvd$event_time, event = df_cvd$event_status)
model_cvd <- coxph(surv_obj ~ digital_usage + age + age_sq + gender + Race + 
                     residence_rural + H_education + marital_status + 
                     equivalized_wealth, data = df_cvd, weights = iptw_weight)
summary(model_cvd)
exp(confint(model_mortality))

model_cvd_unadjusted <- coxph(surv_obj ~ digital_usage, data = df_cvd, weights = iptw_weight)
summary(model_cvd_unadjusted)


model_cvd_nw <- coxph(surv_obj ~ digital_usage + age + age_sq + gender + Race + 
                     residence_rural + H_education + marital_status + 
                     equivalized_wealth, data = df_cvd, weights = iptw_weight)
summary(model_cvd_nw)

####cip####
mean_age <- mean(df_cvd$age, na.rm = TRUE)
mean_age_sq <- mean(df_cvd$age_sq, na.rm = TRUE)
#mean_health_condition <- mean(df_cvd$health_condition, na.rm = TRUE)
mode_gender <- factor('Men')
mode_race <- factor("Non-Hispanic White")
mode_rural <- factor("rural")
mode_education <- factor("Less than upper secondary")
mode_marital <- factor("Married/Parterned")
mode_wealth <- factor("Lower wealth")

newdata <- data.frame(
  digital_usage = factor(levels(df_cvd$digital_usage), levels = levels(df_cvd$digital_usage)),
  age = rep(mean_age, length(levels(df_cvd$digital_usage))),  
  age_sq = rep(mean_age_sq, length(levels(df_cvd$digital_usage))),  
  gender = rep(mode_gender, length(levels(df_cvd$digital_usage))),
  Race = rep(mode_race, length(levels(df_cvd$digital_usage))),
  residence_rural = rep(mode_rural, length(levels(df_cvd$digital_usage))),
  H_education = rep(mode_education, length(levels(df_cvd$digital_usage))),
  marital_status = rep(mode_marital, length(levels(df_cvd$digital_usage))),
  equivalized_wealth = rep(mode_wealth, length(levels(df_cvd$digital_usage)))
)


fit <- survfit(model_cvd_unadjusted, newdata = newdata)
ggsurv_obj <- ggsurvplot(fit,
                         data = df_cvd,
                         fun = "event",
                         conf.int = FALSE,
                         risk.table = FALSE,
                         palette = "Set1",
                         xlab = "Time (Years)",
                         ylab = "Cumulative Incidence (%)",
                         legend.title = "Digital Usage",
                         legend.labs = c("Never users", "Dropouts", "Intermittent Users", "Adopters", "Consistent users"),
                         risk.table.height = 0.25,
                         title = "B.Cardiovascular disease",
                         break.time.by = 12,
                         xscale = "d_y",
                         censor = FALSE,    
                         size = 0.7)         

p_final <- ggsurv_obj$plot + 
  theme_minimal() +  
  scale_x_continuous(
    breaks = seq(0, 106, by = 12),  # 使用129作为x轴的最大值
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

png(file = 'D:\\R project\\Digital\\HRS\\plot\\cvd.png',
    width = 1800,
    height = 1200,
    units = "px",
    res = 200)
print(p_final)
dev.off()

p_final_b <- ggsurv_obj$plot + 
  theme_minimal() +  
  scale_x_continuous(
    breaks = seq(0, 106, by = 12),  # 使用129作为x轴的最大值
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

png(file = 'D:\\R project\\Digital\\HRS\\plot\\cvd_b.png',
    width = 1800,
    height = 1200,
    units = "px",
    res = 200)
print(p_final_b)
dev.off()

####interaction####
#base model
model_mortality_inter <- coxph(Surv(event_time, event_status) ~ 
                                 digital_usage + gender  + age + age_sq + Race + 
                                 residence_rural + H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_cvd, weights = iptw_weight)
tab_model(model_mortality_inter)
conf_int <- confint(model_mortality_inter)
round(exp(conf_int), 2)

#gender interaction
model_mortality_gender_inter <- coxph(Surv(event_time, event_status) ~ 
                                        digital_usage*gender  + age + age_sq + Race + 
                                        residence_rural + H_education + marital_status + 
                                        equivalized_wealth, 
                                      data = df_cvd, weights = iptw_weight)
summary(model_mortality_gender_inter)
conf_int <- confint(model_mortality_gender_inter)
round(exp(conf_int), 2)

#race interaction
model_mortality_race_inter <- coxph(Surv(event_time, event_status) ~ 
                                      digital_usage * Race + gender  + age + age_sq + 
                                      residence_rural + H_education + marital_status + 
                                      equivalized_wealth, 
                                    data = df_cvd, weights = iptw_weight)
summary(model_mortality_race_inter)
conf_int <- confint(model_mortality_race_inter)
round(exp(conf_int), 2)

#urbanicity interaction
model_mortality_rural_inter <- coxph(Surv(event_time, event_status) ~ 
                                       digital_usage * residence_rural + Race + gender  + age + age_sq + 
                                       residence_rural + H_education + marital_status + 
                                       equivalized_wealth, 
                                     data = df_cvd, weights = iptw_weight)
tab_model(model_mortality_rural_inter)
conf_int <- confint(model_mortality_rural_inter)
round(exp(conf_int), 2)

#wealth interaction
model_mortality_wealth_inter <- coxph(Surv(event_time, event_status) ~ 
                                        digital_usage * equivalized_wealth + residence_rural + Race + gender  + age + age_sq + 
                                        residence_rural + H_education + marital_status, 
                                      data = df_cvd, weights = iptw_weight)
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
model_cvd_men <- coxph(Surv(event_time, event_status) ~ 
                               digital_usage + age + age_sq + Race + 
                               residence_rural + H_education + marital_status + 
                               equivalized_wealth, 
                             data = df_cvd[df_cvd$gender == "Men",], weights = iptw_weight)
summary(model_cvd_men)
conf_int <- confint(model_cvd_men)
round(exp(conf_int), 2)
confint(model_cvd_men)

##women
model_cvd_women <- coxph(Surv(event_time, event_status) ~ 
                                 digital_usage  + age + age_sq + Race + 
                                 residence_rural + H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_cvd[df_cvd$gender == "Women",], weights = iptw_weight)
summary(model_cvd_women)
conf_int <- confint(model_cvd_women)
round(exp(conf_int), 2)
tab_model(model_cvd_men,model_cvd_women)

#race
##white
model_cvd_white <- coxph(Surv(event_time, event_status) ~ 
                                 digital_usage + age + age_sq  + 
                                 residence_rural + H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_cvd[df_cvd$Race == "Non-Hispanic White",], weights = iptw_weight)
summary(model_cvd_white)
conf_int <- confint(model_cvd_white)
round(exp(conf_int), 2)
confint(model_cvd_white)

##other
model_cvd_other <- coxph(Surv(event_time, event_status) ~ 
                                 digital_usage  + age + age_sq + 
                                 residence_rural + H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_cvd[df_cvd$Race == "Other",], weights = iptw_weight)
summary(model_cvd_other)
conf_int <- confint(model_cvd_other)
round(exp(conf_int), 2)
tab_model(model_cvd_white,model_cvd_other)

#rural
##rural
model_cvd_rural <- coxph(Surv(event_time, event_status) ~ 
                           digital_usage + age + age_sq  + 
                           H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_cvd[df_cvd$residence_rural == "rural",], weights = iptw_weight)
summary(model_cvd_rural)
confint(model_cvd_rural)
conf_int <- confint(model_cvd_rural)
round(exp(conf_int), 2)

##other
model_cvd_urban <- coxph(Surv(event_time, event_status) ~ 
                           digital_usage  + age + age_sq + 
                           H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_cvd[df_cvd$residence_rural == "urban",], weights = iptw_weight)
summary(model_cvd_urban)
conf_int <- confint(model_cvd_urban)
round(exp(conf_int), 2)
tab_model(model_cvd_rural,model_cvd_urban)

#wealth
##lower
model_adl_low <- coxph(Surv(event_time, event_status) ~ 
                         digital_usage + age + age_sq  + 
                         H_education + marital_status + residence_rural, 
                       data = df_cvd[df_cvd$equivalized_wealth == "Lower wealth",], weights = iptw_weight)
conf_int <- confint(model_adl_low)
round(exp(conf_int), 2)
summary(model_adl_low)
confint(model_adl_low)

##upper
model_adl_high <- coxph(Surv(event_time, event_status) ~ 
                          digital_usage  + age + age_sq + 
                          H_education + marital_status + 
                          residence_rural, 
                        data = df_cvd[df_cvd$equivalized_wealth == "Upper wealth",], weights = iptw_weight)
conf_int <- confint(model_adl_high)
round(exp(conf_int), 2)
summary(model_adl_high)
tab_model(model_adl_low,model_adl_high)

#kp
#kaplan-meier plots
library(survival)
library(survminer)
base_surv <- survfit(Surv(event_time, event_status) ~ digital_usage,data = df_cvd)
ggsurvplot(base_surv, data = df_cvd,legend.title = "Digital Usage",
           legend.labs = levels(as.factor(df_cvd$digital_usage)),
           xlab = "Survival Months",  
           ylab = "Suevival Probability",
           pval = TRUE)


####table####
library(tableone)
library(kableExtra)
result <- df_cvd %>%
  filter(death_year %in% c(2012, 2014, 2016, 2018), !is.na(death_year)) %>%
  group_by(digital_usage, death_year) %>%
  summarise(death_count = n(), .groups = "drop") %>%
  arrange(digital_usage, death_year)

cumulative_result <- result %>%
  group_by(digital_usage) %>%
  mutate(cumulative_death_count = cumsum(death_count)) %>%
  ungroup()

print(cumulative_result)

table(df_cvd$digital_usage)

#Pairwise Comparisons
emmeans_results <- emmeans(model_cvd, ~ digital_usage)
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
