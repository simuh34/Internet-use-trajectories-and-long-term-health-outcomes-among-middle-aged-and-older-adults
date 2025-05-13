df <- read.csv("D:\\R project\\Digital\\HRS\\hrs_imputation.csv")

df$age_sq <- df$age^2

####cardiovascular model####
#delete participants had functional problem in 2010 
df <- subset(df, r10walkra != 1)
df <- subset(df, r10dressa != 1)
df <- subset(df, r10eata != 1)
df <- subset(df, r10batha != 1)
df <- subset(df, r10toilta != 1)
df_adl <- subset(df, r10beda != 1)

#construct outcome cvd event
for (i in 11:14) {
  df_adl[[paste0("adl_", i)]] <- ifelse(df_adl[[paste0("r", i, "walkra")]] + df_adl[[paste0("r", i, "dressa")]]+ df_adl[[paste0("r", i, "eata")]]+ df_adl[[paste0("r", i, "batha")]]+ df_adl[[paste0("r", i, "toilta")]]+ df_adl[[paste0("r", i, "beda")]] > 0, 1, 0)
}

#events_status
df_adl <- df_adl %>%
  mutate(
    event_status = case_when(
      (adl_11 == 1 | adl_12 == 1 | adl_13 == 1 | adl_14 == 1) ~ 1,
      is.na(adl_11) & is.na(adl_12) & is.na(adl_13) & is.na(adl_14) ~ NA_real_,
      TRUE ~ 0  
    )
  )
#drop rows do not have any chronic disease records
df_adl <- df_adl[!is.na(df_adl$event_status),]


#NOTE:The logic behind calculating event_date is as follows: For event_status == 0 (no event), if inv_date14 is not missing, the event_date is set to inv_date14. If inv_date14 is missing but death_date exists and is earlier than the minimum of inv_date14, the event_date is set to death_date. If inv_date14 is missing and the death_date condition does not apply, the event_date is imputed using the median of inv_date14 from available non-missing values. If neither condition applies, the event_date is set to NA. For event_status == 1 (event occurred), if adl_11 == 1, the event_date is set to inv_date11; if adl_12 == 1, the event_date is set to inv_date12; if adl_13 == 1, the event_date is set to inv_date13; and if adl_14 == 1, the event_date is set to inv_date14. If none of these conditions apply, the event_date is set to NA. This approach ensures the event_date is logically determined based on available data and handles missing values appropriately. Temporary columns used for the calculation of the minimum and median of inv_date14 are removed at the end.
df_adl <- df_adl %>%
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
          ifelse(adl_11 == 1, inv_date11, NA), 
          ifelse(adl_12 == 1, inv_date12, NA), 
          ifelse(adl_13 == 1, inv_date13, NA), 
          ifelse(adl_14 == 1, inv_date14, NA)
        ), origin = "1960-01-01"
      ),
      TRUE ~ as.Date(NA)
    )
  ) %>%
  select(-min_inv_date14, -median_inv_date14)

df_adl <- df_adl %>%
  mutate(
    event_time = as.numeric(difftime(event_date, as.Date(inv_date10, origin = "1960-01-01"), units = "days")) / 30.44
  )


#reference group
df_adl$digital_usage <- factor(df_adl$digital_usage, 
                               levels = c("Never users", "Dropouts", "Intermittent Users", "Adopters", "Consistent users"))
df_adl$digital_usage <- relevel(df_adl$digital_usage, ref = "Never users")
df_adl$Race <- factor(df_adl$Race,levels = c("Non-Hispanic White" ,"Other"))
df_adl$Race <- relevel(df_adl$Race,ref = "Non-Hispanic White" )
df_adl$H_education <- factor(df_adl$H_education)
df_adl$H_education <- relevel(df_adl$H_education, ref = "Less than upper secondary")
#df_adl$health_condition <- as.numeric(df_adl$health_condition)
df_adl$residence_rural <- factor(df_adl$residence_rural)
df_adl$residence_rural <- relevel(df_adl$residence_rural, ref = "rural")
df_adl$gender <- relevel(factor(df_adl$gender), ref = "Men")
df_adl$equivalized_wealth <- relevel(factor(df_adl$equivalized_wealth), ref = "Lower wealth")

library(survival)
surv_obj <- Surv(time = df_adl$event_time, event = df_adl$event_status)
model_adl <- coxph(surv_obj ~ digital_usage + age + age_sq + gender + Race + 
                     residence_rural + H_education + marital_status + 
                     equivalized_wealth, data = df_adl, weights = iptw_weight)
summary(model_adl)
exp(confint(model_adl))

model_adl_unadjusted <- coxph(surv_obj ~ digital_usage, data = df_adl, weights = iptw_weight)
summary(model_adl_unadjusted)

model_adl_nw <- coxph(surv_obj ~ digital_usage + age + age_sq + gender + Race + 
                     residence_rural + H_education + marital_status + 
                     equivalized_wealth, data = df_adl, weights = iptw_weight)
summary(model_adl_nw)

####cip####
mean_age <- mean(df_adl$age, na.rm = TRUE)
mean_age_sq <- mean(df_adl$age_sq, na.rm = TRUE)
#mean_health_condition <- mean(df_adl$health_condition, na.rm = TRUE)
mode_gender <- factor('Men')
mode_race <- factor("Non-Hispanic White")
mode_rural <- factor("rural")
mode_education <- factor("Less than upper secondary")
mode_marital <- factor("Married/Parterned")
mode_wealth <- factor("Lower wealth")

newdata <- data.frame(
  digital_usage = factor(levels(df_adl$digital_usage), levels = levels(df_adl$digital_usage)),
  age = rep(mean_age, length(levels(df_adl$digital_usage))),  
  age_sq = rep(mean_age_sq, length(levels(df_adl$digital_usage))),  
  gender = rep(mode_gender, length(levels(df_adl$digital_usage))),
  Race = rep(mode_race, length(levels(df_adl$digital_usage))),
  residence_rural = rep(mode_rural, length(levels(df_adl$digital_usage))),
  H_education = rep(mode_education, length(levels(df_adl$digital_usage))),
  marital_status = rep(mode_marital, length(levels(df_adl$digital_usage))),
  equivalized_wealth = rep(mode_wealth, length(levels(df_adl$digital_usage)))
)


fit <- survfit(model_adl_unadjusted, newdata = newdata)
ggsurv_obj <- ggsurvplot(fit,
                         data = df_adl,
                         fun = "event",
                         conf.int = FALSE,
                         risk.table = FALSE,
                         palette = "Set1",
                         xlab = "Time (Years)",
                         ylab = "Cumulative Incidence (%)",
                         legend.title = "Digital Usage",
                         legend.labs = c("Never users", "Dropouts", "Intermittent Users", "Adopters", "Consistent users"),
                         risk.table.height = 0.25,
                         title = "C.Functional disability",
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


png(file = 'D:\\R project\\Digital\\HRS\\plot\\functional_disability.png',
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


png(file = 'D:\\R project\\Digital\\HRS\\plot\\functional_disability_b.png',
    width = 1800,
    height = 1200,
    units = "px",
    res = 200)
print(p_final_b)
dev.off()

####interaction####
#base model
model_adl_inter <- coxph(Surv(event_time, event_status) ~ 
                                digital_usage + gender  + age + age_sq + Race + 
                                residence_rural + H_education + marital_status + 
                                equivalized_wealth, 
                              data = df_adl, weights = iptw_weight)
tab_model(model_adl_inter)
conf_int <- confint(model_adl_inter)
round(exp(conf_int), 2)

#gender interaction
model_adl_gender_inter <- coxph(Surv(event_time, event_status) ~ 
                                       digital_usage*gender  + age + age_sq + Race + 
                                       residence_rural + H_education + marital_status + 
                                       equivalized_wealth, 
                                     data = df_adl, weights = iptw_weight)
summary(model_adl_gender_inter)
conf_int <- confint(model_adl_gender_inter)
round(exp(conf_int), 2)

#race interaction
model_adl_race_inter <- coxph(Surv(event_time, event_status) ~ 
                                     digital_usage * Race + gender  + age + age_sq + 
                                     residence_rural + H_education + marital_status + 
                                     equivalized_wealth, 
                                   data = df_adl, weights = iptw_weight)
summary(model_adl_race_inter)
conf_int <- confint(model_adl_race_inter)
round(exp(conf_int), 2)

#urbanicity interaction
model_adl_rural_inter <- coxph(Surv(event_time, event_status) ~ 
                                      digital_usage * residence_rural + Race + gender  + age + age_sq + 
                                      residence_rural + H_education + marital_status + 
                                      equivalized_wealth, 
                                    data = df_adl, weights = iptw_weight)
tab_model(model_adl_rural_inter)
conf_int <- confint(model_adl_rural_inter)
round(exp(conf_int), 2)

#wealth interaction
model_adl_wealth_inter <- coxph(Surv(event_time, event_status) ~ 
                                       digital_usage * equivalized_wealth + residence_rural + Race + gender  + age + age_sq + 
                                       residence_rural + H_education + marital_status, 
                                     data = df_adl, weights = iptw_weight)
summary(model_adl_wealth_inter)
conf_int <- confint(model_adl_wealth_inter)
round(exp(conf_int), 2)

#likelihood ratio test
library(lmtest)
lrt_1 <- lrtest(model_adl_inter, model_adl_gender_inter)
lrt_2 <- lrtest(model_adl_inter, model_adl_race_inter)
lrt_3 <- lrtest(model_adl_inter, model_adl_rural_inter)
lrt_4 <- lrtest(model_adl_inter, model_adl_wealth_inter)

lrt_1
lrt_2
lrt_3
lrt_4

#stratified analysis
#gender
##men
model_adl_men <- coxph(Surv(event_time, event_status) ~ 
                                digital_usage + age + age_sq + Race + 
                                residence_rural + H_education + marital_status + 
                                equivalized_wealth, 
                              data = df_adl[df_adl$gender == "Men",], weights = iptw_weight)
summary(model_adl_men)
conf_int <- confint(model_adl_men)
round(exp(conf_int), 2)
confint(model_adl_men)

##women
model_adl_women <- coxph(Surv(event_time, event_status) ~ 
                                  digital_usage  + age + age_sq + Race + 
                                  residence_rural + H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_adl[df_adl$gender == "Women",], weights = iptw_weight)
summary(model_adl_women)
conf_int <- confint(model_adl_women)
round(exp(conf_int), 2)
tab_model(model_adl_men,model_adl_women)

#race
##white
model_adl_white <- coxph(Surv(event_time, event_status) ~ 
                                  digital_usage + age + age_sq  + 
                                  residence_rural + H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_adl[df_adl$Race == "Non-Hispanic White",], weights = iptw_weight)
summary(model_adl_white)
conf_int <- confint(model_adl_white)
round(exp(conf_int), 2)

##other
model_adl_other <- coxph(Surv(event_time, event_status) ~ 
                                  digital_usage  + age + age_sq + 
                                  residence_rural + H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_adl[df_adl$Race == "Other",], weights = iptw_weight)
summary(model_adl_other)
conf_int <- confint(model_adl_other)
round(exp(conf_int), 2)
tab_model(model_adl_white,model_adl_other)

#rural
##rural
model_adl_rural <- coxph(Surv(event_time, event_status) ~ 
                                 digital_usage + age + age_sq  + 
                                 H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_adl[df_adl$residence_rural == "rural",], weights = iptw_weight)
summary(model_adl_rural)
conf_int <- confint(model_adl_rural)
round(exp(conf_int), 2)
confint(model_adl_rural)

##other
model_adl_urban <- coxph(Surv(event_time, event_status) ~ 
                                 digital_usage  + age + age_sq + 
                                 H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_adl[df_adl$residence_rural == "urban",], weights = iptw_weight)
summary(model_adl_urban)
conf_int <- confint(model_adl_urban)
round(exp(conf_int), 2)
tab_model(model_adl_rural,model_adl_urban)

#wealth
##lower
model_adl_low <- coxph(Surv(event_time, event_status) ~ 
                               digital_usage + age + age_sq  + 
                               H_education + marital_status + residence_rural, 
                             data = df_adl[df_adl$equivalized_wealth == "Lower wealth",], weights = iptw_weight)
summary(model_adl_low)
conf_int <- confint(model_adl_low)
round(exp(conf_int), 2)
confint(model_adl_low)

##upper
model_adl_high <- coxph(Surv(event_time, event_status) ~ 
                                digital_usage  + age + age_sq + 
                                H_education + marital_status + 
                                residence_rural, 
                              data = df_adl[df_adl$equivalized_wealth == "Upper wealth",], weights = iptw_weight)
summary(model_adl_high)
conf_int <- confint(model_adl_high)
round(exp(conf_int), 2)
tab_model(model_adl_low,model_adl_high)


#kp
#kaplan-meier plots
library(survival)
library(survminer)
base_surv <- survfit(Surv(event_time, event_status) ~ digital_usage,data = df_adl)
ggsurvplot(base_surv, data = df_adl,legend.title = "Digital Usage",
           legend.labs = levels(as.factor(df_adl$digital_usage)),
           xlab = "Survival Months",  
           ylab = "Suevival Probability",
           pval = TRUE)


####table####
library(tableone)
library(kableExtra)
result <- df_adl %>%
  filter(death_year %in% c(2012,2014,2016,2018), !is.na(death_year)) %>%
  group_by(digital_usage, death_year) %>%
  summarise(death_count = n(), .groups = "drop") %>%
  arrange(digital_usage, death_year)

cumulative_result <- result %>%
  group_by(digital_usage) %>%
  mutate(cumulative_death_count = cumsum(death_count)) %>%
  ungroup()

print(cumulative_result)

table(df_adl$digital_usage)

#Pairwise Comparisons
emmeans_results <- emmeans(model_adl, ~ digital_usage)
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
