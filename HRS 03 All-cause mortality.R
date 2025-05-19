####mortality model####
model_mortality <- coxph(Surv(survival_time, survival_status) ~ 
                           digital_usage + age + age_sq + gender + Race + 
                           residence_rural + H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_mortality, weights = iptw_weight)

####interaction####
model_mortality <- coxph(Surv(survival_time, survival_status) ~ 
                                        digital_usage + gender  + age + age_sq + Race + 
                                        residence_rural + H_education + marital_status + 
                                        equivalized_wealth, 
                                      data = df_mortality, weights = iptw_weight)

model_mortality_gender_inter <- coxph(Surv(survival_time, survival_status) ~ 
                                        digital_usage*gender  + age + age_sq + Race + 
                                        residence_rural + H_education + marital_status + 
                                        equivalized_wealth, 
                                      data = df_mortality, weights = iptw_weight)

model_mortality_race_inter <- coxph(Surv(survival_time, survival_status) ~ 
                                        digital_usage * Race + gender  + age + age_sq + 
                                        residence_rural + H_education + marital_status + 
                                        equivalized_wealth, 
                                      data = df_mortality, weights = iptw_weight)

model_mortality_rural_inter <- coxph(Surv(survival_time, survival_status) ~ 
                                      digital_usage * residence_rural + Race + gender  + age + age_sq + 
                                      residence_rural + H_education + marital_status + 
                                      equivalized_wealth, 
                                    data = df_mortality, weights = iptw_weight)

model_mortality_wealth_inter <- coxph(Surv(survival_time, survival_status) ~ 
                                       digital_usage * equivalized_wealth + residence_rural + Race + gender  + age + age_sq + 
                                       residence_rural + H_education + marital_status, 
                                     data = df_mortality, weights = iptw_weight)

####stratified analysis####
model_mortality_men <- coxph(Surv(survival_time, survival_status) ~ 
                                        digital_usage + age + age_sq + Race + 
                                        residence_rural + H_education + marital_status + 
                                        equivalized_wealth, 
                                      data = df_mortality[df_mortality$gender == "Men",], weights = iptw_weight)

model_mortality_women <- coxph(Surv(survival_time, survival_status) ~ 
                               digital_usage  + age + age_sq + Race + 
                               residence_rural + H_education + marital_status + 
                               equivalized_wealth, 
                             data = df_mortality[df_mortality$gender == "Women",], weights = iptw_weight)

model_mortality_white <- coxph(Surv(survival_time, survival_status) ~ 
                               digital_usage + age + age_sq  + 
                               residence_rural + H_education + marital_status + 
                               equivalized_wealth, 
                             data = df_mortality[df_mortality$Race == "Non-Hispanic White",], weights = iptw_weight)

model_mortality_other <- coxph(Surv(survival_time, survival_status) ~ 
                                 digital_usage  + age + age_sq + 
                                 residence_rural + H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_mortality[df_mortality$Race == "Other",], weights = iptw_weight)

model_mortality_rural <- coxph(Surv(survival_time, survival_status) ~ 
                                 digital_usage + age + age_sq  + 
                                  H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_mortality[df_mortality$residence_rural == "rural",], weights = iptw_weight)

model_mortality_urban <- coxph(Surv(survival_time, survival_status) ~ 
                                 digital_usage  + age + age_sq + 
                                  H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_mortality[df_mortality$residence_rural == "urban",], weights = iptw_weight)

model_mortality_low <- coxph(Surv(survival_time, survival_status) ~ 
                                 digital_usage + age + age_sq  + 
                                 H_education + marital_status + residence_rural, 
                               data = df_mortality[df_mortality$equivalized_wealth == "Lower wealth",], weights = iptw_weight)

model_mortality_high <- coxph(Surv(survival_time, survival_status) ~ 
                                 digital_usage  + age + age_sq + 
                                 H_education + marital_status + 
                                residence_rural, 
                               data = df_mortality[df_mortality$equivalized_wealth == "Upper wealth",], weights = iptw_weight)

####sensitivity analysis####
#lagged analysis
df_mortality_lag <- df_mortality[df_mortality$inw11 == 1,]
df_mortality_lag <- df_mortality[df_mortality_lag$death_year > 2012 | is.na(df_mortality_lag$death_year), ]
model_mortality_lag <- coxph(Surv(survival_time, survival_status) ~ 
                           digital_usage + age + age_sq + gender + Race + 
                           residence_rural + H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_mortality_lag, weights = iptw_weight)

#complete information n = 9891
df_complete <- df_mortality[rowSums(is.na(df_mortality[, c("c_w6","c_w7","c_w8","c_w9","c_w10")])) < 1, ]
model_mortality_complete <- coxph(Surv(survival_time, survival_status) ~ 
                               digital_usage + age + age_sq + gender + Race + 
                               residence_rural + H_education + marital_status + 
                               equivalized_wealth, 
                             data = df_complete, weights = iptw_weight)

#sampling weight
model_mortality_sample <- coxph(Surv(survival_time, survival_status) ~ 
                                    digital_usage + age + age_sq + gender + Race + 
                                    residence_rural + H_education + marital_status + 
                                    equivalized_wealth, 
                                  data = df_mortality, weights = iptw_weight * WEIGHT)

#iptw < 10
df_mortality_iptw5 <- filter(df_mortality, df_mortality$iptw_weight < 10)
model_mortality_iptw5 <- coxph(Surv(survival_time, survival_status) ~ 
                                  digital_usage + age + age_sq + gender + Race + 
                                  residence_rural + H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_mortality_iptw5, weights = iptw_weight * WEIGHT)

#ipcw
model_mortality_sample <- coxph(Surv(survival_time, survival_status) ~ 
                                  digital_usage + age + age_sq + gender + Race + 
                                  residence_rural + H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_mortality, weights = iptw_weight * ipcw_cum_inw14)

#E-value
calc_e_value <- function(rr) {
  if (rr < 1) rr <- 1 / rr  # if rr < 1, take the reciprocal
  return(rr + rr * (rr - 1))
}
rr <- exp(coef(model_mortality))
variable_names <- names(rr)
e_values <- sapply(rr, calc_e_value)
