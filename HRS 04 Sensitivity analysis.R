#1.lagged analysis
df_lag <- df[df$inw11 == 1,]
df_lag <- df[df_lag$death_year > 2012 | is.na(df_lag$death_year), ]
model_lag <- coxph(Surv(survival_time, survival_status) ~ 
                           digital_usage + age + age_sq + gender + Race + 
                           residence_rural + H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_lag, weights = iptw_weight)

#2.complete information n = 9891
model_complete <- coxph(Surv(survival_time, survival_status) ~ 
                               digital_usage + age + age_sq + gender + Race + 
                               residence_rural + H_education + marital_status + 
                               equivalized_wealth, 
                             data = df_complete, weights = iptw_weight)

#3.sampling weight
model_sample <- coxph(Surv(survival_time, survival_status) ~ 
                                    digital_usage + age + age_sq + gender + Race + 
                                    residence_rural + H_education + marital_status + 
                                    equivalized_wealth, 
                                  data = df, weights = iptw_weight * WEIGHT)

#4.iptw < 10
df_iptw5 <- filter(df, df$iptw_weight < 10)
model_iptw5 <- coxph(Surv(survival_time, survival_status) ~ 
                                  digital_usage + age + age_sq + gender + Race + 
                                  residence_rural + H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_iptw5, weights = iptw_weight * WEIGHT)

#5.ipcw
model_sample <- coxph(Surv(survival_time, survival_status) ~ 
                                  digital_usage + age + age_sq + gender + Race + 
                                  residence_rural + H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df, weights = iptw_weight * ipcw_cum_inw14)

#6.E-value
calc_e_value <- function(rr) {
  if (rr < 1) rr <- 1 / rr  # if rr < 1, take the reciprocal
  return(rr + rr * (rr - 1))
}
rr <- exp(coef(model))
variable_names <- names(rr)
e_values <- sapply(rr, calc_e_value)
