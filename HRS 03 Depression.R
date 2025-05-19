####depression model####
model_depression <- coxph(surv_obj ~ digital_usage + age + age_sq + gender + Race + 
                     residence_rural + H_education + marital_status + 
                     equivalized_wealth, data = df_depression, weights = iptw_weight)

####interaction####
model_mortality <- coxph(Surv(event_time, event_status) ~ 
                                 digital_usage + gender  + age + age_sq + Race + 
                                 residence_rural + H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_depression, weights = iptw_weight)

model_mortality_gender_inter <- coxph(Surv(event_time, event_status) ~ 
                                        digital_usage*gender  + age + age_sq + Race + 
                                        residence_rural + H_education + marital_status + 
                                        equivalized_wealth, 
                                      data = df_depression, weights = iptw_weight)

model_mortality_race_inter <- coxph(Surv(event_time, event_status) ~ 
                                      digital_usage * Race + gender  + age + age_sq + 
                                      residence_rural + H_education + marital_status + 
                                      equivalized_wealth, 
                                    data = df_depression, weights = iptw_weight)

model_mortality_rural_inter <- coxph(Surv(event_time, event_status) ~ 
                                       digital_usage * residence_rural + Race + gender  + age + age_sq + 
                                       residence_rural + H_education + marital_status + 
                                       equivalized_wealth, 
                                     data = df_depression, weights = iptw_weight)

model_mortality_wealth_inter <- coxph(Surv(event_time, event_status) ~ 
                                        digital_usage * equivalized_wealth + residence_rural + Race + gender  + age + age_sq + 
                                        residence_rural + H_education + marital_status, 
                                      data = df_depression, weights = iptw_weight)

#stratified analysis
model_depression_men <- coxph(Surv(event_time, event_status) ~ 
                         digital_usage + age + age_sq + Race + 
                         residence_rural + H_education + marital_status + 
                         equivalized_wealth, 
                       data = df_depression[df_depression$gender == "Men",], weights = iptw_weight)

model_depression_women <- coxph(Surv(event_time, event_status) ~ 
                           digital_usage  + age + age_sq + Race + 
                           residence_rural + H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_depression[df_depression$gender == "Women",], weights = iptw_weight)

model_depression_white <- coxph(Surv(event_time, event_status) ~ 
                           digital_usage + age + age_sq  + 
                           residence_rural + H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_depression[df_depression$Race == "Non-Hispanic White",], weights = iptw_weight)

model_depression_other <- coxph(Surv(event_time, event_status) ~ 
                           digital_usage  + age + age_sq + 
                           residence_rural + H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_depression[df_depression$Race == "Other",], weights = iptw_weight)

model_depression_rural <- coxph(Surv(event_time, event_status) ~ 
                           digital_usage + age + age_sq  + 
                           H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_depression[df_depression$residence_rural == "rural",], weights = iptw_weight)

model_depression_urban <- coxph(Surv(event_time, event_status) ~ 
                           digital_usage  + age + age_sq + 
                           H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_depression[df_depression$residence_rural == "urban",], weights = iptw_weight)

model_depression_low <- coxph(Surv(event_time, event_status) ~ 
                         digital_usage + age + age_sq  + 
                         H_education + marital_status + residence_rural, 
                       data = df_depression[df_depression$equivalized_wealth == "Lower wealth",], weights = iptw_weight)

model_depression_high <- coxph(Surv(event_time, event_status) ~ 
                          digital_usage  + age + age_sq + 
                          H_education + marital_status + 
                          residence_rural, 
                        data = df_depression[df_depression$equivalized_wealth == "Upper wealth",], weights = iptw_weight)

final_output <- summary_result[, c("contrast", "ratio", "lower.CL", "upper.CL", "p.value")]
print(final_output)
