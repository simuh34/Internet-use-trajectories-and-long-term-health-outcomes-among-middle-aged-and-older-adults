####cardiovascular model####
model_cvd <- coxph(surv_obj ~ digital_usage + age + age_sq + gender + Race + 
                     residence_rural + H_education + marital_status + 
                     equivalized_wealth, data = df_cvd, weights = iptw_weight)

####interaction####
model_mortality_inter <- coxph(Surv(event_time, event_status) ~ 
                                 digital_usage + gender  + age + age_sq + Race + 
                                 residence_rural + H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_cvd, weights = iptw_weight)

model_mortality_gender_inter <- coxph(Surv(event_time, event_status) ~ 
                                        digital_usage*gender  + age + age_sq + Race + 
                                        residence_rural + H_education + marital_status + 
                                        equivalized_wealth, 
                                      data = df_cvd, weights = iptw_weight)

model_mortality_race_inter <- coxph(Surv(event_time, event_status) ~ 
                                      digital_usage * Race + gender  + age + age_sq + 
                                      residence_rural + H_education + marital_status + 
                                      equivalized_wealth, 
                                    data = df_cvd, weights = iptw_weight)

model_mortality_rural_inter <- coxph(Surv(event_time, event_status) ~ 
                                       digital_usage * residence_rural + Race + gender  + age + age_sq + 
                                       residence_rural + H_education + marital_status + 
                                       equivalized_wealth, 
                                     data = df_cvd, weights = iptw_weight)

model_mortality_wealth_inter <- coxph(Surv(event_time, event_status) ~ 
                                        digital_usage * equivalized_wealth + residence_rural + Race + gender  + age + age_sq + 
                                        residence_rural + H_education + marital_status, 
                                      data = df_cvd, weights = iptw_weight)

####stratified analysis####
model_cvd_men <- coxph(Surv(event_time, event_status) ~ 
                               digital_usage + age + age_sq + Race + 
                               residence_rural + H_education + marital_status + 
                               equivalized_wealth, 
                             data = df_cvd[df_cvd$gender == "Men",], weights = iptw_weight)

model_cvd_women <- coxph(Surv(event_time, event_status) ~ 
                                 digital_usage  + age + age_sq + Race + 
                                 residence_rural + H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_cvd[df_cvd$gender == "Women",], weights = iptw_weight)

model_cvd_white <- coxph(Surv(event_time, event_status) ~ 
                                 digital_usage + age + age_sq  + 
                                 residence_rural + H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_cvd[df_cvd$Race == "Non-Hispanic White",], weights = iptw_weight)

model_cvd_other <- coxph(Surv(event_time, event_status) ~ 
                                 digital_usage  + age + age_sq + 
                                 residence_rural + H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_cvd[df_cvd$Race == "Other",], weights = iptw_weight)

model_cvd_rural <- coxph(Surv(event_time, event_status) ~ 
                           digital_usage + age + age_sq  + 
                           H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_cvd[df_cvd$residence_rural == "rural",], weights = iptw_weight)

model_cvd_urban <- coxph(Surv(event_time, event_status) ~ 
                           digital_usage  + age + age_sq + 
                           H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_cvd[df_cvd$residence_rural == "urban",], weights = iptw_weight)

model_adl_low <- coxph(Surv(event_time, event_status) ~ 
                         digital_usage + age + age_sq  + 
                         H_education + marital_status + residence_rural, 
                       data = df_cvd[df_cvd$equivalized_wealth == "Lower wealth",], weights = iptw_weight)

model_adl_high <- coxph(Surv(event_time, event_status) ~ 
                          digital_usage  + age + age_sq + 
                          H_education + marital_status + 
                          residence_rural, 
                        data = df_cvd[df_cvd$equivalized_wealth == "Upper wealth",], weights = iptw_weight)
