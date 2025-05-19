####dementia model####
model_dementia <- coxph(surv_obj ~ digital_usage + age + age_sq + gender + Race + 
                     residence_rural + H_education + marital_status + 
                     equivalized_wealth, data = df_dementia, weights = iptw_weight)

####interaction####
model_dementia <- coxph(Surv(event_time, event_status) ~ 
                                 digital_usage + gender  + age + age_sq + Race + 
                                 residence_rural + H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_dementia, weights = iptw_weight)

model_dementia_gender_inter <- coxph(Surv(event_time, event_status) ~ 
                                        digital_usage*gender  + age + age_sq + Race + 
                                        residence_rural + H_education + marital_status + 
                                        equivalized_wealth, 
                                      data = df_dementia, weights = iptw_weight)

model_dementia_race_inter <- coxph(Surv(event_time, event_status) ~ 
                                      digital_usage * Race + gender  + age + age_sq + 
                                      residence_rural + H_education + marital_status + 
                                      equivalized_wealth, 
                                    data = df_dementia, weights = iptw_weight)

model_dementia_rural_inter <- coxph(Surv(event_time, event_status) ~ 
                                       digital_usage * residence_rural + Race + gender  + age + age_sq + 
                                       residence_rural + H_education + marital_status + 
                                       equivalized_wealth, 
                                     data = df_dementia, weights = iptw_weight)

model_dementia_wealth_inter <- coxph(Surv(event_time, event_status) ~ 
                                        digital_usage * equivalized_wealth + residence_rural + Race + gender  + age + age_sq + 
                                        residence_rural + H_education + marital_status, 
                                      data = df_dementia, weights = iptw_weight)

####stratified analysis####
model_dementia_men <- coxph(Surv(event_time, event_status) ~ 
                         digital_usage + age + age_sq + Race + 
                         residence_rural + H_education + marital_status + 
                         equivalized_wealth, 
                       data = df_dementia[df_dementia$gender == "Men",], weights = iptw_weight)

model_dementia_women <- coxph(Surv(event_time, event_status) ~ 
                           digital_usage  + age + age_sq + Race + 
                           residence_rural + H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_dementia[df_dementia$gender == "Women",], weights = iptw_weight)

model_dementia_white <- coxph(Surv(event_time, event_status) ~ 
                           digital_usage + age + age_sq  + 
                           residence_rural + H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_dementia[df_dementia$Race == "Non-Hispanic White",], weights = iptw_weight)

model_dementia_other <- coxph(Surv(event_time, event_status) ~ 
                           digital_usage  + age + age_sq + 
                           residence_rural + H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_dementia[df_dementia$Race == "Other",], weights = iptw_weight)

model_dementia_rural <- coxph(Surv(event_time, event_status) ~ 
                                  digital_usage + age + age_sq  + 
                                  H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_dementia[df_dementia$residence_rural == "rural",], weights = iptw_weight)

model_dementia_urban <- coxph(Surv(event_time, event_status) ~ 
                                  digital_usage  + age + age_sq + 
                                  H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_dementia[df_dementia$residence_rural == "urban",], weights = iptw_weight)

model_dementia_low <- coxph(Surv(event_time, event_status) ~ 
                         digital_usage + age + age_sq  + 
                         H_education + marital_status + residence_rural, 
                       data = df_dementia[df_dementia$equivalized_wealth == "Lower wealth",], weights = iptw_weight)

model_dementia_high <- coxph(Surv(event_time, event_status) ~ 
                          digital_usage  + age + age_sq + 
                          H_education + marital_status + 
                          residence_rural, 
                        data = df_dementia[df_dementia$equivalized_wealth == "Upper wealth",], weights = iptw_weight)
