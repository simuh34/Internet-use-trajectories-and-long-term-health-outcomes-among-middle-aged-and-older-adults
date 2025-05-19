####cardiovascular model####
surv_obj <- Surv(time = df_adl$event_time, event = df_adl$event_status)
model_adl <- coxph(surv_obj ~ digital_usage + age + age_sq + gender + Race + 
                     residence_rural + H_education + marital_status + 
                     equivalized_wealth, data = df_adl, weights = iptw_weight)

####interaction####
model_adl_inter <- coxph(Surv(event_time, event_status) ~ 
                                digital_usage + gender  + age + age_sq + Race + 
                                residence_rural + H_education + marital_status + 
                                equivalized_wealth, 
                              data = df_adl, weights = iptw_weight)

model_adl_gender_inter <- coxph(Surv(event_time, event_status) ~ 
                                       digital_usage*gender  + age + age_sq + Race + 
                                       residence_rural + H_education + marital_status + 
                                       equivalized_wealth, 
                                     data = df_adl, weights = iptw_weight)

model_adl_race_inter <- coxph(Surv(event_time, event_status) ~ 
                                     digital_usage * Race + gender  + age + age_sq + 
                                     residence_rural + H_education + marital_status + 
                                     equivalized_wealth, 
                                   data = df_adl, weights = iptw_weight)

model_adl_rural_inter <- coxph(Surv(event_time, event_status) ~ 
                                      digital_usage * residence_rural + Race + gender  + age + age_sq + 
                                      residence_rural + H_education + marital_status + 
                                      equivalized_wealth, 
                                    data = df_adl, weights = iptw_weight)

model_adl_wealth_inter <- coxph(Surv(event_time, event_status) ~ 
                                       digital_usage * equivalized_wealth + residence_rural + Race + gender  + age + age_sq + 
                                       residence_rural + H_education + marital_status, 
                                     data = df_adl, weights = iptw_weight)

#stratified analysis
model_adl_men <- coxph(Surv(event_time, event_status) ~ 
                                digital_usage + age + age_sq + Race + 
                                residence_rural + H_education + marital_status + 
                                equivalized_wealth, 
                              data = df_adl[df_adl$gender == "Men",], weights = iptw_weight)

model_adl_women <- coxph(Surv(event_time, event_status) ~ 
                                  digital_usage  + age + age_sq + Race + 
                                  residence_rural + H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_adl[df_adl$gender == "Women",], weights = iptw_weight)

model_adl_white <- coxph(Surv(event_time, event_status) ~ 
                                  digital_usage + age + age_sq  + 
                                  residence_rural + H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_adl[df_adl$Race == "Non-Hispanic White",], weights = iptw_weight)

model_adl_other <- coxph(Surv(event_time, event_status) ~ 
                                  digital_usage  + age + age_sq + 
                                  residence_rural + H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_adl[df_adl$Race == "Other",], weights = iptw_weight)

model_adl_rural <- coxph(Surv(event_time, event_status) ~ 
                                 digital_usage + age + age_sq  + 
                                 H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_adl[df_adl$residence_rural == "rural",], weights = iptw_weight)

model_adl_urban <- coxph(Surv(event_time, event_status) ~ 
                                 digital_usage  + age + age_sq + 
                                 H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_adl[df_adl$residence_rural == "urban",], weights = iptw_weight)

model_adl_low <- coxph(Surv(event_time, event_status) ~ 
                               digital_usage + age + age_sq  + 
                               H_education + marital_status + residence_rural, 
                             data = df_adl[df_adl$equivalized_wealth == "Lower wealth",], weights = iptw_weight)

model_adl_high <- coxph(Surv(event_time, event_status) ~ 
                                digital_usage  + age + age_sq + 
                                H_education + marital_status + 
                                residence_rural, 
                              data = df_adl[df_adl$equivalized_wealth == "Upper wealth",], weights = iptw_weight)
