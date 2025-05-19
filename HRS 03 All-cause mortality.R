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
