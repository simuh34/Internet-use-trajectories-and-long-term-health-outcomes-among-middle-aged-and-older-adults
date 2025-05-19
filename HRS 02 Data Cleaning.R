#1. construct internet usage
df$digital_usage <- ifelse(df$int_5 == '00000',"Never users",
                                ifelse(df$int_5 == '11111','Consistent users',
                                       ifelse(df$int_5 == '00001'|df$int_5 == '00011'|
                                                df$int_5 == '00111'|df$int_5 == '01111' ,"Adopters", ifelse(df$int_5 == '00010'|df$int_5 == '00100'|df$int_5 == '00110'|df$int_5 == '01000'
                                                                                                            |df$int_5 == '01100'|df$int_5 == '01110'|df$int_5 == '10000'|df$int_5 == '11000'
                                                                                                            |df$int_5 == '11100'|df$int_5 == '11110',"Dropouts","Intermittent Users"))))

#2.ipaw
df <- df %>%
  mutate(

    death_inw11 = ifelse(inw11 == 5, 1, ifelse(inw11 < 5, 0, NA)),
    death_inw12 = ifelse(inw12 == 5, 1, ifelse(inw12 < 5, 0, NA)),
    death_inw13 = ifelse(inw13 == 5, 1, ifelse(inw13 < 5, 0, NA)),
    death_inw14 = ifelse(inw14 == 5, 1, ifelse(inw14 < 5, 0, NA)),
    
  
    lost_inw11 = ifelse(inw11 == 7 | inw11 == 4, 1, ifelse(inw11 == 1, 0, NA)),
    lost_inw12 = ifelse(inw12 == 7 | inw12 == 4, 1, ifelse(inw12 == 1, 0, NA)),
    lost_inw13 = ifelse(inw13 == 7 | inw13 == 4, 1, ifelse(inw13 == 1, 0, NA)),
    lost_inw14 = ifelse(inw14 == 7 | inw14 == 4, 1, ifelse(inw14 == 1, 0, NA))
  )

for (wave in 11:14) {
  death_var <- paste0("death_inw", wave)
  lost_var <- paste0("lost_inw", wave)

  model_death <- glm(as.formula(paste(death_var, "~ age + gender + Race + H_education + marital_status")),
                     data = df, 
                     family = binomial, 
                     control = glm.control(maxit = 1000), 
                     na.action = na.exclude)  
  
  model_lost <- glm(as.formula(paste(lost_var, "~ age + gender + Race + H_education + marital_status")),
                    data = df, 
                    family = binomial, 
                    control = glm.control(maxit = 1000),
                    na.action = na.exclude)  
  
  pred_death <- predict(model_death, type = "response")
  pred_lost <- predict(model_lost, type = "response")
}
for (wave in 11:14) {
  prob_var <- paste0("prob_alive_inw", wave)
  ipcw_var <- paste0("ipcw_inw", wave)
  
  df <- df %>%
    mutate(!!ipcw_var := 1 / get(prob_var))
}
df <- df %>%
  mutate(
    ipcw_cum_inw11 = ipcw_inw11,
    ipcw_cum_inw12 = ifelse(is.na(ipcw_inw11), NA, ipcw_inw12 * ipcw_inw11),
    ipcw_cum_inw13 = ifelse(is.na(ipcw_cum_inw12), NA, ipcw_inw13 * ipcw_cum_inw12),
    ipcw_cum_inw14 = ifelse(is.na(ipcw_cum_inw13), NA, ipcw_inw14 * ipcw_cum_inw13)
  )

df %>%
  select(ipcw_cum_inw11, ipcw_cum_inw12, ipcw_cum_inw13, ipcw_cum_inw14) %>%
  summary()
