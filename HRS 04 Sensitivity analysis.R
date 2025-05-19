#1.complete cases
base_surv <- survfit(Surv(survival_time, survival_status) ~ digital_usage,data = df_complete)
ggsurvplot(base_surv, data = df_complete,legend.title = "Digital Usage",
           legend.labs = levels(as.factor(df_complete$digital_usage)),
           xlab = "Survival Months",  
           ylab = "Suevival Probability",
           pval = TRUE)




