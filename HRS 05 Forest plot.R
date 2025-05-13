library(forestplot)

# A. All-cause mortality
all_cause_mortality <- data.frame(
  Digital_usage = c("Never Users", "Dropouts", "Intermittent Users", "Adopters", "Consistent Users"),
  Hazard_Ratio = c(1, 0.9951414, 0.705467, 0.5694539, 0.6305143),
  CI_lower = c(1, 0.8943883, 0.5908066, 0.4721424, 0.5512219),
  CI_upper = c(1, 1.1072443, 0.8283008, 0.6605438, 0.7166446),
  P_value = c("Ref", ">0.05", "<0.001", "<0.001", "<0.001")
)

# B. Cardiovascular mortality
cardiovascular_mortality <- data.frame(
  Digital_usage = c("Never Users", "Dropouts", "Intermittent Users", "Adopters", "Consistent Users"),
  Hazard_Ratio = c(1, 1.1300355, 0.8655503, 1.0711207, 0.8635),
  CI_lower = c(1, 0.9308225, 0.6821788, 0.8638807, 0.7170029),
  CI_upper = c(1, 1.3309175, 1.0729831, 1.3033944, 1.0100129),
  P_value = c("Ref", ">0.05", ">0.05", ">0.05", ">0.05")
)

# C. Functio1l disability 
functio1l_disability <- data.frame(
  Digital_usage = c("Never Users", "Dropouts", "Intermittent Users", "Adopters", "Consistent Users"),
  Hazard_Ratio = c(1, 0.9269, 0.8606, 0.6508, 0.7073),
  CI_lower = c(1, 0.8003296, 0.7097041, 0.5402379, 0.6105635),
  CI_upper = c(1, 1.0549873, 1.0195367, 0.7601807, 0.8055827),
  P_value = c("Ref", ">0.05", ">0.05", "<0.001", "<0.001")
)

# D. Dementia 
dementia <- data.frame(
  Digital_usage = c("Never Users", "Dropouts", "Intermittent Users", "Adopters", "Consistent Users"),
  Hazard_Ratio = c(1, 1.1724569, 0.5914360, 0.3958620, 0.4723528),
  CI_lower = c(1, 0.8928774, 0.3473255, 0.1914874, 0.3026670),
  CI_upper = c(1, 1.4617997, 0.8440293, 0.5996607, 0.6465061),
  P_value = c("Ref", ">0.05", "<0.05", "<0.001", "<0.001")
)

# E. Depression 
depression <- data.frame(
  Digital_usage = c("Never Users", "Dropouts", "Intermittent Users", "Adopters", "Consistent Users"),
  Hazard_Ratio = c(1, 0.8351, 0.7957, 0.7753,  0.8466),
  CI_lower = c(1, 0.7069053, 0.6406135, 0.6422422, 0.7194137),
  CI_upper = c(1, 0.9794732, 0.9513209, 0.9119951, 0.9728117),
  P_value = c("Ref", "<0.05", "<0.05", "<0.05", "<0.05")
)

library(forestplot)

# A. All-cause mortality 
forestplot(labeltext = all_cause_mortality$Digital_usage,
           mean = all_cause_mortality$Hazard_Ratio,
           lower = all_cause_mortality$CI_lower,
           upper = all_cause_mortality$CI_upper,
           zero = 1,  
           xlab = " ",
           title = NULL,  
           new_page = TRUE,
           is.summary = c(FALSE, rep(FALSE, nrow(all_cause_mortality) - 1)),  
           col = fpColors(box = "grey", line = "black", summary = "darkred"),
           clip = c(0, 1.27),
           boxsize = 0.1,
           xticks = c(0.2, 0.75, 1.0, 1.5),
           xticks.digits = 2,  
           txt_gp = fpTxtGp(
             label = gpar(fontsize = 16),      
             xlab = gpar(fontsize = 18),      
             ticks = gpar(fontsize = 25),    
             legend = gpar(fontsize = 16)      
           )
)

grid.text("A.All-cause mortality", 
          x = unit(0.03, "npc"),
          y = unit(0.95, "npc"),
          just = c("left", "top"),
          gp = gpar(fontsize = 20, fontface = "bold")) 

grid.text("Hazard ratio", 
          x = unit(0.5, "npc"), 
          y = unit(0.04, "npc"), 
          gp = gpar(fontsize = 18))


# 2. Cardiovascular
forestplot(labeltext = cardiovascular_mortality$Digital_usage,
           mean = cardiovascular_mortality$Hazard_Ratio,
           lower = cardiovascular_mortality$CI_lower,
           upper = cardiovascular_mortality$CI_upper,
           zero = 1,  
           xlab = " ",
           title = NULL,   
           new_page = TRUE,
           is.summary = c(FALSE, rep(FALSE, nrow(cardiovascular_mortality) - 1)),  
           col = fpColors(box = "grey", line = "black", summary = "darkred"),
           clip = c(0, 1.27),
           boxsize = 0.1,
           xticks = c(0.2, 0.75, 1.0, 1.5),
           xticks.digits = 2,   
           txt_gp = fpTxtGp(
             label = gpar(fontsize = 16),       
             xlab = gpar(fontsize = 18),        
             ticks = gpar(fontsize = 25),       
             legend = gpar(fontsize = 16)      
           )
)

 
grid.text("B.Cardiovascular diseases", 
          x = unit(0.03, "npc"),
          y = unit(0.95, "npc"),
          just = c("left", "top"),
          gp = gpar(fontsize = 20, fontface = "bold"))  

 
grid.text("Hazard ratio", 
          x = unit(0.5, "npc"), 
          y = unit(0.04, "npc"), 
          gp = gpar(fontsize = 18))

 
forestplot(labeltext = functio1l_disability$Digital_usage,
           mean = functio1l_disability$Hazard_Ratio,
           lower = functio1l_disability$CI_lower,
           upper = functio1l_disability$CI_upper,
           zero = 1,  
           xlab = " ",
           title = NULL,   
           new_page = TRUE,
           is.summary = c(FALSE, rep(FALSE, nrow(functio1l_disability) - 1)),  
           col = fpColors(box = "grey", line = "black", summary = "darkred"),
           clip = c(0, 1.27),
           boxsize = 0.1,
           xticks = c(0.2, 0.75, 1.0, 1.5),
           xticks.digits = 2,   
           txt_gp = fpTxtGp(
             label = gpar(fontsize = 16),       
             xlab = gpar(fontsize = 18),        
             ticks = gpar(fontsize = 25),       
             legend = gpar(fontsize = 16)      
           )
)

 
grid.text("C.Functional disability", 
          x = unit(0.03, "npc"),
          y = unit(0.95, "npc"),
          just = c("left", "top"),
          gp = gpar(fontsize = 20, fontface = "bold"))  

 
grid.text("Hazard ratio", 
          x = unit(0.5, "npc"), 
          y = unit(0.04, "npc"), 
          gp = gpar(fontsize = 18))


# 4.   Dementia  
forestplot(labeltext = dementia$Digital_usage,
           mean = dementia$Hazard_Ratio,
           lower = dementia$CI_lower,
           upper = dementia$CI_upper,
           zero = 1,  
           xlab = " ",
           title = NULL,   
           new_page = TRUE,
           is.summary = c(FALSE, rep(FALSE, nrow(dementia) - 1)),  
           col = fpColors(box = "grey", line = "black", summary = "darkred"),
           clip = c(0, 1.27),
           boxsize = 0.1,
           xticks = c(0.2, 0.75, 1.0, 1.5),
           xticks.digits = 2,   
           txt_gp = fpTxtGp(
             label = gpar(fontsize = 16),       
             xlab = gpar(fontsize = 18),        
             ticks = gpar(fontsize = 25),       
             legend = gpar(fontsize = 16)      
           )
)

 
grid.text("D.Dementia", 
          x = unit(0.03, "npc"),
          y = unit(0.95, "npc"),
          just = c("left", "top"),
          gp = gpar(fontsize = 20, fontface = "bold"))  

 
grid.text("Hazard ratio", 
          x = unit(0.5, "npc"), 
          y = unit(0.04, "npc"), 
          gp = gpar(fontsize = 18))

# 5.   Depression  
forestplot(labeltext = depression$Digital_usage,
           mean = depression$Hazard_Ratio,
           lower = depression$CI_lower,
           upper = depression$CI_upper,
           zero = 1,  
           xlab = " ",
           title = NULL,   
           new_page = TRUE,
           is.summary = c(FALSE, rep(FALSE, nrow(depression) - 1)),  
           col = fpColors(box = "grey", line = "black", summary = "darkred"),
           clip = c(0, 1.27),
           boxsize = 0.1,
           xticks = c(0.2, 0.75, 1.0, 1.5),
           xticks.digits = 2,   
           txt_gp = fpTxtGp(
             label = gpar(fontsize = 16),       
             xlab = gpar(fontsize = 18),        
             ticks = gpar(fontsize = 25),       
             legend = gpar(fontsize = 16)      
           )
)

 
grid.text("E.Depression", 
          x = unit(0.03, "npc"),
          y = unit(0.95, "npc"),
          just = c("left", "top"),
          gp = gpar(fontsize = 20, fontface = "bold"))  

 
grid.text("Hazard ratio", 
          x = unit(0.5, "npc"), 
          y = unit(0.04, "npc"), 
          gp = gpar(fontsize = 18))


