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


