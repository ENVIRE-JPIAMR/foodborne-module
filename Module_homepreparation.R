## Home preparation module

# Decimal reduction time at the exposure temperature in the protected area
data$R_ref <- 10^(data$D_value_beta * data$T_protec + data$D_value_alpha)

#Prevalence of contaminated servings after cooking 
data$Prev_home_cook <- data$P_undercook * data$Prev_pproc

# Number of bacteria on one undercooked portion post cook 
data$C_home_cook <- data$Prop_protec * 10^(log(data$C_pproc) - data$Time_protec / data$R_ref)


## Store outputs

output$Prev_home_cook <- data$Prev_home_cook
output$C_home_cook    <- data$C_home_cook

