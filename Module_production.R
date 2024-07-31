## Production module

#flock prevalence of AMR 
data$Prev_f <- data$Prev_farm_type

#flock status
data$B_flock_status <- rbinom(Runs, 1, data$Prev_f)
data$B_flock_status <- ifelse(data$B_flock_status == 1,"p","n")

#Prevalence of birds from positive flock internally colonized at pre-harvest(within flock prevalence)
data$Prev_wfp_col <- data$Prev_wfp_col_base 

#number of bacteria on positive birds exterior at pre-harvest
data$C_btp <- data$C_barn * data$Amount_fec 

#prevalence of birds externally colonized at pre-harvest
data$Prev_wfp_ext <- rpert(Runs, data$Prev_wfp_ext.min[1], data$Prev_wfp_col , data$Prev_wfp_ext.max[1])

#probability of carry-over from a positive flock transported earlier on that day
data$P_ccbf <- (1 - (1 - data$Prev_f)^data$N_transp) * data$F_cross_trans

#probability for cross-contamination to occur during transport
data$P_ccwf <- 1 - (1- data$Prev_wfp_ext)^data$N_contact

#Probablity a negative bird will become contaminated during transport
data$P_pos <- data$P_ccwf + data$P_ccbf - (data$P_ccwf * data$P_ccbf)

#Prevalence of externally contaminated birds after transport
data$Prev_prod <- ifelse(data$B_flock_status=="p", 
                         data$Prev_wfp_ext + (1 - data$Prev_wfp_ext) * data$P_pos,
                         data$P_ccbf)
data$Prev_prod <- ifelse(data$Prev_prod > 1, 1, data$Prev_prod)

#Number of bacteria on positive birds exterior after transport
data$C_prod <- ifelse(data$B_flock_status=="p", 
                      data$C_btp * data$F_transp,  
                      data$C_prod_n)

## Store outputs

output$B_flock_status <- data$B_flock_status
output$load           <- data$C_barn
output$prev           <- data$Prev_farm_type
output$init_prev      <- data$Prev_wfp_col_base
output$C_prod         <- data$C_prod
output$Prev_prod      <- data$Prev_prod
  
  