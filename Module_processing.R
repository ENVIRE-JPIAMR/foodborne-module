## Processing module
#TODO: create environment w/ argument Runs

####################
## Scalding stage ##
####################

#Prevalene of externally contaminated birds after scalding
data$Prev_scald <- ifelse(data$C_prod > runif(1, 10^data$C_prod.min, 10^data$C_prod.max),
                          1,
                          data$Prev_prod)

#Proportion of bacteria remaining after scalding
data$Prop_scald <- 10^(-data$F_scald_soft)

#Number of bacteria remaining after scalding
data$C_scald <- data$C_prod * data$Prop_scald

########################
## Defeathering stage ##
########################

#Average number of carcasses between seeder bird and random bird at defeathering
data$N_df_add <- 1/data$Prev_scald

#Number of bacteria added to birds following a contaminated bird during defeathering
data$C_cross_df <- data$C_scald * 10^(data$a_df * log(data$N_df_add) + data$b_df)

#Does cross-contamination occur during defeathering?
data$B_cross_df_01 <- ifelse(data$B_flock_status=="p",
                             rbinom(Runs, 1, data$P_cross_df_p),
                             rbinom(Runs, 1, data$P_cross_df_n))

#Proportion of negative carcasses becoming positive via cross-contamination during defeathering
data$Prop_neg_df <- ifelse(data$B_cross_df_01 == 1,  
                           ifelse(data$B_flock_status=="p",
                                  (1-	data$Prev_scald)*data$P_cross_df_p,
                                  (1-	data$Prev_scald)*data$P_cross_df_n
                           ), 
                           0)

#Prevalence of contaminated carcasses after defeathering 
data$Prev_df <- ifelse( data$B_cross_df_01 ==1, 
                        data$Prev_scald + data$Prop_neg_df, 
                        data$Prev_scald)

#Number of bacteria on carcasses given gross-contamination during defeathering
data$C_conta_df <- ifelse(
  data$B_flock_status == "p",
  ifelse(
    data$P_cross_df_p != 0,
    sample(
      c(data$C_scald * 10 ^ data$F_df + data$C_cross_df, data$C_cross_df),
      size = Runs,
      prob = c(
        data$Prev_scald * data$P_cross_df_p,
        (1 - data$Prev_scald) * data$P_cross_df_p
      ),
      replace = TRUE
    ),
    data$C_cross_df
  ),
  ifelse(
    data$P_cross_df_n != 0,
    sample(
      c(data$C_scald * 10 ^ data$F_df + data$C_cross_df, data$C_cross_df),
      size = Runs,
      prob = c(
        data$Prev_scald * data$P_cross_df_n,
        (1 - data$Prev_scald) * data$P_cross_df_n
      ),
      replace = TRUE
    ),
    data$C_cross_df
  )
)

#Number of bacteria on carcasses after defeathering
data$C_df <- ifelse( data$B_cross_df_01 ==1, 
                     data$C_conta_df, 
                     data$C_scald * 10^data$F_df)

########################
## Evisceration stage ##
########################

#Are the viscera lacerated?
data$B_vis_cut <- rbinom(Runs, 1, data$P_cut_vis)

#Are the bird colonized?
data$B_bird_col <- rbinom(Runs, 1, data$Prev_wfp_col)

#Number of bacteria added in case of colonized viscera rupture
data$C_spill_cfu <- data$C_caecal * data$spill_weight

#Number of bacteria on carcass after potential viscera laceration
data$C_vis_cut <- ifelse(data$B_vis_cut == 1 & data$B_bird_col == 1, 
                         data$C_df + data$C_spill_cfu, 
                         data$C_df)

#Probability of cross-contamination to occur during evisceration for positive flocks
data$P_cross_ev_n <- data$P_cross_df_n

#Proportion of negative carcasses becoming positive via cross-conamination during evisceration
data$Prop_neg_ev <- ifelse(data$B_flock_status=="p",
                           (1-data$Prev_df)* data$P_cross_ev_p,
                           (1-data$Prev_df)* data$P_cross_ev_n)

#Does cross-contamination occur at evisceration ?
data$B_cross_ev_01 <- ifelse(data$B_flock_status=="p",
                             rbinom(Runs, 1, data$P_cross_ev_p),
                             rbinom(Runs, 1, data$P_cross_ev_n))

#Prevalence of contaminated carcasses from flocks after evisceration
data$Prev_ev <- ifelse(data$B_cross_ev_01 ==1, 
                              data$Prev_df + data$Prop_neg_ev, 
                              data$Prev_df)


#Number of bacteria on carcasses from flocks after evisceration 
data$C_ev <- ifelse(data$B_cross_ev_01 ==1, 
                    data$C_vis_cut + data$C_vis_cut*data$F_ev, 
                    data$C_vis_cut)

###################
## Washing stage ##
###################

#Prevalence of conaminated carcasses after chilling
data$Prev_proc <- data$Prev_ev

#Proportion of cells remaining after washing
data$Prop_wash <- ifelse(
  data$wash_type > 0.625,
  10 ^ data$F_wash_IOBW,
  10 ^ (data$F_wash_IOBW + data$F_wash_PE)
)

#Adding dampening factor
data$Prop_wash <- data$Prop_wash * (1 + data$F_wash_adj)

#Number of bacteria remaining after washing
data$C_wash <- data$C_ev * data$Prop_wash

####################
## Chilling stage ##
####################

#Number of bacteria remaining after chilling
data$C_chill <- data$C_wash * data$F_chill

######################
## Portioning stage ##
######################

#Number of bacteria on a random product
data$C_proc <- ifelse(
  data$Prop_product == 1,
  data$C_chill,
  data$C_chill * data$P_skin * data$Prop_cm * data$Prop_product
) 
