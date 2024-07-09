## Post-processing module
#TODO: create environment w/ argument Runs

############################################
## Retail, transport & home storage stage ##
############################################

#Breast and carcass portion size
data$breast_size <-
  rgamma(
    Runs,
    (data$Breast_size_mean / data$Breast_size_sd) ^ 2,
    (data$Breast_size_sd ^ 2) / data$Breast_size_mean
  )

data$carcass_size <-
  data$Prop_carc_yield * rgamma(
    Runs,
    (data$Carcass_size_mean / data$Carcass_size_sd) ^ 2,
    (data$Carcass_size_sd ^ 2) / data$Carcass_size_mean
  )

data$Size <- ifelse(data$Prop_product == 1, 
                    data$carcass_size, 
                    data$breast_size)

#Maximum possible number of CFU on a carcass or breast
data$C_max <- data$C_MPD * data$Size

#Average chicken temperature during home transport
data$T_avg_trans<- (data$T_retail + data$T_post_trans)/2

#Generation time in food product at retail
data$Time_gen_retail <-
  data$Time_gen_min / ((data$T_retail - data$T_growth_min) / (data$T_growth_opt - data$T_growth_min)) ^
  2

#Generation time in food product during transport
data$Time_gen_trans <-
  data$Time_gen_min / ((data$T_avg_trans - data$T_growth_min) / (data$T_growth_opt - data$T_growth_min)) ^
  2

#Generation time in food product at home
data$Time_gen_fridge  <-
  data$Time_gen_min / ((data$T_fridge - data$T_growth_min) / (data$T_growth_opt - data$T_growth_min)) ^
  2

#Growth factor G (per h) at retail 
data$G_retail <- ifelse(data$T_retail > data$T_growth_min, 
                        exp(log(2)/data$Time_gen_retail * (data$Time_retail*24)),
                        1)

#Growth factor G (per h) during transport 
data$G_trans <- ifelse(data$T_avg_trans > data$T_growth_min, 
                       exp(log(2)/data$Time_gen_trans * data$Time_trans/60),
                       1)

#Growth factor G (per h) at home
data$G_fridge <- ifelse(data$T_fridge > data$T_growth_min, 
                        exp(log(2)/data$Time_gen_fridge * (data$Time_fridge*24)),
                        1)

#Bacteria load of contaminated products after post-processing
data$C_ret <- data$C_proc*data$G_retail * data$G_trans*data$G_fridge

#####################
## Packaging stage ##
#####################

#Bacterial reduction load due to packaging
data$Carcass_fresh <- rbinom(Runs, 1, data$Prop_carcass_fresh)
data$Breast_MAP <- rbinom(Runs, 1, data$Prop_breast_MAP)

data$C_pack <- ifelse(
  data$Prop_product == 1,
  ifelse(data$Carcass_fresh, data$C_ret, data$C_ret * data$F_freezer),
  ifelse(
    data$Breast_MAP == 1 &
      (
        data$Time_retail * 24 + data$Time_trans / 60 + data$Time_fridge * 24 > 7 *
          24
      ),
    data$C_ret * 10 ^ data$F_pack,
    data$C_ret
  )
)

#Adjusted bacteria load of contaminated products after post-processing
data$C_pproc <- ifelse(data$C_pack < 1, 
                       1, 
                       ifelse(data$C_pack > data$C_max, 
                              data$C_max, 
                              data$C_pack))

#Adjusted prevalence of contaminated products after post-processing
data$Prev_pproc <- ifelse(data$C_pproc == 1,
                          data$Prev_proc * (1- dpois(0, data$C_pproc)),
                          data$Prev_proc)
