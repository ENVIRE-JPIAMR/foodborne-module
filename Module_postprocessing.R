#########################################################################################################################################
# FSA project FS307037 
# Development of a modular assessment framework to quantify the risk of AMR exposure via food products: example of chicken and lettuce
##########################################################################################################################################
# File Name:     Module_postprocessing.R
# Description:   Calculate variables associated with the post processing module

# Company:       Ausvet Eu
# Author:        Céline Faverjon
# Role:          Epidemiologist
# Email:         celine@ausvet.eu
# Date:          03.02.21
##########################################################################################################################################

##########################################################################################################################################

#breast or carcass size
data$Size <- ifelse(data$Product_cut =="no", 
                    data$Prop_carc_yield*rnorm(1, data$Size_mean) , 
                    rnorm(1, data$Size_mean)*data$Prop_product)

#Maximum possible number of CFU on a carcass or breast
data$C_max <- data$C_MPD * data$Size

#average chicken temperature during home transport
data$T_avg_trans<- (data$T_retail + data$T_post_trans)/2

#Generation time in food product at retail 
data$Time_gen_retail <- data$Time_gen_min/((data$T_retail-data$T_growth_min)/(data$T_growth_opt - data$T_growth_min))^2

#Generation time in food product during transport 
data$Time_gen_trans <- data$Time_gen_min/((data$T_avg_trans - data$T_growth_min)/(data$T_growth_opt - data$T_growth_min))^2

#Generation time in food product at home
data$Time_gen_fridge  <- data$Time_gen_min/((data$T_fridge - data$T_growth_min)/(data$T_growth_opt - data$T_growth_min))^2

#Growth factor G at retail 
data$G_retail <- ifelse(data$T_retail > data$T_growth_min, 
                        exp(log(2)/data$Time_gen_retail * (data$Time_retail*24)),
                        1)

#Growth factor G during transport 
data$G_trans <- ifelse(data$T_avg_trans > data$T_growth_min, 
                       exp(log(2)/data$Time_gen_trans * data$Time_trans/60),
                       1)

#Growth factor G at home
data$G_fridge <- ifelse(data$T_fridge > data$T_growth_min, 
                        exp(log(2)/data$Time_gen_fridge * (data$Time_fridge*24)),
                        1)

# bacteria load of contaminated products after post-processing
data$C_ret <- data$C_proc*data$G_retail*data$G_trans*data$G_fridge

# Bacterial reduction load due to packaging
data$C_pack <- ifelse( data$Pack_type == "MAP" & (data$Time_retail*24 + data$Time_trans/60+ data$Time_fridge*24 > 7*24),
                       data$C_ret * 10^data$F_pack,
                       data$C_ret)

# adjusted bacteria load of contaminated products after post-processing
data$C_pproc <- ifelse(data$C_pack <1, 
                       1, 
                       ifelse(data$C_pack > data$C_max, 
                              data$C_max, 
                              data$C_pack))

# adjusted prevalence of contaminated products after post-processing
data$Prev_pproc <- ifelse(data$C_pproc ==1,
                          data$Prev_proc * (1- dpois(0, data$C_pproc)),
                          data$Prev_proc)
