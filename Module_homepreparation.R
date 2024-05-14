#########################################################################################################################################
# FSA project FS307037 
# Development of a modular assessment framework to quantify the risk of AMR exposure via food products: example of chicken and lettuce
##########################################################################################################################################
# File Name:     Module_homepreparation.R
# Description:   Calculate variables associated with the home prepartion module

# Company:       Ausvet Eu
# Author:        Céline Faverjon
# Role:          Epidemiologist
# Email:         celine@ausvet.eu
# Date:          03.02.21
##########################################################################################################################################

##########################################################################################################################################

# Volume of fluid diluting for a piece of chicken meat
data$V_dil <- ifelse(data$Product_cut =="no",
                     runif(1, 150 , 205),
                     data$Size_mean/1495.6* runif(1, 150 , 205) )

# Decimal reduction time at the exposure temperature in the protected area
data$R_ref <- 10^(-0.139*data$T_protec +8.58)

# Transfer factor
data$tsf <- data$Prop_fluid* data$V_ing/data$V_dil

#Prevalence of contaminated servings after cooking 
data$Prev_home_cook <- data$P_undercook* data$Prev_pproc

# Number of bacteria on one undercooked portion post cook 
data$C_home_cook <- data$Prop_protec* 10^(log(data$C_pproc) - data$Time_protec/ data$R_ref)

# Probability of exposure through cross contamination
data$P_cc <- data$P_h_wash* data$Prev_pproc

# Number of bacteria ingested by cross contamination
data$C_cc <- data$tsf* data$C_pproc

# Adjusted probability of exposure through cross contamination
data$P_home_cc <- data$P_cc * (1- dpois(0,data$C_cc))

# Adjusted number of bacteria ingested by cross contamination
data$C_home_cc <- ifelse(data$C_cc <1, 
                         1, 
                         data$C_cc)
