
rm(list = ls())
library(plyr)
setwd("C:/Users/amr418/Desktop/RaCA")

####Load RaCA data####


raca_loc <- read.csv("RaCA_loc.csv")
raca_data <- read.csv("RaCA_samples.csv")

####Join data to locations####
raca_comp <- join(raca_data, raca_loc, type  = "left")

####Remove unnecessary columns
raca_comp<- raca_comp[,!names(raca_comp) %in% c("hor_top","hor_bot","Bulkdensity","SOC_pred1","BD1","BDmeasured","BDmethod","lay_depth_to_top","lay_depth_to_bottom","horizon_designation","Model_desg","Model_BD","total_thic","SOC_count","Lab_count","Depth_to_R","Non_R_Samp","SOC_thickn","USE", "hzn_desgn","M","OBJECTID")] 


####Calculate SOC if CaCO3 present####

raca_comp$soc <- ifelse(is.na(raca_comp$caco3), raca_comp$c_tot_ncs, raca_comp$c_tot_ncs - (0.11*raca_comp$caco3))

####Save csv file ####
write.csv(raca_comp, "RaCA_12.6.2016.csv")
