###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima and Willamette River 
# Basins
# DATA PREPARATION
###############################################################################

#By : Francisco Guerrero
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

#Loading packages:

# Run for the first time only
#install.packages(librarian)

#To run this code in macOS it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(tidyverse)
set.seed(2703)

# Data:

#values

#Yakima River Basin
yrb_lgc_o <- read.csv("assets/data/raw/220725_yrb_resp_vars_legacy.csv",
                      stringsAsFactors = TRUE)
yrb_spt_o <- read.csv("assets/data/raw/230110_yrb_spatial_camp.csv", 
                      stringsAsFactors = TRUE)
yrb_rsp_o <- read.csv("assets/data/raw/230116_yrb_respt_vars.csv", 
                      stringsAsFactors = TRUE)
yrb_hbc_o <- read.csv("assets/data/raw/230117_yrb_hbgc_vars.csv", 
                      stringsAsFactors = TRUE)

#Willamette River Basin
wlm_rsp_o <- read.csv("assets/data/raw/cum_resp_WM_mass_data_0116_2023.csv", 
                      stringsAsFactors = TRUE)
wlm_rsp_i <- read.csv("assets/data/raw/model_resp_wm_rf0116.csv", stringsAsFactors = TRUE)
wlm_hbc_o <- read.csv("assets/data/raw/nhd_WM_streamdatabase_annual_resp_mass_01162023.csv", 
                      stringsAsFactors = TRUE)
wlm_hbc_i <- read.csv("assets/data/raw/model_resp_annual_wm_input_output_df_01_16_2023.csv",
                      stringsAsFactors = TRUE)

# Re-ordering Willamette River Basin Data

#The data from the Willamette River Basin were provided by K. Son in different 
#spreadsheets and organization compared to the YRB. We are going to start by
#reorganizing these data sets such that we could easily bind them with the YRB
#data sets.

#First, let's take a look into the column names for the respiration data from 
#YRB

var_names <- colnames(yrb_rsp_o)

# [1] "COMID"                       "FromNode"                    "ToNode"                      "Hydrosq"                    
# [5] "CAT_STREAM_LENGTH"           "CAT_STREAM_SLOPE"            "TOT_STREAM_LENGTH"           "TOT_STREAM_SLOPE"           
# [9] "CAT_BASIN_AREA"              "TOT_BASIN_AREA"              "pred_stream_area_m2_fill"    "pred_logw_m"                
# [13] "stream_length_m"             "cum_totco2g_day"             "cum_stream_area_m2"          "cum_stream_length_m"        
# [17] "cum_totco2g_day_Tsurface_m2" "cum_totco2g_day_Tdrain_m2"  

#Second, we need to identify where these variables are stored withing the WLM 
#datasets

glimpse(wlm_rsp_o)
glimpse(wlm_rsp_i)
glimpse(wlm_hbc_o)
glimpse(wlm_hbc_i)

p <- ggplot(wlm_hbc_i,aes(TotDASqKM,logDA_km2))+
  geom_point()
p

#We will have to paste together columns from both datasets in different orders

wlm_rsp_1 <- cbind(wlm_rsp_o[,1:3],wlm_hbc_o[,c(1,5,7)])


# header info

#PENDING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!