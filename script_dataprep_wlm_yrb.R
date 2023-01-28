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
#reorganizing these data sets so that we could easily bind them with the YRB
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

#Let's pull the needed columns from each dataset

wlm_hbc_om <- dplyr::select(wlm_hbc_o,COMID,CAT_STREAM_SLOPE,TOT_STREAM_SLOPE,TOT_BASIN_AREA)
wlm_hbc_im <- dplyr::select(wlm_hbc_i,COMID,TotDASqKM,length_m)
wlm_rsp_om <- dplyr::select(wlm_rsp_o,COMID,FromNode,ToNode,cum_totco2g_day,cum_stream_area_m2,
                            cum_stream_length_m, cum_totco2g_day_Tdrain_m2, cum_totco2g_day_Tsurface_m2)
wlm_rsp_im <- dplyr::select(wlm_rsp_i,COMID,totco2g_day_fill,pred_stream_area_m2_fill,stream_length_m)

#Since merge only takes two inputs at a time, we have to merge our datasets sequentially
wlm_hbc_mg <- unique(merge(wlm_hbc_om,wlm_hbc_im,by = "COMID"))
wlm_rsp_mg <- unique(merge(wlm_rsp_om,wlm_rsp_im,by = "COMID"))
wlm_rsp_mg0 <- unique(merge(wlm_hbc_mg,wlm_rsp_mg,by="COMID"))
wlm_rsp_mg0$pred_logw_m <- log((wlm_rsp_mg0$pred_stream_area_m2_fill/wlm_rsp_mg0$stream_length_m),10)

#We will now reorganize columns to bind the YRM and WLM datasets
yrb_rsp_m1 <- dplyr::select(yrb_rsp_o, 
                            COMID, 
                            FromNode,
                            ToNode,
                            TOT_BASIN_AREA,
                            stream_length_m,
                            pred_logw_m,
                            pred_stream_area_m2_fill,
                            cum_stream_length_m,
                            cum_stream_area_m2,
                            cum_totco2g_day,
                            cum_totco2g_day_Tsurface_m2,
                            cum_totco2g_day_Tdrain_m2)

wlm_rsp_m1 <- dplyr::select(wlm_rsp_mg0,
                            COMID, 
                            FromNode,
                            ToNode,
                            TOT_BASIN_AREA,
                            stream_length_m,
                            pred_logw_m,
                            pred_stream_area_m2_fill,
                            cum_stream_length_m,
                            cum_stream_area_m2,
                            cum_totco2g_day,
                            cum_totco2g_day_Tsurface_m2,
                            cum_totco2g_day_Tdrain_m2)

yrb_rsp_m1$basin <- "Yakima"
wlm_rsp_m1$basin <- "Willamette"

scl_rsp <- rbind(yrb_rsp_m1,wlm_rsp_m1)

#Test plot

p <- ggplot(na.omit(scl_rsp),aes(TOT_BASIN_AREA,cum_totco2g_day_Tdrain_m2,color = basin))+
  geom_point(alpha = 0.35)+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
p

#Let's make sure that the log-transformed values, do correspond with the
#linear scale values (a previous inspection showed otherwise)

wlm_hbc_i$logDA_km2 = log(wlm_hbc_i$TotDASqKM,10)

#We will have to paste together columns from both datasets in different orders

wlm_rsp_1 <- cbind(wlm_rsp_o[,1:3],wlm_hbc_o[,c(1,5,7)])


# header info

#PENDING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!