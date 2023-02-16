###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima and Willamette River 
# Basins
# DATA PREPARATION
###############################################################################
# RESPIRATION DATA
###############################################################################

#By : Francisco Guerrero
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

#Loading packages:

# Run for the first time only
#install.packages("librarian")

#To run this code in macOS it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(tidyverse, GGally) #it searches for the packages in your library,
#if you don't have them installed, it will proceed with the installation and it 
#will bring them into your work space (like library will commonly do).
set.seed(2703)

# Data:

# Commit test

#values

#Yakima River Basin (yrb)
yrb_lgc_o <- read.csv("assets/data/raw/220725_yrb_resp_vars_legacy.csv",
                      stringsAsFactors = TRUE) #Dataset used for the AGU poster with uncorrected cumulative values
#i.e. respiration rates were not normalized by watershed area)
yrb_spt_o <- read.csv("assets/data/raw/230110_yrb_spatial_camp.csv", 
                      stringsAsFactors = TRUE) #Predicted respiration rates at field locations
yrb_rsp_o <- read.csv("assets/data/raw/230116_yrb_respt_vars.csv", 
                      stringsAsFactors = TRUE) #Updated dataset with corrected cumulative values
yrb_hbc_o <- read.csv("assets/data/raw/230117_yrb_hbgc_vars.csv", 
                      stringsAsFactors = TRUE) #Hydro-biogeochemical variables (including residence
#time and hyporheic exchange)

#Willamette River Basin (wlm)

#Unlike the yrb datasets, the wlm datasets for both respiration (rsp) and hydro-biogeochemical 
#variables (hbc) are split into two different datasets. To keep the naming across datasets
#consistent, I'm using "_o" for original and "_i" as a sequential index for other raw datasets
#that would be merged under the labels "rsp" or "hbc".

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

#Let's pull the needed columns from each dataset (adding "m" at the end for "merging" within
#the same basin)

wlm_hbc_om <- dplyr::select(wlm_hbc_o,COMID,CAT_STREAM_SLOPE,TOT_STREAM_SLOPE,TOT_BASIN_AREA)
wlm_hbc_im <- dplyr::select(wlm_hbc_i,COMID,TotDASqKM,length_m)
wlm_rsp_om <- dplyr::select(wlm_rsp_o,COMID,FromNode,ToNode,cum_totco2g_day,cum_stream_area_m2,
                            cum_stream_length_m, cum_totco2g_day_Tdrain_m2, cum_totco2g_day_Tsurface_m2)
wlm_rsp_im <- dplyr::select(wlm_rsp_i,COMID,totco2g_day_fill,pred_stream_area_m2_fill,stream_length_m)

#Since merge only takes two inputs at a time, we have to merge our data sets sequentially
wlm_hbc_mg <- unique(merge(wlm_hbc_om,wlm_hbc_im,by = "COMID"))
wlm_rsp_mg <- unique(merge(wlm_rsp_om,wlm_rsp_im,by = "COMID"))
wlm_rsp_mg0 <- unique(merge(wlm_rsp_mg,wlm_hbc_mg,by="COMID"))
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

#Merging with additional hydro-biogeochemical data for both watersheds

yrb_hbc_m1 <- select(yrb_hbc_o,
                     COMID,
                     StreamOrde,
                     logQ_m3_div_s,
                     logwbkf_m,
                     logd_m,
                     logdbkf_m,
                     D50_m,
                     pred_annual_DOC,
                     pred_annual_DO,
                     no3_conc_mg_l,
                     logRT_total_hz_s,
                     logq_hz_total_m_s)

wlm_hbc_m1 <- select(wlm_hbc_i,
                     COMID,
                     StreamOrde,
                     logQ_m3_div_s,
                     logwbkf_m,
                     logd_m,
                     logdbkf_m,
                     D50_m,
                     pred_annual_DOC,
                     pred_annual_DO,
                     no3_conc_mg_l,
                     logRT_total_hz_s,
                     logq_hz_total_m_s)

yrb_rsp_m2 <- unique(merge(yrb_rsp_m1,yrb_hbc_m1,by = "COMID"))
wlm_rsp_m2 <- unique(merge(wlm_rsp_m1,wlm_hbc_m1,by = "COMID"))

yrb_wlm_rsp <- rbind(yrb_rsp_m2,wlm_rsp_m2)

#Test plot

p0 <- ggplot(yrb_wlm_rsp,
             aes(TOT_BASIN_AREA,
                 cum_totco2g_day_Tdrain_m2))+ 
  geom_point(alpha = 0.35, color = "gray")+
  geom_point(data = na.omit(yrb_wlm_rsp),# Excluding RF-generated data  (in gray color)
             aes(TOT_BASIN_AREA,
                 cum_totco2g_day_Tdrain_m2,
                 color = basin),
             alpha = 0.35)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(slope = 1, intercept = -2.5)+
  # geom_abline(slope = 0.5, intercept =-4.25,linetype = "dashed")+
  # geom_abline(slope = 1, intercept =-3,linetype = "solid")+
  # geom_abline(slope = 1.5, intercept =-1.75,linetype = "dashed")+
  facet_wrap(~basin, 
             ncol = 2)+
  ggtitle("Total Cumulative Respiration per Day per Square Meter of Watershed Area")
p0

#We observe zero values for cumulative respiration in both datasets for at least a couple
#orders of magnitude (~0.1 - 1 km2) of total watershed area. Let's take a look at those:

nrow(filter(yrb_wlm_rsp,cum_totco2g_day_Tdrain_m2==0))

# 253 values

#Let's use our test plot again, this time after removing zero values: 

p1 <- ggplot(filter(yrb_wlm_rsp, cum_totco2g_day_Tdrain_m2 > 0),
            aes(TOT_BASIN_AREA,cum_totco2g_day_Tdrain_m2,color = basin))+
  geom_point(alpha = 0.35)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(slope = 0.5, intercept =-4.25,linetype = "dashed")+
  geom_abline(slope = 1, intercept =-3,linetype = "solid")+
  geom_abline(slope = 1.5, intercept =-1.75,linetype = "dashed")+
  facet_wrap(~basin, ncol = 2)
p1

#We observe that cumulative watershed function deviates from a scaling relation-
#ship at larger watersheds. This needs to be explored further.


#Let's also take a look at the scaling relationships between stream length and 
#surface area, with watershed area

yrb_wlm_rsp %>% select(COMID,
                   basin,
                   TOT_BASIN_AREA,
                   cum_stream_area_m2,
                   cum_stream_length_m) %>% 
  gather(key = "variable",value = "value",c(4:5),factor_key = TRUE) %>% 
  ggplot(aes(TOT_BASIN_AREA,value,color=variable, fill = variable))+
  # geom_smooth(method = "lm")+
  # geom_hex()+
  geom_point(alpha = 0.35)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(slope = 1, intercept = 2.95, linetype = "dashed")+
  facet_wrap(variable~basin,ncol = 2)
  
#In both the Willamette and Yakima River basins, the cumulative stream length scales
#linearly with watershed area, while cumulative stream area scales superlinearly (scaling
#exponent >1. Reference dashed line has a scaling exponent = 1).

#Lastly, let's take a look at potential correlations among physical variables. This could 
#illuminate ways in which we could approach gap filling (if needed)

yrb_wlm_rsp %>% na.omit(yrb_wlm_rsp) %>% select(COMID,
                   basin,
                   TOT_BASIN_AREA,
                   pred_stream_area_m2_fill,
                   StreamOrde,
                   logQ_m3_div_s,
                   D50_m,
                   logRT_total_hz_s,
                   logq_hz_total_m_s) %>%
  mutate(logwsd_are = log(TOT_BASIN_AREA,10),
         logstm_are = log(pred_stream_area_m2_fill,10),
         logd50_m = log(D50_m,10),
         logq_m3 = logQ_m3_div_s,10) %>% 
  ggpairs(columns = 8:12,
          aes(color = basin ,alpha = 0.05))

###############################################################################
#Saving this data set as a "processed" file

write.csv(yrb_wlm_rsp,"assets/data/processed/230202_yrb_wlm_resp_dat.csv",row.names = FALSE)
  

################################################################################
# LAND USE DATA
################################################################################

yrb_lnd0 <- read.csv("assets/data/raw/230117_yrb_cmid_land_2011.csv",stringsAsFactors=TRUE)
wlm_lnd0 <- wlm_hbc_o %>% select(COMID,
                                 urban,
                                 forest,
                                 wetland,
                                 agrc,
                                 shrub,
                                 turban,
                                 tforest,
                                 twetland,
                                 tagrc,
                                 tshrub) # double check with Kyongho if 2011
yrb_lnd0$basin <- "Yakima"
wlm_lnd0$basin <- "Willamette"

yrb_wlm_lnd <- rbind(yrb_lnd0,wlm_lnd0)

#Let's rename some columns

yrb_wlm_lnd <- rename(yrb_wlm_lnd,
              comid = COMID,
              urbn = urban,
              frst = forest,
              wtnd = wetland, 
              shrb = shrub,
              urbn_t = turban,
              frst_t = tforest,
              wtnd_t = twetland,
              agrc_t = tagrc,
              shrb_t = tshrub)

# This land use data set only contains percentage cover for the main land uses. So,
# not all the land uses add up to 100%. Let's double check for these cases, as well as
# other potential anomalies

yrb_wlm_lnd <- yrb_wlm_lnd %>% group_by(comid) %>% 
  mutate(tot_loc = round(urbn + frst + wtnd + agrc + shrb,2)) %>% 
  mutate(tot_acm = round(urbn_t + frst_t + wtnd_t + agrc_t + shrb_t,2)) 

summary(yrb_wlm_lnd$tot_loc)
summary(yrb_wlm_lnd$tot_acm)

# Although the median for total land with a categorized use is between 95 -98% for 
# total watershed and local catchment respectively. We also observe some problematic values
# corresponding to zeroes, and minimum values below 2%. Let's plot the distribution of 
# the data to have a better look (I'm using a threshold of 75% of total landcover): 

yrb_wlm_lnd %>% select(comid,basin,tot_loc,tot_acm) %>% 
  gather(key = "area.type", value = "cover", c(3:4),factor_key = TRUE) %>% 
  ggplot(aes(area.type,cover,color = area.type))+
  geom_hline(yintercept = 75)+
  geom_boxplot()+
  facet_wrap(~basin)

# We have a sizable number of data points below 75% of the area with a categorized use. For the 
# remaining of this analysis we would have to omit these points. But we will have to replace the
# data set for one that includes all the land cover types. 

yrb_wlm_lndf <- filter(yrb_wlm_lnd,tot_loc > 74.99 & tot_acm >74.99) # We have dropped 1682 data points (~11% of 
#the original data set)

#Let's check the new distribution of land with categorized uses: 

yrb_wlm_lndf %>% select(comid,basin,tot_loc,tot_acm) %>% 
  gather(key = "area.type", value = "cover", c(3:4),factor_key = TRUE) %>% 
  ggplot(aes(area.type,cover,color = area.type))+
  geom_hline(yintercept = 75)+
  geom_boxplot()+
  facet_wrap(~basin)

# For the the landscape heterogeneity analysis is better to make sure that all the values add to 
# a 100%, so we will need to recalculate percentage cover for total cover both below 
# or above 100%

yrb_wlm_lndf <- yrb_wlm_lndf %>% 
  mutate(agrc = if_else(tot_loc!=100.00,agrc*(100/tot_loc),agrc)) %>% 
  mutate(frst = if_else(tot_loc!=100.00,frst*(100/tot_loc),frst)) %>% 
  mutate(shrb = if_else(tot_loc!=100.00,shrb*(100/tot_loc),shrb)) %>% 
  mutate(urbn = if_else(tot_loc!=100.00,urbn*(100/tot_loc),urbn)) %>% 
  mutate(wtnd = if_else(tot_loc!=100.00,wtnd*(100/tot_loc),wtnd)) %>%  
  mutate(tot_loc = if_else(tot_loc!=100.00,agrc+frst+shrb+urbn+wtnd,tot_loc)) %>%
  mutate(agrc_t = if_else(tot_acm!=100.00,agrc_t*(100/tot_acm),agrc_t)) %>% 
  mutate(frst_t = if_else(tot_acm!=100.00,frst_t*(100/tot_acm),frst_t)) %>% 
  mutate(shrb_t = if_else(tot_acm!=100.00,shrb_t*(100/tot_acm),shrb_t)) %>% 
  mutate(urbn_t = if_else(tot_acm!=100.00,urbn_t*(100/tot_acm),urbn_t)) %>% 
  mutate(wtnd_t = if_else(tot_acm!=100.00,wtnd_t*(100/tot_acm),wtnd_t)) %>% 
  mutate(tot_acm = if_else(tot_acm!=100.00,agrc_t+frst_t+shrb_t+urbn_t+wtnd_t,tot_acm))

# By checking again with summary(lndf), we verify that all land cover uses add to 100%
# At least for areas that had above 75% of land categorized by the uses selected by 
# Son et al., 2022.

summary(yrb_wlm_lndf)

###############################################################################
#Saving this data set as a "processed" file

write.csv(yrb_wlm_lndf,"assets/data/processed/230202_yrb_wlm_land_filtered_dat.csv",row.names = FALSE)

################################################################################
# HEADER INFO
################################################################################

#PENDING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






