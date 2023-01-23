###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# DATA PREPARATION
###############################################################################

#By : Francisco Guerrero
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

#Loading packages:

#Run for the first time only
#install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(tidyr)
set.seed(2703)

#Data:

#header info

# PENDING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#values
lgc_o <- read.csv("assets/data/220725_yrb_resp_vars_legacy.csv",stringsAsFactors = TRUE)
spt_o <- read.csv("assets/data/230110_yrb_spatial_camp.csv", stringsAsFactors = TRUE)
rsp_o <- read.csv("assets/data/230116_yrb_respt_vars.csv", stringsAsFactors = TRUE)
hbc_o <- read.csv("assets/data/230117_yrb_hbgc_vars.csv", stringsAsFactors = TRUE)
lnd_o <- read.csv("assets/data/230117_yrb_cmid_land_2011.csv",stringsAsFactors=TRUE)

#lnd_o is the land use data used by Son et al., 2022a and 2022b to model inputs of 
#DOC, NO3, and OD to the YR (Although the map in the figures uses NLDC-2016).

# Merging data sets for analysis 

# We start by extracting the desired variables from the legacy data set including the
# unique id's (comid) as well as the local respiration rates (rsp_loc) and the invalid
# cumulative respiration rates. This last one just to compare how much change to expect in 
# the figures produced with the updated data sets. 

lgc_m <- dplyr::select(lgc_o,COMID,
                       totco2g_m2_day_fill,
                       cum_totco2g_m2_day) %>% 
  rename(comid = COMID,
         rsp_loc = totco2g_m2_day_fill,
         rsp_acm_lg = cum_totco2g_m2_day)

glimpse(lgc_m)

#Merging with respt_vars

# First, let's prepare the dataset by renaming variables:

# resp_o variables
rsp_m <- rename(rsp_o,
                comid = COMID,
                rch_lgt = CAT_STREAM_LENGTH, 
                rch_slp = CAT_STREAM_SLOPE,
                rvr_lgt = TOT_STREAM_LENGTH,
                rvr_slp = TOT_STREAM_SLOPE,
                cat_are = CAT_BASIN_AREA,
                wsd_are = TOT_BASIN_AREA,
                rch_lgtm = stream_length_m,
                rch_are = pred_stream_area_m2_fill,
                clgt_rch = cum_stream_length_m,
                csar_rch = cum_stream_area_m2,
                crsp_mss = cum_totco2g_day,
                crsp_sta = cum_totco2g_day_Tsurface_m2,
                crsp_wsa = cum_totco2g_day_Tdrain_m2)
rsp_m$rch_wdt <- 10^(rsp_o$pred_logw_m) # adding a column with stream width in 
# m instead of the logarithm. 

# Now, let's remove the logarithmic value
rsp_m <- dplyr::select(rsp_m,-pred_logw_m)

# Now we proceed to merge:
rsp_m1 <- as_tibble(unique(merge(lgc_m,rsp_m,by = "comid")))


# Let's compare the original values of cumulative respiration per unit area of 
# stream segment (used in the AGU poster) with the corrected values:
p <- ggplot(rsp_m1,aes(rsp_acm_lg, crsp_sta))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()
p

# Data show that the previous calculation was underestimating most of the values
# of cumulative respiration. What invalidated the previous calculation was that 
# instead of accumulating the total mass of CO2 first, and then normalizing it by
# the stream area, the rates per unit of stream area were added cumulatively along 
# the stream channel. This cumulative rates do not have a clear physical interpretation.

# Let's now remove the column with invalid cumulative rates to avoid further confusions.

rsp_m1 <- dplyr::select(rsp_m1,-rsp_acm_lg)

# The next step is to merge rsp_dat with the data set containing hydro_biogeochemical
# variables. First, let's modify column names in the hbc data set

hbc_m <- dplyr::select(hbc_o,
                       COMID,
                       D50_m,
                       StreamOrde,
                       logwbkf_m,
                       pred_annual_DOC,
                       pred_annual_DO,
                       no3_conc_mg_l,
                       logRT_total_hz_s,#needs to be back transformed
                       logq_hz_total_m_s,#needs to be back transformed
                       totco2_o2g_m2_day,
                       totco2_ang_m2_day)

hbc_m <- rename(hbc_m,
         comid = COMID,  
         d50m = D50_m,
         order = StreamOrde,
         bkf_wdt = logwbkf_m,
         doc_annual = pred_annual_DOC,
         do_annual = pred_annual_DO,
         nitrates = no3_conc_mg_l,
         res_time = logRT_total_hz_s,
         hz_exchng = logq_hz_total_m_s,
         aer_resp = totco2_o2g_m2_day,
         anb_resp = totco2_ang_m2_day)

# Let's back transform the values for residence time and hyporheic exchange which are
# expressed in logarithms

hbc_m$res_time <- 10^(hbc_m$res_time)
hbc_m$hz_exchng <- 10^(hbc_m$hz_exchng)


# Let's now merge the respiration data with the hydro-biogeochemical variables:

rsp_dat <- as_tibble(unique(merge(rsp_m1,hbc_m,by = "comid")))

# Finally, let's merge the predicted data for the spatial study with all the 
# contextual variables in rsp_dat

spt_dat0 <- dplyr::select(spt_o,-totco2g_m2_day)
  
spt_dat0 <-  rename(spt_dat0,
                    comid = COMID,
                    prd_tot = pred_total,
                    rsp_locs = totco2g_m2_day_fill,
                    site_id = site_ID)
                     
spt_dat <- as_tibble(unique(merge(spt_dat0,rsp_dat,by="comid")))

# Let's have a sneak peak on the local scaling between watershed area and 
# local respiration rates

p <- ggplot(spt_dat,aes(wsd_are,rsp_locs))+
  geom_smooth(span = 0.9)+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_vline(xintercept = 325, linetype = "dashed")
p

# Let's now save these data sets for further analysis.

write.csv(rsp_dat,"assets/data/230120_yrb_rsp_dat.csv")
write.csv(spt_dat,"assets/data/230120_yrb_spt_dat.csv")

################################################################################
################################################################################





