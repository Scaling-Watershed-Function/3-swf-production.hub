###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# LANDSCAPE HETEROGENEITY ANALYSIS
###############################################################################

#By : Francisco Guerrero
#Data source: Datasets generated with "data_prep_scaling_script.R"

#Loading packages:

#Run for the first time only
#install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(ggplot2,dplyr,plot3D,plot3Drgl,readr,rgl,tidyverse,entropy,GGally,
                 stringr)
set.seed(2703)

#Data:




# Processing land cover data

#We calculate landscape heterogeneity as the entropy of the proportions of the 
#different landcover in both the local and cumulative drainage area to each 
#stream segment. Let's first create a working data set, which right now is just
#a copy of the original lnd:

dat <- as_tibble(dat_o)
lnd <- as_tibble(lnd_o)
my_colors <- c("#F564E3","#00BA38","#F8766D","#B79F00","#619CFF","#00BFC4")

#Let's shorten column names

lnd <- rename(lnd,
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

lnd <- lnd %>% group_by(COMID) %>% 
  mutate(tot_loc = round(urbn + frst + wtnd + agrc + shrb,2)) %>% 
  mutate(tot_acm = round(urbn_t + frst_t + wtnd_t + agrc_t + shrb_t,2)) 

summary(lnd$tot_loc)
summary(lnd$tot_acm)

# Although the mean for total land with a categorized use is between 91 -92% for 
# total watershed and local catchment respectively. We also observe values above
# 100%.

zeroes_loc <- nrow(filter(lnd,tot_loc==0)) #21 cases
zeroes_tot <- nrow(filter(lnd,tot_acm==0)) #0 cases


above_loc <- nrow(filter(lnd,tot_loc > 100.000)) #240 cases
above_tot <- nrow(filter(lnd,tot_acm > 100.000)) #106 cases

# Finally let's check how much of the total land cover is represented in the data

thrsh_loc <- nrow(filter(lnd,tot_loc < 60)) #289 cases
thrsh_tot <- nrow(filter(lnd,tot_acm < 60)) #161 cases

# We will remove these cases for the moment and check the data set

lnd_1 <- filter(lnd,tot_loc > 60)
lnd_2 <- filter(lnd_1,tot_acm > 60)



above_loc <- nrow(filter(lnd,tot_loc > 100.000)) #240 cases
above_tot <- nrow(filter(lnd,tot_acm > 100.000)) #106 cases

# Furthermore, we want to check that all the land use categories add up to at least
# 60% of the coverage

rep_loc <- nrow(filter(lnd,tot_loc <60)) #289 cases
rep_tot <- nrow(filter(lnd,tot_acm <60)) #161 cases


# At a local level, 0's may represent situations in which land cover types, other 
# than those included in the dataset, were dominant (like pastures). Above 100 percent
# values are probably the result of rounding up or down percentages. The same could 
# be said about the above 100 percent values at the watershed scale. In those cases, we
# need to adjust land use percentages to add up to 100 exactly. 

lnd <- lnd %>% 
  mutate(agrc = if_else(tot_loc>100.00,agrc*(100/tot_loc),agrc)) %>% 
  mutate(frst = if_else(tot_loc>100.00,frst*(100/tot_loc),frst)) %>% 
  mutate(shrb = if_else(tot_loc>100.00,shrb*(100/tot_loc),shrb)) %>% 
  mutate(urbn = if_else(tot_loc>100.00,urbn*(100/tot_loc),urbn)) %>% 
  mutate(wtnd = if_else(tot_loc>100.00,wtnd*(100/tot_loc),wtnd)) %>%  
  mutate(tot_loc = if_else(tot_loc>100.00,agrc+frst+shrb+urbn+wtnd,tot_loc)) %>%
  mutate(agrc_t = if_else(tot_acm>100.00,agrc_t*(100/tot_acm),agrc_t)) %>% 
  mutate(frst_t = if_else(tot_acm>100.00,frst_t*(100/tot_acm),frst_t)) %>% 
  mutate(shrb_t = if_else(tot_acm>100.00,shrb_t*(100/tot_acm),shrb_t)) %>% 
  mutate(urbn_t = if_else(tot_acm>100.00,urbn_t*(100/tot_acm),urbn_t)) %>% 
  mutate(wtnd_t = if_else(tot_acm>100.00,wtnd_t*(100/tot_acm),wtnd_t)) %>% 
  mutate(tot_acm = if_else(tot_acm>100.00,agrc_t+frst_t+shrb_t+urbn_t+wtnd_t,tot_acm))

# Let's double check the presence of above 100% values:

above_loc <- filter(lnd,tot_loc > 100.000) #27 cases
above_tot <- filter(lnd,tot_acm > 100.000) #17 cases

# Since these data set only contain the proportions of the largest categories, the 
# remaining percentage should correspond to "other" uses that are not explicitly 
# included. For the Yakima river basin this could be Hay and Pastures. We will assing
# the remaining percentages to "othr". 

lnd <- lnd %>% group_by(COMID) %>% 
  mutate(othr = if_else(100-tot_loc>0,100-tot_loc,0)) %>% 
  mutate(othr_t = if_else(100-tot_acm>0,100-tot_acm,0)) %>% 
  mutate(p_urbn = urbn/100) %>% 
  mutate(p_frst = frst/100) %>% 
  mutate(p_agrc = agrc/100) %>% 
  mutate(p_wtnd = wtnd/100) %>% 
  mutate(p_shrb = shrb/100) %>% 
  mutate(p_othr = othr/100) %>% 
  mutate(p_urbn_t = urbn_t/100) %>% 
  mutate(p_frst_t = frst_t/100) %>% 
  mutate(p_agrc_t = agrc_t/100) %>% 
  mutate(p_wtnd_t = wtnd_t/100) %>% 
  mutate(p_shrb_t = shrb_t/100) %>% 
  mutate(p_othr_t = othr_t/100) 


################################################################################
# Entropy analysis
################################################################################

# Let's start with a simple calculation of the Shannon's entropy as a proxy for 
# land use heterogeneity

# Making row-wise operations (https://dplyr.tidyverse.org/articles/rowwise.html)
lnd <- lnd %>% rowwise() %>% 
  mutate(hl = entropy(c(p_agrc,
                        p_frst,
                        p_othr,
                        p_shrb,
                        p_urbn,
                        p_wtnd),unit = "log")) %>% 
  mutate(hrl = hl/log(6)) %>% 
  mutate(ht = entropy(c(p_agrc_t,
                        p_frst_t,
                        p_othr_t,
                        p_shrb_t,
                        p_urbn_t,
                        p_wtnd_t),unit = "log")) %>% 
  mutate(hrt = ht/log(6)) 

p4 <- ggplot(lnd,aes(hrl,hrt))+
  geom_point()
p4


# Information content analysis

# Using Shannon's entropy calculations, we can identify which land use types 
# either locally or at the watershed scale contribute with most of the information
# about spatial variablity. 

# We are going to use resampling to estimate the uncertainty about the information
# contribution from the land use components.

# Let's start with local analysis

# Local dataset
lnd_el <- select(lnd,
                 p_agrc,
                 p_frst,
                 p_othr,
                 p_shrb,
                 p_urbn,
                 p_wtnd)

# Creating a matrix for results

ncols = 4
nrows = 6
ssz = 600
ic_loc <- matrix(1:nrows,nrows,ncols, 
                 dimnames = list(c("Agriculture","Forests","Pastures","Shrublands","Urban","Wetlands"),
                                 c("Yjn_l","Hn_l","Hmaxn_l", "In_l")))

ag_list <- list()
fr_list <- list()
pt_list <- list()
sr_list <- list()
ub_list <- list()
wt_list <- list()

# Number of iterations 
itn = 100

for(i in 1:itn){
  if (i == itn +1){
    break
  }
  loc_im <- lnd_el[sample(nrow(lnd_el),size=600,replace = FALSE),]
  loc_im <- as.matrix(loc_im)[,order(colnames(loc_im))]
  iml <- loc_im[,c(2:7)]/sum(loc_im[,c(2:7)])
  for(j in 1:ncol(iml)){
    yjn = sum(iml[,j])
    hn = entropy(iml[,j], unit = "log")
    hmaxn = log(nrow(lnd_el))
    ic_loc[j,1]=yjn
    ic_loc[j,2]=hn
    ic_loc[j,3]=hmaxn
    ic_loc[j,4]=yjn%*%(hmaxn-hn)
  }
  ag_list[[i]] <- ic_loc[1,]
  fr_list[[i]] <- ic_loc[2,]
  pt_list[[i]] <- ic_loc[3,]
  sr_list[[i]] <- ic_loc[4,]
  ub_list[[i]] <- ic_loc[5,]
  wt_list[[i]] <- ic_loc[6,]
  
}
ag_l = as_tibble(do.call("rbind",ag_list))
ag_l <- ag_l %>% mutate(use="Agriculture")
fr_l = as_tibble(do.call("rbind",fr_list))
fr_l <- fr_l %>% mutate(use = "Forests")
pt_l = as_tibble(do.call("rbind",pt_list))
pt_l <- pt_l %>% mutate(use = "Pastures")
sr_l = as_tibble(do.call("rbind",sr_list))
sr_l <- sr_l %>% mutate(use = "Shurblands")
ub_l = as_tibble(do.call("rbind",ub_list))
ub_l <- ub_l %>% mutate(use = "Urban")
wt_l = as_tibble(do.call("rbind",wt_list))
wt_l <- wt_l %>% mutate(use = "Wetlands")

local_im <- rbind(ag_l,fr_l,pt_l,sr_l,ub_l,wt_l)

# Watershed Scale

# Watershed dataset
lnd_et <- select(lnd,
                 p_agrc_t,
                 p_frst_t,
                 p_othr_t,
                 p_shrb_t,
                 p_urbn_t,
                 p_wtnd_t)

# Creating a matrix for results

ncols = 4
nrows = 6
ssz = 600
ic_tot <- matrix(1:nrows,nrows,ncols, 
                 dimnames = list(c("Agriculture","Forests","Pastures","Shrublands","Urban","Wetlands"),
                                 c("Yjn_l","Hn_l","Hmaxn_l", "In_l")))

agt_list <- list()
frt_list <- list()
ptt_list <- list()
srt_list <- list()
ubt_list <- list()
wtt_list <- list()

# Number of iterations 
itn = 1000

for(i in 1:itn){
  if (i == itn +1){
    break
  }
  tot_im <- lnd_et[sample(nrow(lnd_et),size=600,replace = FALSE),]
  tot_im <- as.matrix(tot_im)[,order(colnames(tot_im))]
  imt <- tot_im[,c(2:7)]/sum(tot_im[,c(2:7)])
  for(j in 1:ncol(imt)){
    yjn = sum(imt[,j])
    hn = entropy(imt[,j], unit = "log")
    hmaxn = log(nrow(lnd_et))
    ic_tot[j,1]=yjn
    ic_tot[j,2]=hn
    ic_tot[j,3]=hmaxn
    ic_tot[j,4]=yjn%*%(hmaxn-hn)
  }
  agt_list[[i]] <- ic_tot[1,]
  frt_list[[i]] <- ic_tot[2,]
  ptt_list[[i]] <- ic_tot[3,]
  srt_list[[i]] <- ic_tot[4,]
  ubt_list[[i]] <- ic_tot[5,]
  wtt_list[[i]] <- ic_tot[6,]
  
}
agt_l = as_tibble(do.call("rbind",agt_list))
agt_l <- agt_l %>% mutate(use="Agriculture")
frt_l = as_tibble(do.call("rbind",frt_list))
frt_l <- frt_l %>% mutate(use = "Forests")
ptt_l = as_tibble(do.call("rbind",ptt_list))
ptt_l <- ptt_l %>% mutate(use = "Pastures")
srt_l = as_tibble(do.call("rbind",srt_list))
srt_l <- srt_l %>% mutate(use = "Shurblands")
ubt_l = as_tibble(do.call("rbind",ubt_list))
ubt_l <- ubt_l %>% mutate(use = "Urban")
wtt_l = as_tibble(do.call("rbind",wtt_list))
wtt_l <- wtt_l %>% mutate(use = "Wetlands")

wshd_im <- rbind(agt_l,frt_l,ptt_l,srt_l,ubt_l,wtt_l)


p5 <- ggplot(local_im,aes(x = reorder(use,-In_l), y = In_l, fill = use, color = use))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = my_colors)+
  scale_fill_manual(values = my_colors)
p5


p6 <- ggplot(wshd_im,aes(x = reorder(use,-In_l), y = In_l, fill = use, color = use))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = my_colors)+
  scale_fill_manual(values = my_colors)+
  xlab("Land Use")+
  ylab("Contribution to landscape heterogeneity\n(as Shannon's entropy)")
p6

# Information content analysis suggest the following groups (combining information-rich
# land use categories with information-poor land use categories). This allows for keeping
# the discriminating power of the categories, but reducing the variability that contribute
# less to the patterns

# We will calculate the entropy over the reduced groups and merge this dataset with 
# biogeochemical data

lnd_s0 <- select(lnd,COMID,p_agrc,p_frst,p_othr,p_shrb,p_urbn,p_wtnd,
                 p_agrc_t,p_frst_t,p_othr_t,p_shrb_t,p_urbn_t,p_wtnd_t)

lnd_s0 <- lnd_s0 %>% group_by(COMID) %>% 
  mutate(p_ant = p_agrc + p_urbn + p_othr) %>% 
  mutate(p_frt = p_frst + p_wtnd) %>% 
  mutate(p_shb = p_shrb) %>% 
  mutate(p_ant_t = p_agrc_t + p_urbn_t + p_othr_t) %>% 
  mutate(p_frt_t = p_frst_t + p_wtnd_t) %>% 
  mutate(p_shb_t = p_shrb_t)

lnd_cvr <- select(lnd_s0,COMID,p_ant,p_frt,p_shb,p_ant_t,p_frt_t,p_shb_t)

lnd_cvr <- lnd_cvr %>% rowwise() %>% 
  mutate(hl = entropy(c(p_ant,
                        p_frt,
                        p_shb),unit = "log")) %>% 
  mutate(hrl = hl/log(3)) %>% 
  mutate(ht = entropy(c(p_ant_t,
                        p_frt_t,
                        p_shb_t),unit = "log")) %>% 
  mutate(hrt = ht/log(3)) 

# Let's remove the old land use columns from the biogeochemical dataset before we
# merge it with the "lnd_cvr" dataset

bgc <- select(dat,COMID,FromNode,ToNode,AreSqKM,cum_totco2g_m2_day,CAT_STREAM_LENGTH,CAT_STREAM_SLOPE,
              TOT_STREAM_LENGTH,TOT_STREAM_SLOPE,CAT_BASIN_AREA,TOT_BASIN_AREA,
              totco2g_m2_day_fill,logDA_km2,logw_m,length_m,D50_m,StreamOrde,pred_annual_DOC,pred_annual_DO,
              no3_conc_mg_l,logRT_total_hz_s,logq_hz_total_m_s,totco2_o2g_m2_day,
              totco2_ang_m2_day)
bgc <- rename(bgc,
              acm_resp = cum_totco2g_m2_day,
              reach_lgt = CAT_STREAM_LENGTH, 
              reach_slp = CAT_STREAM_SLOPE,
              river_lgt = TOT_STREAM_LENGTH,
              river_slp = TOT_STREAM_SLOPE,
              catch_area = CAT_BASIN_AREA,
              wshd_area = TOT_BASIN_AREA,
              reach_resp = totco2g_m2_day_fill,
              d50m = D50_m,
              order = StreamOrde,
              doc_annual = pred_annual_DOC,
              do_annual = pred_annual_DO,
              nitrates = no3_conc_mg_l,
              res_time = logRT_total_hz_s,
              hz_exchng = logq_hz_total_m_s,
              aer_resp = totco2_o2g_m2_day,
              anb_resp = totco2_ang_m2_day)

#Merging with lnd_s0

bgc_lnds0 <- as_tibble(unique(merge(bgc,lnd_cvr,by = "COMID")))

p <- ggplot(bgc_lnds0,aes(wshd_area,acm_resp,color=hrt))+
  geom_point(aes(alpha=hrt))+
  scale_x_log10()+
  scale_y_log10()
p


#Merging bgc with lnd_cvr

bgc_lnd0 <- merge(bgc,lnd_cvr,by = "COMID") # There are duplicates COMIDs in both 
#datasets, to filter those out:

bgc_lnd <- as_tibble(unique(bgc_lnd0))

# There are 1340 NA values in the following variables
# d50m
# order
# doc_annual
# do_annual
# nitrates
# res_time
# hz_exchng
# aer_resp
# anb_resp

# These rows might be dropped from the data set in following analysis
# provided that the main results remain unaffected.

write.csv(bgc_lnd,"230109_scaling_lnd_bgc.csv")