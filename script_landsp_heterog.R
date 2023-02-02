###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# LANDSCAPE HETEROGENEITY ANALYSIS
###############################################################################

#By : Francisco Guerrero
#Data source: Data sets generated with "script_data_preparation.R"

#Loading packages:

#Run for the first time only
#install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(tidyverse,#(includes ggplot2, readr, dplyr, tidyr, and more...)
                 entropy)

set.seed(2703)

#Data:
dat_o <- read.csv("assets/data/raw/230120_yrb_rsp_dat.csv",stringsAsFactors = TRUE)
lnd_o <- read.csv("assets/data/raw/230117_yrb_cmid_land_2011.csv",stringsAsFactors=TRUE)

# Processing land cover data

#We calculate landscape heterogeneity as the entropy of the proportions of the 
#different land covers in both the local and cumulative drainage area to each 
#stream segment. Let's first create a working data set, which right now is just
#a copy of the original lnd:

dat <- as_tibble(dat_o)
lnd <- as_tibble(lnd_o)
my_colors <- c("#F564E3","#00BA38","#B79F00","#F8766D","#619CFF")

#Let's shorten column names

lnd <- rename(lnd,
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

lnd <- lnd %>% group_by(comid) %>% 
  mutate(tot_loc = round(urbn + frst + wtnd + agrc + shrb,2)) %>% 
  mutate(tot_acm = round(urbn_t + frst_t + wtnd_t + agrc_t + shrb_t,2)) 

summary(lnd$tot_loc)
summary(lnd$tot_acm)

# Although the median for total land with a categorized use is between 95 -98% for 
# total watershed and local catchment respectively. We also observe some problematic values
# corresponding to zeroes, and minimum values below 2%. Let's plot the distribution of 
# the data to have a better look: 

lnd %>% select(comid,tot_loc,tot_acm) %>% 
  gather(key = "area.type", value = "cover", c(2:3),factor_key = TRUE) %>% 
  ggplot(aes(area.type,cover,color = area.type))+
  geom_hline(yintercept = 75)+
  geom_boxplot()

# We have a sizable number of data points below 75% of the area with a categorized use. For the 
# remaining of this analysis we would have to omit these points. But we will have to replace the
# data set for one that includes all the land cover types. 

lndf <- filter(lnd,tot_loc > 74.99 & tot_acm >74.99) # We have dropped 806 data points (~12% of the original data set)

#Let's check the new distribution of land with categorized uses: 

lndf %>% select(comid,tot_loc,tot_acm) %>% 
  gather(key = "area.type", value = "cover", c(2:3),factor_key = TRUE) %>% 
  ggplot(aes(area.type,cover,color = area.type))+
  geom_hline(yintercept = 75)+
  geom_boxplot()

# For the entropy analysis is better to make sure before hand that all the values add to 
# a 100%, so we will need to recalculate percentage cover for total cover both below 
# or above 100%

lndf <- lndf %>% 
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


################################################################################
# Entropy analysis
################################################################################

# Let's start with a simple calculation of the Shannon's entropy as a proxy for 
# land use heterogeneity

# Making row-wise operations (https://dplyr.tidyverse.org/articles/rowwise.html)
lndf <- lndf %>% rowwise() %>% 
  mutate(hl = entropy(c(agrc,
                        frst,
                        shrb,
                        urbn,
                        wtnd),unit = "log")) %>% 
  mutate(hrl = hl/log(5)) %>% 
  mutate(ht = entropy(c(agrc_t,
                        frst_t,
                        shrb_t,
                        urbn_t,
                        wtnd_t),unit = "log")) %>% 
  mutate(hrt = ht/log(5)) 

p4 <- ggplot(lndf,aes(hrl,hrt))+
  geom_point()
p4

# Information content analysis

# Using Shannon's entropy calculations, we can identify which land use types 
# either locally or at the watershed scale contribute with most of the information
# about spatial variability. 

# We are going to use re sampling to estimate the uncertainty about the information
# contribution from the land use components.

# Let's start with local analysis

# Local data set
lnd_el <- select(lndf,
                 comid,
                 agrc,
                 frst,
                 shrb,
                 urbn,
                 wtnd)

# Creating a matrix for results

ncols = 4
nrows = 5
ssz = 600
ic_loc <- matrix(1:nrows,nrows,ncols, 
                 dimnames = list(c("Agriculture","Forests","Shrublands","Urban","Wetlands"),
                                 c("Yjn_l","Hn_l","Hmaxn_l", "In_l")))

ag_list <- list()
fr_list <- list()
sr_list <- list()
ub_list <- list()
wt_list <- list()

# Number of iterations 
itn = 1000

for(i in 1:itn){
  if (i == itn +1){
    break
  }
  loc_im <- lnd_el[sample(nrow(lnd_el),size=ssz,replace = FALSE),]
  iml <- loc_im[,c(2:ncol(loc_im))]/sum(loc_im[,c(2:ncol(loc_im))])
  for(j in 1:ncol(iml)){
    yjn = sum(iml[,j])
    hn = entropy(iml[,j], unit = "log")
    hmaxn = log(nrow(iml))
    ic_loc[j,1]=yjn
    ic_loc[j,2]=hn
    ic_loc[j,3]=hmaxn
    ic_loc[j,4]=yjn%*%(hmaxn-hn)
  }
  ag_list[[i]] <- ic_loc[1,]
  fr_list[[i]] <- ic_loc[2,]
  sr_list[[i]] <- ic_loc[3,]
  ub_list[[i]] <- ic_loc[4,]
  wt_list[[i]] <- ic_loc[5,]
  
}
ag_l = as_tibble(do.call("rbind",ag_list))
ag_l <- ag_l %>% mutate(use="Agriculture")
fr_l = as_tibble(do.call("rbind",fr_list))
fr_l <- fr_l %>% mutate(use = "Forests")
sr_l = as_tibble(do.call("rbind",sr_list))
sr_l <- sr_l %>% mutate(use = "Shurblands")
ub_l = as_tibble(do.call("rbind",ub_list))
ub_l <- ub_l %>% mutate(use = "Urban")
wt_l = as_tibble(do.call("rbind",wt_list))
wt_l <- wt_l %>% mutate(use = "Wetlands")

local_im <- rbind(ag_l,fr_l,sr_l,ub_l,wt_l)

# Let's check the results with a box-plot 

p5 <- ggplot(local_im,aes(x = reorder(use,-In_l), y = In_l, fill = use, color = use))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = my_colors)+
  scale_fill_manual(values = my_colors)+
  labs(x="Land use",y ="Information Contribution")+
  theme(legend.position = "none")
p5

# Watershed Scale

# Watershed data set
lnd_et <- select(lndf,
                 comid,
                 agrc_t,
                 frst_t,
                 shrb_t,
                 urbn_t,
                 wtnd_t)

# Creating a matrix for results

ncols = 4
nrows = 5
ssz = 600
ic_tot <- matrix(1:nrows,nrows,ncols, 
                 dimnames = list(c("Agriculture","Forests","Shrublands","Urban","Wetlands"),
                                 c("Yjn_l","Hn_l","Hmaxn_l", "In_l")))

agt_list <- list()
frt_list <- list()
srt_list <- list()
ubt_list <- list()
wtt_list <- list()

# Number of iterations 
itn = 1000

for(i in 1:itn){
  if (i == itn +1){
    break
  }
  tot_im <- lnd_et[sample(nrow(lnd_et),size=ssz,replace = FALSE),]
  imt <- tot_im[,c(2:ncol(tot_im))]/sum(tot_im[,c(2:ncol(tot_im))])
  for(j in 1:ncol(imt)){
    yjn = sum(imt[,j])
    hn = entropy(imt[,j], unit = "log")
    hmaxn = log(nrow(imt))
    ic_tot[j,1]=yjn
    ic_tot[j,2]=hn
    ic_tot[j,3]=hmaxn
    ic_tot[j,4]=yjn%*%(hmaxn-hn)
  }
  agt_list[[i]] <- ic_tot[1,]
  frt_list[[i]] <- ic_tot[2,]
  srt_list[[i]] <- ic_tot[3,]
  ubt_list[[i]] <- ic_tot[4,]
  wtt_list[[i]] <- ic_tot[5,]
  
}
agt_l = as_tibble(do.call("rbind",agt_list))
agt_l <- agt_l %>% mutate(use="Agriculture")
frt_l = as_tibble(do.call("rbind",frt_list))
frt_l <- frt_l %>% mutate(use = "Forests")
srt_l = as_tibble(do.call("rbind",srt_list))
srt_l <- srt_l %>% mutate(use = "Shurblands")
ubt_l = as_tibble(do.call("rbind",ubt_list))
ubt_l <- ubt_l %>% mutate(use = "Urban")
wtt_l = as_tibble(do.call("rbind",wtt_list))
wtt_l <- wtt_l %>% mutate(use = "Wetlands")

wshd_im <- rbind(agt_l,frt_l,srt_l,ubt_l,wtt_l)


p6 <- ggplot(wshd_im,aes(x = reorder(use,-In_l), y = In_l, fill = use, color = use))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = my_colors)+
  scale_fill_manual(values = my_colors)+
  xlab("Land Use")+
  ylab("Contribution to landscape heterogeneity\n(as Shannon's entropy)")+
  theme(legend.position = "none")
p6

# Information content analysis suggest the percentage cover of forests, agriculture, and shrublands,
# contribute the most to the landscape heterogeneity (without accounting for spatial correlations).
# We could use the composition law (see Jaynes, 1957 -Statistical mechanics and information theory),
# to group the less informative categories (urban and wetlands) with their spatial correlates, 
# (agriculture and forests, respectively)

# We will calculate the entropy over the reduced groups and merge this dataset with 
# biogeochemical data

lnd_m0 <- select(lndf,
                 comid,
                 agrc,
                 frst,
                 shrb,
                 urbn,
                 wtnd,
                 agrc_t,
                 frst_t,
                 shrb_t,
                 urbn_t,
                 wtnd_t)

lnd_m0 <- lnd_m0 %>% group_by(comid) %>% 
  mutate(p_ant = agrc + urbn) %>% 
  mutate(p_frt = frst + wtnd) %>% 
  mutate(p_shb = shrb) %>% 
  mutate(p_ant_t = agrc_t + urbn_t) %>% 
  mutate(p_frt_t = frst_t + wtnd_t) %>% 
  mutate(p_shb_t = shrb_t)


lnd_m <- lnd_m0 %>% rowwise() %>% 
  mutate(hl = entropy(c(p_ant,
                        p_frt,
                        p_shb),unit = "log")) %>% 
  mutate(hrl = hl/log(3)) %>% 
  mutate(ht = entropy(c(p_ant_t,
                        p_frt_t,
                        p_shb_t),unit = "log")) %>% 
  mutate(hrt = ht/log(3)) 

#Merging landscape heterogeneity and biogeochemical data

bgc_lnd0 <- merge(lnd_m, dat_o, by = "comid") # There are duplicates comid's in both 
#datasets, to filter those out:

bgc_lnd <- bgc_lnd0 # additional 30 points were dropped, potentially those mismatching
#across the data sets

# Saving the working data set for exploratory analysis and plots
write.csv(bgc_lnd,"assets/data/230123_scaling_lnd_bgc.csv")

######################################################################################################
######################################################################################################