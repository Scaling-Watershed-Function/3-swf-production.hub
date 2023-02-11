###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# LANDSCAPE HETEROGENEITY ANALYSIS
###############################################################################

#By : Francisco Guerrero
#Data source: Data sets generated with "script_data_prep_wlm_yrb.R"

#Loading packages:

#Run for the first time only
#install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(tidyverse,#(includes ggplot2, readr, dplyr, tidyr, and more...)
                 entropy, usethis)

set.seed(2703)

# Tentative color palette for land uses (to be changed using colors corresponding
# to the national database)

my_colors <- c("#F564E3","#00BA38","#B79F00","#F8766D","#619CFF")

#Data:
dat_o <- read.csv("assets/data/processed/230202_yrb_wlm_resp_dat.csv",stringsAsFactors = TRUE)
lnd_o <- read.csv("assets/data/processed/230202_yrb_wlm_land_filtered_dat.csv",stringsAsFactors=TRUE)

#Let's first create a working data set, which right now is just
#a copy of the original lnd:

dat <- as_tibble(dat_o)
lnd <- as_tibble(lnd_o)

################################################################################
# Entropy analysis
################################################################################

# Let's start with a simple calculation of the Shannon's entropy as a proxy for 
# land use heterogeneity

# Making row-wise operations (https://dplyr.tidyverse.org/articles/rowwise.html)
lnd <- lnd %>% rowwise() %>% 
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

# Let's compare basins in terms of their landscape heterogeneity:

lnd %>% select(basin,hrl,hrt) %>% 
  rename(Basin = basin,
         Local = hrl,
         Total = hrt) %>% 
  gather(key = "Extent",value = "Entropy",c(2:3)) %>% 
  ggplot(aes(Basin,Entropy,color = Basin))+
  geom_boxplot(alpha = 0.5)+
  ylab("Relative Shannon's Entropy")+
  facet_wrap(~Extent,ncol = 2)
  
# Despite the striking differences in landscape configuration, we do not observe
# a commensurate difference in landscape entropies.

# Information content analysis

# Using Shannon's entropy calculations, we can identify which land use types 
# either locally or at the watershed scale contribute with most of the information
# about spatial variability. 

# We are going to use re sampling to estimate the uncertainty about the information
# contribution from the land use components.

# Let's start with local analysis

# Willamette River Basin

# Local data set
lnd_el <- filter(lnd,basin == "Willamette") %>% #Notice the specification of the watershed of interest
  select(agrc,
         frst,
         shrb,
         urbn,
         wtnd)

# Creating a matrix for results

ncols = 4
nrows = 5
ssz = 1000
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
  iml <- loc_im[,c(1:ncol(loc_im))]/sum(loc_im[,c(1:ncol(loc_im))])
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

wlm_local_im <- rbind(ag_l,fr_l,sr_l,ub_l,wt_l) # Notice the "wlm" prefix in the name
# of the output file

# Let's check the results with a box-plot 

p5 <- ggplot(wlm_local_im,aes(x = reorder(use,-In_l), y = In_l, fill = use, color = use))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = my_colors)+
  scale_fill_manual(values = my_colors)+
  labs(x="Land use",y ="Information Contribution")+
  theme(legend.position = "none")+
  ggtitle("Willamette River Basin (Local Drainage Area)")
p5

# Yakima River Basin

# Local data set
lnd_el <- filter(lnd,basin == "Yakima") %>% #Notice the specification of the watershed of interest
  select(agrc,
         frst,
         shrb,
         urbn,
         wtnd)

# Creating a matrix for results

ncols = 4
nrows = 5
ssz = 1000
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
  iml <- loc_im[,c(1:ncol(loc_im))]/sum(loc_im[,c(1:ncol(loc_im))])
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

yrb_local_im <- rbind(ag_l,fr_l,sr_l,ub_l,wt_l) # Notice the "yrb" prefix in the name
# of the output file

# Let's check the results with a box-plot 

p5 <- ggplot(yrb_local_im,aes(x = reorder(use,-In_l), y = In_l, fill = use, color = use))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = my_colors)+
  scale_fill_manual(values = my_colors)+
  labs(x="Land use",y ="Information Contribution")+
  theme(legend.position = "none")+
  ggtitle("Yakima River Basin (Local Drainage Area)")
p5

# Overall (Local Information Contribution)

# Local data set
lnd_el <- lnd %>% #Notice that no watershed is specified in the analysis
  select(agrc,
         frst,
         shrb,
         urbn,
         wtnd)

# Creating a matrix for results

ncols = 4
nrows = 5
ssz = 1400
ic_loc <- matrix(1:nrows,nrows,ncols, 
                 dimnames = list(c("Agriculture","Forests","Shrublands","Urban","Wetlands"),
                                 c("Yjn_l","Hn_l","Hmaxn_l", "In_l")))

ag_list <- list()
fr_list <- list()
sr_list <- list()
ub_list <- list()
wt_list <- list()

# Number of iterations 
itn = 5000

for(i in 1:itn){
  if (i == itn +1){
    break
  }
  loc_im <- lnd_el[sample(nrow(lnd_el),size=ssz,replace = FALSE),]
  iml <- loc_im[,c(1:ncol(loc_im))]/sum(loc_im[,c(1:ncol(loc_im))])
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

ywb_local_im <- rbind(ag_l,fr_l,sr_l,ub_l,wt_l) # Notice the "ywb" prefix in the name
# of the output file

# Let's check the results with a box-plot 

p5 <- ggplot(ywb_local_im,aes(x = reorder(use,-In_l), y = In_l, fill = use, color = use))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = my_colors)+
  scale_fill_manual(values = my_colors)+
  labs(x="Land use",y ="Information Contribution")+
  theme(legend.position = "none")+
  ggtitle("Information contribution across River Basins (Local Drainage Area)")
p5