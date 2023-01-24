###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# FIGURES
###############################################################################

#By : Francisco Guerrero
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

# SETTINGS
# Loading packages:

# Run for the first time only
# install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

# Also, you may need to install the GIT credential manager following the instructions
# from: https://github.com/GitCredentialManager/git-credential-manager/blob/main/README.md

rm()

librarian::shelf(tidyverse,# for plotting
                 plot3D,# for 3D plots
                 plot3Drgl,# for interactive 3D plots
                 rgl,# required by plot3Drgl
                 entropy,#Information theory calculations
                 GGally,#pair plots
                 scales,# manipulating log scales
                 stringr,# editing text
                 Hmisc,# Harrell's miscellaneaous for stats
                 gtable)# To manipulate ggplot objects

theme_httn<-  theme(axis.text=element_text(colour="black",size=22),
                    axis.title = element_text(size = 32, face = "bold"),
                    panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
                    panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
                    panel.border = element_rect(fill=NA, colour = "black", linewidth = 1.5),
                    panel.background=element_rect(fill="white"),
                    axis.ticks.length = unit(0.254, "cm"),
                    axis.ticks = element_line(colour = "black", linewidth = 1), 
                    axis.line = element_line(colour = "black"),
                    legend.position = c(0.85,0.25),
                    legend.direction = "vertical",
                    legend.background = element_blank(),
                    legend.key.size = unit(1.0, 'lines'),#Changing spacing between legend keys
                    legend.title = element_text())
set.seed(2703)

#Data:

#header info

# pending!!!!!

#values
bgc_cln0 <- read.csv("assets/data/230123_scaling_lnd_bgc.csv", 
                  stringsAsFactors=TRUE)

bgc_cln0 <- bgc_cln0[,2:ncol(bgc_cln0)]

summary(bgc_cln0)

# Son et al 2022 used a random forest model to fill in the prediction gaps due to 
# lack of hyporheic residence time and exchange data. 

# We will start comparing the scaling relationships across these two type of predictions
# since the underlying process to each model is different. 

# To do so, we will create a new variable that would help us to separate the two data sets
# easily

bgc_cln <- bgc_cln0 %>% 
  mutate(model = ifelse(is.na(hz_exchng)==TRUE,"rff","rcm"))

# Now, let's take a quick look at the scaling relationships that could be observed
# through these data sets:

# Exploratory plot Cumulative Watershed Function vs. Watershed Area

# exploratory plot

p <- ggplot(bgc_cln,aes(wsd_are,crsp_wsa, color = model))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(intercept = -3, slope = 1)+
  facet_wrap(~model)
p

# At a first glance the overall behavior seems consistent across the two data sets, except
# for the extremely low values (below 1-e-07) predicted by the random forest model.

summary(filter(bgc_cln,model=="rff"))

# We have zero values for cumulative watershed respiration. Let's check how many of them we have:
length(filter(bgc_cln,crsp_wsa==0))

# 51 in total. 

#Let's try the exploratory plot again:
p <- ggplot(filter(bgc_cln,crsp_wsa>0),aes(wsd_are,crsp_wsa, color = model))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(intercept = -3, slope = 1)+
  facet_wrap(~model)
p

# The original data needs to be tidy up with respect to factor levels for land 
# cover to make it into a long format


################################################################################
# Figures
################################################################################



# Let's start with a quick pairs plot to have a glimpse of the relationships among
# variables

# Physical variables & Land Use

# adding a smooth loess to the paired plot

# how to: https://stackoverflow.com/questions/35085261/how-to-use-loess-method-in-ggallyggpairs-using-wrap-function

my_fn <- function(data, mapping, pts=list(), smt=list(), ...){
  ggplot(data = data, mapping = mapping, ...) + 
    do.call(geom_point, pts) +
    do.call(geom_smooth, smt) 
}

# Let's start looking at correlation between some physical and biogeochemical variables:


paired_plot <- select(bgc_cln,
                      comid,
                      wsd_are,
                      frst,
                      clgt_rch,
                      csar_rch,
                      d50m,
                      crsp_wsa,
                      rch_lgtm,
                      res_time,
                      hz_exchng) %>% 
  mutate(log.wsd_are = log(wsd_are,10),
         log.frst = ifelse(frst==0,0,log(frst,10)),
         log.clgt = log(clgt_rch,10),
         log.csar = log(csar_rch,10),
         log.d50m = log(d50m,10),
         log.crsp = log(crsp_wsa,10),
         log.rlgt = log(rch_lgtm,10),
         log.rest = log(res_time,10),
         log.hzex = log(hz_exchng,10)) %>% 
  select(c(11:19)) %>% 
  ggpairs(lower = list(continuous = 
                         wrap(my_fn,
                              pts=list(size=0.1, colour="gray"), 
                              smt=list(method="loess", se=F, size=1, colour="blue",span=0.8))))        
paired_plot
# 

####################################################### Jan 23 2023 #########################
p <- ggplot(bgc_cln,aes(wshd_area,logw_m))+
  geom_point()+
  scale_x_log10()
p

# ggsave(file="guerrero_etal_22_scaling_respiration_physical.png",
#        width = 15,
#        height = 10,
#        units = "in")

# Respiration and biogeochemical variables

paired_plot_b <- select(bgc_cln,
                      acm_resp,
                      doc_annual,
                      do_annual,
                      nitrates,
                      aer_resp,
                      anb_resp) %>% 
  mutate(log_resp = log10(acm_resp)) %>% 
  mutate(log_doc = log10(doc_annual)) %>% 
  mutate(log_do = log10(do_annual)) %>%
  mutate(log_no3t = log10(nitrates)) %>% 
  mutate(log_ae_resp = log10(aer_resp)) %>% 
  mutate(log_an_resp = log10(anb_resp)) %>% 
  select(c(7:12)) %>% 
  ggpairs(lower = list(continuous = 
                         wrap(my_fn,
                              pts=list(size=0.1, colour="gray"), 
                              smt=list(method="loess", se=F, size=1, colour="blue",span=0.8))))
paired_plot_b
# 
# ggsave(file="guerrero_etal_22_scaling_respiration_biogeochem.png",
#        width = 15,
#        height = 10,
#        units = "in")


# let's explore relationships with continuous variables expressed as categories for 
# easier visualization

################################################################################
# Calculating Quantiles


#I'm going to try calculating the quantiles with Hmisc::cut2, which allows
# for the inclusion of zeroes

# https://stackoverflow.com/questions/46750635/cut-and-quantile-in-r-in-not-including-zero

qlabel <- c("Q10","Q20","Q30","Q40","Q50","Q60","Q70","Q80+")

bgc_cln <- bgc_cln %>% 
  mutate(ent_cat = factor(Hmisc::cut2(hrt, g = 8),labels = qlabel)) %>% 
  mutate(rst_cat = factor(Hmisc::cut2(res_time, g = 8),labels = qlabel)) %>% 
  mutate(hze_cat = factor(Hmisc::cut2(hz_exchng, g = 8),labels = qlabel)) %>% 
  mutate(d50_cat = factor(Hmisc::cut2(log10(d50m), g = 8),labels = qlabel))


# Creating a quasi-sequential color palette for discrete categories
# Source: https://www.ibm.com/design/language/color/

my_dcolors <- c("#a6c8ff","#78a9ff","#4589ff","#0f62fe",
                "#00539a","#003a6d","#012749","#061727")

my_rcolors <- c("#fff1f1","#ffd7d9","#ffb3b8","#fa4d56",
               "#da1e28","#a2191f","#750e13","#2d0709")

my_mcolors <- c("#ffd6e8","#ffafd2","#ff7eb6","#ee5396",
                "#d62670","#9f1853","#740937","#510224")

# Creating breaks for logarithmic scale 
# (see: https://r-graphics.org/recipe-axes-axis-log)

breaks <- 10^(-10:10)
breaks_c <- 10^seq(-10,10,by=4)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

# Landscape entropy and scaling

# exploratory plot

p <- ggplot(bgc_cln,aes(wsd_are,crsp_wsa, color = ent_cat))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(intercept = -3, slope = 1)+
  facet_wrap(~ent_cat)
p

# Inset plot
ent_quant_i <- ggplot(bgc_cln,aes(wsd_are,
                                crsp_wsa,
                                color=ent_cat))+
  geom_smooth(method="lm",fullrange = TRUE, alpha = 0.3)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c, 
                limits = c(10^-6,10^6),
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  scale_color_manual(values = my_mcolors)+
  geom_abline(slope=1.0, color = "red", linetype = "dashed", size = 1.5)+
  guides(color=guide_legend(title = "Landscape Entropy\n(quartiles)"))+
  theme_httn+
  theme(legend.position ="none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 16))+
  guides(alpha = "none")
ent_quant_i

ent_ins <- ggplotGrob(ent_quant_i)

# Main plot
ent_quant <- ggplot(bgc_cln,aes(wsd_are,
                                 crsp_wsa,
                                 color=ent_cat))+
  geom_point(size = 2.5,aes(alpha = hrt))+
  # geom_smooth(method="lm",fullrange = TRUE, se=FALSE)+
  # facet_wrap(~ent_cat,nrow = 2)+
  # geom_point(aes(alpha=p_frt_t), size = 2.5)+
  # geom_point(aes(alpha=p_shb_t), size = 2.5)+
  # geom_point(aes(alpha=p_ant_t), size = 2.5)+
  # geom_smooth(method="lm",fullrange = TRUE, se=FALSE)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c, 
                limits = c(10^-6,10^6),
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  annotation_custom(
    grob = ent_ins,
    xmin = 2,
    xmax = 4,
    ymin = -6,
    ymax = -2) +
  scale_color_manual(values = my_mcolors)+
  geom_abline(slope=1.0, color = "red", linetype = "dashed", size = 1.5)+
  guides(color=guide_legend(title = "Landscape Entropy\n(quartiles)"))+
  theme_httn+
  theme(legend.position =c(0.88,0.53),
        # panel.grid.minor= element_blank(), 
        # panel.grid.major =element_blank(),
        legend.text = element_text(size=16),
        legend.title = element_text(size=18),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 18, face = "bold"))+
  guides(alpha = "none")
ent_quant

ent_quant <- ent_quant +   ggtitle(paste("Interpretation:",
                         "\nMore homogeneous landscapes (i.e. low entropy) seem to exhibit a bimodal",
                         "\ndistribution for potential scaling exponents. ore heterogeneous landscapes",
                         "\nappear to come close to linear scaling.",
                         sep = " "))
ent_quant

ggsave(file="guerrero_etal_23_scaling_respiration_entropy.png",
       width = 12,
       height = 12,
       units = "in")

# Entropy and DOC

p <- ggplot(na.omit(bgc_cln),aes(p_frt_t, hrt, color = log(doc_annual)))+
  geom_point()
p

# Residence time and scaling

# Inset plots with log scales

# https://stackoverflow.com/questions/20271233/how-can-you-get-ggplot2-to-display-an-inset-figure-when-the-main-one-has-a-log-s


# Inset plot
hzt_quant_i <- ggplot(bgc_cln,aes(wshd_area,
                                  acm_resp,
                                  color=rst_cat))+
  geom_smooth(method="lm",fullrange = TRUE, alpha = 0.3)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c, 
                limits = c(10^-6,10^6),
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  scale_color_manual(values = my_dcolors)+
  geom_abline(slope=1.0, color = "red", linetype = "dashed", size = 1.5)+
  guides(color=guide_legend(title = "Landscape Entropy\n(quartiles)"))+
  theme_httn+
  theme(legend.position ="none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 16))+
  guides(alpha = "none")
hzt_quant_i

hzt_ins <- ggplotGrob(hzt_quant_i)

# Main plot
hzt_quant <- ggplot(bgc_cln,aes(wshd_area,
                                acm_resp,
                                color=rst_cat))+
  geom_abline(slope=1.0, color = "red", linetype = "dashed", size = 1.5)+
  # facet_wrap(~rst_cat,nrow = 2)+
  # geom_point(aes(alpha=p_frt_t), size = 0.95)+
  # geom_point(aes(alpha=p_shb_t), size = 0.95)+
  # geom_point(aes(alpha=p_ant_t), size = 2.5)+
  geom_point(size = 2.5, aes(alpha = res_time))+
  geom_smooth(method="lm",fullrange = TRUE, se=FALSE)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c,
                limits = c(10^-6,10^6),
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  scale_color_manual(values = my_dcolors)+
  guides(color=guide_legend(title = "Residence time\n(log-scale quantiles)"))+
  theme_httn+
  theme(legend.position =c(0.88,0.53),
        # panel.grid.minor= element_blank(), 
        # panel.grid.major =element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 18, face = "bold"))+
  guides(alpha = "none")
hzt_quant


hzt_quant <- hzt_quant + ggtitle(paste("Interpretation:",
                                         "\nthere is a clear progression towards superlinear scaling with increasing residence",
                                         "\ntimes in the hyporheic zone",
                                         sep = " "))
hzt_quant

ggsave(file="guerrero_etal_23_scaling_respiration_hz_time_all.png",
       width = 12,
       height = 12,
       units = "in")


# Hyporheic exchange and scaling

hze_quant <- ggplot(bgc_cln,aes(wshd_area,
                                acm_resp,
                                color=hze_cat))+
  facet_wrap(~hze_cat, nrow = 2)+
  geom_point(aes(alpha=p_frt_t), size = 0.95)+
  geom_point(aes(alpha=p_shb_t), size = 0.95)+
  # geom_point(aes(alpha=p_ant_t), size = 2.5)+
  geom_smooth(method="lm",fullrange = TRUE, se=FALSE)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  scale_color_manual(values = my_dcolors)+
  geom_abline(slope=1.0, color = "red", linetype = "dashed", size = 1.5)+
  guides(color=guide_legend(title = "Hyporheic exchange\n(log-scale quantiles)"))+
  theme_httn+
  theme(legend.position = "none",
        panel.grid.minor= element_blank(), 
        panel.grid.major =element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=16),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold"))+
  guides(alpha = "none")
hze_quant

hze_quant <- hze_quant +   ggtitle(paste("Interpretation:",
                                         "\nHyporheic exchange also shows fittin trends across the data. However",
                                         "\nmost of the trends tend to be linear or sublinear. When superlinear",
                                         "\ntrends are observed, at lower exchange rates, there is much more spread",
                                         "\naround the scaling line when compared to residence time.",
                                         sep = " "))
hze_quant

ggsave(file="guerrero_etal_22_scaling_respiration_hyporheic.png",
       width = 15,
       height = 12,
       units = "in")

# D50 and scaling

prt_quant <- ggplot(bgc_cln,aes(wshd_area,
                                         acm_resp,
                                         color=d50_cat))+
  facet_wrap(~d50_cat, nrow = 2)+
  geom_point(aes(alpha=p_frt_t), size = 0.95)+
  geom_point(aes(alpha=p_shb_t), size = 0.95)+
  # geom_point(aes(alpha=p_ant_t), size = 2.5)+
  geom_smooth(method="lm",fullrange = TRUE, se=FALSE)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  scale_color_manual(values = my_dcolors)+
  geom_abline(slope=1.0, color = "red", linetype = "dashed", size = 1.5)+
  guides(color=guide_legend(title = "Particle size\n(log-scale quantiles)"))+
  theme_httn+
  theme(legend.position = "none",
        panel.grid.minor= element_blank(), 
        panel.grid.major =element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=16),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold"))+
  guides(alpha = "none")
prt_quant

prt_quant <- prt_quant +   ggtitle(paste("Interpretation:",
                                         "\nSmaller particle sizes result in longer residence times and move towards",
                                         "\nsuperlinear scaling. Large particle size seem to converge around linear or",
                                         "\neven sublinear scaling",
                                         sep = " "))
prt_quant

ggsave(file="guerrero_etal_22_scaling_respiration_hyporheic.png",
       width = 15,
       height = 12,
       units = "in")

#########################################################################################
# Interaction between landscape heterogeneity, residence time, and hyporheic exchange
# as related to scaling behavior
#########################################################################################

new.labs <- c("HZt-Q10","HZt-Q20", "HZt-Q30","HZt-Q40","HZt-Q50",
                          "HZt-Q60","HZt-Q70","HZt-Q80+")
names(new.labs) <- c("Q10","Q20","Q30","Q40","Q50","Q60","Q70","Q80+")

bgc_cln %>% select(wshd_area,
                   acm_resp,
                   p_frt_t,
                   p_ant_t,
                   p_shb_t,
                   hz_exchng,
                   res_time,
                   ent_cat,
                   hze_cat,
                   rst_cat,
                   hrt) %>% 
  gather(c(3:5),key="use",value = "fraction") %>% 
  mutate(use = fct_relevel(use,c("p_shb_t","p_frt_t","p_ant_t"))) %>% 
  arrange(use) %>% 
  ggplot(aes(wshd_area,acm_resp,color=use))+
  facet_wrap(~rst_cat, nrow = 2, labeller = labeller(rst_cat = new.labs))+
  geom_abline(slope=1.0, color = "red", linetype = "solid", size = 0.75)+
  geom_smooth(aes(wshd_area,acm_resp),method = "lm", inherit.aes = FALSE,
              fullrange = TRUE, color = "black", size = 0.65, se = TRUE, fill = "gray",
              alpha = 0.7)+
  geom_point(aes(alpha = fraction),size = 2.5)+
  scale_x_log10(breaks = breaks_c, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  scale_color_manual(values = c("#7b3294","#008837","#dfc27d"))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  theme_httn+
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=16),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold"))
        # strip.background = element_blank())

ggsave(file="guerrero_etal_22_scaling_slopes_entropy.png",
       width = 20,
       height = 10,
       units = "in")

################################################################################
#Normalizing cumulative respiration by watershed area
################################################################################

# We need to calculate the stream surface area in squared for each segment

bgc_cln <- bgc_cln %>% 
  mutate(strm_sa = 10^logw_m * length_m) %>% 
  mutate(tot_acm = acm_resp * strm_sa ) %>% 
  mutate(acm_wshd = tot_acm/wshd_area)


p <- ggplot(bgc_cln,aes(wshd_area,acm_wshd))+
  geom_point()+
  scale_x_log10()+
  geom_abline(slope=1.0, color = "red", linetype = "dashed", size = 1.5)+
  scale_y_log10()
p

# Inset plot
ent_quant_i <- ggplot(bgc_cln,aes(wshd_area,
                                  acm_wshd,
                                  color=ent_cat))+
  geom_smooth(method="lm",fullrange = TRUE, alpha = 0.3)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c, 
                limits = c(10^-2,10^6),
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  scale_color_manual(values = my_mcolors)+
  geom_abline(slope=1.0, color = "red", linetype = "dashed", size = 1.5)+
  guides(color=guide_legend(title = "Landscape Entropy\n(quartiles)"))+
  theme_httn+
  theme(legend.position ="none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 16))+
  guides(alpha = "none")
ent_quant_i

ent_ins <- ggplotGrob(ent_quant_i)

# Main plot
ent_quant <- ggplot(bgc_cln,aes(wshd_area,
                                acm_wshd,
                                color=ent_cat))+
  geom_point(size = 2.5,aes(alpha = hrt))+
  geom_smooth(method="lm",fullrange = TRUE, se=FALSE)+
  facet_wrap(~ent_cat,nrow = 2)+
  # geom_point(aes(alpha=p_frt_t), size = 2.5)+
  # geom_point(aes(alpha=p_shb_t), size = 2.5)+
  # geom_point(aes(alpha=p_ant_t), size = 2.5)+
  # geom_smooth(method="lm",fullrange = TRUE, se=FALSE)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c, 
                limits = c(10^-2,10^6.5),
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  # annotation_custom(
  #   grob = ent_ins,
  #   xmin = 2,
  #   xmax = 4,
  #   ymin = -6,
  #   ymax = -2) +
  scale_color_manual(values = my_mcolors)+
  geom_abline(slope=1.0, color = "red", linetype = "dashed", size = 1.5)+
  guides(color=guide_legend(title = "Landscape Entropy\n(quartiles)"))+
  theme_httn+
  theme(legend.position =c(0.88,0.53),
        # panel.grid.minor= element_blank(), 
        # panel.grid.major =element_blank(),
        legend.text = element_text(size=16),
        legend.title = element_text(size=18),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 18, face = "bold"))+
  guides(alpha = "none")
ent_quant

# Residence time and scaling

# Inset plot
hzt_quant_i <- ggplot(bgc_cln,aes(wshd_area,
                                  acm_wshd,
                                  color=hze_cat))+
  geom_smooth(method="lm",fullrange = TRUE, alpha = 0.3)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c, 
                limits = c(10^-6,10^6),
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  scale_color_manual(values = my_dcolors)+
  geom_abline(slope=1.0, color = "red", linetype = "dashed", size = 1.5)+
  guides(color=guide_legend(title = "Landscape Entropy\n(quartiles)"))+
  theme_httn+
  theme(legend.position ="none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 16))+
  guides(alpha = "none")
hzt_quant_i

hzt_ins <- ggplotGrob(hzt_quant_i)

# Main plot
hzt_quant <- ggplot(bgc_cln,aes(wshd_area,
                                acm_wshd,
                                color=hze_cat))+
  geom_abline(slope=1.0, color = "red", linetype = "dashed", size = 1.5)+
  # facet_wrap(~rst_cat,nrow = 2)+
  # geom_point(aes(alpha=p_frt_t), size = 0.95)+
  # geom_point(aes(alpha=p_shb_t), size = 0.95)+
  # geom_point(aes(alpha=p_ant_t), size = 2.5)+
  geom_point(size = 2.5, aes(alpha = res_time))+
  geom_smooth(method="lm",fullrange = TRUE, se=FALSE)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c,
                limits = c(10^-6,10^6),
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  annotation_custom(
    grob = hzt_ins,
    xmin = 2,
    xmax = 4,
    ymin = -6,
    ymax = -2) +
  scale_color_manual(values = my_dcolors)+
  guides(color=guide_legend(title = "Residence time\n(log-scale quantiles)"))+
  theme_httn+
  theme(legend.position =c(0.88,0.53),
        # panel.grid.minor= element_blank(), 
        # panel.grid.major =element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 18, face = "bold"))+
  guides(alpha = "none")
hzt_quant


hzt_quant <- hzt_quant + ggtitle(paste("Interpretation:",
                                       "\nthere is a clear progression towards superlinear scaling with increasing residence",
                                       "\ntimes in the hyporheic zone",
                                       sep = " "))
hzt_quant

#########################################################################################
# Interaction between landscape heterogeneity, residence time, and hyporrheic exchange
# as related to scaling behavior
#########################################################################################

new.labs <- c("HZt-Q10","HZt-Q20", "HZt-Q30","HZt-Q40","HZt-Q50",
              "HZt-Q60","HZt-Q70","HZt-Q80+")
names(new.labs) <- c("Q10","Q20","Q30","Q40","Q50","Q60","Q70","Q80+")

bgc_cln %>% select(wshd_area,
                   acm_wshd,
                   p_frt_t,
                   p_ant_t,
                   p_shb_t,
                   hz_exchng,
                   res_time,
                   ent_cat,
                   hze_cat,
                   rst_cat,
                   d50_cat,
                   hrt) %>% 
  gather(c(3:5),key="use",value = "fraction") %>% 
  mutate(use = fct_relevel(use,c("p_shb_t","p_frt_t","p_ant_t"))) %>% 
  arrange(use) %>% 
  ggplot(aes(wshd_area,acm_wshd,color=use))+
  facet_wrap(~rst_cat, nrow = 2, labeller = labeller(hze_cat = new.labs))+
  geom_abline(slope=1.0, color = "red", linetype = "solid", size = 0.75)+
  geom_smooth(aes(wshd_area,acm_wshd),method = "lm", inherit.aes = FALSE,
              fullrange = TRUE, color = "black", size = 0.65, se = TRUE, fill = "gray",
              alpha = 0.7)+
  geom_point(aes(alpha = fraction),size = 2.5)+
  scale_x_log10(breaks = breaks_c, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*km^-2*d^-1,")"))))+
  scale_color_manual(values = c("#7b3294","#008837","#dfc27d"))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  theme_httn+
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=16),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold"))















# Changing color strips according to residence times

# https://stackoverflow.com/questions/19440069/ggplot2-facet-wrap-strip-color-based-on-variable-in-data-set

p1 <- bgc_cln %>% select(wshd_area,
                         acm_resp,
                         p_frt_t,
                         p_ant_t,
                         p_shb_t,
                         hz_exchng,
                         res_time,
                         ent_cat,
                         hze_cat,
                         rst_cat,
                         hrt) %>% 
  gather(c(3:5),key="use",value = "fraction") %>% 
  mutate(use = fct_relevel(use,c("p_shb_t","p_frt_t","p_ant_t"))) %>% 
  arrange(use) %>% 
  ggplot(aes(wshd_area,acm_resp,color=use))+
  facet_wrap(~rst_cat, nrow = 2, labeller = labeller(rst_cat = new.labs))+
  geom_abline(slope=1.0, color = "red", linetype = "solid", size = 0.75)+
  geom_smooth(aes(wshd_area,acm_resp),method = "lm", inherit.aes = FALSE,
              fullrange = TRUE, color = "black", size = 0.65, se = TRUE, fill = "gray",
              alpha = 0.7)+
  geom_point(aes(alpha = fraction),size = 2.5)+
  scale_x_log10(breaks = breaks_c, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  scale_color_manual(values = c("#7b3294","#008837","#dfc27d"))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  theme_httn+
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=16),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold"))

dummy <- bgc_cln %>% select(wshd_area,
                         acm_resp,
                         p_frt_t,
                         p_ant_t,
                         p_shb_t,
                         hz_exchng,
                         res_time,
                         ent_cat,
                         hze_cat,
                         rst_cat,
                         hrt) %>% 
  gather(c(3:5),key="use",value = "fraction") %>% 
  mutate(use = fct_relevel(use,c("p_shb_t","p_frt_t","p_ant_t"))) %>% 
  arrange(use) %>% 
  ggplot(aes(wshd_area,acm_resp,color=use))+
  facet_wrap(~rst_cat, nrow = 2, labeller = labeller(rst_cat = new.labs))+
  geom_rect(aes(fill=rst_cat), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  scale_fill_manual(values = my_dcolors)+
  scale_x_log10(breaks = breaks_c, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  scale_color_manual(values = c("#7b3294","#008837","#dfc27d"))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  theme_httn+
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=16),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold"))

dummy <- ggplot(data = d, aes(x = farm, y = weight))+ facet_wrap(~fruit) + 
  geom_rect(aes(fill=size), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  theme_minimal()

library(gtable)

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(dummy)

gtable_select <- function (x, ...) 
{
  matches <- c(...)
  x$layout <- x$layout[matches, , drop = FALSE]
  x$grobs <- x$grobs[matches]
  x
}

panels <- grepl(pattern="panel", g2$layout$name)
strips <- grepl(pattern="strip_t", g2$layout$name)
g2$layout$t[panels] <- g2$layout$t[panels] - 1
g2$layout$b[panels] <- g2$layout$b[panels] - 1

new_strips <- gtable_select(g2, panels | strips)
grid.newpage()
grid.draw(new_strips)

gtable_stack <- function(g1, g2){
  g1$grobs <- c(g1$grobs, g2$grobs)
  g1$layout <- transform(g1$layout, z= z-max(z), name="g2")
  g1$layout <- rbind(g1$layout, g2$layout)
  g1
}
## ideally you'd remove the old strips, for now they're just covered
new_plot <- gtable_stack(g1, new_strips)
grid.newpage()
grid.draw(new_plot)


p <- ggplot(bgc_cln,aes(hrt,res_time))+
  geom_point()
p







# Landscape entropy and scaling (poster plot, alternative version)

ent_quant <- ggplot(bgc_cln,aes(wshd_area,
                                acm_resp,
                                color=hrt))+
  geom_point(size = 2.5,aes(alpha = hrt))+
  # geom_smooth(method="lm",fullrange = TRUE, se=FALSE)+
  # facet_wrap(~ent_cat,nrow = 2)+
  # geom_point(aes(alpha=p_frt_t), size = 0.95)+
  # geom_point(aes(alpha=p_shb_t), size = 0.95)+
  # geom_point(aes(alpha=p_ant_t), size = 2.5)+
  # geom_smooth(method="lm",fullrange = TRUE, se=FALSE)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  scale_color_distiller(palette = "Greens")+
  geom_abline(slope=1.0, color = "red", linetype = "dashed", size = 1.5)+
  guides(color=guide_legend(title = "Landscape Entropy\n(quartiles)"))+
  theme_httn+
  theme(legend.position =c(0.85,0.15),
        # panel.grid.minor= element_blank(), 
        # panel.grid.major =element_blank(),
        legend.text = element_text(size=16),
        legend.title = element_text(size=18),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 18, face = "bold"))+
  guides(alpha = "none")
ent_quant

ent_quant <- ent_quant +   ggtitle(paste("Interpretation:",
                                         "\nMore homogeneous landscapes (i.e. low entropy) seem to exhibit a bimodal",
                                         "\ndistribution for potential scaling exponents. ore heterogeneous landscapes",
                                         "\nappear to come close to linear scaling.",
                                         sep = " "))
ent_quant

ggsave(file="guerrero_etal_22_scaling_respiration_entropy.png",
       width = 15,
       height = 12,
       units = "in")








# Landscape heterogeneity and scaling (re-organizing layers)

bgc_cln %>% select(wshd_area,
                   acm_resp,
                   p_frt_t,
                   p_ant_t,
                   p_shb_t,
                   hz_exchng,
                   res_time,
                   ent_cat,
                   hze_cat,
                   rst_cat,
                   hrt) %>% 
  gather(c(3:5),key="use",value = "fraction") %>% 
  mutate(use = fct_relevel(use,c("p_frt_t","p_ant_t","p_shb_t"))) %>% 
  arrange(use) %>% 
  ggplot(aes(wshd_area,acm_resp,color=ent_cat))+
  # facet_wrap(~use)+
  # facet_wrap(~rst_cat, nrow = 2, labeller = labeller(rst_cat = new.labs))+
  geom_abline(slope=1.0, color = "red", linetype = "solid", size = 0.75)+
  geom_smooth(aes(wshd_area,acm_resp),method = "lm", inherit.aes = FALSE,
              fullrange = TRUE, color = "black", size = 0.65, se = TRUE, fill = "gray",
              alpha = 0.7)+
  geom_point(alpha = .05, size = 2.5)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  # scale_color_manual(values = c("#008837","#dfc27d","#7b3294"))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  theme_httn+
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=16),
        plot.title = element_text(size = 16))
# strip.text = element_blank(),













# Trying with Plot3D

# I had to break the data this way to use a consistent color scale 
# across the plots. I have not figure out how to display a triangular
# color scale with three different colored end points corresponding
# to the three landscape categories.

x <- c(bgc_cln$p_frt_t)
y <- c(bgc_cln$p_shb_t)
z <- c(bgc_cln$hrt)
c <- c(log(bgc_cln$acm_resp))

x1 <- c(filter(bgc_cln,p_frt_t>0.35)$p_frt_t)
y1 <- c(filter(bgc_cln,p_frt_t>0.35)$p_shb_t)
z1 <- c(filter(bgc_cln,p_frt_t>0.35)$hrt)

x2 <- c(filter(bgc_cln,p_frt_t<0.35 & p_shb_t<0.35)$p_frt_t)
y2 <- c(filter(bgc_cln,p_frt_t<0.35 & p_shb_t<0.35)$p_shb_t)
z2 <- c(filter(bgc_cln,p_frt_t<0.35 & p_shb_t<0.35)$hrt)

x3 <- c(filter(bgc_cln,p_frt_t<0.35 & p_ant_t<0.35)$p_frt_t)
y3 <- c(filter(bgc_cln,p_frt_t<0.35 & p_ant_t<0.35)$p_shb_t)
z3 <- c(filter(bgc_cln,p_frt_t<0.35 & p_ant_t<0.35)$hrt)

x4 <- c(filter(bgc_cln,p_shb_t>0.35 & p_ant_t>0.35)$p_frt_t)
y4 <- c(filter(bgc_cln,p_shb_t>0.35 & p_ant_t>0.35)$p_shb_t)
z4 <- c(filter(bgc_cln,p_shb_t>0.35 & p_ant_t>0.35)$hrt)


scatter3D(x, y, z, 
          clab = c("Cumulative","respiration"),
          ylab = "Shrubscapes (fraction)",
          xlab = "Forestcapes (fraction)",
          zlab = c("Landscape heterogeneity", "(Shannon's entropy)"),
          bty = "g",
          alpha = 1,
                # main = "landscape entropy",
                colvar = c,
                col = ramp.col(c("darkorchid","#f5f5f5","darkorange")),
                # col = ramp.col(c("#7b3294","#dfc27d","#008837")),
                expand = 0.5,
                theta = 30,
                phi =10,
                pch = 20,
                cex =1.0,
                ticktype = "detailed")


#Adding marginal projection at bottom plane only
scatter3D_fancy <- function(x, y, z,..., colvar = x)
{
  panelfirst <- function(pmat) {
    # XY <- trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
    # scatter2D(XY$x, 
    #           XY$y, 
    #           col = ramp.col(c("#a6611a","#dfc27d","#008837")),
    #           colvar =x,
    #           pch = ".", 
    #           cex = 2, 
    #           add = TRUE, 
    #           colkey = FALSE)
    
    # XY <- trans3D(x, y =rep(max(y), length(y)), z, pmat = pmat)
    # scatter2D(XY$x, 
    #           XY$y, 
    #           col = ramp.col(c("#a6611a","#dfc27d","#008837")),
    #           colvar =x,
    #           pch = ".", 
    #           cex = 2, 
    #           add = TRUE, 
    #           colkey = FALSE)
    
    XY <- trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
    scatter2D(XY$x, 
              XY$y, 
              col = ramp.col(c("darkorchid","#f5f5f5","darkorange")),
              # col = ramp.col(c("#7b3294","#dfc27d","#008837")),
              colvar = c, 
              pch = ".", 
              cex = 2.0, 
              add = TRUE, 
              colkey = FALSE)
    
  }
  scatter3D(x, y, z, ..., colvar = c, panel.first=panelfirst,
            colkey = list(length = 0.25, width = 1.5, cex.clab = 1.0, side = 1)) 
}

scatter3D_fancy(x, y, z, 
                clab = c("Cummulative","respiration","[Log scale]"),
                ylab = "Shrubscapes (fraction)",
                xlab = "Forestcapes (fraction)",
                zlab = c("Landscape heterogeneity", "(Shannon's entropy)"),
                bty = "g",
                alpha = 0.95,
                # main = "landscape entropy",
                colvar = c,
                col = ramp.col(c("darkorchid","#f5f5f5","darkorange")),
                # col = ramp.col(c("#7b3294","#dfc27d","#008837")),
                expand = 0.5,
                theta = 35,
                phi =10,
                pch = 20,
                cex =1.0,
                ticktype = "detailed")




















scatter3D(x1, y1, z1, 
          add = TRUE,
          alpha = 0.5,
          colkey = FALSE, 
          colvar = x1,
          col = ramp.col(c("#7b3294","#dfc27d","#008837")))



# col = ramp.col(c("#dfc27d","#f5f5f5","#008837"))


scatter3D(x1, y1, z1, add = TRUE, colkey = FALSE, 
          pch = 19, cex = 0.5, col = "black")
scatter3D(x2, y2, z2, add = TRUE, colkey = FALSE, 
          pch = 19, cex = 0.5, col = "black")
scatter3D(x3, y3, z3, add = TRUE, colkey = FALSE, 
          pch = 19, cex = 0.5, col = "black")
scatter3D(x4, y4, z4, add = TRUE, colkey = FALSE, 
          pch = 19, cex = 0.5, col = "black")


scatter3D(x, y, z, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed", expand = 0.5,
          theta = 35,alpha=0.01)
scatter3D(x1, y1, z1, add = TRUE, colkey = FALSE, 
          pch = 19, cex = 0.5, col = "black")
scatter3D(x2, y2, z2, add = TRUE, colkey = FALSE, 
          pch = 19, cex = 0.5, col = "black")
scatter3D(x3, y3, z3, add = TRUE, colkey = FALSE, 
          pch = 19, cex = 0.5, col = "black")
scatter3D(x4, y4, z4, add = TRUE, colkey = FALSE, 
          pch = 19, cex = 0.5, col = "black")


x <- c(bgc_cln$p_frt_t)
y <- c(bgc_cln$p_shb_t)
z <- c(bgc_cln$hrt)


scatter3D_fancy <- function(x, y, z,..., colvar = x)
{
  panelfirst <- function(pmat) {
    # XY <- trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
    # scatter2D(XY$x, 
    #           XY$y, 
    #           col = ramp.col(c("#a6611a","#dfc27d","#008837")),
    #           colvar =x,
    #           pch = ".", 
    #           cex = 2, 
    #           add = TRUE, 
    #           colkey = FALSE)
    
    # XY <- trans3D(x, y =rep(max(y), length(y)), z, pmat = pmat)
    # scatter2D(XY$x, 
    #           XY$y, 
    #           col = ramp.col(c("#a6611a","#dfc27d","#008837")),
    #           colvar =x,
    #           pch = ".", 
    #           cex = 2, 
    #           add = TRUE, 
    #           colkey = FALSE)
    
    XY <- trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
    scatter2D(XY$x, 
              XY$y, 
              col = ramp.col(c("#dfc27d","#f5f5f5","#008837")),
              # col = ramp.col(c("#7b3294","#dfc27d","#008837")),
              colvar = x, 
              pch = ".", 
              cex = 2, 
              add = TRUE, 
              colkey = FALSE)
    
  }
  scatter3D(x, y, z, ..., colvar = x, panel.first=panelfirst,
            colkey = list(length = 0.25, width = 1.5, cex.clab = 1.0, side = 1)) 
}

scatter3D_fancy(x, y, z, 
          clab = c("Forestcapes cover","(as a fraction)"),
          ylab = "Shrubscapes (fraction)",
          xlab = "Forestcapes (fraction)",
          zlab = c("Landscape heterogeneity", "(Shannon's entropy)"),
          bty = "g",
          alpha = 0.35,
          # main = "landscape entropy",
          colvar = x,
          col = ramp.col(c("#dfc27d","#f5f5f5","#008837")),
          # col = ramp.col(c("#7b3294","#dfc27d","#008837")),
          expand = 0.5,
          theta = 35,
          phi =10,
          pch = 20,
          cex =1.0,
          ticktype = "detailed")



# rgl.snapshot("guerrero_etal_22_entropy_landscape.png", fmt = 'png')
# This line did not work to export the plot, so I had to save it as a png using 
# the "Export" button from the "Plots" tab. 


# Trying with Plot3D

x1 <- c(filter(bgc_cln,p_frt_t>0.35)$p_frt_t)
y1 <- c(filter(bgc_cln,p_frt_t>0.35)$p_shb_t)
z1 <- c(filter(bgc_cln,p_frt_t>0.35)$hrt)

x2 <- c(filter(bgc_cln,p_frt_t<0.35 & p_shb_t<0.35)$p_frt_t)
y2 <- c(filter(bgc_cln,p_frt_t<0.35 & p_shb_t<0.35)$p_shb_t)
z2 <- c(filter(bgc_cln,p_frt_t<0.35 & p_shb_t<0.35)$hrt)

x3 <- c(filter(bgc_cln,p_frt_t<0.35 & p_ant_t<0.35)$p_frt_t)
y3 <- c(filter(bgc_cln,p_frt_t<0.35 & p_ant_t<0.35)$p_shb_t)
z3 <- c(filter(bgc_cln,p_frt_t<0.35 & p_ant_t<0.35)$hrt)

x4 <- c(filter(bgc_cln,p_shb_t>0.35 & p_ant_t>0.35)$p_frt_t)
y4 <- c(filter(bgc_cln,p_shb_t>0.35 & p_ant_t>0.35)$p_shb_t)
z4 <- c(filter(bgc_cln,p_shb_t>0.35 & p_ant_t>0.35)$hrt)

td_dat <- 

scatter3D_fancy_t <- function(x, y, z,..., colvar = x)
{
  panelfirst <- function(pmat) {
    # XY <- trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
    # scatter2D(XY$x, 
    #           XY$y, 
    #           col = ramp.col(c("#a6611a","#dfc27d","#008837")),
    #           colvar =x,
    #           pch = ".", 
    #           cex = 2, 
    #           add = TRUE, 
    #           colkey = FALSE)
    
    # XY <- trans3D(x, y =rep(max(y), length(y)), z, pmat = pmat)
    # scatter2D(XY$x, 
    #           XY$y, 
    #           col = ramp.col(c("#a6611a","#dfc27d","#008837")),
    #           colvar =x,
    #           pch = ".", 
    #           cex = 2, 
    #           add = TRUE, 
    #           colkey = FALSE)
    
    XY <- trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
    scatter2D(XY$x, 
              XY$y, 
              col = ramp.col(c("#dfc27d","#f5f5f5","#008837")),
              # col = ramp.col(c("#7b3294","#dfc27d","#008837")),
              colvar = x, 
              pch = ".", 
              cex = 2, 
              add = TRUE, 
              colkey = FALSE)
    
  }
  oldmar <- par("mar")
  par (mar = par("mar"))
  scatter3D(x1, y1, z1, ..., colvar = x, panel.first=panelfirst,
            col = ramp.col(c("#dfc27d","#f5f5f5","#008837")),
            colkey = list(length = 0.25, width = 1.5, cex.clab = 1.0, side = 1))
  scatter3D(x2, y2, z2, ..., colvar = x2, panel.first=NULL,
            col = ramp.col(c("#7b3294","#dfc27d","#008837")),
            add = TRUE,
            colkey = NULL)
  scatter3D(x3, y3, z3, ..., colvar = x3, panel.first=NULL,
            col = ramp.col(c("#7b3294","#dfc27d","#008837")),
            add = TRUE,
            colkey = NULL)
par(mar = oldmar)
}

scatter3D_fancy_t(x1, y1, z1, 
                clab = c("Forestcapes cover","(as a fraction)"),
                ylab = "Shrubscapes (fraction)",
                xlab = "Forestcapes (fraction)",
                zlab = c("Landscape heterogeneity", "(Shannon's entropy)"),
                bty = "g",
                alpha = 0.5,
                # main = "landscape entropy",
                colvar = x,
                # col = ramp.col(c("#dfc27d","#f5f5f5","#008837")),
                # col = ramp.col(c("#7b3294","#dfc27d","#008837")),
                expand = 0.5,
                theta = 35,
                phi =10,
                pch = 20,
                cex =1.0,
                ticktype = "detailed")


scatter3D(x, y, z, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed", expand = 0.5,
          theta = 35,alpha=0.01)
scatter3D(x1, y1, z1, add = TRUE, colkey = FALSE, 
          pch = 19, cex = 0.5, col = "black")
scatter3D(x2, y2, z2, add = TRUE, colkey = FALSE, 
          pch = 19, cex = 0.5, col = "black")
scatter3D(x3, y3, z3, add = TRUE, colkey = FALSE, 
          pch = 19, cex = 0.5, col = "black")
scatter3D(x4, y4, z4, add = TRUE, colkey = FALSE, 
          pch = 19, cex = 0.5, col = "black")






















































scatter3D_fancy <- function(x, y, z,..., colvar = x)
{
  panelfirst <- function(pmat) {
    # XY <- trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
    # scatter2D(XY$x, 
    #           XY$y, 
    #           col = ramp.col(c("#a6611a","#dfc27d","#008837")),
    #           colvar =x,
    #           pch = ".", 
    #           cex = 2, 
    #           add = TRUE, 
    #           colkey = FALSE)
    
    # XY <- trans3D(x, y =rep(max(y), length(y)), z, pmat = pmat)
    # scatter2D(XY$x, 
    #           XY$y, 
    #           col = ramp.col(c("#a6611a","#dfc27d","#008837")),
    #           colvar =x,
    #           pch = ".", 
    #           cex = 2, 
    #           add = TRUE, 
    #           colkey = FALSE)
    
    XY <- trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
    scatter2D(XY$x, 
              XY$y, 
              # col = ramp.col(c("#a6611a","#dfc27d","#008837")),
              col = ramp.col(c("#7b3294","#dfc27d","#008837")),
              colvar = x, 
              pch = ".", 
              cex = 2, 
              add = TRUE, 
              colkey = FALSE)
    
  }
  oldmar <- par("mar")
  par (mar = par("mar") + c(0, 0, 0, 0))
  scatter3D(x, y, z, ..., 
            colvar = x, 
            panel.first=panelfirst,
            col = ramp.col(c("#a6611a","#dfc27d","#008837")),
            colkey = list(length = 0.25, width = 1.5, cex.clab = 1.0, side = 1))
  
  scatter3D(x, y,
            z = rep(-max(z)/2, length.out = length(x)),
            colvar = colvar, col = ramp.col(c("#7b3294","#dfc27d","#008837")),
            add = TRUE, pch = 18, clab = NULL,
            colkey = list(length = 0.5, width = 0.5,
                          dist = 0.05, cex.axis = 0.8, cex.clab = 0.8)
  )
  par(mar = oldmar)
}

scatter3D_fancy(x, y, z, 
                clab = c("Forestcapes cover","(as a fraction)"),
                ylab = "Shrubscapes (fraction)",
                xlab = "Forestcapes (fraction)",
                zlab = c("Landscape heterogeneity", "(Shannon's entropy)"),
                bty = "g",
                alpha = 0.35,
                # main = "landscape entropy",
                colvar = y,
                # col = ramp.col(c("#7b3294","#dfc27d","#008837")),
                expand = 0.5,
                theta = 35,
                phi =10,
                pch = 20,
                cex =1.0,
                ticktype = "detailed")
















































bgc_plot0 <- na.omit(bgc_cln) %>% select(wshd_area,
                   acm_resp,
                   p_frt_t,
                   p_ant_t,
                   p_shb_t,
                   ent_cat) %>% 
  gather(c(3:5),key="use",value = "fraction")


p <- ggplot(bgc_plot0,aes(wshd_area,acm_resp,color=use))+
  geom_point(aes(alpha = fraction),size = 1.5)+
  facet_wrap(~ent_cat)+
  scale_x_log10(breaks = breaks, 
              labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks_c, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  scale_color_manual(values = c("#7b3294","#008837","#dfc27d"))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  geom_abline(slope=1.0, color = "red", linetype = "dotted")+
  theme_httn+
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=16),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold"))
p

ggsave(file="guerrero_etal_22_scaling_slopes_entropy.png",
       width = 15,
       height = 10,
       units = "in")

# We have several groups with different landscape composition between Q20 and Q60.
# I'm going to try to separate them using the reference line for 1:1 linear scaling
##################################################################################
#Formally, this needs to be done with an entropy classification algorithm
##################################################################################

bgc_cln <- bgc_cln %>% 
  group_by(COMID) %>% 
  mutate(wr_diff = round(acm_resp - wshd_area,digits = 2)) %>% 
  mutate(wr_sign = if_else(wr_diff!= 0, wr_diff/abs(wr_diff),0))

# I'm going to plot the data as in the previous figure (bare bones) and use ggplot.build to separate
# the different groups:

bgc_plot1 <- na.omit(bgc_cln) %>% select(COMID,
                                        wshd_area,
                                        acm_resp,
                                        p_frt_t,
                                        p_ant_t,
                                        p_shb_t,
                                        res_time,
                                        hrt,
                                        reach_slp,
                                        river_slp,
                                        hz_exchng,
                                        wr_sign,
                                        ent_cat) %>% 
  gather(c(4:6),key="use",value = "fraction")

bgc_plot1 <- bgc_plot1 %>% 
  mutate(sign_cat=if_else(wr_sign==0,"linear",if_else(wr_sign==1,"up","down"))) %>% 
  mutate(n_ent_cat=paste(ent_cat,sign_cat,sep = "-")) 
glimpse(bgc_plot1)

bgc_plot2 <- tibble(bgc_plot1 %>% 
  group_by(n_ent_cat) %>% 
  count(n_ent_cat))
glimpse(bgc_plot2)   

bgc_plot3 <- merge(bgc_plot1,bgc_plot2)
glimpse(bgc_plot3)

# Let's take a look at the new entropy categories

p <- ggplot(filter(bgc_plot3,n>150),aes(wshd_area, acm_resp,color=n_ent_cat))+
  geom_point(alpha = 0.05)+
  geom_smooth(method = "lm", fullrange = TRUE)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(slope = 1)+
  facet_wrap(~ent_cat)
p

# Let's use some resampling to calculate the distributions of the scaling exponents

# first, we need to spread the dataset:

bgc_rsm <- spread(bgc_plot3,use,fraction)






# Number of iterations 
rtc = levels(bgc_cln$srt_cat)
cls = 12
itn = 1:1000

ncols = 11
nrows = length(itn)
ssz = 100


# Make an empty tibble with all the column headers, plus the iteration.
scl_results <- tibble(category = as.character(),
                      iteratn = as.numeric(),
                      wsa_avg = as.numeric(),
                      rsp_avg = as.numeric(),
                      hzt_avg = as.numeric(),
                      ant_avg = as.numeric(),
                      frt_avg = as.numeric(),
                      shb_avg = as.numeric(),
                      hrt_avg = as.numeric(),
                      scl_exp = as.numeric(),
                      scl_int = as.numeric(),
                      pvl_exp = as.numeric(),
                      pvl_mod = as.numeric(),
                      r_sqard = as.numeric(),
                      rch_slp = as.numeric(),
                      riv_slp = as.numeric(),
                      res_mdn = as.numeric())

for (i in rtc){
  for( j in itn){
    dat <- bgc_cln %>% filter(srt_cat==i)
    tst = dat[sample(nrow(dat),size=ssz,replace = TRUE),]
    sm <- lm(log(acm_resp)~log(wshd_area),data =tst)
    st <- summary(sm)
    rsq <- as.numeric(st$r.squared)
    int <- as.numeric(sm$coefficients[1])
    slp <- as.numeric(sm$coefficients[2])
    mrs <- as.numeric(median(sm$residuals))
    p_slp <- as.numeric(st$coefficients[2,4])
    p_mod <- as.numeric(pf(st$fstatistic[1],         
                           st$fstatistic[2], 
                           st$fstatistic[3], 
                           lower.tail = FALSE))
    wsa <- mean(tst$wshd_area)
    rsp <- mean(tst$acm_resp)
    hzt <- mean(tst$res_time)
    ant <- mean(tst$p_ant_t)
    frt <- mean(tst$p_frt_t)
    shb <- mean(tst$p_shb_t)
    hrt <- mean(tst$hrt)
    ssl <- mean(tst$reach_slp)
    rsl <- mean(tst$river_slp)
    row <- tibble(category = i, 
                  iteratn =j, 
                  wsa_avg = wsa,
                  rsp_avg = rsp,
                  hzt_avg = hzt,
                  ant_avg = ant,
                  frt_avg = frt,
                  shb_avg = shb,
                  hrt_avg = hrt,
                  scl_int = int,
                  scl_exp = slp,
                  pvl_exp = p_slp,
                  pvl_mod = p_mod,
                  r_sqard = rsq,
                  rch_slp = rsp,
                  riv_slp = rsl,
                  res_mdn = mrs)
    scl_results <- scl_results %>% add_row(row)
  }
}























































  wsa <- mean(tst$wshd_area)
rsp <- mean(tst$acm_resp)
hzt <- mean(tst$res_time)
ant <- mean(tst$p_ant_t)
frt <- mean(tst$p_frt_t)
shb <- mean(tst$p_shb_t)
hrt <- mean(tst$hrt)
ssl <- mean(tst$reach_slp)
rsl <- mean(tst$river_slp)

p <- ggplot(bgc_plot,aes(wshd_area,acm_resp))+
  geom_point(alpha = 0.5)+
  facet_wrap(~ent_cat)+
  scale_x_log10()+
  scale_y_log10()+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  geom_abline(slope=1.0, color = "red", linetype = "dotted")
p

g_dat <- ggplot_build(p)# complete dataset for the plot
p_dat <- g_dat[1]$data# dataset containing information about the panels
b_dat <- g_dat[3]$plot$data# the actual dataset as organized within the plot

# relationship between residence time and slope
p <- ggplot(bgc_cln,aes(reach_slp,res_time))+
  geom_point()+
  geom_smooth()
p

# Calculating slopes, intercepts, and regression significance for multiple levels
# of residence time

# Creating a matrix for results

# Stratified Resampling

stratified <- bgc_cln %>% 
  group_by(srt_cat) %>% 
  slice_sample(n=ssz,replace=TRUE) %>% 
  mutate(slope = as.numeric(summary(lm(log(acm_resp)~log(wshd_area)))$coefficients[2]))

p <- ggplot(stratified,aes(hrt,slope))+
  # scale_x_log10()+
  # scale_y_log10()+
  geom_point()
p

stratified_a <- bgc_cln %>% 
  group_by(rt_cat) %>% 
  slice_sample(n=ssz,replace=TRUE) %>% 
  mutate(slope = as.numeric(summary(lm(log(acm_resp)~log(wshd_area)))$coefficients[2])) %>% 
  ggplot(aes(hrt,slope))+
  # scale_x_log10()+
  # scale_y_log10()+
  geom_point()
stratified_a



p <- ggplot(stratified,aes(wshd_area,acm_resp,color = srt_cat))+
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm",se=FALSE)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(slope=1,color = "red", linetype = "dashed", size = 1.2)+
  facet_wrap(~srt_cat)
p

gb <- ggplot_build(p)

# Number of iterations 
rtc = levels(bgc_cln$srt_cat)
cls = 12
itn = 1:1000

ncols = 11
nrows = length(itn)
ssz = 100


# Make an empty tibble with all the column headers, plus the iteration.
scl_results <- tibble(category = as.character(),
                          iteratn = as.numeric(),
                          wsa_avg = as.numeric(),
                          rsp_avg = as.numeric(),
                          hzt_avg = as.numeric(),
                          ant_avg = as.numeric(),
                          frt_avg = as.numeric(),
                          shb_avg = as.numeric(),
                          hrt_avg = as.numeric(),
                          scl_exp = as.numeric(),
                          scl_int = as.numeric(),
                          pvl_exp = as.numeric(),
                          pvl_mod = as.numeric(),
                          r_sqard = as.numeric(),
                          rch_slp = as.numeric(),
                          riv_slp = as.numeric(),
                          res_mdn = as.numeric())

for (i in rtc){
  for( j in itn){
    dat <- bgc_cln %>% filter(srt_cat==i)
    tst = dat[sample(nrow(dat),size=ssz,replace = TRUE),]
    sm <- lm(log(acm_resp)~log(wshd_area),data =tst)
    st <- summary(sm)
    rsq <- as.numeric(st$r.squared)
    int <- as.numeric(sm$coefficients[1])
    slp <- as.numeric(sm$coefficients[2])
    mrs <- as.numeric(median(sm$residuals))
    p_slp <- as.numeric(st$coefficients[2,4])
    p_mod <- as.numeric(pf(st$fstatistic[1],         
                           st$fstatistic[2], 
                           st$fstatistic[3], 
                           lower.tail = FALSE))
    wsa <- mean(tst$wshd_area)
    rsp <- mean(tst$acm_resp)
    hzt <- mean(tst$res_time)
    ant <- mean(tst$p_ant_t)
    frt <- mean(tst$p_frt_t)
    shb <- mean(tst$p_shb_t)
    hrt <- mean(tst$hrt)
    ssl <- mean(tst$reach_slp)
    rsl <- mean(tst$river_slp)
    row <- tibble(category = i, 
                  iteratn =j, 
                  wsa_avg = wsa,
                  rsp_avg = rsp,
                  hzt_avg = hzt,
                  ant_avg = ant,
                  frt_avg = frt,
                  shb_avg = shb,
                  hrt_avg = hrt,
                  scl_int = int,
                  scl_exp = slp,
                  pvl_exp = p_slp,
                  pvl_mod = p_mod,
                  r_sqard = rsq,
                  rch_slp = rsp,
                  riv_slp = rsl,
                  res_mdn = mrs)
   scl_results <- scl_results %>% add_row(row)
  }
}

scl_lines <- scl_results %>% 
  select(category,scl_exp,scl_int,r_sqard,pvl_exp,pvl_mod,hzt_avg) %>% 
  filter(r_sqard>0.8 & pvl_mod < 0.001) %>% 
  group_by(category) %>% 
  summarise(m_scl_exp = mean(scl_exp),
            m_scl_int = mean(scl_int),
            m_rsq = mean(r_sqard),
            m_pex = mean(pvl_exp),
            m_pmd = mean(pvl_mod),
            m_hzt = mean(hzt_avg))

p_exp <- ggplot(scl_lines,aes(m_hzt,m_scl_exp))+
  geom_point()+
  geom_smooth(span = 0.95)+
  xlab("Average Residence time (1k-resampling)")+
  ylab("Average Scaling Exponent (1k-resampling)")+
  geom_hline(yintercept = 1.0,linetype = "dashed", size = 1.0, color = "red")
p_exp

p_int <- ggplot(scl_lines,aes(m_hzt,m_scl_int))+
  geom_point()+
  geom_smooth(span = 0.95)+
  xlab("Average Residence time (1k-resampling)")+
  ylab("Average Intercept (1k-resampling)")+
  geom_hline(yintercept = 0.0,linetype = "dashed", size = 1.0, color = "red")
p_int

p_int <- ggplot(scl_lines,aes(m_hzt,m_scl_exp))+
  geom_point()+
  geom_smooth()
p_int


p_mres <- ggplot(scl_results,aes(x = category, y = scl_exp))+
  geom_boxplot()+
  geom_point(aes(alpha=0.002))+
  geom_smooth()+
  scale_y_log10()+
  # scale_color_manual(values = my_dcolors)+
  geom_hline(yintercept = 1,color="red")
p_mres
  
  
  
###############################################################################
#FIGURES
###############################################################################
# Creating breaks for logarithmic scale 
# (see: https://r-graphics.org/recipe-axes-axis-log)

breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))


 p0 <- ggplot(bgc_cln,aes(wshd_area, acm_resp, color = rt_cat))+
     # geom_point()+
     geom_smooth(method = "lm", se=FALSE)+
     # geom_point(aes(alpha = hrt), size = 2.5)+
     # geom_smooth(data = filter(rgl_dat,rsd>0 & acm_resp>100),aes(wshd_area,acm_resp),
     #             method = "lm")+
     # geom_smooth(data = filter(rgl_dat,rsd<0 & wshd_area>50),aes(wshd_area,acm_resp),
     #             method = "lm")+
     scale_x_log10(breaks = breaks, minor_breaks = minor_breaks, 
                                     labels = trans_format("log10", math_format(10^.x)))+
     scale_y_log10(breaks = breaks, minor_breaks = minor_breaks,
                                     labels = trans_format("log10", math_format(10^.x)))+
     xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
     ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
     annotation_logticks(size = 0.75, sides = "tblr")+
     # scale_color_distiller("Hyporheic\nexchange",palette = "Blues",direction = 1,
     #                       breaks = c(0.0,0.25,0.50, 0.75,1.0), limits =c(0,1))+
     geom_abline(slope =0.85,yintercept = 10000, size = 0.75, linetype = "dashed", color="red")+
     geom_vline(xintercept = 0.01, linetype = "dotted")+
     geom_hline(yintercept = 0.01, linetype = "dotted")+
     guides(alpha = "none")+
     theme_httn+
     theme(axis.text=element_text(colour="black",size=16),
                     axis.title = element_text(size = 24, face = "bold"),
                     legend.text = element_text(size = 12),
                     legend.title = element_text(face="bold", size = 16),
                     plot.background = element_blank())
p0


p <- ggplot(scl_results,aes(hzt_avg,scl_exp))+
  geom_point()+
  geom_smooth()
p


p1 <- ggplot(bgc_cln,aes(wshd_area, acm_resp, color = hrt))+
  geom_point()+
  geom_point(aes(alpha = hrt), size = 2.5)+
  scale_x_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  scale_color_distiller("Landscape\nheterogeneity",palette = "Blues",direction = 1,
                        breaks = c(0.0,0.25,0.50, 0.75,1.0), limits =c(0,1))+
  geom_abline(slope =1.00, size = 0.75, linetype = "dashed", color="red")+
  geom_smooth()
  # geom_abline(slope =1.02, intercept =  2.63, size = 0.75, linetype = "dashed")+
  # geom_abline(slope =1.33, intercept =  0.88, size = 0.75, linetype = "dashed")+
  # geom_abline(slope =1.61, intercept = -0.70, size = 0.75, linetype = "dashed")+
  # geom_abline(slope =1.77, intercept = -1.68, size = 0.75, linetype = "dashed")+
  # geom_abline(slope =1.94, intercept = -4.18, size = 0.75, linetype = "dashed")+
  # geom_abline(slope =2.04, intercept = -6.72, size = 0.75, linetype = "dashed")+
  # geom_abline(slope =2.09, intercept = -5.32, size = 0.75, linetype = "dashed")+
  # geom_abline(slope =2.14, intercept = -7.85, size = 0.75, linetype = "dashed")+
  guides(alpha = "none")+
  theme_httn+
  theme(axis.text=element_text(colour="black",size=16),
        axis.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face="bold", size = 16),
        plot.background = element_blank())
p1

ggsave(file="guerrero_etal_22_scaling_landscape_entropy.png",
       width = 10,
       height = 10,
       units = "in")

# Forestcapes

p2 <- ggplot(filter(bgc_lnd,acm_resp>0.0015),aes(wshd_area, acm_resp, color = p_frt_t))+
  geom_point(aes(alpha = p_frt_t), size = 2.5)+
  scale_x_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  scale_color_distiller("Forestcapes\ncover",palette = "Greens",direction = 1,
                        breaks = c(0.0,0.25,0.50, 0.75,1.0), limits =c(0,1))+
  geom_abline(slope =1, size = 0.75, linetype = "dashed", color="red")+
  geom_vline(xintercept = 0.01, linetype = "dotted")+
  geom_hline(yintercept = 0.01, linetype = "dotted")+
  guides(alpha = "none", reverse = TRUE)+
  theme_httn+
  theme(axis.text=element_text(colour="black",size=16),
        axis.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face="bold", size = 16),
        plot.background = element_blank())
p2

ggsave(file="guerrero_etal_22_scaling_forestcapes_cover.png",
       width = 10,
       height = 10,
       units = "in")


# Shurblandscapes

p3 <- ggplot(filter(bgc_lnd,acm_resp>0.0015),aes(wshd_area, acm_resp, color = p_shb_t))+
  geom_point(aes(alpha = p_shb_t), size = 2.5)+
  scale_x_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  scale_color_distiller("Shurblandcapes\ncover",palette = "Oranges",direction = 1,
                        breaks = c(0.0,0.25,0.50, 0.75,1.0), limits =c(0,1))+
  geom_abline(slope =1, size = 0.75, linetype = "dashed", color="red")+
  geom_vline(xintercept = 0.01, linetype = "dotted")+
  geom_hline(yintercept = 0.01, linetype = "dotted")+
  guides(alpha = "none", reverse = TRUE)+
  theme_httn+
  theme(axis.text=element_text(colour="black",size=16),
        axis.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face="bold", size = 16),
        plot.background = element_blank())
p3

ggsave(file="guerrero_etal_22_scaling_shrublandcapes_cover.png",
       width = 10,
       height = 10,
       units = "in")

# Humanscapes

p4 <- ggplot(filter(bgc_lnd,acm_resp>0.0015),aes(wshd_area, acm_resp, color = p_ant_t))+
  geom_point(aes(alpha = p_ant_t), size = 2.5)+
  scale_x_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  scale_color_distiller("Humanscapes\ncover",palette = "Purples",direction = 1,
                        breaks = c(0.0,0.25,0.50, 0.75,1.0), limits =c(0,1))+
  geom_abline(slope =1, size = 0.75, linetype = "dashed", color="red")+
  geom_vline(xintercept = 0.01, linetype = "dotted")+
  geom_hline(yintercept = 0.01, linetype = "dotted")+
  guides(alpha = "none", reverse = TRUE)+
  theme_httn+
  theme(axis.text=element_text(colour="black",size=16),
        axis.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face="bold", size = 16),
        plot.background = element_blank())
p4

ggsave(file="guerrero_etal_22_scaling_humanscapes_cover.png",
       width = 10,
       height = 10,
       units = "in")


# # Entropy plot
# 
# with(dat,scatter3D(tot_other_3,tot_ntrl_3,h_rel_3,bty ='b2',
#                    colvar = as.integer(lnd_cat),
#                    clab=c("Relative","Entropy"), 
#                    theta = 15, phi =20, main = "Landscape Heterogeneity", 
#                    xlab = "Mixed cover", ylab = "Natural cover",
#                    zlab = "Relative entropy"))
# plotrgl()