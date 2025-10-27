##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
########################################## FIGURE 2/SUPP 2 ########################################## 
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################

##########################################
#########################################
############ IND FEC ####################
##########################################
#########################################

nonbagged20 <- read.csv("wormsindfec20_nonbagged.csv", stringsAsFactors = TRUE)
nonbagged25 <- read.csv("wormsindfec25_nonbagged.csv", stringsAsFactors = TRUE)


two.way <- aov(total_progeny ~ strain_bac, data = nonbagged20)
summary(two.way)
# Df  Sum Sq Mean Sq F value   Pr(>F)    
# strain_bac   3 1106323  368774   26.58 1.38e-08 ***
#   Residuals   30  416288   13876                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
# 20NZK35_OP50-20NKZ35_WOUb2   51.08889 -127.5684  229.746225 0.8639772
# 20fog2_WOUb2-20fog2_OP50    276.30000  133.0555  419.544545 0.0000662 ************************

nonbagged20_fog2<-filter(nonbagged20, strain == "fog-2")
nonbagged20_fog2_OP50<-filter(nonbagged20_fog2, bacterial_strain == "E. coli OP50")
nonbagged20_fog2_WOUb2<-filter(nonbagged20_fog2, bacterial_strain == "Klebsiella sp. WOUb2")
cohen.d(nonbagged20_fog2_OP50$total_progeny, nonbagged20_fog2_WOUb2$total_progeny)
# Cohen's d
# 
# d estimate: -1.882235 (large)
# 95 percent confidence interval:
#     lower     upper 
# -3.010823 -0.753646 


two.way <- aov(total_progeny ~ strain_bac, data = nonbagged25)
summary(two.way)
# Df  Sum Sq Mean Sq F value   Pr(>F)    
# strain_bac   3  790192  263397   16.68 1.07e-08 ***
#   Residuals   90 1421505   15795                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
# 25NZK35_WOUb2-25NZK35_OP50   28.61353  -74.91558  132.14263 0.8874808
# 25fog2_WOUb2-25fog2_OP50    178.89286   88.36971  269.41600 0.0000082  **********

nonbagged25_fog2<-filter(nonbagged25, strain == "fog-2")
nonbagged25_fog2_OP50<-filter(nonbagged25_fog2, bacterial_strain == "E. coli OP50")
nonbagged25_fog2_WOUb2<-filter(nonbagged25_fog2, bacterial_strain == "Klebsiella sp. WOUb2")
cohen.d(nonbagged25_fog2_OP50$total_progeny, nonbagged25_fog2_WOUb2$total_progeny)
# Cohen's d
# 
# d estimate: -1.141961 (large)
# 95 percent confidence interval:
#   lower      upper 
# -1.7375632 -0.5463579



##########################################
#########################################
############ DEV ####################
##########################################
#########################################


dev20 <- read.csv("dev20C.csv", stringsAsFactors = TRUE)
dev25 <- read.csv("dev25C.csv", stringsAsFactors = TRUE)


#20C
two.way <- aov(midpoint ~ strain_bac, data = dev20)
summary(two.way)
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = midpoint ~ strain_bac, data = dev20)
# 
# $strain_bac
# diff        lwr      upr     p adj
# 20fog2_WOUb2-20fog2_OP50 -8.076022 -17.749034  1.59699 0.1337445
# 20NKZ_WOUb2-20NKZ_OP50    4.744835  -6.449501 15.93917 0.6803187





#25C
two.way <- aov(midpoint ~ strain_bac, data = dev25)
summary(two.way)
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
#   Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = midpoint ~ strain_bac, data = dev25)
# 
# $strain_bac
# diff       lwr        upr     p adj
# 25fog2_WOUb2-25fog2_OP50 -11.419118 -17.29584 -5.5423936 0.0000219 ************
# 25NKZ_WOUb2-25NKZ_OP50    -8.236547 -16.00190 -0.4711904 0.0336247 ************

fog225<-filter(dev25, strain == "fog-2")
fog225_OP50<-filter(fog225, bacterial_strain == "E. coli OP50")
fog225_WOUb2<-filter(fog225, bacterial_strain == "Klebsiella sp. WOUb2")
cohen.d(fog225_OP50$midpoint, fog225_WOUb2$midpoint)
# Cohen's d
# 
# d estimate: 2.452585 (large)
# 95 percent confidence interval:
#    lower    upper 
# 1.552771 3.352399 

NKZ3525<-filter(dev25, strain == "NZK35")
NKZ3525_OP50<-filter(NKZ3525, bacterial_strain == "E. coli OP50")
NKZ3525_WOUb2<-filter(NKZ3525, bacterial_strain == "Klebsiella sp. WOUb2")
cohen.d(NKZ3525_OP50$midpoint, NKZ3525_WOUb2$midpoint)
# Cohen's d
# 
# d estimate: 0.914613 (large)
# 95 percent confidence interval:
#       lower       upper 
# -0.05359841  1.88282437 






##########################################
#########################################
############ VIA ####################
##########################################
#########################################


via20 <- read.csv("via20.csv", stringsAsFactors = TRUE)
via25 <- read.csv("via25.csv", stringsAsFactors = TRUE)

#### STATS ####
#20C
two.way <- aov(frac_via ~ strain_bac, data = via20)
summary(two.way)
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = frac_via ~ strain_bac, data = via20)
# 
# $strain_bac
# diff        lwr         upr     p adj
# 20fog2_WOUb2-20fog2_OP50  0.01362566 -0.1806973  0.207948626 0.9975025
# 20NKZ_WOUb2-20NKZ_OP50   -0.03099230 -0.2558774  0.193892831 0.9818218


#25C
two.way <- aov(frac_via ~ strain_bac, data = via25)
summary(two.way)
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = frac_via ~ strain_bac, data = via25)
# 
# $strain_bac
# diff         lwr           upr     p adj
# 25fog2_WOUb2-25fog2_OP50  0.02559245 -0.1201513 0.17133615 0.9621616
# 25N2_WOUb2-25N2_OP50     -0.034094010 -0.15445464  0.0862666192 0.9572065
# 25NKZ_WOUb2-25NKZ_OP50   -0.02207060 -0.2146527 0.17051148 0.9888857








##########################################
#########################################
############ END REP ####################
##########################################
#########################################


worms20_nonbagged_endrep <- read.csv("worms20_nonbagged_endrep.csv", stringsAsFactors = TRUE)
View(worms20_nonbagged_endrep)
worms25_nonbagged_endrep <- read.csv("worms25_nonbagged_endrep.csv", stringsAsFactors = TRUE)


two.way <- aov(end_rep ~ strain_bac, data = worms20_nonbagged_endrep)
summary(two.way)
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
# diff        lwr          upr     p adj
# 20NZK35_OP50-20NKZ35_WOUb2  3.2 -0.2284269 6.628427 0.0741537 
# 20fog2_WOUb2-20fog2_OP50    1.3 -1.4488569 4.048857 0.5787282


two.way <- aov(end_rep ~ strain_bac, data = worms25_nonbagged_endrep)
summary(two.way)
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
# diff       lwr        upr     p adj
# 25NZK35_WOUb2-25NZK35_OP50 -0.2874396 -1.7231406 1.148261 0.9530683
# 25fog2_WOUb2-25fog2_OP50    0.7785714 -0.4767679 2.033911 0.3707364







##########################################
#########################################
############ BODY SIZE ####################
##########################################
#########################################



dat_ele <- read.csv("dat_ele.csv", stringsAsFactors = TRUE)
View(dat_ele)
dat_ino <- read.csv("dat_ino.csv", stringsAsFactors = TRUE)


two.way <- aov(length_microns ~ sp_bac_sex, data = dat_ele)
summary(two.way)
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
# 95% family-wise confidence level
# 
# Fit: aov(formula = length_microns ~ sp_bac_sex, data = dat_ele)
# 
# $sp_bac_sex
# diff        lwr       upr    p adj
# E_R_fem-E_OP50_female      206.6775  158.52707  254.8280 0.00e+00 *****************
# E_R_male-E_OP50_male       103.1752   47.46907  158.8814 3.16e-05

two.way <- aov(length_microns ~ sp_bac_sex, data = dat_ino)
summary(two.way)
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
# 95% family-wise confidence level
# 
# Fit: aov(formula = length_microns ~ sp_bac_sex, data = dat_ino)
# 
# $sp_bac_sex
# diff        lwr       upr     p adj
# I_R_fem-I_OP50_female       51.63150  -66.78188  170.0449 0.6623970
# I_R_male-I_OP50_male        76.16265  -45.17496  197.5003 0.3575602





############################################################################################################################
##############################################################################################################################
##############################################################################################################################
########################################## Supp Fig 3 ########################################## 
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################



N2F2_20<-read.csv("N2F2_20.csv", header = T)
N2F2_25<-read.csv("N2F2_25.csv", header = T)

two.way <- aov(total_progeny ~ strain_bac, data = N2F2_20)
summary(two.way)
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = total_progeny ~ strain_bac, data = N2F2_20)
# 
# $strain_bac
# diff        lwr        upr     p adj
# 20fog2_WOUb2-20fog2_OP50      276.30000  169.28228  383.31772 0.0000000 ******************
# 20PD1074_WOUb2-20PD1074_OP50   11.24444  -52.23537   74.72425 0.9662932


two.way <- aov(total_progeny ~ strain_bac, data = N2F2_25)
summary(two.way)
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = total_progeny ~ strain_bac, data = N2F2_25)
# 
# $strain_bac
# diff        lwr        upr     p adj
# 25fog2_WOUb2-25fog2_OP50      178.89286   94.86353  262.92218 0.0000012 *******************
# 25PD1074_WOUb2-25PD1074_OP50   23.51894  -58.40612  105.44400 0.8767549










