######## Libraries #########

library(ggforce) 
# citation("ggforce")
# Pedersen T (2022). ggforce: Accelerating 'ggplot2'. R package version 0.4.1,
# <https://CRAN.R-project.org/package=ggforce>.
#### USED FOR SINA PLOT!!!

library(cowplot)
# citation("cowplot")
# Wilke C (2020). cowplot: Streamlined Plot Theme and Plot Annotations for 'ggplot2'. R package
# version 1.1.1, <https://CRAN.R-project.org/package=cowplot>.
#### USED FOR THEME IN PLOTS!! 

library(reshape2)
# citation("reshape2")
# Hadley Wickham (2007). Reshaping Data with the reshape Package. Journal of Statistical Software,
# 21(12), 1-20. URL http://www.jstatsoft.org/v21/i12/.
#### USED TO MELT DATA FROM WIDE TO LONG

library(tidyverse)
# citation("tidyverse")
# Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L,
# Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu
# V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal
# of Open Source Software_, *4*(43), 1686. doi:10.21105/joss.01686
# <https://doi.org/10.21105/joss.01686>.

# GGPLOT2 REFERENCE 
## H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.

# TIDYR RFERENCE
## Wickham H, Vaughan D, Girlich M (2023). _tidyr: Tidy Messy Data. R package version 1.3.0,
## <https://CRAN.R-project.org/package=tidyr>.

# DPLYR REFERENCE
## Wickham H, François R, Henry L, Müller K, Vaughan D (2023). _dplyr: A Grammar of Data Manipulation. R package version 1.1.3, <https://CRAN.R-project.org/package=dplyr>.

library(ggsignif)

library(effsize)
# citation("effsize")
# Torchiano M (2020). _effsize: Efficient Effect Size Computation_. doi:10.5281/zenodo.1480624
# <https://doi.org/10.5281/zenodo.1480624>, R package version 0.8.1,
# <https://CRAN.R-project.org/package=effsize>.
#### USED FOR COHENS D

library(lemon)
# citation("lemon")
# Edwards S (2023). _lemon: Freshing Up your 'ggplot2' Plots_. R package version 0.4.7,
# <https://CRAN.R-project.org/package=lemon>.

library(ggpubr)
# citation("ggpubr")
# Kassambara A (2023). _ggpubr: 'ggplot2' Based Publication Ready Plots_. R package version
# 0.6.0, <https://CRAN.R-project.org/package=ggpubr>.


library(lmtest)
# install.packages("lmtest")
# citation("lmtest")
# Achim Zeileis, Torsten Hothorn (2002). Diagnostic Checking in Regression Relationships. R
# News 2(3), 7-10. URL https://CRAN.R-project.org/doc/Rnews/


library(lsr)
# install.packages("lsr")
# citation("lsr")
# Navarro, D. J. (2015) Learning statistics with R: A tutorial for psychology students and
# other beginners. (Version 0.6) University of New South Wales. Sydney, Australia


library(RColorBrewer)
brewer.pal(2, "Set1")
#[1] "#E41A1C" "#377EB8" "#4DAF4A"


install.packages("patchwork")
library(patchwork)




###################################################################################################################################################
###################################################################################################################################################
################# INDIVIDUAL FECUNDITY ###########################################################################################################
###################################################################################################################################################
###################################################################################################################################################

### Importing inopinata data ###
ino <- read.csv("woub2_ind_fec_ci_total.csv", stringsAsFactors = TRUE)
View(ino)

# Assigning inopinata data to an object to remove NA's in the "day bagged" column
dat_na_zero_ino <- ino

# Grabbing subset of data with Na's then putting a zero into those spots 
dat_na_zero_ino[is.na(dat_na_zero_ino)] <- 0

# Creating a "total progeny" column and adding up the total progeny for an individual reproductive female
dat_na_zero_ino$total_progeny <- dat_na_zero_ino$progeny_day_0 +
  dat_na_zero_ino$progeny_day_1 +
  dat_na_zero_ino$progeny_day_2 + 
  dat_na_zero_ino$progeny_day_3 + 
  dat_na_zero_ino$progeny_day_4 + 
  dat_na_zero_ino$progeny_day_5 + 
  dat_na_zero_ino$progeny_day_6 + 
  dat_na_zero_ino$progeny_day_7 + 
  dat_na_zero_ino$progeny_day_8 +
  dat_na_zero_ino$progeny_day_9 + 
  dat_na_zero_ino$progeny_day_10 + 
  dat_na_zero_ino$progeny_day_11 + 
  dat_na_zero_ino$progeny_day_12 + 
  dat_na_zero_ino$progeny_day_13 + 
  dat_na_zero_ino$progeny_day_14 + 
  dat_na_zero_ino$progeny_day_15 + 
  dat_na_zero_ino$progeny_day_16

# selecting necessary variables to create a figure with
dat_na_zero_ino <- dat_na_zero_ino %>% 
  select(plate_id, bacterial_strain, species, strain, strain_bac, total_progeny, temperature, status)



### Importing elegans data ###
ele <- read.csv("woub2_ind_fec_ce_total.csv", stringsAsFactors = TRUE)
View(ele)

# Assigning elegans data to an object to remove NA's in the "day bagged" column
dat_na_zero_ele <- ele

# Grabbing subset of data with Na's then putting a zero into those spots 
dat_na_zero_ele[is.na(dat_na_zero_ele)] <- 0

# Creating a "total progeny" column and adding up the total progeny for an individual reproductive female
dat_na_zero_ele$total_progeny <- dat_na_zero_ele$progeny_day_0 +
  dat_na_zero_ele$progeny_day_1 +
  dat_na_zero_ele$progeny_day_2 + 
  dat_na_zero_ele$progeny_day_3 + 
  dat_na_zero_ele$progeny_day_4 + 
  dat_na_zero_ele$progeny_day_5 + 
  dat_na_zero_ele$progeny_day_6 + 
  dat_na_zero_ele$progeny_day_7 + 
  dat_na_zero_ele$progeny_day_8 + 
  dat_na_zero_ele$progeny_day_9 + 
  dat_na_zero_ele$progeny_day_10 + 
  dat_na_zero_ele$progeny_day_11 + 
  dat_na_zero_ele$progeny_day_12


# selecting necessary variables to create a figure with
dat_na_zero_ele <- dat_na_zero_ele %>% 
  select(plate_id, bacterial_strain, strain, species, strain_bac, total_progeny, temperature, status)

# binding datasets together 
wormsindfec <- rbind(dat_na_zero_ino, dat_na_zero_ele)
View(wormsindfec)

# total bagged and non-bagged
wormsindfec_all <- rbind(dat_na_zero_ino, dat_na_zero_ele)
View(wormsindfec_all)

# Bagged worms
wormsindfec_bagged <- filter(wormsindfec, status == "bagged")
View(wormsindfec_bagged)

#Non-bagged worms
wormsindfec_nonbagged<-filter(wormsindfec, status != "bagged")
View(wormsindfec_nonbagged)

### filtering temps for all three sets of data ###
# all
wormsindfec20_all<-filter(wormsindfec_all, temperature == "20")
View(wormsindfec20_all)
write.csv(wormsindfec20_all, "wormsindfec20_all")
wormsindfec25_all<-filter(wormsindfec_all, temperature == "25")
View(wormsindfec25_all)
write.csv(wormsindfec25_all, "wormsindfec25_all")
#non-bagged
wormsindfec20_nonbagged<-filter(wormsindfec_nonbagged, temperature == "20")
View(wormsindfec20_nonbagged)
write.csv(wormsindfec20_nonbagged, "wormsindfec20_nonbagged")
wormsindfec25_nonbagged<-filter(wormsindfec_nonbagged, temperature == "25")
View(wormsindfec25_nonbagged)
write.csv(wormsindfec25_nonbagged, "wormsindfec25_nonbagged")
#bagged
wormsindfec20_bagged<-filter(wormsindfec_bagged, temperature == "20")
View(wormsindfec20_bagged)
write.csv(wormsindfec20_bagged, "wormsindfec20_bagged")
wormsindfec25_bagged<-filter(wormsindfec_bagged, temperature == "25")
View(wormsindfec25_bagged)
write.csv(wormsindfec25_bagged, "wormsindfec25_bagged")















###################################################################################################################################################
###################################################################################################################################################
################# DEVELOPMENTAL TIMING ############################################################################################################
###################################################################################################################################################
###################################################################################################################################################

setwd("/Users/austinlink/Desktop/DATANEW/DEVTIME")
# importing data
ci20 <- read.csv("ci20.csv", stringsAsFactors = TRUE)
ci25 <- read.csv("ci25.csv", stringsAsFactors = TRUE)
ce20 <- read.csv("ce20.csv", stringsAsFactors = TRUE)
ce25 <- read.csv("ce25_DEV.csv", stringsAsFactors = TRUE)
# Used this version of ce25 beacause I had to troubleshoot the file and delete the zeroes at the end for the code to work, so once the worms hit adulthood with no changes in this file, I trimmed everything after 

View(ci20)
View(ci25)
View(ce20)
View(ce25)


##### Establishing custom function ##########
func_log_inf_conf <- function(dat,x,y){
  
  f <- function (x) 1/(1+exp(-x))
  the_glm <- glm(y ~ x, family="binomial", data=dat)
  conf <- confint(the_glm)
  #the inflection point
  
  p <- 0.5
  q <- (log(p/(1-p)) - coef(the_glm)[1]) / coef(the_glm)[2]
  
  # confidence interval at inflection point
  upper_q <- (log(p/(1-p)) - conf[1,1]) / conf[2,1]
  lower_q <- (log(p/(1-p)) - conf[1,2]) / conf[2,2]
  
  #return
  ret <- c(q, lower_q, upper_q)
  return(ret)
}





########### CINO20 ###############


ci20$plate_number <- as.factor(ci20$plate_number)

# convert wide data to long data
dat_melt_ci20 <- melt(ci20,id.vars = c("plate_number","bacterial_strain", "species", "strain_bac", "temperature", "strain"))

# making a day column
dat_melt_ci20$hour <- 0

# correcting the day column for the long data, "hour 0" was the time of the egg lay 
dat_melt_ci20[dat_melt_ci20$variable == "embryos_hour_0",]$hour <- 0
dat_melt_ci20[dat_melt_ci20$variable == "embryos_hour_96",]$hour <- 96
dat_melt_ci20[dat_melt_ci20$variable == "embryos_hour_120",]$hour <- 120
dat_melt_ci20[dat_melt_ci20$variable == "embryos_hour_144",]$hour <- 144
dat_melt_ci20[dat_melt_ci20$variable == "embryos_hour_168",]$hour <- 168
dat_melt_ci20[dat_melt_ci20$variable == "embryos_hour_192",]$hour <- 192
dat_melt_ci20[dat_melt_ci20$variable == "larvae_hour_0",]$hour <- 0
dat_melt_ci20[dat_melt_ci20$variable == "larvae_hour_96",]$hour <- 96
dat_melt_ci20[dat_melt_ci20$variable == "larvae_hour_120",]$hour <- 120
dat_melt_ci20[dat_melt_ci20$variable == "larvae_hour_144",]$hour <- 144
dat_melt_ci20[dat_melt_ci20$variable == "larvae_hour_168",]$hour <- 168
dat_melt_ci20[dat_melt_ci20$variable == "larvae_hour_192",]$hour <- 192
dat_melt_ci20[dat_melt_ci20$variable == "L4_hour_0",]$hour <- 0
dat_melt_ci20[dat_melt_ci20$variable == "L4_hour_96",]$hour <- 96
dat_melt_ci20[dat_melt_ci20$variable == "L4_hour_120",]$hour <- 120
dat_melt_ci20[dat_melt_ci20$variable == "L4_hour_144",]$hour <- 144
dat_melt_ci20[dat_melt_ci20$variable == "L4_hour_168",]$hour <- 168
dat_melt_ci20[dat_melt_ci20$variable == "L4_hour_192",]$hour <- 192
dat_melt_ci20[dat_melt_ci20$variable == "adults_hour_0",]$hour <- 0
dat_melt_ci20[dat_melt_ci20$variable == "adults_hour_96",]$hour <- 96
dat_melt_ci20[dat_melt_ci20$variable == "adults_hour_120",]$hour <- 120
dat_melt_ci20[dat_melt_ci20$variable == "adults_hour_144",]$hour <- 144
dat_melt_ci20[dat_melt_ci20$variable == "adults_hour_168",]$hour <- 168
dat_melt_ci20[dat_melt_ci20$variable == "adults_hour_192",]$hour <- 192





# make stage column and then filling it with stages
dat_melt_ci20$stage <- "embryo"
dat_melt_ci20$stage <- factor(dat_melt_ci20$stage, levels=c("embryo", "larvae", "L4","Adult","Gravid Female"))


# correcting stages for each line
dat_melt_ci20[dat_melt_ci20$variable == "embryos_hour_0",]$stage <- "embryo"
dat_melt_ci20[dat_melt_ci20$variable == "embryos_hour_96",]$stage <- "embryo"
dat_melt_ci20[dat_melt_ci20$variable == "embryos_hour_120",]$stage <- "embryo"
dat_melt_ci20[dat_melt_ci20$variable == "embryos_hour_144",]$stage <- "embryo"
dat_melt_ci20[dat_melt_ci20$variable == "embryos_hour_168",]$stage <- "embryo"
dat_melt_ci20[dat_melt_ci20$variable == "embryos_hour_192",]$stage <- "embryo"
dat_melt_ci20[dat_melt_ci20$variable == "larvae_hour_0",]$stage <- "larvae"
dat_melt_ci20[dat_melt_ci20$variable == "larvae_hour_96",]$stage <- "larvae"
dat_melt_ci20[dat_melt_ci20$variable == "larvae_hour_120",]$stage <- "larvae"
dat_melt_ci20[dat_melt_ci20$variable == "larvae_hour_144",]$stage <- "larvae"
dat_melt_ci20[dat_melt_ci20$variable == "larvae_hour_168",]$stage <- "larvae"
dat_melt_ci20[dat_melt_ci20$variable == "larvae_hour_192",]$stage <- "larvae"
dat_melt_ci20[dat_melt_ci20$variable == "L4_hour_0",]$stage <- "L4"
dat_melt_ci20[dat_melt_ci20$variable == "L4_hour_96",]$stage <- "L4"
dat_melt_ci20[dat_melt_ci20$variable == "L4_hour_120",]$stage <- "L4"
dat_melt_ci20[dat_melt_ci20$variable == "L4_hour_144",]$stage <- "L4"
dat_melt_ci20[dat_melt_ci20$variable == "L4_hour_168",]$stage <- "L4"
dat_melt_ci20[dat_melt_ci20$variable == "L4_hour_192",]$stage <- "L4"
dat_melt_ci20[dat_melt_ci20$variable == "adults_hour_0",]$stage <- "Adult"
dat_melt_ci20[dat_melt_ci20$variable == "adults_hour_96",]$stage <- "Adult"
dat_melt_ci20[dat_melt_ci20$variable == "adults_hour_120",]$stage <- "Adult"
dat_melt_ci20[dat_melt_ci20$variable == "adults_hour_144",]$stage <- "Adult"
dat_melt_ci20[dat_melt_ci20$variable == "adults_hour_168",]$stage <- "Adult"
dat_melt_ci20[dat_melt_ci20$variable == "adults_hour_192",]$stage <- "Adult"




# rename value to be "number of worms"
colnames(dat_melt_ci20)[colnames(dat_melt_ci20) == 'value'] <- 'number_of_worms'


# Assigning data to a new object to preserve previous object 
filt_dat_ci20 <- dat_melt_ci20


#dcast(ID + seq ~ condition, data = DF, value.var = "Value")
filt_dat_ci20 <- subset(filt_dat_ci20, select = -variable)


#put underscore between "Gravid Female"
levels(filt_dat_ci20$stage)[levels(filt_dat_ci20$stage)=="Gravid Female"] <- "Gravid_Female"


filt_dat_cast_ci20 <- dcast(filt_dat_ci20, ...~stage,value.var="number_of_worms")


#copy to new variable
datb_no_NA_ci20 <- filt_dat_cast_ci20


#fill NA's with zero
datb_no_NA_ci20[is.na(datb_no_NA_ci20)] <- 0


##add L4 or older column


datb_no_NA_ci20$L4_older <- datb_no_NA_ci20$L4 + datb_no_NA_ci20$Adult


##add all worms per plate
datb_no_NA_ci20$total_worms_observed <- datb_no_NA_ci20$L4 + datb_no_NA_ci20$Adult + datb_no_NA_ci20$embryo + datb_no_NA_ci20$larvae


##fraction at each stage
datb_no_NA_ci20$fra_L1_L3 <- datb_no_NA_ci20$larvae/datb_no_NA_ci20$total_worms_observed
datb_no_NA_ci20$fra_L4_older <- datb_no_NA_ci20$L4_older/datb_no_NA_ci20$total_worms_observed
datb_no_NA_ci20$fra_adult <- datb_no_NA_ci20$Adult/datb_no_NA_ci20$total_worms_observed



##make variables to put the df's in
ad_coeff_df <- NULL
L4_coeff_df <- NULL


#for each plate, get the logistic model inflection point (median hour) for the timing of the developmental milestone (L4 or adult onset)
for (i in levels(datb_no_NA_ci20$plate_number)){
  the_plate <- datb_no_NA_ci20[datb_no_NA_ci20$plate_number == i,]
  
  #get minimum number of worms observed
  min_worms_plate <- min(the_plate$total_worms_observed)
  
  #normalize to minimum number of worms observed
  
  the_plate$L1_L3_norm <- round(the_plate$fra_L1_L3*min_worms_plate)
  the_plate$L4_older_norm <- round(the_plate$fra_L4_older*min_worms_plate)
  the_plate$total_adult_norm <- round(the_plate$fra_adult*min_worms_plate)
  
  #get the worms not at milestone
  the_plate$not_L4 <- min_worms_plate-the_plate$L4_older_norm
  the_plate$not_adult <- min_worms_plate-the_plate$total_adult_norm
  #na's are 0
  the_plate[is.na(the_plate)] <- 0
  #expand the number of rows according to number of worms at milestone-- each worm gets a row per time observed
  dev_data_expand_at_L4 <- the_plate[rep(1:nrow(the_plate), the_plate$L4_older_norm),]
  dev_data_expand_at_adult <- the_plate[rep(1:nrow(the_plate), the_plate$total_adult_norm),]
  
  #expand the number of rows according to number of worms NOT at milestone-- each worm gets a row per time observed
  
  dev_data_expand_NOT_at_L4 <- the_plate[rep(1:nrow(the_plate), the_plate$not_L4),]
  dev_data_expand_NOT_at_adult <- the_plate[rep(1:nrow(the_plate), the_plate$not_adult),]
  
  
  #add column of ones to hatched worms
  dev_data_expand_at_L4$milestone_status <- rep(1,nrow(dev_data_expand_at_L4))
  dev_data_expand_at_adult$milestone_status <- rep(1,nrow(dev_data_expand_at_adult))
  
  #add column of zeros to not hatched worms
  dev_data_expand_NOT_at_L4$milestone_status <- rep(0,nrow(dev_data_expand_NOT_at_L4))
  dev_data_expand_NOT_at_adult$milestone_status <- rep(0,nrow(dev_data_expand_NOT_at_adult))
  
  
  L4_data_expand <- rbind(dev_data_expand_at_L4, dev_data_expand_NOT_at_L4)
  Adult_data_expand <- rbind(dev_data_expand_at_adult, dev_data_expand_NOT_at_adult)
  
  L4_coeff <- func_log_inf_conf(L4_data_expand,L4_data_expand$hour,L4_data_expand$milestone_status)
  
  Adult_coeff <- func_log_inf_conf(Adult_data_expand,Adult_data_expand$hour,Adult_data_expand$milestone_status)
  
  L4_df_to_add <- data.frame(bacterial_strain=unique(as.character(L4_data_expand$bacterial_strain)), plate_number=unique(as.character(L4_data_expand$plate_number)),strain_bac=unique(as.character(L4_data_expand$strain_bac)),strain=unique(as.character(L4_data_expand$strain)), Milestone= "L4", midpoint=L4_coeff[1],midpoint_95_CI_low=L4_coeff[2],midpoint_95_CI_high=L4_coeff[3])
  
  Adult_df_to_add <- data.frame(bacterial_strain=unique(as.character(Adult_data_expand$bacterial_strain)), plate_number=unique(as.character(Adult_data_expand$plate_number)),strain_bac=unique(as.character(Adult_data_expand$strain_bac)),strain=unique(as.character(Adult_data_expand$strain)), Milestone= "Adult", midpoint=Adult_coeff[1],midpoint_95_CI_low=Adult_coeff[2],midpoint_95_CI_high=Adult_coeff[3])
  
  L4_coeff_df <- rbind(L4_coeff_df,L4_df_to_add)
  
  ad_coeff_df <- rbind(ad_coeff_df,Adult_df_to_add)
}

inopinata_df20 <- rbind(L4_coeff_df,ad_coeff_df)

inopinata_df20$species <- "C. inopinata"

inopinata_df20



########### CINO25 ###############

ci25$plate_number <- as.factor(ci25$plate_number)

# convert wide data to long data
dat_melt_ci25 <- melt(ci25,id.vars = c("plate_number","bacterial_strain", "species", "strain_bac", "temperature", "strain"))

# making a day column
dat_melt_ci25$hour <- 0

# correcting the day column for the long data, "hour 0" was the time of the egg lay 
dat_melt_ci25[dat_melt_ci25$variable == "embryos_hour_0",]$hour <- 0
dat_melt_ci25[dat_melt_ci25$variable == "embryos_hour_72",]$hour <- 72
dat_melt_ci25[dat_melt_ci25$variable == "embryos_hour_84",]$hour <- 84
dat_melt_ci25[dat_melt_ci25$variable == "embryos_hour_96",]$hour <- 96
dat_melt_ci25[dat_melt_ci25$variable == "embryos_hour_118",]$hour <- 118
dat_melt_ci25[dat_melt_ci25$variable == "larvae_hour_0",]$hour <- 0
dat_melt_ci25[dat_melt_ci25$variable == "larvae_hour_72",]$hour <- 72
dat_melt_ci25[dat_melt_ci25$variable == "larvae_hour_84",]$hour <- 84
dat_melt_ci25[dat_melt_ci25$variable == "larvae_hour_96",]$hour <- 96
dat_melt_ci25[dat_melt_ci25$variable == "larvae_hour_118",]$hour <- 118
dat_melt_ci25[dat_melt_ci25$variable == "L4_hour_0",]$hour <- 0
dat_melt_ci25[dat_melt_ci25$variable == "L4_hour_72",]$hour <- 72
dat_melt_ci25[dat_melt_ci25$variable == "L4_hour_84",]$hour <- 84
dat_melt_ci25[dat_melt_ci25$variable == "L4_hour_96",]$hour <- 96
dat_melt_ci25[dat_melt_ci25$variable == "L4_hour_118",]$hour <- 118
dat_melt_ci25[dat_melt_ci25$variable == "adults_hour_0",]$hour <- 0
dat_melt_ci25[dat_melt_ci25$variable == "adults_hour_72",]$hour <- 72
dat_melt_ci25[dat_melt_ci25$variable == "adults_hour_84",]$hour <- 84
dat_melt_ci25[dat_melt_ci25$variable == "adults_hour_96",]$hour <- 96
dat_melt_ci25[dat_melt_ci25$variable == "adults_hour_118",]$hour <- 118






# make stage column and then filling it with stages
dat_melt_ci25$stage <- "embryo"
dat_melt_ci25$stage <- factor(dat_melt_ci25$stage, levels=c("embryo", "larvae", "L4","Adult","Gravid Female"))


# correcting stages for each line
dat_melt_ci25[dat_melt_ci25$variable == "embryos_hour_0",]$stage <- "embryo"
dat_melt_ci25[dat_melt_ci25$variable == "embryos_hour_72",]$stage <- "embryo"
dat_melt_ci25[dat_melt_ci25$variable == "embryos_hour_84",]$stage <- "embryo"
dat_melt_ci25[dat_melt_ci25$variable == "embryos_hour_96",]$stage <- "embryo"
dat_melt_ci25[dat_melt_ci25$variable == "embryos_hour_118",]$stage <- "embryo"
dat_melt_ci25[dat_melt_ci25$variable == "larvae_hour_0",]$stage <- "larvae"
dat_melt_ci25[dat_melt_ci25$variable == "larvae_hour_72",]$stage <- "larvae"
dat_melt_ci25[dat_melt_ci25$variable == "larvae_hour_84",]$stage <- "larvae"
dat_melt_ci25[dat_melt_ci25$variable == "larvae_hour_96",]$stage <- "larvae"
dat_melt_ci25[dat_melt_ci25$variable == "larvae_hour_118",]$stage <- "larvae"
dat_melt_ci25[dat_melt_ci25$variable == "L4_hour_0",]$stage <- "L4"
dat_melt_ci25[dat_melt_ci25$variable == "L4_hour_72",]$stage <- "L4"
dat_melt_ci25[dat_melt_ci25$variable == "L4_hour_84",]$stage <- "L4"
dat_melt_ci25[dat_melt_ci25$variable == "L4_hour_96",]$stage <- "L4"
dat_melt_ci25[dat_melt_ci25$variable == "L4_hour_118",]$stage <- "L4"
dat_melt_ci25[dat_melt_ci25$variable == "adults_hour_0",]$stage <- "Adult"
dat_melt_ci25[dat_melt_ci25$variable == "adults_hour_72",]$stage <- "Adult"
dat_melt_ci25[dat_melt_ci25$variable == "adults_hour_84",]$stage <- "Adult"
dat_melt_ci25[dat_melt_ci25$variable == "adults_hour_96",]$stage <- "Adult"
dat_melt_ci25[dat_melt_ci25$variable == "adults_hour_118",]$stage <- "Adult"





# rename value to be "number of worms"
colnames(dat_melt_ci25)[colnames(dat_melt_ci25) == 'value'] <- 'number_of_worms'


# Assigning data to a new object to preserve previous object 
filt_dat_ci25 <- dat_melt_ci25


#dcast(ID + seq ~ condition, data = DF, value.var = "Value")
filt_dat_ci25 <- subset(filt_dat_ci25, select = -variable)


#put underscore between "Gravid Female"
levels(filt_dat_ci25$stage)[levels(filt_dat_ci25$stage)=="Gravid Female"] <- "Gravid_Female"


filt_dat_cast_ci25 <- dcast(filt_dat_ci25, ...~stage,value.var="number_of_worms")


#copy to new variable
datb_no_NA_ci25 <- filt_dat_cast_ci25


#fill NA's with zero
datb_no_NA_ci25[is.na(datb_no_NA_ci25)] <- 0


##add L4 or older column


datb_no_NA_ci25$L4_older <- datb_no_NA_ci25$L4 + datb_no_NA_ci25$Adult


##add all worms per plate
datb_no_NA_ci25$total_worms_observed <- datb_no_NA_ci25$L4 + datb_no_NA_ci25$Adult + datb_no_NA_ci25$embryo + datb_no_NA_ci25$larvae


##fraction at each stage
datb_no_NA_ci25$fra_L1_L3 <- datb_no_NA_ci25$larvae/datb_no_NA_ci25$total_worms_observed
datb_no_NA_ci25$fra_L4_older <- datb_no_NA_ci25$L4_older/datb_no_NA_ci25$total_worms_observed
datb_no_NA_ci25$fra_adult <- datb_no_NA_ci25$Adult/datb_no_NA_ci25$total_worms_observed


##make variables to put the df's in
ad_coeff_df <- NULL
L4_coeff_df <- NULL


#for each plate, get the logistic model inflection point (median hour) for the timing of the developmental milestone (L4 or adult onset)
for (i in levels(datb_no_NA_ci25$plate_number)){
  the_plate <- datb_no_NA_ci25[datb_no_NA_ci25$plate_number == i,]
  
  #get minimum number of worms observed
  min_worms_plate <- min(the_plate$total_worms_observed)
  
  #normalize to minimum number of worms observed
  
  the_plate$L1_L3_norm <- round(the_plate$fra_L1_L3*min_worms_plate)
  the_plate$L4_older_norm <- round(the_plate$fra_L4_older*min_worms_plate)
  the_plate$total_adult_norm <- round(the_plate$fra_adult*min_worms_plate)
  
  #get the worms not at milestone
  the_plate$not_L4 <- min_worms_plate-the_plate$L4_older_norm
  the_plate$not_adult <- min_worms_plate-the_plate$total_adult_norm
  #na's are 0
  the_plate[is.na(the_plate)] <- 0
  #expand the number of rows according to number of worms at milestone-- each worm gets a row per time observed
  dev_data_expand_at_L4 <- the_plate[rep(1:nrow(the_plate), the_plate$L4_older_norm),]
  dev_data_expand_at_adult <- the_plate[rep(1:nrow(the_plate), the_plate$total_adult_norm),]
  
  #expand the number of rows according to number of worms NOT at milestone-- each worm gets a row per time observed
  
  dev_data_expand_NOT_at_L4 <- the_plate[rep(1:nrow(the_plate), the_plate$not_L4),]
  dev_data_expand_NOT_at_adult <- the_plate[rep(1:nrow(the_plate), the_plate$not_adult),]
  
  
  #add column of ones to hatched worms
  dev_data_expand_at_L4$milestone_status <- rep(1,nrow(dev_data_expand_at_L4))
  dev_data_expand_at_adult$milestone_status <- rep(1,nrow(dev_data_expand_at_adult))
  
  #add column of zeros to not hatched worms
  dev_data_expand_NOT_at_L4$milestone_status <- rep(0,nrow(dev_data_expand_NOT_at_L4))
  dev_data_expand_NOT_at_adult$milestone_status <- rep(0,nrow(dev_data_expand_NOT_at_adult))
  
  
  L4_data_expand <- rbind(dev_data_expand_at_L4, dev_data_expand_NOT_at_L4)
  Adult_data_expand <- rbind(dev_data_expand_at_adult, dev_data_expand_NOT_at_adult)
  
  L4_coeff <- func_log_inf_conf(L4_data_expand,L4_data_expand$hour,L4_data_expand$milestone_status)
  
  Adult_coeff <- func_log_inf_conf(Adult_data_expand,Adult_data_expand$hour,Adult_data_expand$milestone_status)
  
  L4_df_to_add <- data.frame(bacterial_strain=unique(as.character(L4_data_expand$bacterial_strain)), plate_number=unique(as.character(L4_data_expand$plate_number)),strain_bac=unique(as.character(L4_data_expand$strain_bac)),strain=unique(as.character(L4_data_expand$strain)), Milestone= "L4", midpoint=L4_coeff[1],midpoint_95_CI_low=L4_coeff[2],midpoint_95_CI_high=L4_coeff[3])
  
  Adult_df_to_add <- data.frame(bacterial_strain=unique(as.character(Adult_data_expand$bacterial_strain)), plate_number=unique(as.character(Adult_data_expand$plate_number)),strain_bac=unique(as.character(Adult_data_expand$strain_bac)),strain=unique(as.character(Adult_data_expand$strain)), Milestone= "Adult", midpoint=Adult_coeff[1],midpoint_95_CI_low=Adult_coeff[2],midpoint_95_CI_high=Adult_coeff[3])
  
  L4_coeff_df <- rbind(L4_coeff_df,L4_df_to_add)
  
  ad_coeff_df <- rbind(ad_coeff_df,Adult_df_to_add)
}

inopinata_df25 <- rbind(L4_coeff_df,ad_coeff_df)

inopinata_df25$species <- "C. inopinata"

inopinata_df<-rbind(inopinata_df20,inopinata_df25)

inopinata_df






################################ Cele20 ###############################

# making plate number into a factor
ce20$plate_number <- as.factor(ce20$plate_number)

# convert wide data to long data
dat_melt_ce20 <- melt(ce20,id.vars = c("plate_number", "bacterial_strain", "species", "strain_bac", "temperature", "strain"))

# making a hour column
dat_melt_ce20$hour <- 0

# correcting the day column for the long data, "day 0" was the day of the egg lay 
dat_melt_ce20[dat_melt_ce20$variable == "embryos_hour_0",]$hour <- 0
dat_melt_ce20[dat_melt_ce20$variable == "embryos_hour_48",]$hour <- 48
dat_melt_ce20[dat_melt_ce20$variable == "embryos_hour_60",]$hour <- 60
dat_melt_ce20[dat_melt_ce20$variable == "embryos_hour_80",]$hour <- 80
dat_melt_ce20[dat_melt_ce20$variable == "embryos_hour_100",]$hour <- 100
dat_melt_ce20[dat_melt_ce20$variable == "larvae_hour_0",]$hour <- 0
dat_melt_ce20[dat_melt_ce20$variable == "larvae_hour_48",]$hour <- 48
dat_melt_ce20[dat_melt_ce20$variable == "larvae_hour_60",]$hour <- 60
dat_melt_ce20[dat_melt_ce20$variable == "larvae_hour_80",]$hour <- 80
dat_melt_ce20[dat_melt_ce20$variable == "larvae_hour_100",]$hour <- 100
dat_melt_ce20[dat_melt_ce20$variable == "L4_hour_0",]$hour <- 0
dat_melt_ce20[dat_melt_ce20$variable == "L4_hour_48",]$hour <- 48
dat_melt_ce20[dat_melt_ce20$variable == "L4_hour_60",]$hour <- 60
dat_melt_ce20[dat_melt_ce20$variable == "L4_hour_80",]$hour <- 80
dat_melt_ce20[dat_melt_ce20$variable == "L4_hour_100",]$hour <- 100
dat_melt_ce20[dat_melt_ce20$variable == "adults_hour_0",]$hour <- 0
dat_melt_ce20[dat_melt_ce20$variable == "adults_hour_48",]$hour <- 48
dat_melt_ce20[dat_melt_ce20$variable == "adults_hour_60",]$hour <- 60
dat_melt_ce20[dat_melt_ce20$variable == "adults_hour_80",]$hour <- 80
dat_melt_ce20[dat_melt_ce20$variable == "adults_hour_100",]$hour <- 100






# make stage column and then filling it with stages
dat_melt_ce20$stage <- "embryo"
dat_melt_ce20$stage <- factor(dat_melt_ce20$stage, levels=c("embryo", "larvae", "L4","Adult","Gravid Female"))


# correcting stages for each line
dat_melt_ce20[dat_melt_ce20$variable == "embryos_hour_0",]$stage <- "embryo"
dat_melt_ce20[dat_melt_ce20$variable == "embryos_hour_48",]$stage <- "embryo"
dat_melt_ce20[dat_melt_ce20$variable == "embryos_hour_60",]$stage <- "embryo"
dat_melt_ce20[dat_melt_ce20$variable == "embryos_hour_80",]$stage <- "embryo"
dat_melt_ce20[dat_melt_ce20$variable == "embryos_hour_100",]$stage <- "embryo"
dat_melt_ce20[dat_melt_ce20$variable == "larvae_hour_0",]$stage <- "larvae"
dat_melt_ce20[dat_melt_ce20$variable == "larvae_hour_48",]$stage <- "larvae"
dat_melt_ce20[dat_melt_ce20$variable == "larvae_hour_60",]$stage <- "larvae"
dat_melt_ce20[dat_melt_ce20$variable == "larvae_hour_80",]$stage <- "larvae"
dat_melt_ce20[dat_melt_ce20$variable == "larvae_hour_100",]$stage <- "larvae"
dat_melt_ce20[dat_melt_ce20$variable == "L4_hour_0",]$stage <- "L4"
dat_melt_ce20[dat_melt_ce20$variable == "L4_hour_48",]$stage <- "L4"
dat_melt_ce20[dat_melt_ce20$variable == "L4_hour_60",]$stage <- "L4"
dat_melt_ce20[dat_melt_ce20$variable == "L4_hour_80",]$stage <- "L4"
dat_melt_ce20[dat_melt_ce20$variable == "L4_hour_100",]$stage <- "L4"
dat_melt_ce20[dat_melt_ce20$variable == "adults_hour_0",]$stage <- "Adult"
dat_melt_ce20[dat_melt_ce20$variable == "adults_hour_48",]$stage <- "Adult"
dat_melt_ce20[dat_melt_ce20$variable == "adults_hour_60",]$stage <- "Adult"
dat_melt_ce20[dat_melt_ce20$variable == "adults_hour_80",]$stage <- "Adult"
dat_melt_ce20[dat_melt_ce20$variable == "adults_hour_100",]$stage <- "Adult"




# rename value to be "number of worms"
colnames(dat_melt_ce20)[colnames(dat_melt_ce20) == 'value'] <- 'number_of_worms'

# Assigning data to a new object to preserve previous object 
filt_dat_ce20 <- dat_melt_ce20


#dcast(ID + seq ~ condition, data = DF, value.var = "Value")
filt_dat_ce20 <- subset(filt_dat_ce20, select = -variable)

View(filt_dat_ce20)


filt_dat_cast_ce20 <- dcast(filt_dat_ce20, ...~stage,value.var="number_of_worms")


#copy to new variable
datb_no_NA_ce20 <- filt_dat_cast_ce20


#fill NA's with zero
datb_no_NA_ce20[is.na(datb_no_NA_ce20)] <- 0


##add L4 or older column
datb_no_NA_ce20$L4_older <- datb_no_NA_ce20$L4 + datb_no_NA_ce20$Adult


##add all worms per plate
datb_no_NA_ce20$total_worms_observed <- datb_no_NA_ce20$L4 + datb_no_NA_ce20$Adult + datb_no_NA_ce20$embryo + datb_no_NA_ce20$larvae

##fraction at each stage
datb_no_NA_ce20$fra_L1_L3 <- datb_no_NA_ce20$larvae/datb_no_NA_ce20$total_worms_observed
datb_no_NA_ce20$fra_L4_older <- datb_no_NA_ce20$L4_older/datb_no_NA_ce20$total_worms_observed
datb_no_NA_ce20$fra_adult <- datb_no_NA_ce20$Adult/datb_no_NA_ce20$total_worms_observed


##make variables to put the df's in
ad_coeff_df <- NULL
L4_coeff_df <- NULL


for (i in levels(datb_no_NA_ce20$plate_number)){
  the_plate <- datb_no_NA_ce20[datb_no_NA_ce20$plate_number == i,]
  
  #get minimum number of worms observed
  min_worms_plate <- min(the_plate$total_worms_observed)
  
  #normalize to minimum number of worms observed
  
  the_plate$L1_L3_norm <- round(the_plate$fra_L1_L3*min_worms_plate)
  the_plate$L4_older_norm <- round(the_plate$fra_L4_older*min_worms_plate)
  the_plate$total_adult_norm <- round(the_plate$fra_adult*min_worms_plate)
  
  #get the worms not at milestone
  the_plate$not_L4 <- min_worms_plate-the_plate$L4_older_norm
  the_plate$not_adult <- min_worms_plate-the_plate$total_adult_norm
  #na's are 0
  the_plate[is.na(the_plate)] <- 0
  #expand the number of rows according to number of worms at milestone-- each worm gets a row per time observed
  dev_data_expand_at_L4 <- the_plate[rep(1:nrow(the_plate), the_plate$L4_older_norm),]
  dev_data_expand_at_adult <- the_plate[rep(1:nrow(the_plate), the_plate$total_adult_norm),]
  
  #expand the number of rows according to number of worms NOT at milestone-- each worm gets a row per time observed
  
  dev_data_expand_NOT_at_L4 <- the_plate[rep(1:nrow(the_plate), the_plate$not_L4),]
  dev_data_expand_NOT_at_adult <- the_plate[rep(1:nrow(the_plate), the_plate$not_adult),]
  
  
  #add column of ones to hatched worms
  dev_data_expand_at_L4$milestone_status <- rep(1,nrow(dev_data_expand_at_L4))
  dev_data_expand_at_adult$milestone_status <- rep(1,nrow(dev_data_expand_at_adult))
  
  #add column of zeros to not hatched worms
  dev_data_expand_NOT_at_L4$milestone_status <- rep(0,nrow(dev_data_expand_NOT_at_L4))
  dev_data_expand_NOT_at_adult$milestone_status <- rep(0,nrow(dev_data_expand_NOT_at_adult))
  
  
  L4_data_expand <- rbind(dev_data_expand_at_L4, dev_data_expand_NOT_at_L4)
  Adult_data_expand <- rbind(dev_data_expand_at_adult, dev_data_expand_NOT_at_adult)
  
  L4_coeff <- func_log_inf_conf(L4_data_expand,L4_data_expand$hour,L4_data_expand$milestone_status)
  
  Adult_coeff <- func_log_inf_conf(Adult_data_expand,Adult_data_expand$hour,Adult_data_expand$milestone_status)
  
  L4_df_to_add <- data.frame(bacterial_strain=unique(as.character(L4_data_expand$bacterial_strain)), plate_number=unique(as.character(L4_data_expand$plate_number)),strain_bac=unique(as.character(L4_data_expand$strain_bac)),strain=unique(as.character(L4_data_expand$strain)), Milestone= "L4", midpoint=L4_coeff[1],midpoint_95_CI_low=L4_coeff[2],midpoint_95_CI_high=L4_coeff[3])
  
  Adult_df_to_add <- data.frame(bacterial_strain=unique(as.character(Adult_data_expand$bacterial_strain)), plate_number=unique(as.character(Adult_data_expand$plate_number)),strain_bac=unique(as.character(Adult_data_expand$strain_bac)),strain=unique(as.character(Adult_data_expand$strain)), Milestone= "Adult", midpoint=Adult_coeff[1],midpoint_95_CI_low=Adult_coeff[2],midpoint_95_CI_high=Adult_coeff[3])
  
  L4_coeff_df <- rbind(L4_coeff_df,L4_df_to_add)
  
  ad_coeff_df <- rbind(ad_coeff_df,Adult_df_to_add)
}


elegans_df20 <- rbind(L4_coeff_df,ad_coeff_df)

elegans_df20$species <- "C. elegans"









################################ Cele25 ###############################

# making plate number into a factor
ce25$plate_number <- as.factor(ce25$plate_number)

# convert wide data to long data
dat_melt_ce25 <- melt(ce25,id.vars = c("plate_number", "bacterial_strain", "species", "strain_bac", "temperature", "strain"))

# making a hour column
dat_melt_ce25$hour <- 0

# correcting the day column for the long data, "day 0" was the day of the egg lay 
dat_melt_ce25[dat_melt_ce25$variable == "embryos_hour_0",]$hour <- 0
dat_melt_ce25[dat_melt_ce25$variable == "embryos_hour_31",]$hour <- 31
dat_melt_ce25[dat_melt_ce25$variable == "embryos_hour_40",]$hour <- 40
dat_melt_ce25[dat_melt_ce25$variable == "embryos_hour_48",]$hour <- 48
dat_melt_ce25[dat_melt_ce25$variable == "embryos_hour_60",]$hour <- 60
dat_melt_ce25[dat_melt_ce25$variable == "embryos_hour_80",]$hour <- 80

dat_melt_ce25[dat_melt_ce25$variable == "larvae_hour_0",]$hour <- 0
dat_melt_ce25[dat_melt_ce25$variable == "larvae_hour_31",]$hour <- 31
dat_melt_ce25[dat_melt_ce25$variable == "larvae_hour_40",]$hour <- 40
dat_melt_ce25[dat_melt_ce25$variable == "larvae_hour_48",]$hour <- 48
dat_melt_ce25[dat_melt_ce25$variable == "larvae_hour_60",]$hour <- 60
dat_melt_ce25[dat_melt_ce25$variable == "larvae_hour_80",]$hour <- 80

dat_melt_ce25[dat_melt_ce25$variable == "L4_hour_0",]$hour <- 0
dat_melt_ce25[dat_melt_ce25$variable == "L4_hour_31",]$hour <- 31
dat_melt_ce25[dat_melt_ce25$variable == "L4_hour_40",]$hour <- 40
dat_melt_ce25[dat_melt_ce25$variable == "L4_hour_48",]$hour <- 48
dat_melt_ce25[dat_melt_ce25$variable == "L4_hour_60",]$hour <- 60
dat_melt_ce25[dat_melt_ce25$variable == "L4_hour_80",]$hour <- 80

dat_melt_ce25[dat_melt_ce25$variable == "adults_hour_0",]$hour <- 0
dat_melt_ce25[dat_melt_ce25$variable == "adults_hour_31",]$hour <- 31
dat_melt_ce25[dat_melt_ce25$variable == "adults_hour_40",]$hour <- 40
dat_melt_ce25[dat_melt_ce25$variable == "adults_hour_48",]$hour <- 48
dat_melt_ce25[dat_melt_ce25$variable == "adults_hour_60",]$hour <- 60
dat_melt_ce25[dat_melt_ce25$variable == "adults_hour_80",]$hour <- 80







# make stage column and then filling it with stages
dat_melt_ce25$stage <- "embryo"
dat_melt_ce25$stage <- factor(dat_melt_ce25$stage, levels=c("embryo", "larvae", "L4","Adult","Gravid Female"))


# correcting stages for each line
dat_melt_ce25[dat_melt_ce25$variable == "embryos_hour_0",]$stage <- "embryo"
dat_melt_ce25[dat_melt_ce25$variable == "embryos_hour_31",]$stage <- "embryo"
dat_melt_ce25[dat_melt_ce25$variable == "embryos_hour_40",]$stage <- "embryo"
dat_melt_ce25[dat_melt_ce25$variable == "embryos_hour_48",]$stage <- "embryo"
dat_melt_ce25[dat_melt_ce25$variable == "embryos_hour_60",]$stage <- "embryo"
dat_melt_ce25[dat_melt_ce25$variable == "embryos_hour_80",]$stage <- "embryo"

dat_melt_ce25[dat_melt_ce25$variable == "larvae_hour_0",]$stage <- "larvae"
dat_melt_ce25[dat_melt_ce25$variable == "larvae_hour_31",]$stage <- "larvae"
dat_melt_ce25[dat_melt_ce25$variable == "larvae_hour_40",]$stage <- "larvae"
dat_melt_ce25[dat_melt_ce25$variable == "larvae_hour_48",]$stage <- "larvae"
dat_melt_ce25[dat_melt_ce25$variable == "larvae_hour_60",]$stage <- "larvae"
dat_melt_ce25[dat_melt_ce25$variable == "larvae_hour_80",]$stage <- "larvae"

dat_melt_ce25[dat_melt_ce25$variable == "L4_hour_0",]$stage <- "L4"
dat_melt_ce25[dat_melt_ce25$variable == "L4_hour_31",]$stage <- "L4"
dat_melt_ce25[dat_melt_ce25$variable == "L4_hour_40",]$stage <- "L4"
dat_melt_ce25[dat_melt_ce25$variable == "L4_hour_48",]$stage <- "L4"
dat_melt_ce25[dat_melt_ce25$variable == "L4_hour_60",]$stage <- "L4"
dat_melt_ce25[dat_melt_ce25$variable == "L4_hour_80",]$stage <- "L4"

dat_melt_ce25[dat_melt_ce25$variable == "adults_hour_0",]$stage <- "Adult"
dat_melt_ce25[dat_melt_ce25$variable == "adults_hour_31",]$stage <- "Adult"
dat_melt_ce25[dat_melt_ce25$variable == "adults_hour_40",]$stage <- "Adult"
dat_melt_ce25[dat_melt_ce25$variable == "adults_hour_48",]$stage <- "Adult"
dat_melt_ce25[dat_melt_ce25$variable == "adults_hour_60",]$stage <- "Adult"
dat_melt_ce25[dat_melt_ce25$variable == "adults_hour_80",]$stage <- "Adult"





# rename value to be "number of worms"
colnames(dat_melt_ce25)[colnames(dat_melt_ce25) == 'value'] <- 'number_of_worms'

# Assigning data to a new object to preserve previous object 
filt_dat_ce25 <- dat_melt_ce25


#dcast(ID + seq ~ condition, data = DF, value.var = "Value")
filt_dat_ce25 <- subset(filt_dat_ce25, select = -variable)



filt_dat_cast_ce25 <- dcast(filt_dat_ce25, ...~stage,value.var="number_of_worms")


#copy to new variable
datb_no_NA_ce25 <- filt_dat_cast_ce25


#fill NA's with zero
datb_no_NA_ce25[is.na(datb_no_NA_ce25)] <- 0


##add L4 or older column
datb_no_NA_ce25$L4_older <- datb_no_NA_ce25$L4 + datb_no_NA_ce25$Adult


##add all worms per plate
datb_no_NA_ce25$total_worms_observed <- datb_no_NA_ce25$L4 + datb_no_NA_ce25$Adult + datb_no_NA_ce25$embryo + datb_no_NA_ce25$larvae

##fraction at each stage
datb_no_NA_ce25$fra_L1_L3 <- datb_no_NA_ce25$larvae/datb_no_NA_ce25$total_worms_observed
datb_no_NA_ce25$fra_L4_older <- datb_no_NA_ce25$L4_older/datb_no_NA_ce25$total_worms_observed
datb_no_NA_ce25$fra_adult <- datb_no_NA_ce25$Adult/datb_no_NA_ce25$total_worms_observed


##make variables to put the df's in
ad_coeff_df <- NULL
L4_coeff_df <- NULL


for (i in levels(datb_no_NA_ce25$plate_number)){
  the_plate <- datb_no_NA_ce25[datb_no_NA_ce25$plate_number == i,]
  
  #get minimum number of worms observed
  min_worms_plate <- min(the_plate$total_worms_observed)
  
  #normalize to minimum number of worms observed
  
  the_plate$L1_L3_norm <- round(the_plate$fra_L1_L3*min_worms_plate)
  the_plate$L4_older_norm <- round(the_plate$fra_L4_older*min_worms_plate)
  the_plate$total_adult_norm <- round(the_plate$fra_adult*min_worms_plate)
  
  #get the worms not at milestone
  the_plate$not_L4 <- min_worms_plate-the_plate$L4_older_norm
  the_plate$not_adult <- min_worms_plate-the_plate$total_adult_norm
  #na's are 0
  the_plate[is.na(the_plate)] <- 0
  #expand the number of rows according to number of worms at milestone-- each worm gets a row per time observed
  dev_data_expand_at_L4 <- the_plate[rep(1:nrow(the_plate), the_plate$L4_older_norm),]
  dev_data_expand_at_adult <- the_plate[rep(1:nrow(the_plate), the_plate$total_adult_norm),]
  
  #expand the number of rows according to number of worms NOT at milestone-- each worm gets a row per time observed
  
  dev_data_expand_NOT_at_L4 <- the_plate[rep(1:nrow(the_plate), the_plate$not_L4),]
  dev_data_expand_NOT_at_adult <- the_plate[rep(1:nrow(the_plate), the_plate$not_adult),]
  
  
  #add column of ones to hatched worms
  dev_data_expand_at_L4$milestone_status <- rep(1,nrow(dev_data_expand_at_L4))
  dev_data_expand_at_adult$milestone_status <- rep(1,nrow(dev_data_expand_at_adult))
  
  #add column of zeros to not hatched worms
  dev_data_expand_NOT_at_L4$milestone_status <- rep(0,nrow(dev_data_expand_NOT_at_L4))
  dev_data_expand_NOT_at_adult$milestone_status <- rep(0,nrow(dev_data_expand_NOT_at_adult))
  
  
  L4_data_expand <- rbind(dev_data_expand_at_L4, dev_data_expand_NOT_at_L4)
  Adult_data_expand <- rbind(dev_data_expand_at_adult, dev_data_expand_NOT_at_adult)
  
  L4_coeff <- func_log_inf_conf(L4_data_expand,L4_data_expand$hour,L4_data_expand$milestone_status)
  
  Adult_coeff <- func_log_inf_conf(Adult_data_expand,Adult_data_expand$hour,Adult_data_expand$milestone_status)
  
  L4_df_to_add <- data.frame(bacterial_strain=unique(as.character(L4_data_expand$bacterial_strain)), plate_number=unique(as.character(L4_data_expand$plate_number)),strain_bac=unique(as.character(L4_data_expand$strain_bac)),strain=unique(as.character(L4_data_expand$strain)), Milestone= "L4", midpoint=L4_coeff[1],midpoint_95_CI_low=L4_coeff[2],midpoint_95_CI_high=L4_coeff[3])
  
  Adult_df_to_add <- data.frame(bacterial_strain=unique(as.character(Adult_data_expand$bacterial_strain)), plate_number=unique(as.character(Adult_data_expand$plate_number)),strain_bac=unique(as.character(Adult_data_expand$strain_bac)),strain=unique(as.character(Adult_data_expand$strain)), Milestone= "Adult", midpoint=Adult_coeff[1],midpoint_95_CI_low=Adult_coeff[2],midpoint_95_CI_high=Adult_coeff[3])
  
  L4_coeff_df <- rbind(L4_coeff_df,L4_df_to_add)
  
  ad_coeff_df <- rbind(ad_coeff_df,Adult_df_to_add)
}


elegans_df25 <- rbind(L4_coeff_df,ad_coeff_df)

elegans_df25$species <- "C. elegans"



elegans_df<-rbind(elegans_df20,elegans_df25)

elegans_df





dev20<-rbind(elegans_df20,inopinata_df20)
dev25<-rbind(elegans_df25,inopinata_df25)
write.csv(dev20, "dev20.csv")
write.csv(dev25, "dev25.csv")











# # Combining both the elegans and inopinata dataframes into one to plot 
# big_df <- rbind(inopinata_df,elegans_df)
# big_df <- big_df %>% 
#   mutate(hour = midpoint)
# big_df$Milestone <- factor(big_df$Milestone, levels=c("L4", "Adult"))
# 
# # Pull out the adult/maturation milestone only
# adult_df <- big_df %>% 
#   filter(Milestone == "Adult")
# 
# 
# maturation <- ggplot(adult_df, aes(x=strain,y=midpoint, group = strain_bac)) +
#   geom_sina(aes(colour=bacterial_strain),size=1.5,scale="width") +
#   stat_summary(aes(group=strain_bac),fun = mean, fun.min = mean, fun.max = mean, geom = "crossbar", width = 0.5, colour="black",position = position_dodge(width = 0.9)) +
#   scale_y_continuous(limits=c(0,175),breaks=seq(from=0,to=175,by=25)) +
#   labs(fill = "Bacterial Strains") +
#   guides(color = guide_legend(title = "Bacterial Strains")) +
#   xlab("Nematode species") +
#   ylab("Maturation Time (days)") +
#   theme_cowplot() +
#   theme(axis.title.x = element_text(size=12),
#         axis.title.y = element_text(size=12),
#         axis.text.x = element_text(size=12,face="italic"),
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 12)) +
#   theme(legend.text = element_text(face="italic"))
# 
# maturation







###################################################################################################################################################
###################################################################################################################################################
################# VIABILITY #######################################################################################################################
###################################################################################################################################################
###################################################################################################################################################


# Getting the fraction of surviving worms from the initial egg-lay and selecting variables necessary for plotting
via_ci20 <- ci20 %>%
  mutate(frac_via = adults_hour_192/embryos_hour_0) %>% 
  select(plate_number, bacterial_strain, strain_bac, frac_via, species, strain, temperature)
via_ci25 <- ci25 %>%
  mutate(frac_via = adults_hour_118/embryos_hour_0) %>% 
  select(plate_number, bacterial_strain, strain_bac, frac_via, species, strain, temperature)

via_ci<-rbind(via_ci20,via_ci25)


via_ce20 <- ce20 %>%
  mutate(frac_via = adults_hour_80/embryos_hour_0) %>% 
  select(plate_number, bacterial_strain, strain_bac, frac_via, species, strain, temperature)
via_ce25 <- ce25 %>%
  mutate(frac_via = adults_hour_80/embryos_hour_0) %>% 
  select(plate_number, bacterial_strain, strain_bac, frac_via, species, strain, temperature)

via_ce<-rbind(via_ce20,via_ce25)


#VIA20
via20 <-rbind(via_ce20,via_ci20)
write.csv(via20, "via20.csv")
#VIA25
via25 <-rbind(via_ce25,via_ci25)
write.csv(via25, "via25.csv")



###################################################################################################################################################
###################################################################################################################################################
################# BODY SIZE #######################################################################################################################
###################################################################################################################################################
###################################################################################################################################################

# Load in data
dat_ele<-read.csv("bodysize.csv", header=T)
dat_ino<-read.csv("bodysize.csv",header=T)



#Filtering for species
dat_ele<-filter(dat_ele, species == "C. elegans")
dat_ino<-filter(dat_ino, species == "C. inopinata")
write.csv(dat_ele, "dat_ele.csv")
write.csv(dat_ino, "dat_ino.csv")







###################################################################################################################################################
################# REPRODUCTIVE DURATION ###########################################################################################################
###################################################################################################################################################
###################################################################################################################################################

ino <- read.csv("woub2_ind_fec_ci_total.csv", stringsAsFactors = TRUE) %>% 
  select(end_rep, strain_bac,strain,bacterial_strain, temperature, status)
View(ino)
ele <- read.csv("woub2_ind_fec_ce_total.csv", stringsAsFactors = TRUE)%>% 
  select(end_rep, strain_bac,strain,bacterial_strain, temperature, status)
View(ele)

ino20<-ino %>% 
  filter(temperature=="20")
ino25<-ino %>% 
  filter(temperature=="25")
ele20<-ele %>% 
  filter(temperature=="20")
ele25<-ele %>% 
  filter(temperature=="25")


# Getting the end of reproduction

worms20<-rbind(ino20, ele20)
write.csv(worms20, "worms20_endrep")

worms25<-rbind(ino25, ele25)
write.csv(worms25, "worms25_endrep")



# Bagged worms
wormsindfec_bagged20 <- filter(worms20, status == "bagged")
View(wormsindfec_bagged20)
write.csv(wormsindfec_bagged20, "worms20_bagged_endrep.csv")

wormsindfec_bagged25 <- filter(worms25, status == "bagged")
View(wormsindfec_bagged25)
write.csv(wormsindfec_bagged25, "worms25_bagged_endrep.csv")

#Non-bagged worms
wormsindfec_nonbagged20<-filter(worms20, status != "bagged")
View(wormsindfec_nonbagged20)
write.csv(wormsindfec_nonbagged20, "worms20_nonbagged_endrep.csv")

wormsindfec_nonbagged25<-filter(worms25, status != "bagged")
View(wormsindfec_nonbagged25)
write.csv(wormsindfec_nonbagged25, "worms25_nonbagged_endrep.csv")



############################
############################
######################################



##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################






















