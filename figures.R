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




######################
######################
######################
##### IND FEC #####
######################
######################
######################

wormsindfec20_nonbagged<-read.csv("wormsindfec20_nonbagged.csv", header = T)
wormsindfec25_nonbagged<-read.csv("wormsindfec25_nonbagged.csv", header = T)

individualfecundity20_nonbagged <- ggplot(wormsindfec20_nonbagged, aes(x=factor(strain, level=c('fog-2', 'PD1074', 'NKZ35')), y = total_progeny, group = strain_bac, color = bacterial_strain)) +
  geom_sina(size=1, alpha=1,scale="width") +
  stat_summary(aes(group=strain_bac),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  theme_cowplot() +
  labs(fill = "Bacterial Strains") +
  guides(color = "none",shape="none") +
  scale_color_manual(values = c("#E41A1C", "#377EB8")) +
  ylab("Number of embryos laid") +
  scale_y_continuous(limits=c(-5,850), breaks=c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900)) +
  theme(axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position="none") +
  theme(axis.text.x=element_text(face="italic"), legend.text = element_text(face="italic"))


individualfecundity25_nonbagged<- ggplot(wormsindfec25_nonbagged, aes(x=factor(strain, level=c('fog-2', 'NKZ35')), y = total_progeny, group = strain_bac, color = bacterial_strain)) +
  geom_sina(size=1, alpha=1,scale="width") +
  stat_summary(aes(group=strain_bac),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  theme_cowplot() +
  labs(fill = "Bacterial Strains") +
  guides(color = "none",shape="none") + 
  scale_color_manual(values = c("#E41A1C", "#377EB8")) +
  ylab("Number of embryos laid") +
  scale_y_continuous(limits=c(-5,850), breaks=c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900)) +
  theme(axis.title.y = element_text(size=12),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size=12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position="none") +
  theme(axis.text.x=element_text(face="italic"), legend.text = element_text(face="italic"))


######################
######################
######################
##### MATURATION #####
######################
######################
######################
dev20C<-read.csv("dev20C.csv", header=T)
dev25C<-read.csv("dev25C.csv", header=T)


maturation20 <- ggplot(dev20C, aes(x=strain,y=midpoint)) +
  geom_sina(aes(colour=bacterial_strain),size=1,scale="width") +
  stat_summary(aes(group=bacterial_strain),fun = mean, fun.min = mean, fun.max = mean, geom = "crossbar", width = 0.5, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits=c(0,175),breaks=seq(from=0,to=175,by=25)) +
  labs(fill = "Bacterial Strains") +
  guides(color = guide_legend(title = "Bacterial Strains")) +
  scale_color_manual(values = c("#E41A1C", "#377EB8")) +
  ylab("Maturation Time (hours)") +
  theme_cowplot() +
  theme(axis.title.y = element_text(size=12),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size=12,face="italic"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position="none") 

maturation20

maturation25 <- ggplot(dev25C, aes(x=strain,y=midpoint)) +
  geom_sina(aes(colour=bacterial_strain),size=1,scale="width") +
  stat_summary(aes(group=bacterial_strain),fun = mean, fun.min = mean, fun.max = mean, geom = "crossbar", width = 0.5, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits=c(0,100),breaks=seq(from=0,to=100,by=10)) +
  labs(fill = "Bacterial Strains") +
  guides(color = guide_legend(title = "Bacterial Strains")) +
  scale_color_manual(values = c("#E41A1C", "#377EB8")) +
  ylab("Maturation Time (hours)") +
  theme_cowplot() +
  theme(axis.title.y = element_text(size=12),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size=12,face="italic"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position="none") 

maturation25


######################
######################
######################
##### VIABILITY #####
######################
######################
######################
#read data in
via20<-read.csv("via20.csv", header=T)
via25<-read.csv("via25.csv", header=T)


# Plot VIA20!!!
viability20 <- ggplot(via20, aes(x = strain, y = frac_via, group = strain_bac, color = bacterial_strain)) + 
  geom_sina() +
  stat_summary(aes(group=strain_bac),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0, 1.3), breaks = seq(0,1.3,.1)) +
  labs(fill = "Bacterial Strains") +
  guides(color = "none",shape="none") +
  xlab("Nematode strain") +
  ylab("Fraction of surviving adults") +
  theme_cowplot() +
  scale_colour_brewer(palette="Set1") +
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12,face="italic"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  theme(legend.text = element_text(face="italic"))

# Calling plot object to view it 
viability20

# Plot VIA25!!!
viability25 <- ggplot(via25, aes(x = strain, y = frac_via, group = strain_bac, color = bacterial_strain)) + 
  geom_sina() +
  stat_summary(aes(group=strain_bac),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0, 1.3), breaks = seq(0,1.3,.1)) +
  labs(fill = "Bacterial Strains") +
  guides(color = "none",shape="none") +
  xlab("Nematode strain") +
  ylab("Fraction of surviving adults") +
  theme_cowplot() +
  scale_colour_brewer(palette="Set1") +
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12,face="italic"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  theme(legend.text = element_text(face="italic"))

viability25

######################
######################
######################
##### END REP #####
######################
######################
######################
worms20_nonbagged_endrep<-read.csv("worms20_nonbagged_endrep.csv", header=T)
worms25_nonbagged_endrep<-read.csv("worms25_nonbagged_endrep.csv", header=T)

# Plot NONBAGGED 20!!!!!
worms20_nonbagged_endrep <- ggplot(worms20_nonbagged_endrep, aes(x = strain, y = end_rep, group = strain_bac, color = bacterial_strain)) + 
  geom_sina(size=1.25, alpha=1,scale="width") +
  stat_summary(aes(group=strain_bac),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0,15,1)) +
  labs(fill = "Bacterial Strains") +
  scale_fill_continuous(guide = guide_legend()) +
  xlab("Nematode strain") +
  ylab("End of reproduction (days)") +
  theme_cowplot() +
  scale_colour_brewer(palette="Set1") +
  theme(axis.text.x=element_text(face="italic")) + 
  theme(axis.text.x=element_text(face="italic"), legend.text = element_text(face="italic")) +
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size=12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))


# Plot NONBAGGED 25!!!!
worms25_nonbagged_endrep <- ggplot(worms25_nonbagged_endrep, aes(x = strain, y = end_rep, group = strain_bac, color = bacterial_strain)) + 
  geom_sina(size=1.25, alpha=1,scale="width") +
  stat_summary(aes(group=strain_bac),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0, 13), breaks = seq(0,13,1)) +
  labs(fill = "Bacterial Strains") +
  guides(color = "none",shape="none") +
  xlab("Nematode strain") +
  ylab("End of reproduction (days)") +
  theme_cowplot() +
  scale_colour_brewer(palette="Set1") +
  theme(axis.text.x=element_text(face="italic")) + 
  theme(axis.text.x=element_text(face="italic"), legend.text = element_text(face="italic")) +
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(size=12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

worms20_nonbagged_endrep
worms25_nonbagged_endrep


######################
######################
######################
##### BODY SIZE #####
######################
######################
######################

# Load in data
dat_ele<-read.csv("bodysize.csv", header=T)
dat_ino<-read.csv("bodysize.csv",header=T)



#Filtering for species
dat_ele<-filter(dat_ele, species == "C. elegans")
dat_ino<-filter(dat_ino, species == "C. inopinata")
write.csv(dat_ele, "dat_ele.csv")
write.csv(dat_ino, "dat_ino.csv")


# Load in data
dat_ele<-read.csv("dat_ele.csv", header=T)
dat_ino<-read.csv("dat_ino.csv",header=T)




# C elegans body size 
bodyele<-ggplot(dat_ele, aes(x = bacteria, y = length_microns, group = sp_bac_sex, shape = sex)) +
  geom_sina(size=0.75,aes(colour=bacteria)) +
  stat_summary(aes(group=sp_bac_sex),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0, 2300), breaks = seq(0,2400,200)) +
  guides(color = "none",shape="none") +
  xlab("Bacterial Strain") +
  ylab("C. elegans length (microns)") +
  theme_cowplot(font_size = 12) +
  theme(plot.title = element_text(hjust = 1)) +
  theme(axis.text.x=element_text(face="italic"),
        axis.title.x=element_blank()) +
  scale_colour_brewer(palette="Set1") +
  scale_shape_manual(values=c(15, 17)) +
  theme(axis.title.x=element_blank())

# C inopinata body size 

bodyino<-ggplot(dat_ino, aes(x = bacteria, y = length_microns, group = sp_bac_sex, shape = sex)) +
  geom_sina(aes(colour=bacteria),size=0.75) +
  stat_summary(aes(group=sp_bac_sex),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0, 2300), breaks = seq(0,2400,200)) +
  scale_fill_continuous(guide = guide_legend()) +
  xlab("Bacterial Strain") +
  ylab("C. inopinata length (microns)") +
  theme_cowplot(font_size = 12) +
  theme(plot.title = element_text(hjust = 1)) +
  theme(axis.text.x=element_text(face="italic"),
        axis.title.x=element_blank()) +
  scale_colour_brewer(palette="Set1") +
  scale_shape_manual(values=c(15, 17)) +
  theme(axis.title.x=element_blank())

bodyino


# Making Figure #2
a2<-individualfecundity25_nonbagged
b2<-maturation25
c2<-viability25
d2<-worms25_nonbagged_endrep
e2<-bodyele
f2<-bodyino


figure2<-(a2+b2+c2)/(d2+e2+f2)
figure2


# Making SUPP Figure #2
asup2<-individualfecundity20_nonbagged
bsup2<-maturation20
csup2<-viability20
dsup2<-worms20_nonbagged_endrep



suppfigure2<-(asup2+bsup2)/(csup2+dsup2)
suppfigure2





########################
########################
########################
#### COMPARE N2 FOG2 Supp fig 3 ###
########################
########################
########################



N2F2_20<-read.csv("N2F2_20.csv", header = T)
N2F2_25<-read.csv("N2F2_25.csv", header = T)

N2F2_20 <- ggplot(N2F2_20, aes(x=factor(strain, level=c('fog-2', 'PD1074')), y = total_progeny, group = strain_bac, color = bacterial_strain)) +
  geom_sina(size=1, alpha=1,scale="width") +
  stat_summary(aes(group=strain_bac),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  theme_cowplot() +
  labs(fill = "Bacterial Strains") +
  guides(color = "none",shape="none") +
  scale_color_manual(values = c("#E41A1C", "#377EB8")) +
  ylab("Number of embryos laid") +
  scale_y_continuous(limits=c(-5,850), breaks=c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900)) +
  theme(axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position="none") +
  theme(axis.text.x=element_text(face="italic"), legend.text = element_text(face="italic"))

N2F2_25<- ggplot(N2F2_25, aes(x=factor(strain, level=c('fog-2', 'PD1074')), y = total_progeny, group = strain_bac, color = bacterial_strain)) +
  geom_sina(size=1, alpha=1,scale="width") +
  stat_summary(aes(group=strain_bac),fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.25, colour="black",position = position_dodge(width = 0.9)) +
  theme_cowplot() +
  scale_fill_continuous(guide = guide_legend()) +
  labs(fill = "Bacterial Strains") +
  scale_color_manual(values = c("#E41A1C", "#377EB8")) +
  ylab("Number of embryos laid") +
  scale_y_continuous(limits=c(-5,850), breaks=c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900)) +
  theme(axis.title.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  theme(axis.text.x=element_text(face="italic"), legend.text = element_text(face="italic"))


N2F2_20
N2F2_25

# Making supp fig 3 #
supa3<-N2F2_20
supb3<-N2F2_25

supfigure3<-(supa3+supb3)
supfigure3






