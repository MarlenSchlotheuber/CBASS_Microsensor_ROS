# Manuscript R script
# ROS Manuscript
# Marlen Schlotheuber

# install packages
install.packages("AICcmodavg")
install.packages("CRAN")
install.packages("MuMIn")
install.packages("nlme")

# load libaries
library(tidyverse)
library(readr)
library(emmeans)
library(drc)
library(dplyr)
library(MASS)
library(MuMIn)
library(nlme)
library(ggplot2)
library(gridExtra)
library(ggforce)
library(ggdist)
library(gghalves)

# set working directory
setwd("~/Nextcloud/Master thesis/Manuscript/Data/CBASS_data/PAM_VBA_CN/Data for R")

# load data set
# baceline temperature control: PAM measurements MAREE & CBASS1 27°C and 30°C
PAM_control <- read.table("CBASS1_MAREE_control_pam.csv", header = T, sep=";")
# CBASS pilot run: 27°C,30°C,33°C,35°C.,36°C,39°C
PAM_cbass1 <- read.table("CBASS1_pam.csv", header = T, sep=";")

# CBASS + Microsensor data
PAM_cbass8 <- read.table("CBASS8_pam.csv", header = T, sep=";")
CN_cbass8 <- read.table("CBASS8_cellcount.csv", header = T, sep=";")
VBA_cbass8 <- read.table("CBASS8_VBA_new.csv", header = T, sep=";")

# transform data
PAM_PAM_control$Fac.Temperature <- as.factor(PAM_PAM_control$Temperature)
PAM_cbass1$Fac.Temperature <- as.factor(PAM_cbass1$Temperature)
PAM_cbass8$Fac.Temperature <- as.factor(PAM_cbass8$Temperature)
CN_cbass8$Fac.Temperature <- as.factor(CN_cbass8$Temperature)
VBA_cbass8$Fac.Temperature <- as.factor(VBA_cbass8$Temperature)

# Check column names of data
colnames(PAM_control)
colnames(PAM_cbass1)
colnames(PAM_cbass8)
colnames(CN_cbass8)
colnames(VBA_cbass8)

#check structure of data
str(PAM_control)
str(PAM_cbass1)
str(PAM_cbass8)
str(CN_cbass8)
str(VBA_cbass8)

# Manuscipt Figures
# Figure 3B
PAM_plot <-  ggplot(PAM_cbass8, aes(Fac.Temperature, Pam.mean)) + 
  geom_violin(fill = "grey90") + 
  geom_boxplot(width = .2, outlier.shape = NA, coef = 0)+ geom_point()+
  theme_classic()+
  theme(axis.text = element_text(size = 12,colour = "black"),
        axis.title = element_text(size=12),
        title = element_text(size=14))+
  ggtitle("Photosynthetic efficiency")+
  xlab("Temperature [°C]")+
  ylab("Photosynthetic efficiency [Fv/Fm]")
  
PAM_plot

# Figure 3C
VBA_plot <-  ggplot(VBA_cbass8, aes(Fac.Temperature, Bleaching.Mean)) + 
  geom_violin(fill = "grey90") + 
  geom_boxplot(width = .2, outlier.shape = NA, coef = 0)+ geom_point()+
  theme_classic()+
  theme(axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size=12),
        title = element_text(size=14))+
  ggtitle("Visual bleaching assessment")+
  xlab("Temperature [°C]")+
  ylab("Coral pigmentation [%]")

VBA_plot

# Figure 3D
CN_plot <-  ggplot(CN_cbass8, aes(Fac.Temperature, Cell.Nr.divided)) + 
  geom_violin(fill = "grey90") + 
  geom_boxplot(width = .2, outlier.shape = NA, coef = 0)+ geom_point()+
  theme_classic()+
  theme(axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size=12),
        title = element_text(size=14))+
  ggtitle("Symbiodiniaceae cell number")+
  xlab("Temperature [°C]")+
  ylab(bquote(Cell~number~(10^6)))

CN_plot

# plot Figure 3B-D in one plot_ no X-axis title for PAM and VBA
# Figure 3B
PAM_plot_notitle <-  ggplot(PAM_cbass8, aes(Fac.Temperature, Pam.mean)) + 
  geom_violin(fill = "grey90") + 
  geom_boxplot(width = .2, outlier.shape = NA, coef = 0)+ geom_point()+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 19, colour = "black"),
        title = element_text(size=19))+
  ggtitle("Photosynthetic efficiency")+
  xlab("Temperature [°C]")+
  ylab("Photosynthetic efficiency [Fv/Fm]")

PAM_plot_notitle

# Figure 3C
VBA_plot_notitle <-  ggplot(VBA_cbass8, aes(Fac.Temperature, Bleaching.Mean)) + 
  geom_violin(fill = "grey90") + 
  geom_boxplot(width = .2, outlier.shape = NA, coef = 0)+ geom_point()+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 19, colour = "black"),
        title = element_text(size=19))+
  ggtitle("Visual bleaching assessment")+
  xlab("Temperature [°C]")+
  ylab("Coral pigmentation [%]")

VBA_plot_notitle

# Figure 3D
CN_plot_title <-  ggplot(CN_cbass8, aes(Fac.Temperature, Cell.Nr.divided)) + 
  geom_violin(fill = "grey90") + 
  geom_boxplot(width = .2, outlier.shape = NA, coef = 0)+ geom_point()+
  theme_classic()+
  theme(axis.text = element_text(size = 19, colour = "black"),
        axis.title = element_text(size=19),
        title = element_text(size=19))+
  ggtitle("Symbiodiniaceae cell number")+
  xlab("Temperature [°C]")+
  ylab(bquote(Cell~number~(10^6)))

CN_plot_title

grid.arrange(PAM_plot_notitle, VBA_plot_notitle, CN_plot_title)

# Supplementary Data 
# Table 1
#(1) Is 30°C significantly affecting the photosynthetic efficiency of P.damicornis?
# Analysis of variance (1-way) ANOVA
pam_control_anova <- aov(Pam.mean ~ Fac.Temperature, data = PAM_control)
summary(pam_control_anova)
# p-value for temperature is not significant: 0.674
# Check with Tukey between temperatures
TukeyHSD(pam_control_anova)
plot (pam_control_anova)

# check if a different model would be better
# GLS model
pam_cbass1_gls <- gls(Pam.mean ~ Fac.Temperature, data = PAM_control)
summary(pam_control_gls)

# LM model
pam_control_lm <- lm(Pam.mean ~ Fac.Temperature, data = PAM_control)
summary(pam_control_lm)
AICc(pam_control_anova, pam_control_gls,pam_control_lm)
# AICc (AICC is for small sample groups) is smaller for Anova, therefore is anova the better
# Anova and lm is the same.The Tukey function after an anova model is better since it shows the difference between the groups

# Table 2:
#(1) How does temperature impact photosynthetic efficiency in the CBASS+ Microsensor assay?
# Analysis of variance (1-way) ANOVA
pam_cbass8_anova <- aov(Pam.mean ~ Fac.Temperature, data = PAM_cbass8)
summary(pam_cbass8_anova)
# p-value for temperature is significant: 2.28e-07 ***

# Check with Tukey between temperatures
TukeyHSD(pam_cbass8_anova)
# p-value is significant for: 36-30, 39-30, 36-33, 39-33,39-36°C
plot (pam_cbass8_anova)
# check normality and homogenity: nicely on the line maybe two outlier? and residuals: good distribution

# (2) How does temperature impact coral pigmentation in the CBASS+ Microsensor assay?
# Analysis of variance (1-way) ANOVA
VBA_cbass8_anova <- aov(Bleaching.Mean ~ Fac.Temperature, data = VBA_cbass8)
summary(VBA_cbass8_anova)
# p-value for temperature is significant: 0.0116 *

# Check with Tukey between temperatures
TukeyHSD(VBA_cbass8_anova)
# p-value is significant for: 36-30, 39-30, 36-33, 39-33,39-36°C
plot (pam_cbass8_anova)
# check normality and homogenity


# (3) How does temperature impact Symbioodiniaceae cell number in the CBASS+ Microsensor assay?
# Analysis of variance (1-way) ANOVA
CN_cbass8_anova <- aov(Cell.Nr ~ Fac.Temperature, data = CN_cbass8)
summary(CN_cbass8_anova)
# p-value for temperature is significant: 0.0214 *

# Check with Tukey between temperatures
TukeyHSD(CN_cbass8_anova)
# p-value is significant for: 36-30, 39-30, 36-33, 39-33,39-36°C
plot (pam_cbass8_anova)
# check normality and homogenity

# Supplement Figures
# Figure 1. CBASS pilot run to identify the baseline temperature
# Plot ED50 Model
PAM_cbass1_DRC = drm(Pam.mean ~ Temperature, data = PAM_cbass1,
                     fct = LL.3(names = c('hill', 'max', 'ed50')))

summary(PAM_cbass1_DRC)
plot(PAM_cbass1_DRC)
points(PAM_cbass1_DRC$Temperature, PAM_cbass1_DRC$Pam.mean)

PAM_cbass1_DRC$coefficients[3]
ED50_PAM_cbass1 <- ED(PAM_cbass1_DRC, c(50))[,1]

#### Coeffs ####
Coeffs<-c(PAM_cbass1_DRC$coefficients[3])
Data_Coeffs<-data.frame("ED50"=c(PAM_cbass1_DRC$coefficients[3]))

# plot
temp_x<- seq(20, 40, length = 100)
line_width=2
offsets<-c(0.1875,0.0625,-0.0625,-0.1875)

i<-1 #P. damicornis
matplot(temp_x, predict(PAM_cbass1_DRC, data.frame(Temp = temp_x), interval="confidence"),
        type="l",col="#2c7bb6",lty=c(1,3,3),lwd=line_width,ylab="Photosynthetic efficiency [Fv/Fm]",xlab="Temperature [°C]", xlim=c(26,40),ylim=c(0,0.7), cex.axis=1.5, cex.lab=1)
with(PAM_cbass1,matpoints(Temperature-offsets[i],Pam.mean,pch=18, col="#2c7bb6", cex=1.5))
legend("bottomleft","P. damicornis",pch=18, col="#2c7bb6",pt.cex=1.5, bty="n",cex=1)
title(main="Photosynthesic efficiency")

# Figure 6
# Plot ED50 Model
PAM_cbass8_DRC = drm(Pam.mean ~ Temperature, data = PAM_cbass8,
                     fct = LL.3(names = c('hill', 'max', 'ed50')))

summary(PAM_cbass8_DRC)
plot(PAM_cbass8_DRC)
points(PAM_cbass8_DRC$Temperature, PAM_cbass8_DRC$Pam.mean)

PAM_cbass8_DRC$coefficients[3]
ED50_PAM_cbass8 <- ED(PAM_cbass8_DRC, c(50))[,1]

#### Coeffs ####
Coeffs<-c(PAM_cbass8_DRC$coefficients[3])
Data_Coeffs<-data.frame("ED50"=c(PAM_cbass8_DRC$coefficients[3]))

# plot
temp_x<- seq(30, 40, length = 100)
line_width=2
offsets<-c(0.1875,0.0625,-0.0625,-0.1875)

i<-1 #P. damicornis
matplot(temp_x, predict(PAM_cbass8_DRC, data.frame(Temp = temp_x), interval="confidence"),
        type="l",col="#2c7bb6",lty=c(1,3,3),lwd=line_width,ylab="Photosynthetic efficiency [Fv/Fm]",xlab="Temperature [°C]", xlim=c(30,39.5),ylim=c(0,0.68), cex.axis=1.5, cex.lab=1)
with(PAM_cbass8,matpoints(Temperature-offsets[i],Pam.mean,pch=18, col="#2c7bb6", cex=1.5))
legend("bottomleft","P. damicornis",pch=18, col="#2c7bb6",pt.cex=1.5, bty="n",cex=1)
title(main="Photosynthesic efficiency")
abline(v=ED50_PAM_cbass8, col=c("#2c7bb6"))
text(paste(ED50_PAM_cbass8), c(0.1),labels=paste(as.character(round(ED50_PAM_cbass8, digits=1.5)),"°C",sep=""),col=c("#2c7bb6"), cex=1.5)

