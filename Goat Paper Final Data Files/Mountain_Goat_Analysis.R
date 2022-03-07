###### Mountain Goat Data Analysis ######

setwd()

library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(dplyr)
theme_set(theme_pubr())


### Coastal and Interior Density Bar Plot ###

Coast_Interior <- read.csv("Density_Location_Clean.csv",header=TRUE,sep=",",stringsAsFactors = FALSE)

Coast_Interior_Frame <- Coast_Interior %>% 
  group_by(Location) %>%
  summarise(grp.mean = mean(Density))
Coast_Interior_Frame

Dens_Coast_Int <- ggplot(Coast_Interior, aes(x = Density))
Dens_Coast_Int + geom_histogram(binwidth = 0.2, aes(color = Location, fill = Location),
                                alpha = 0.4, position = "dodge") +
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=14), 
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=14),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        plot.margin = unit(c(0,1,0,0.5), "cm")) +
  ylab("Count") + xlab(bquote('Goats /'~km^2)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_y_continuous(expand=c(0,0), breaks = c(0,2,4,6,8)) +
  scale_x_continuous(expand=c(0,0)) +
  geom_vline(aes(xintercept = 0.25), 
             linetype = "dashed", size = 1, col = "red") +
  geom_vline(aes(xintercept = grp.mean, color = Location),
             data = Coast_Interior_Frame, linetype = "dashed", size = 1)

##### Interview Data #####

#Read in dataframe
data<-read.csv("Interview_Data.csv")
head(data)

#clean variable names
data<-dplyr::rename(data, pn = ï..pn)
data<-dplyr::rename(data, "2019" = X2019)
data<-dplyr::rename(data, "2010s" = X2010s)
data<-dplyr::rename(data, "2000s" = X2000s)
data<-dplyr::rename(data, "1990s" = X1990s)
data<-dplyr::rename(data, "1980s" = X1980s)

#Reshape data structure
rawdata<-data
data <- reshape::melt(data, id=c("pn"))

#Rename variable
data<-dplyr::rename(data, time = variable)
#Data<-dplyr::rename(data, "estimated number of days out of out 10 with mtn goat sighting" = value)
data$time<-as.factor(data$time)
data$time<-(droplevels(data)$time)

data$pn<-as.factor(data$pn)
data$pn<-(droplevels(data)$pn)

# Plotting - Reverse x-axis, clean, and remove background

p2 <- ggplot(data = data, aes(x = time, y = value, group = pn, color = pn)) +
  geom_line(size=1) + 
  geom_point() +
  theme_bw() + 
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=14, colour = "black"), 
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=14, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits=c(0, 10), breaks = seq(0, 10, 2)) +
  scale_x_discrete(limits = unique(rev(data$time))) +
  ylab("Estimated days/10 with mtn goat sighting") +
  xlab("Time Period")
plot(p2)


##### CPUE Hunt Data #####

library(mgcv)
library(rlist)
library(tidyverse) 
library(dplyr)
library(gamm4)

# Import hunt data, and split by coastal and interior WMUs. Note that the CSV file removes species other than mountain goats, removes the '_99" WMUs that are summaries of WMUs, has calculated KPD (for reference) and places data in long format.

CPUE_R_NR_Hunters_Calculated_Split <- read.csv("BGHS_Clean_GAM.csv", header = TRUE, sep = ",",stringsAsFactors = FALSE)
CPUE_R_NR_Hunters_Calculated_Split$LOCATION <- ifelse(CPUE_R_NR_Hunters_Calculated_Split$WMU %in% c(103,110,112,113,114,115,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,313,314,315,316,332,333,504,505,506,507,508,509,510,511,601,602,603,604,608,609,610,611,613,614,615,616,617,621,622,626,627,628,629,630,805), 'Coastal', 'Interior')
CPUE_R_NR_Hunters_Calculated_Split$LOCATION <- as.factor(CPUE_R_NR_Hunters_Calculated_Split$LOCATION)
CPUE_R_NR_Hunters_Calculated_Split$R_NR <- as.factor(CPUE_R_NR_Hunters_Calculated_Split$R_NR)
CPUE_R_NR_Hunters_Calculated_Split$WMU <- as.factor(CPUE_R_NR_Hunters_Calculated_Split$WMU)

# Optional: create separate objects for coastal and interior areas
CPUE_R_NR_Hunters_Calculated_Split_Coastal <- CPUE_R_NR_Hunters_Calculated_Split %>%
  filter(WMU %in% c(114,115,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,313,314,315,316,332,333,504,505,506,507,508,509,510,511,601,602,603,604,608,609,610,611,614,615,616,617,621,622,626,627,628,629,630,805)) %>%
  filter(!is.na(KPD))

CPUE_R_NR_Hunters_Calculated_Split_Interior <- CPUE_R_NR_Hunters_Calculated_Split %>%
  filter(!WMU %in% c(114,115,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,313,314,315,316,332,333,504,505,506,507,508,509,510,511,601,602,603,604,608,609,610,611,614,615,616,617,621,622,626,627,628,629,630,805)) %>%
  filter(!is.na(KPD))

# Create columns for residency status and location

CPUE_dat_mutated1 <- CPUE_R_NR_Hunters_Calculated_Split %>%
  mutate(R_Coastal = +(R_NR == "R" & LOCATION == "Coastal")) %>%
  mutate(R_Interior = +(R_NR == "R" & LOCATION == "Interior")) %>%
  mutate(NR_Coastal = +(R_NR == "NR" & LOCATION == "Coastal")) %>%
  mutate(NR_Interior = +(R_NR == "NR" & LOCATION == "Interior")) %>%
  mutate(R = +(R_NR == "R")) %>%
  mutate(NR = +(R_NR == "NR")) %>%
  mutate(Coastal = +(LOCATION == "Coastal")) %>%
  mutate(Interior = +(LOCATION == "Interior")) %>%
  mutate(Log_Days = +(log(DAYS))) %>%
  mutate(R_NR_Location = paste(R_NR, LOCATION, sep = ""))
CPUE_dat_mutated1$KILLS <- as.numeric(CPUE_dat_mutated1$KILLS)
CPUE_dat_mutated1$R_Coastal <- as.numeric(CPUE_dat_mutated1$R_Coastal)
CPUE_dat_mutated1$NR_Coastal <- as.numeric(CPUE_dat_mutated1$NR_Coastal)
CPUE_dat_mutated1$R_Interior <- as.numeric(CPUE_dat_mutated1$R_Interior)
CPUE_dat_mutated1$NR_Interior <- as.numeric(CPUE_dat_mutated1$NR_Interior)
CPUE_dat_mutated1$R <- as.numeric(CPUE_dat_mutated1$R)
CPUE_dat_mutated1$NR <- as.numeric(CPUE_dat_mutated1$NR)
CPUE_dat_mutated1$Coastal <- as.numeric(CPUE_dat_mutated1$Coastal)
CPUE_dat_mutated1$Interior <- as.numeric(CPUE_dat_mutated1$Interior)
CPUE_dat_mutated1$Log_Days <- as.numeric(CPUE_dat_mutated1$Log_Days)
CPUE_dat_mutated1$HUNT_YEAR <- as.numeric(CPUE_dat_mutated1$HUNT_YEAR)

# Filter NA values

CPUE_dat_mutated1 <- CPUE_dat_mutated1 %>%
  filter(!is.na(KILLS))

CPUE_dat_mutated1 <- CPUE_dat_mutated1 %>%
  filter(!is.na(DAYS)) %>%
  filter(DAYS > 0)

# Change WMU and R_NR_Location to factors

CPUE_dat_mutated1$WMU <- as.factor(CPUE_dat_mutated1$WMU)
CPUE_dat_mutated1$R_NR_Location <- as.factor(CPUE_dat_mutated1$R_NR_Location)

#Check to make sure

class(CPUE_dat_mutated1$R_NR_Location)


##### Data inspection plots and summary stats #####

CPUE_dat_mutated1 %>% group_by(LOCATION) %>% tally()
CPUE_dat_mutated1 %>% group_by(R_NR) %>% tally()
plot(CPUE_dat_mutated1$LOCATION)
plot(CPUE_dat_mutated1$R_NR)

hist(CPUE_dat_mutated1$KILLS)
hist(CPUE_dat_mutated1$DAYS)

R_NR_Summary_Plot <- CPUE_dat_mutated1 %>% count(WMU, R_NR)
ggplot(R_NR_Summary_Plot, aes(color = R_NR, fill = R_NR, x = WMU, y = n)) +
  geom_bar(stat = "identity", position = "dodge")

C_I_Summary_Plot <- CPUE_dat_mutated1 %>% count(WMU, LOCATION)
ggplot(C_I_Summary_Plot, aes(color = LOCATION, fill = LOCATION, x = WMU, y = n)) +
  geom_bar(stat = "identity", position = "dodge")

# Creating total kills over time plot - here each dot corresponds to a different WMU at a specific time
Kills_Over_Time <- ggplot(CPUE_dat_mutated1, aes(x = HUNT_YEAR, y = KILLS)) +
  geom_point()+
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=16), 
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0,1,0,0.5), "cm")) +
  ylab("Mountain Goat Kills") + xlab("Year")
Kills_Over_Time

Kills_Per_Day_Over_Time <- ggplot(CPUE_dat_mutated1, aes(x = HUNT_YEAR, y = KILLS/DAYS)) +
  geom_point()+
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=16), 
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0,1,0,0.5), "cm")) +
  ylab("Mountain Goat Kills/Day") + xlab("Year")
Kills_Per_Day_Over_Time

Hunters <- ggplot(CPUE_dat_mutated1, aes(x = HUNT_YEAR, y = HUNTERS)) +
  geom_point()+
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=16), 
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0,1,0,0.5), "cm")) +
  ylab("Hunters") + xlab("Year")
Hunters


##### GAMM Model Fitting #####

# Removing 1970's data to align with interview data start (1980)

CPUE_Dat_1980 <- CPUE_dat_mutated1 %>%
  filter(HUNT_YEAR >= 1980)

summary(CPUE_Dat_1980$R_NR)

# Null model with intercept and WMU
CPUE_GAM_Intercept_WMU_gamm4_1980 <- gamm4(KILLS ~ s(HUNT_YEAR) + offset(Log_Days), 
                                           random = ~(1 | WMU), data = CPUE_Dat_1980, family = poisson(link = "log"))

# R vs NR Hunters model
CPUE_GAM_R_NR_gamm4_1980 <- gamm4(KILLS ~ s(HUNT_YEAR) + offset(Log_Days) + #implicitly, this makes the 'baseline' resident hunters
                                    s(HUNT_YEAR, by=NR), random = ~(1 | WMU), data = CPUE_Dat_1980, family = poisson(link = "log"))

# Coastal vs Interior Model
CPUE_GAM_C_I_gamm4_1980 <- gamm4(KILLS ~ s(HUNT_YEAR) + offset(Log_Days) + #implicitly, this makes the 'baseline' Interior WMUs
                                   s(HUNT_YEAR, by=Coastal), random = ~(1 | WMU), data = CPUE_Dat_1980, family = poisson(link = "log"))

# No interaction effect of resident status and location
CPUE_GAM_R_NR_C_I_No_Interaction_gamm4_1980 <- gamm4(KILLS ~ s(HUNT_YEAR) + offset(Log_Days) +
                                                       s(HUNT_YEAR, by=NR) +
                                                       s(HUNT_YEAR, by=Coastal),
                                                     random = ~(1 | WMU), data = CPUE_Dat_1980, family = poisson(link = "log"))
# With interaction
CPUE_GAM_R_NR_C_I_gamm4_1980 <- gamm4(KILLS ~ s(HUNT_YEAR) + offset(Log_Days) +
                                        s(HUNT_YEAR, by=NR) +
                                        s(HUNT_YEAR, by=Coastal) +
                                        s(HUNT_YEAR, by=NR_Coastal),
                                      random = ~(1 | WMU), data = CPUE_Dat_1980, family = poisson(link = "log"))
plot(CPUE_GAM_R_NR_C_I_gamm4_1980$gam)
summary(CPUE_GAM_R_NR_C_I_gamm4_1980$gam)

#Loglikelihood of mer (lienar) component
logLik(CPUE_GAM_Intercept_WMU_gamm4_1980$mer)
logLik(CPUE_GAM_R_NR_gamm4_1980$mer)
logLik(CPUE_GAM_C_I_gamm4_1980$mer)
logLik(CPUE_GAM_R_NR_C_I_No_Interaction_gamm4_1980$mer)
logLik(CPUE_GAM_R_NR_C_I_gamm4_1980$mer)

# AIC
AIC(CPUE_GAM_Intercept_WMU_gamm4_1980$mer, 
    CPUE_GAM_R_NR_gamm4_1980$mer, 
    CPUE_GAM_C_I_gamm4_1980$mer, 
    CPUE_GAM_R_NR_C_I_No_Interaction_gamm4_1980$mer, 
    CPUE_GAM_R_NR_C_I_gamm4_1980$mer)

# Checks
gam.check(CPUE_GAM_Intercept_WMU_gamm4_1980$gam)
gam.check(CPUE_GAM_R_NR_gamm4_1980$gam)
gam.check(CPUE_GAM_R_NR_C_I_No_Interaction_gamm4_1980$gam)
gam.check(CPUE_GAM_R_NR_C_I_gamm4_1980$gam)

# Using 'performance' package
library(performance)
library(see)

check_model(CPUE_GAM_Intercept_WMU_gamm4_1980$mer)
check_model(CPUE_GAM_R_NR_gamm4_1980$mer)
check_model(CPUE_GAM_C_I_gamm4_1980$mer)
check_model(CPUE_GAM_R_NR_C_I_gamm4_1980$mer)
model_performance(CPUE_GAM_Intercept_WMU_gamm4_1980$mer)
model_performance(CPUE_GAM_R_NR_gamm4_1980$mer)
model_performance(CPUE_GAM_C_I_gamm4_1980$mer)
model_performance(CPUE_GAM_R_NR_C_I_gamm4_1980$mer)

###

# Plotting CPUE model

plot.CPUE.null.mod = function(model = CPUE_GAM_C_I_gamm4_1980, trans = sqrt){
  model.terms = attr(model$gam$terms,'term.labels')[-1]
  min.year = 1980
  dat = CPUE_Dat_1980
  pred.dat = data.frame(HUNT_YEAR=seq(min.year,2018,0.1), Log_Days = 0, KILLS = NA, Coastal = 0, NR = 0, NR_Coastal = 0)
  mod.pred = predict(object = model$gam, newdata = pred.dat, type = 'link', se.fit = TRUE)
  pred.dat$KPD = exp(mod.pred$fit) 
  
  null.subset = rep(TRUE, nrow(dat))
  if(length(model.terms) > 0){
    for(column in model.terms){
      null.subset = null.subset & dat[,column] != 1
    }
  }
  plot(dat$HUNT_YEAR[null.subset], trans(dat$KPD[null.subset]), pch = 16, col = '00000000', ann = FALSE, ylim = c(0,0.5))
  lines(pred.dat$HUNT_YEAR, trans(pred.dat$KPD), col = "black")
  title(xlab = 'Year', ylab = 'sqrt(KPD)')
  
  pred.dat$UCI = exp(mod.pred$fit + 2*mod.pred$se.fit)
  pred.dat$LCI = exp(mod.pred$fit - 2*mod.pred$se.fit)
  
  polygon(c(pred.dat$HUNT_YEAR, rev(pred.dat$HUNT_YEAR)), 
          trans(c(pred.dat$LCI, rev(pred.dat$UCI))),
          border=NA, col = '#00000025')
}

plot.CPUE.null.mod()

plot.CPUE.overlay.mod = function(model = CPUE_GAM_C_I_gamm4_1980, trans = sqrt, 
                                 use.terms = list(Coastal = 1, NR = 1, NR_Coastal = 1),
                                 point.col = '#ff000015', line.col = 'red', ci.col = '#ff000025'){
  model.terms = attr(model$gam$terms,'term.labels')[-1]
  min.year = 1980; if(use.terms$NR == 1 & 'NR' %in% model.terms) min.year = 1981
  dat = CPUE_Dat_1980
  pred.dat = data.frame(HUNT_YEAR=seq(min.year,2018,0.1), Log_Days = 0, KILLS = NA, Coastal = use.terms$Coastal, NR = use.terms$NR, NR_Coastal = use.terms$NR_Coastal)
  mod.pred = predict(object = model$gam, newdata = pred.dat, type = 'link', se.fit = TRUE)
  pred.dat$KPD = exp(mod.pred$fit) 
  
  subset = rep(1, nrow(dat))
  for(column in model.terms){
    subset = subset & dat[,column] == use.terms[[column]]
  }
  points(dat$HUNT_YEAR[subset], trans(dat$KPD[subset]), pch = 16, col = point.col)
  lines(pred.dat$HUNT_YEAR, trans(pred.dat$KPD), col = line.col)
  
  pred.dat$UCI = exp(mod.pred$fit + 2*mod.pred$se.fit)
  pred.dat$LCI = exp(mod.pred$fit - 2*mod.pred$se.fit)
  
  polygon(c(pred.dat$HUNT_YEAR, rev(pred.dat$HUNT_YEAR)), 
          trans(c(pred.dat$LCI, rev(pred.dat$UCI))),
          border=NA, col = ci.col)
}

plot.CPUE.overlay.mod()

# Full mod model with points
plot.CPUE.null.mod(model = CPUE_GAM_R_NR_C_I_gamm4_1980)
plot.CPUE.overlay.mod(model = CPUE_GAM_R_NR_C_I_gamm4_1980, use.terms = list(Coastal = 1, NR = 1, NR_Coastal = 1),
                      point.col = adjustcolor('#ff000015', alpha.f = 1.5), line.col = 'red', ci.col = '#ff000025')

plot.CPUE.overlay.mod(model = CPUE_GAM_R_NR_C_I_gamm4_1980, use.terms = list(Coastal = 1, NR = 0, NR_Coastal = 0),
                      point.col = adjustcolor('#0000ff15', alpha.f = 1.5), line.col = 'blue', ci.col = '#0000ff25')

plot.CPUE.overlay.mod(model = CPUE_GAM_R_NR_C_I_gamm4_1980, use.terms = list(Coastal = 0, NR = 1, NR_Coastal = 0),
                      point.col = adjustcolor('#ffaa0015', alpha.f = 1.5), line.col = 'yellow', ci.col = '#ffaa0025')

# Full mod model without points
plot.CPUE.null.mod(model = CPUE_GAM_R_NR_C_I_gamm4_1980)
plot.CPUE.overlay.mod(model = CPUE_GAM_R_NR_C_I_gamm4_1980, use.terms = list(Coastal = 1, NR = 1, NR_Coastal = 1),
                      point.col = adjustcolor('#ff000015', alpha.f = 0), line.col = 'red', ci.col = '#ff000025')

plot.CPUE.overlay.mod(model = CPUE_GAM_R_NR_C_I_gamm4_1980, use.terms = list(Coastal = 1, NR = 0, NR_Coastal = 0),
                      point.col = adjustcolor('#0000ff15', alpha.f = 0), line.col = 'blue', ci.col = '#0000ff25')

plot.CPUE.overlay.mod(model = CPUE_GAM_R_NR_C_I_gamm4_1980, use.terms = list(Coastal = 0, NR = 1, NR_Coastal = 0),
                      point.col = adjustcolor('#ffaa0015', alpha.f = 0), line.col = 'yellow', ci.col = '#ffaa0025')

# Plotting without sqrt trans and reduced y-axis

par(mar=c(4.5,4.5,4,6))

plot.CPUE.null.mod = function(model = CPUE_GAM_C_I_gamm4_1980){
  model.terms = attr(model$gam$terms,'term.labels')[-1]
  min.year = 1980
  dat = CPUE_Dat_1980
  pred.dat = data.frame(HUNT_YEAR=seq(min.year,2018,0.1), Log_Days = 0, KILLS = NA, Coastal = 0, NR = 0, NR_Coastal = 0)
  mod.pred = predict(object = model$gam, newdata = pred.dat, type = 'link', se.fit = TRUE)
  pred.dat$KPD = exp(mod.pred$fit) 
  
  null.subset = rep(TRUE, nrow(dat))
  if(length(model.terms) > 0){
    for(column in model.terms){
      null.subset = null.subset & dat[,column] != 1
    }
  }
  plot(dat$HUNT_YEAR[null.subset], dat$KPD[null.subset], pch = 16, col = '00000000', ann = FALSE, ylim = c(0,0.2))
  lines(pred.dat$HUNT_YEAR, pred.dat$KPD, col = "black")
  title(xlab = 'Year', ylab = 'Kills per Day', cex.lab=1.5)
  
  pred.dat$UCI = exp(mod.pred$fit + 2*mod.pred$se.fit)
  pred.dat$LCI = exp(mod.pred$fit - 2*mod.pred$se.fit)
  
  polygon(c(pred.dat$HUNT_YEAR, rev(pred.dat$HUNT_YEAR)), 
          c(pred.dat$LCI, rev(pred.dat$UCI)),
          border=NA, col = '#00000025')
}

plot.CPUE.null.mod()

plot.CPUE.overlay.mod = function(model = CPUE_GAM_C_I_gamm4_1980, 
                                 use.terms = list(Coastal = 1, NR = 1, NR_Coastal = 1),
                                 point.col = '#ff000015', line.col = 'red', ci.col = '#ff000025'){
  model.terms = attr(model$gam$terms,'term.labels')[-1]
  min.year = 1980; if(use.terms$NR == 1 & 'NR' %in% model.terms) min.year = 1981
  dat = CPUE_Dat_1980
  ylim = c(0,0.2)
  pred.dat = data.frame(HUNT_YEAR=seq(min.year,2018,0.1), Log_Days = 0, KILLS = NA, Coastal = use.terms$Coastal, NR = use.terms$NR, NR_Coastal = use.terms$NR_Coastal)
  mod.pred = predict(object = model$gam, newdata = pred.dat, type = 'link', se.fit = TRUE)
  pred.dat$KPD = exp(mod.pred$fit) 
  
  subset = rep(1, nrow(dat))
  for(column in model.terms){
    subset = subset & dat[,column] == use.terms[[column]]
  }
  points(dat$HUNT_YEAR[subset], dat$KPD[subset], pch = 16, col = point.col)
  lines(pred.dat$HUNT_YEAR, pred.dat$KPD, col = line.col)
  
  pred.dat$UCI = exp(mod.pred$fit + 2*mod.pred$se.fit)
  pred.dat$LCI = exp(mod.pred$fit - 2*mod.pred$se.fit)
  
  polygon(c(pred.dat$HUNT_YEAR, rev(pred.dat$HUNT_YEAR)), 
          c(pred.dat$LCI, rev(pred.dat$UCI)),
          border=NA, col = ci.col)
}

plot.CPUE.overlay.mod()

# Full mod model with points
plot.CPUE.null.mod(model = CPUE_GAM_R_NR_C_I_gamm4_1980)
plot.CPUE.overlay.mod(model = CPUE_GAM_R_NR_C_I_gamm4_1980, use.terms = list(Coastal = 1, NR = 1, NR_Coastal = 1),
                      point.col = adjustcolor('#ff000015', alpha.f = 1.5), line.col = 'red', ci.col = '#ff000025')

plot.CPUE.overlay.mod(model = CPUE_GAM_R_NR_C_I_gamm4_1980, use.terms = list(Coastal = 1, NR = 0, NR_Coastal = 0),
                      point.col = adjustcolor('#0000ff15', alpha.f = 1.5), line.col = 'blue', ci.col = '#0000ff25')

plot.CPUE.overlay.mod(model = CPUE_GAM_R_NR_C_I_gamm4_1980, use.terms = list(Coastal = 0, NR = 1, NR_Coastal = 0),
                      point.col = adjustcolor('#ffaa0015', alpha.f = 1.5), line.col = 'yellow', ci.col = '#ffaa0025')

# Full mod model without points
plot.CPUE.null.mod(model = CPUE_GAM_R_NR_C_I_gamm4_1980)
plot.CPUE.overlay.mod(model = CPUE_GAM_R_NR_C_I_gamm4_1980, use.terms = list(Coastal = 1, NR = 1, NR_Coastal = 1),
                      point.col = adjustcolor('#ff000015', alpha.f = 0), line.col = 'red', ci.col = '#ff000025')

plot.CPUE.overlay.mod(model = CPUE_GAM_R_NR_C_I_gamm4_1980, use.terms = list(Coastal = 1, NR = 0, NR_Coastal = 0),
                      point.col = adjustcolor('#0000ff15', alpha.f = 0), line.col = 'blue', ci.col = '#0000ff25')

plot.CPUE.overlay.mod(model = CPUE_GAM_R_NR_C_I_gamm4_1980, use.terms = list(Coastal = 0, NR = 1, NR_Coastal = 0),
                      point.col = adjustcolor('#ffa50015', alpha.f = 0), line.col = 'orange', ci.col = '#ffa50050')



##### M_F_J Ratios ####

library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(reshape2)

M_F_J_Ratios  <- read.csv("M_F_J_Ratios.csv", header = TRUE, sep = ",",stringsAsFactors = FALSE)
F_Ratios_509 <- read.csv("509_Female_Ratios.csv", header = TRUE, sep = ",",stringsAsFactors = FALSE)
F_Ratios_603 <- read.csv("603_Female_Ratios.csv", header = TRUE, sep = ",",stringsAsFactors = FALSE)
Total_Kills_509_603 <- read.csv("509_603_Total_Kills.csv", header = TRUE, sep = ",",stringsAsFactors = FALSE)
Total_Kills_509_603_subset = Total_Kills_509_603[,c(1,3,4)]
Total_Kills_509_603_subset_melted = melt(Total_Kills_509_603_subset, id=c("YEAR"))

Total_Kills <- ggplot(Total_Kills_509_603_subset_melted) + 
  geom_line(aes(x = YEAR, y = value, color = variable), stat = "identity", size = 1) +
  geom_point(aes(x = YEAR, y = value, color = variable), stat = "identity", size = 1) +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(size=16), 
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        legend.position = c(0.95,0.8),
        plot.margin = unit(c(0.5,1,0,0.5), "cm")) +
  ylab("Kills") + xlab(bquote("Year")) + 
  ylim(0, 20) +
  scale_color_manual(name = "WMU", labels = c("509", "603"), values=c("grey", "black")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

Total_Kills

Prop_Female_509 <- ggplot(F_Ratios_509, aes(x = YEAR, y = FEMALE_RATIO)) + 
  geom_bar(stat = "identity", color="grey", fill = "grey", width = 0.75) +
  geom_text(aes(label= KILLS), position = position_dodge(1.5), size=3, vjust=-0.35) +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(size=16), 
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        plot.margin = unit(c(0.5,1,0,0.5), "cm")) +
  ylab("Prop. Female (509)") + xlab(bquote("Year")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

Prop_Female_509

Prop_Female_603 <- ggplot(F_Ratios_603, aes(x = YEAR, y = FEMALE_RATIO)) + 
  geom_bar(stat = "identity", color="black", fill = "black", width = 0.75) +
  geom_text(aes(label= KILLS), position = position_dodge(1.5), size=3, vjust=-0.35) +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16), 
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        plot.margin = unit(c(0.5,1,0,0.5), "cm")) +
  ylab("Prop. Female (603)") + xlab(bquote("Year")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

Prop_Female_603

library(cowplot)
plot_grid(Total_Kills, Prop_Female_509, Prop_Female_603, align = "h", axis = "bt", labels = "AUTO", ncol = 1)


##### Elevation with KX Territory

KX_Elev_Dat <- read.csv("KX_TT_Mainland_Elev.csv", header = TRUE, sep = ",",stringsAsFactors = FALSE)

KX_Elev_Plot <- ggplot(KX_Elev_Dat, aes(x=VALUE, y=COUNT, width=10)) + 
  geom_bar(stat="identity") +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16), 
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        plot.margin = unit(c(0.5,1,0.5,0.5), "cm")) +
  ylab("Pixel Count") + 
  xlab("Elevation") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  geom_vline(aes(xintercept = 600.69), 
             linetype = "dashed", size = 0.6, col = "red")
KX_Elev_Plot


