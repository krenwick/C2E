################################################################################
# Examine output from model runs with species-specific parameters
# NOTE: make sure model is the C2E version!
# CHANGE FILE PATH lines 22 and 29 if on a different computer
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=10))
library(data.table)
library(zoo)
library(grid)
library(gridExtra)

# SET WORKING DIRECTORY:
setwd("~/Documents/C2E/")
outname1 <- "OrigRun"
outname2 <- "OrigRun_Irr"

#-------------------------------------------------------------------------------

# Run model with new ambient conditions:
# CHECK: is correct model compiled???
system("/Users/poulterlab1/version-control/LPJ-GUESS/ModelFiles/modules/./guess /Users/poulterlab1/Documents/C2E/Code/OriginalParms.ins")

################################################################################
# Re-run model using irrigation driver data

# Run model with new ins then compare output to flux data:
# CHECK: is correct model compiled???
system("/Users/poulterlab1/version-control/LPJ-GUESS/ModelFiles/modules/./guess /Users/poulterlab1/Documents/C2E/Code/OriginalParms_Irr.ins")

################################################################################
# PLOT TIME!
################################################################################
# Read in reference data from Konza:
k <- read.csv("Data/RefData/anpp_1991_2012.csv")

# Model output: ambient
npp <- read.table("Output/anpp_OrigRun.txt", header=T) %>%
  mutate(Source="Model", Treatment="control") %>%
  mutate_at(vars(ANGE:Total), funs(.*2000)) %>%
  mutate(year=Year+860) %>%
  filter(year>=1991) %>%
  select(year,Source,Treatment,Total) %>%
  mutate(Total=Total*0.4761905) %>% # so just above-ground
  rename(NPP=Total)

# Model output: irrigation
npp2 <- read.table("Output/anpp_OrigRun_Irr.txt", header=T) %>%
  mutate(Source="Model", Treatment="irrigated") %>%
  mutate_at(vars(ANGE:Total), funs(.*2000)) %>%
  mutate(year=Year+860) %>%
  filter(year>=1991) %>%
  select(year,Source,Treatment,Total) %>%
  mutate(Tot=ifelse(year<=(1990), Total*0.4761905, NA)) %>%
  mutate(Tot=ifelse(year>1990 & year<2000, Total*0.5235602, Tot)) %>%
  mutate(Tot=ifelse(year>=2000, Total*0.5494505, Tot)) %>%
  select(-Total) %>%
  rename(NPP=Tot)

knpp <- k %>% rename(Treatment=treatment, NPP=anpp_gm2) %>%
  mutate(Source="Field") %>%
  select(year,Source,Treatment,NPP)

npp1 <- rbind(npp,npp2,knpp) %>%
  rename(Year=year)

mod <- filter(npp1, Source=="Model")
field <- filter(npp1, Source=="Field")
ggplot(data=field, aes(x=Year, y=NPP)) +
  geom_bar(stat="identity", fill="lightgray", color="black") +
  geom_point(data=mod, color="chartreuse4", size=2) +
  ylab(expression(ANPP~(g~m^{-2}))) +
  xlab("Year") +
  facet_wrap(~Treatment)

# what is the R2?
r <- spread(npp1, Source, NPP) 
rc<- filter(r, Treatment=="control")
ri<- filter(r, Treatment=="irrigated")
summary(lm(data=rc, Field~Model)) # -0.03, not significant
summary(lm(data=ri, Field~Model)) # .098 with standard values

# What is the mean abosolute error?
r %>% mutate(diff=abs(Field-Model)) %>%
  group_by(Treatment) %>%
  summarize(mean(diff))
# control: 82
# irrigated: 204
# all standard parms, drought tol .0001: 82/203, .00/.10
# standard but old (19.6) SLA: 87/220, .02/.21
################################################################################
# Plot annual FPC by species
################################################################################
fpc1 <- read.table("Output/fpc_OrigRun.txt", header=T) %>%
  mutate(Treatment="Ambient")
fpc3 <- read.table("Output/fpc_OrigRun_Irr.txt", header=T) %>%
  mutate(Treatment="Irrigation")

fpc <- rbind.data.frame(fpc1,fpc3) %>%
  mutate(Year=Year+860) %>%
  filter(Year>=1980) %>%
  gather(Species, FPC, ANGE:PAVI)

# (tried full time series- converges early)
ggplot(data=fpc, aes(x=Year,y=FPC, color=Species)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~Treatment)

################################################################################
# Make some diagnostic plots
################################################################################
# Plot ambient vs. irrigation
m1 <- spread(mod, Treatment, NPP)
ggplot(data=m1, aes(x=control, y=irrigated)) +
  geom_point() +
  geom_abline(slope=1,intercept=0) +
  coord_fixed(xlim=c(200,800), ylim=c(200,800))

################################################################################
# Plot annual NPP by species- same pattern as FPC?
################################################################################
# Model output: ambient
npp <- read.table("Output/anpp_OrigRun.txt", header=T) %>%
  mutate(Source="Model", Treatment="control") %>%
  mutate_at(vars(ANGE:Total), funs(.*2000)) %>%
  mutate(Year=Year+860) %>%
  filter(Year>=1991) %>%
  mutate_at(vars(ANGE:Total), funs(.*0.4761905)) %>%
  select(Year,Source,Treatment,ANGE:PAVI) %>%
  gather(Species, ANPP, ANGE:PAVI)

# Model output: irrigation
npp2 <- read.table("Output/anpp_OrigRun_Irr.txt", header=T) %>%
  mutate(Source="Model", Treatment="irrigated") %>%
  mutate_at(vars(ANGE:Total), funs(.*2000)) %>%
  mutate(Year=Year+860) %>%
  filter(Year>=1991) %>%
  mutate_at(vars(ANGE:Total), funs(ifelse(Year<=1990, .*0.4761905, 
                                          ifelse(Year>1990 & Year<2000, .*0.5235602,
                                                 .*0.5494505)))) %>%
  select(Year,Source,Treatment,ANGE:PAVI) %>%
  gather(Species, ANPP, ANGE:PAVI)


npp3 <- rbind(npp,npp2) 

ggplot(data=npp3, aes(x=Year, y=ANPP, color=Species)) +
  geom_point() +
  geom_line() +
  ylab(expression(ANPP~(g~m^{-2}))) +
  facet_wrap(~Treatment)

