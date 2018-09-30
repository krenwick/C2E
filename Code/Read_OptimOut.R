################################################################################
# Examine output from hyalite optimization runs
# NOTE: make sure model on git is MASTER and compiled with old phenology!
# New phenology model is in LPJfiles
################################################################################
rm(list=ls())
library(DEoptim)
library(tidyverse); theme_set(theme_bw(base_size=10))
library(data.table)
library(zoo)
library(grid)
library(gridExtra)

# SET WORKING DIRECTORY:
setwd("~/Documents/C2E/")
outname1 <- "OptimRun"
outname2 <- "OptimRun_Irr"
object1 <- "OptimOut1.RData"

#-------------------------------------------------------------------------------
# Get object from DEparoptim_summergreen1:

load(object1)
summary(DE1)
DE1$optim
LPJG
ins
########################
# Make new ins file with these parameter values:
insfile <- "Code/OptimDummy.ins" # name of ins file to use
# gsub: replaces all occurences of a pattern
ins  <- readLines(insfile)
tx  <- gsub(pattern = "drought_tolval1", replace = DE1$optim$bestmem[1], x = ins)
tx  <- gsub(pattern = "drought_tolval2", replace = DE1$optim$bestmem[2], x = tx)
tx  <- gsub(pattern = "aphenmaxval1", replace = DE1$optim$bestmem[3], x = tx)
tx  <- gsub(pattern = "aphenmaxval2", replace = DE1$optim$bestmem[4], x = tx)
tx  <- gsub(pattern = "ltormaxval1", replace = DE1$optim$bestmem[5], x = tx)
tx  <- gsub(pattern = "ltormaxval2", replace = DE1$optim$bestmem[6], x = tx)
tx  <- gsub(pattern = "wscalminval1", replace = DE1$optim$bestmem[7], x = tx)
tx  <- gsub(pattern = "wscalminval2", replace = DE1$optim$bestmem[8], x = tx)
tx  <- gsub(pattern = "parff_minval1", replace = DE1$optim$bestmem[9], x = tx)
tx  <- gsub(pattern = "parff_minval2", replace = DE1$optim$bestmem[10], x = tx)
tx  <- gsub(pattern = "npatch 10", replace = "npatch 100", x = tx)
insname <- "OptimParms"
tx  <- gsub(pattern = "randomval", replace = outname1, x = tx)
tx  <- gsub(pattern = "\\./", replace = "Output/", x = tx)
writeLines(tx, con=paste("Code/",insname,".ins", sep=""))

# Run model with new ins then compare output to flux data:
# CHECK: is correct model compiled???
system("/Users/poulterlab1/version-control/LPJ-GUESS/ModelFiles/modules/./guess /Users/poulterlab1/Documents/C2E/Code/OptimParms.ins")

################################################################################
# Re-run model using irrigation driver data
# Make new ins file with these parameter values:
insfile2 <- "Code/OptimParms.ins" # name of ins file to use
# gsub: replaces all occurences of a pattern
ins  <- readLines(insfile2)
insname <- "OptimParms_Irr"
tx  <- gsub(pattern = "ppt.txt", replace = "ppt_irr.txt", x = tx)
tx  <- gsub(pattern = "wetd.txt", replace = "wetd_irr.txt", x = tx)
tx  <- gsub(pattern = outname1, replace = outname2, x = tx)
writeLines(tx, con=paste("Code/",insname,".ins", sep=""))

# Run model with new ins then compare output to flux data:
# CHECK: is correct model compiled???
system("/Users/poulterlab1/version-control/LPJ-GUESS/ModelFiles/modules/./guess /Users/poulterlab1/Documents/C2E/Code/OptimParms_Irr.ins")

################################################################################
# PLOT TIME!
################################################################################
# Read in reference data from Konza:
k <- read.csv("Data/RefData/anpp_1991_2012.csv")

# Model output: ambient
npp <- read.table("Output/OptimOut1_run/anpp_OptimRun.txt", header=T) %>%
  mutate(Source="Model", Treatment="control") %>%
  mutate_at(vars(ANGE:Total), funs(.*2000)) %>%
  mutate(year=Year+860) %>%
  filter(year>=1991) %>%
  select(year,Source,Treatment,Total) %>%
  mutate(Total=Total*0.4761905) %>% # so just above-ground
  rename(NPP=Total)

# Model output: irrigation
npp2 <- read.table("Output/OptimOut1_run/anpp_OptimRun_Irr.txt", header=T) %>%
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
summary(lm(data=rc, Field~Model)) # .03, not significant
summary(lm(data=ri, Field~Model)) # .36

# What is the mean abosolute error?
r %>% mutate(diff=abs(Field-Model)) %>%
  group_by(Treatment) %>%
  summarize(mean(diff))
#contro: 87
#irrigated: 203
#OptimOut0: 76/192, .16/.09, uses cover
#OptimOut: 87/203, .03/.36 uses cover
#OptimOut1: 65/147, .32/.19, just NPP
#OptimOut2:
################################################################################
# Plot annual FPC by species
################################################################################
fpc1 <- read.table("Output/OptimOut0_run/fpc_OptimRun.txt", header=T) %>%
  mutate(Treatment="Ambient")
fpc3 <- read.table("Output/OptimOut0_run/fpc_OptimRun_Irr.txt", header=T) %>%
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
# Plot annual NPP by species- same pattern as FPC?
################################################################################
# Model output: ambient
npp <- read.table("Output/anpp_OptimRun.txt", header=T) %>%
  mutate(Source="Model", Treatment="control") %>%
  mutate_at(vars(ANGE:Total), funs(.*2000)) %>%
  mutate(Year=Year+860) %>%
  filter(Year>=1991) %>%
  mutate_at(vars(ANGE:Total), funs(.*0.4761905)) %>%
  select(Year,Source,Treatment,ANGE:PAVI) %>%
  gather(Species, ANPP, ANGE:PAVI)

# Model output: irrigation
npp2 <- read.table("Output/anpp_OptimRun_Irr.txt", header=T) %>%
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

