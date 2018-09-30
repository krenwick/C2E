################################################################################
# Plots of model output for the C2E project
################################################################################
rm(list=ls()) 
library(tidyverse); theme_set(theme_bw(base_size=18))
library(data.table)
library(zoo)

# Set working directory
setwd("~/Documents/C2E/Output")

# Read in reference data from Konza:
k <- read.csv("../Data/RefData/anpp_1991_2012.csv")
head(k)

# Read in model output (monthly) ------------------------------------
# Pull in monthly data from orig1
orig1 <- NULL 
vars=c("mgpp","mrh","mra","mnee","mevap","maet","mlai","mwcont_upper")
for(var in vars) {
  data=paste("orig1_",var,".out", sep="")
  b <- fread(data, header=T)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=1991) %>%
    gather(Month,orig1, Jan:Dec) %>%
    mutate(Variable=var) %>%
    select(Year, Month,Variable,orig1)
  orig1 <- rbind.data.frame(orig1,b1)
}

orig2 <- NULL 
for(var in vars) {
  data=paste("orig2_",var,".out", sep="")
  b <- fread(data, header=T)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=1991) %>%
    gather(Month,orig2, Jan:Dec) %>%
    mutate(Variable=var) %>%
    select(Year, Month,Variable,orig2)
  orig2 <- rbind.data.frame(orig2,b1)
}

orig3 <- NULL 
for(var in vars) {
  data=paste("orig3_",var,".out", sep="")
  b <- fread(data, header=T)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=1991) %>%
    gather(Month,orig3, Jan:Dec) %>%
    mutate(Variable=var) %>%
    select(Year, Month,Variable,orig3)
  orig3 <- rbind.data.frame(orig3,b1)
}

origs <- merge(orig1, orig2, by=c("Year", "Month","Variable")) %>%
  merge(.,orig3,by=c("Year", "Month","Variable")) %>%
  gather(Model,Value,orig1:orig3)
  
head(origs)
origs$D <- as.yearmon(paste(origs$Year, origs$Month), "%Y %b")
origs$Date <- as.Date(origs$D)

################################################################################
# Plot annual NPP with reference conditions
################################################################################
o1 <- read.table("orig1_anpp.out", header=T) %>%
  mutate(Model="orig1") %>%
  mutate(year=Year+860) %>%
  mutate(Total=Total*0.4761905) # so just above-ground
o3 <- read.table("orig3_anpp.out", header=T) %>%
  mutate(Model="orig3") %>%
  mutate(year=Year+860) %>%
  mutate(Tot=ifelse(year<=(1990), Total*0.4761905, NA)) %>%
  mutate(Tot=ifelse(year>1990 & year<2000, Total*0.5235602, Tot)) %>%
  mutate(Tot=ifelse(year>=2000, Total*0.5494505, Tot)) %>%
  select(-Total) %>%
  rename(Total=Tot)

npp <- rbind.data.frame(o1,o3) %>%
  mutate_at(vars(ANGE:Total), funs(.*2000)) %>%
  filter(year>=1991) %>%
  select(year,Model,Total) %>%
  rename(Source=Model, NPP=Total)

knpp <- k %>% rename(Source=treatment, NPP=anpp_gm2) %>%
  select(year,Source,NPP)
npp1 <- rbind(npp,knpp) 

ggplot(data=npp1, aes(x=year, y=NPP, color=Source)) +
  geom_point() +
  geom_line()

################################################################################
# Plot annual FPC by species
################################################################################
fpc1 <- read.table("orig1_fpc.out", header=T) %>%
  mutate(Model="orig1")
fpc2 <- read.table("orig2_fpc.out", header=T) %>%
  mutate(Model="orig2")
fpc3 <- read.table("orig3_fpc.out", header=T) %>%
  mutate(Model="orig3")

fpc <- rbind.data.frame(fpc1,fpc2,fpc3) %>%
  mutate(Year=Year+860) %>%
  filter(Year>=1980) %>%
  gather(Species, FPC, ANGE:PAVI) %>%
  filter(Model != "orig2")

# (tried full time series- converges early)
ggplot(data=fpc, aes(x=Year,y=FPC, color=Model, linetype=Species)) +
  geom_line() + 
  geom_point()

# Plot together, just orig3, to see if equal 100:
ggplot(data=fpc, aes(x=Year,y=FPC,fill=Species)) +
  geom_area(position="stack") +
  facet_wrap(~Model)
# Total FPC is ~60%

################################################################################
# Compare biomass output to reference
################################################################################
b1 <- read.table("orig1_cmass.out", header=T) %>%
  mutate(Model="orig1")
b2 <- read.table("orig2_cmass.out", header=T) %>%
  mutate(Model="orig2")
b3 <- read.table("orig3_cmass.out", header=T) %>%
  mutate(Model="orig3")

bio <- rbind.data.frame(b1,b2,b3) %>%
  mutate(Year=Year+860) %>%
  filter(Year>=1991) %>%
  group_by(Model) %>%
  summarise_at(vars(ANGE:Total),mean)
bio
# Total biomass should be  .5673 kgC/m2
# I get .273-.31 (should I double?)

# Try cpools -----------------------------------------
cp1 <- read.table("orig1_cpool.out", header=T) %>%
  mutate(Model="orig1")
cp2 <- read.table("orig2_cpool.out", header=T) %>%
  mutate(Model="orig2")
cp3 <- read.table("orig3_cpool.out", header=T) %>%
  mutate(Model="orig3")

cp <- rbind.data.frame(cp1,cp2,cp3) %>%
  mutate(Year=Year+860) %>%
  filter(Year>=1991) %>%
  group_by(Model) %>%
  summarise_at(vars(VegC:Total),mean)
# -	Soil carbon pools should be: (9kgC/m2 total)
cp # I get less-not sure what numbers to compare though

################################################################################
# Compare soil moisture (upper layer) output to reference
################################################################################
mwc <- filter(origs,Variable=="mwcont_upper") %>%
  rename(Source=Model) %>%
  select(Year:Value, -Variable) %>%
  mutate(Value=Value*.23) %>%
  filter(Source!="orig2")

# read in reference data and aggregate to monthly
m <- read.csv("../Data/RefData/soil_moisture_knz_0-15cm_2007_2012_daily.csv") %>%
  group_by(year,month,transect) %>%
  summarise(Value=mean(soil_moist_volumetric)) %>%
  mutate(Source=ifelse(transect=="control","Ambient","Irrigation")) %>%
  #filter(transect=="control") %>%
  ungroup() %>%
  mutate(month = month.abb[month]) %>%
  rename(Year=year, Month=month) %>%
  select(-transect)

sm <- merge(mwc,m, by=c("Year","Month","Source","Value"), all=T) %>%
  select(Year:Value) %>%
  filter(Year>=2005) %>%
  spread(Source,Value) %>%
  na.omit() %>%
  gather(Source,Value,Ambient:orig3) %>%
  arrange(Year,Month)
sm$D <- as.yearmon(paste(sm$Year, sm$Month), "%Y %b")
sm$Date <- as.Date(sm$D)
sm2 <- sm %>% arrange(Date)

idx <- c(1, diff(sm2$Date))
i2 <- c(1,which(idx > 31), nrow(sm2)+1)
sm2$grp <- rep(1:length(diff(i2)), diff(i2))
sm2$g <- paste(sm2$grp,sm2$Source)

sm3 <- filter(sm2, Year>2010)
ggplot(data=sm3, aes(x=Date, y=Value, color=Source)) +
  geom_point() +
  geom_line(aes(group = g)) +
  scale_x_date(date_breaks = "6 months",date_labels = "%Y-%b")

#####################################################
# Plot ambiet and model as a function of precip
#####################################################
amb <- read.table("../Data/ppt.txt", header=F)
irr <- read.table("../Data/ppt_irr.txt")
head(amb)
names(amb) <- c("Lat","Lon","Year",seq_along(month.abb))
amb2 <- amb %>% mutate(yr=seq(1:nrow(amb))) %>%
  mutate(Year=yr+859) %>%
  gather(Month,Ambient,`1`:`12`)
names(irr) <- c("Lat","Lon","Year",seq_along(month.abb))
irr2 <- irr %>% mutate(yr=seq(1:nrow(irr))) %>%
  mutate(Year=yr+859) %>%
  gather(Month,Irrigated,`1`:`12`)
ppt <- merge(amb2,irr2,by=c("Lat","Lon","Year","Month")) %>%
  filter(Year>1990) %>%
  mutate(Month=as.numeric(Month)) %>%
  #filter(Month>3 & Month<10) %>%
  group_by(Year) %>%
  summarise_at(vars(Ambient,Irrigated), sum) %>%
  rename(control=Ambient,irrigated=Irrigated) %>%
  gather(Treatment, Precipitation, control:irrigated)

ppt2 <- merge(ppt,npp1, by=c("Year","Treatment")) %>%
  filter(Treatment=="control")
ppteffect <- ggplot(data=ppt2, aes(x=Precipitation, y=NPP, color=Source, fill=Source)) +
  geom_point() +
  geom_smooth(method="lm") +
  ylab(expression(ANPP~(g~m^{-2}))) +
  xlab("Total Annual Precip (ambient)") +
  theme(legend.position=c(.01,.99), legend.justification=c(0,1))
ppteffect

################################################################################
# Is there something weird going on with my precip data?
################################################################################
# amb <- read.table("../Data/ppt.txt", header=F)
# irr <- read.table("../Data/ppt_irr.txt")
# head(amb)
# names(amb) <- c("Lat","Lon","Year",seq_along(month.abb))
# amb2 <- amb %>% mutate(yr=seq(1:nrow(amb))) %>%
#   mutate(Year=yr+859) %>%
#   gather(Month,Ambient,`1`:`12`)
# names(irr) <- c("Lat","Lon","Year",seq_along(month.abb))
# irr2 <- irr %>% mutate(yr=seq(1:nrow(irr))) %>%
#   mutate(Year=yr+859) %>%
#   gather(Month,Irrigated,`1`:`12`)
# ppt <- merge(amb2,irr2,by=c("Lat","Lon","Year","Month")) %>%
#   filter(Year>2005)
# ppt$D <- as.yearmon(paste(ppt$Year, ppt$Month), "%Y %m")
# ppt$Date <- as.Date(ppt$D)
# ggplot(data=ppt, aes(x=Date,y=Ambient)) +
#   #geom_point() +
#   geom_line(color="red") +
#   geom_line(aes(x=Date,y=Irrigated), color="blue")
# 
# #####################################################
# # Plot temperature
# #####################################################
# temp <- read.table("../Data/temp.txt", header=F)
# names(temp) <- c("Lat","Lon","Year",seq_along(month.abb))
# temp2 <- temp %>% mutate(yr=seq(1:nrow(temp))) %>%
#   mutate(Year=yr+859) %>%
#   gather(Month,Ambient,`1`:`12`) %>%
#   filter(Year>1990)
# 
# temp2$D <- as.yearmon(paste(temp2$Year, temp2$Month), "%Y %m")
# temp2$Date <- as.Date(temp2$D)
# ggplot(data=temp2, aes(x=Date,y=Ambient)) +
#   geom_line() 
# nothing strange happened in 1998 to explain FPC spike
  