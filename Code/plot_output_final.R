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

################################################################################
# Plot annual NPP with reference conditions
################################################################################
# Model output: ambient
npp <- read.table("orig1_anpp.out", header=T) %>%
  mutate(Source="Model", Treatment="control") %>%
  mutate_at(vars(ANGE:Total), funs(.*2000)) %>%
  mutate(year=Year+860) %>%
  filter(year>=1991) %>%
  select(year,Source,Treatment,Total) %>%
  mutate(Total=Total*0.4761905) %>% # so just above-ground
  rename(NPP=Total)

# Model output: irrigation
npp2 <- read.table("orig3_anpp.out", header=T) %>%
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

amb <- filter(knpp, Source=="control")
mod <- filter(npp1, Source=="Model")
field <- filter(npp1, Source=="Field")
NPP <- ggplot(data=field, aes(x=Year, y=NPP)) +
  geom_bar(stat="identity", fill="lightgray", color="black") +
  geom_point(data=mod, color="chartreuse4", size=2) +
  ylab(expression(ANPP~(g~m^{-2}))) +
  xlab("Year") +
  facet_wrap(~Treatment)
NPP

ggsave("../Figs/NPP.pdf", plot=NPP, width = 10, height = 5, 
       units = 'in')

# what is the R2?
r <- spread(npp1, Source, NPP) 
rc<- filter(r, Treatment=="control")
ri<- filter(r, Treatment=="irrigated")
summary(lm(data=rc, Field~Model)) # -0.03, not significant
summary(lm(data=ri, Field~Model)) # .22- so the optimized is better
################################################################################
# Plot annual FPC by species
################################################################################
fpc1 <- read.table("orig1_fpc.out", header=T) %>%
  mutate(Treatment="Control")
fpc3 <- read.table("orig3_fpc.out", header=T) %>%
  mutate(Treatment="Irrigation")

fpc <- rbind.data.frame(fpc1,fpc3) %>%
  mutate(Year=Year+860) %>%
  filter(Year>=1970) %>%
  gather(Species, FPC, ANGE:PAVI)

# (tried full time series- converges early)
fpc <- ggplot(data=fpc, aes(x=Year,y=FPC, color=Species)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~Treatment) +
  ylab("Foliar Projective Cover (%)")
fpc

ggplot(data=fpc, aes(x=Year,y=FPC, fill=Species)) +
  geom_area(position="stack") +
  facet_wrap(~Treatment) +
  ylab("Foliar Projective Cover (%)")

ggsave("../Figs/FPC.pdf", plot=fpc, width = 10, height = 5, 
       units = 'in')

################################################################################
# Plot annual NPP by species- same pattern as FPC?
################################################################################
# Model output: ambient
npp <- read.table("orig1_anpp.out", header=T) %>%
  mutate(Source="Model", Treatment="control") %>%
  mutate_at(vars(ANGE:Total), funs(.*2000)) %>%
  mutate(Year=Year+860) %>%
  filter(Year>=1991) %>%
  mutate_at(vars(ANGE:Total), funs(.*0.4761905)) %>%
  select(Year,Source,Treatment,ANGE:PAVI) %>%
  gather(Species, ANPP, ANGE:PAVI)

# Model output: irrigation
npp2 <- read.table("orig3_anpp.out", header=T) %>%
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

ggsave("../Figs/ppteffect.pdf", plot=ppteffect, width = 5, height = 4, 
       units = 'in')

# Read in model output (monthly) ------------------------------------
# Pull in monthly data from orig1
orig1 <- NULL 
vars=c("mgpp","mrh","mra","mnee","mevap","maet","mlai","mwcont_upper","mrunoff")
vars=c("mrunoff")
for(var in vars) {
  data=paste("orig1_",var,".out", sep="")
  b <- fread(data, header=T)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=1991) %>%
    gather(Month,Control, Jan:Dec) %>%
    mutate(Variable=var) %>%
    select(Year, Month,Variable,Control)
  orig1 <- rbind.data.frame(orig1,b1)
}

orig3 <- NULL 
for(var in vars) {
  data=paste("orig3_",var,".out", sep="")
  b <- fread(data, header=T)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=1991) %>%
    gather(Month,Irrigated, Jan:Dec) %>%
    mutate(Variable=var) %>%
    select(Year, Month,Variable,Irrigated)
  orig3 <- rbind.data.frame(orig3,b1)
}
head(orig3)

origs <- merge(orig1, orig3, by=c("Year", "Month","Variable")) %>%
  gather(Treatment,Value,Control:Irrigated) %>%
  group_by(Year,Treatment,Variable) %>%
  summarise_at(vars(Value), sum)

head(origs)
origs$D <- as.yearmon(paste(origs$Year, origs$Month), "%Y %b")
origs$Date <- as.Date(origs$D)
################################################################################
# Plot run-off from Ambient vs. Irrigation
################################################################################
run <- filter(origs, Variable=="mrunoff")

# runoff <- ggplot(data=run, aes(x=Year, y=Value, fill=Treatment)) +
#   geom_bar(stat="identity", position="dodge") +
#   scale_fill_manual(values=c("burlywood4","cadetblue3")) +
#   #scale_x_date(date_breaks = "2 months",date_labels = "%b") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         strip.background = element_blank(),
#         strip.text = element_blank(),
#         panel.grid.minor = element_blank(),
#         # panel.border = element_rect(colour = "black"),
#         legend.position = c(.99,.99), legend.justification=c(1,1)) +
#   ylab(expression(Runoff~(mm~month^{-1}))) +
#   xlab("Month (2010-2012")
# runoff
# 
# ggsave("../Figs/runoff.pdf", plot=runoff, width = 7, height = 4, 
#        units = 'in')
