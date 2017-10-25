################################################################################
# Convert output data to correct units and format for Kevin
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=12)) # sized for print

# Set working directory:
setwd("~/Documents/C2E/Output")
amb <- "orig1"
irr <- "orig3"

################################################
npp1 <- read.table(paste(amb,"_anpp.out",sep=""), header=T) %>%
  mutate(Treatment="Control")
npp2 <- read.table(paste(irr,"_anpp.out",sep=""), header=T) %>%
  mutate(Treatment="Irrigation")
npp_annual <- rbind.data.frame(npp1,npp2) %>%
  mutate(Model="LPJGUESS") %>%
  mutate_at(vars(ANGE:Total), funs(.*2000)) %>%
  mutate_at(vars(ANGE:Total), funs(.*0.4761905))

write.csv(npp_annual_gm-2, "../RequestedOutput/npp_annual.csv", row.names=F)

# Read in model output (monthly) ------------------------------------
# Pull in monthly data from orig1
orig1 <- NULL 
vars=c("mgpp","mrh","mra","mnee","mnpp","mevap","maet","mlai","mwcont_upper",
       "mwcont_lower","mrunoff")
for(var in vars) {
  data=paste("orig1_",var,".out", sep="")
  b <- fread(data, header=T)
  b1 <- b %>% mutate(Year=Year+860) %>% 
    filter(Year>=1980) %>%
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

origs <- merge(orig1, orig3, by=c("Year", "Month","Variable")) %>%
  gather(Treatment,Value,Control:Irrigated) %>%
  mutate(Model="LPJGUESS")

head(origs)
write.csv(origs, "../RequestedOutput/all_monthly_kgCm-2.csv", row.names=F) 

mnpp <- filter(origs,Variable=="mnpp") %>%
  mutate(Value=Value*2000) %>%
  mutate(MNPP=ifelse(Treatment=="Control",Value*0.4761905,
              ifelse(Year<=1990, Value*0.4761905, 
              ifelse(Treatment=="Irrigation" & Year>1990 & Year<2000, Value*0.5235602,
              Value*0.5494505))))
 
write.csv(mnpp, "../RequestedOutput/anpp_monthly_gm-2")


