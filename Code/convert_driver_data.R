################################################################################
# Convert driver data into format needed for LPJ-GUESS
# Original data files provided by Kevin
################################################################################
rm(list=ls())
library(tidyverse); theme_set(theme_bw(base_size=12)) # sized for print

# Set working directory:
setwd("~/Documents/C2E/Data")

# Coordinate of study site:
ll <- c(39.11, -96.61)

############ Read in data and convert to monthly ###############################
ambient <- read.csv("knz_amb_forcing_91-12.csv")
irr <- read.csv("knz_irr_forcing_91-12.csv")

ambient2 <- ambient %>% 
  mutate(date=format(strptime(paste(year, doy), format="%Y %j"))) %>%
  group_by(year,doy,date) %>% # group by day to calculate days with precip
  summarise(dayppt=sum(precp), daytemp=mean(tair), daysol=mean(rad_h)) %>%
  mutate(month=format(as.Date(date), "%m")) %>%
  group_by(year, month) %>%
  summarise(temp=mean(daytemp), ppt=sum(dayppt), meansol=mean(daysol),
            wet=sum(dayppt>0)) 
irr2 <- irr %>%
  mutate(date=format(strptime(paste(year, doy), format="%Y %j"))) %>%
  group_by(year,doy,date) %>% # group by day to calculate days with precip
  summarise(dayppt=sum(precp), daytemp=mean(tair), daysol=mean(rad_h)) %>%
  mutate(month=format(as.Date(date), "%m")) %>%
  group_by(year, month) %>%
  summarise(temp=mean(daytemp), ppt=sum(dayppt), meansol=mean(daysol),
            wet=sum(dayppt>0)) 

ambient2$lon <- ll[2]
ambient2$lat <-ll[1]
irr2$lon <- ll[2]
irr2$lat <- ll[1]

# Function to take data and put into form for LPJ-GUESS read-in
# Data must be long-form, include lat-lon, and have variables as columns
# nyear: number of simulation years desired, ... = variable to use
format_guess <- function(data, nyear, ...){ # use dots since passing text to dplyr problematic
  d1 <- data %>% select(lon, lat, year, month, ...) %>%
    spread(month, ...)
  # Round up then chop off first few rows to get right #
  d2 <- sample(d1[],nyear-nrow(d1), replace=T)
  s <- sample(c(1:nrow(d1)),1)
  d2 <-  d1[sample(c(1:nrow(d1)),(nyear-nrow(d1)), replace=T),]
  d3 <- rbind(d2,d1)
  return(d3)
}

# I have 22 years of site-specific climate data (1991-2012)
# We agreed to do a 1000-year spin-up, then 1860-1990 with repeated met (131 yr)
# and increasing CO2. So I need 1000+131+22=1153 yrs
temp <- format_guess(ambient2,1153, temp)
srad <- format_guess(ambient2,1153, meansol)
ppt <- format_guess(ambient2,1153, ppt)
wetd <- format_guess(ambient2,1153, wet)
ppt_irr <- format_guess(irr2,1153, ppt)
wetd_irr <- format_guess(irr2,1153, wet)

# But wait! I actually want spin-up to be ambient, not irrigation:
ppt_irr[1:(nrow(ppt_irr)-22),] <- ppt[1:(nrow(ppt_irr)-22),]
wetd_irr[1:(nrow(ppt_irr)-22),] <- wetd[1:(nrow(ppt_irr)-22),]


write.table(temp, "temp.txt", sep=" ",
            quote=F, row.names=F, col.names=F)
write.table(ppt, "ppt.txt", sep=" ",
            quote=F, row.names=F, col.names=F)
write.table(srad, "srad.txt", sep=" ",
            quote=F, row.names=F, col.names=F)
write.table(wetd, "wetd.txt", sep=" ",
            quote=F, row.names=F, col.names=F)
write.table(ppt_irr, "ppt_irr.txt", sep=" ",
            quote=F, row.names=F, col.names=F)
write.table(wetd_irr, "wetd_irr.txt", sep=" ",
            quote=F, row.names=F, col.names=F)

################################################################################
# Combine driver files into 1 so can run both experiments at once?

################################################################################
# Convert CO2 data into correct format
################################################################################
c1 <- read.table("global_co2_ann_1860_2016.txt",header=F)
head(c1) # 1860 = 286.42, data through 2016! We want through 2012
c2 <- filter(c1, V1<=2012)
yr <- seq(1,1153,1)
co <- rep(286,1000)
co2 <- c(co,c2$V2)
df <- cbind.data.frame(yr,co2)

# Write:
write.table(df, "co2_C2E.txt",quote=F,row.names=F,col.names=F)


