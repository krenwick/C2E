################################################################################
# Use the DEoptim package ON HYALITE and IN PARALLEL
################################################################################
rm(list=ls())
library(DEoptim)
library(dtplyr)
library(tidyr)
library(zoo)

# SET WORKING DIRECTORY:
setwd("./")

# to test on home laptop:
#setwd("~/Documents/C2E/Data/RefData")

# Read in annual NPP data (ambient)
NPP <- read.csv("Data/anpp_1991_2012.csv") %>%
  dplyr::filter(treatment=="control") %>%
  dplyr::rename(Year=year, Field=anpp_gm2) %>%
  dplyr::mutate(Field=Field/2000) %>% # make units kgC/m2
  dplyr::mutate(Field=Field*2.1) %>% # include below-ground C
  dplyr::select(Year,Field)

# Function to run LPJ-GUESS and return vector of residuals----------------------
# Depends on ins and flux data already existing in memory (ins and df4)
# Depends on tidyr and data.table packages

# First, read ins into memory:
ins  <- readLines("OptimDummy.ins") # only outputs NPP and LAI

# NOTE: to run in paralle might need to wrap in foreach
# Otherwise would over-write the temp ins file (I think)
LPJG <- function(par) {
  random <- runif(1,0,100)
  tx  <- gsub(pattern = "drought_tolval1", replace = par[1], x = ins)
  tx  <- gsub(pattern = "drought_tolval2", replace = par[2], x = tx)
  tx  <- gsub(pattern = "aphenmaxval1", replace = par[3], x = tx)
  tx  <- gsub(pattern = "aphenmaxval2", replace = par[4], x = tx)
  tx  <- gsub(pattern = "ltormaxval1", replace = par[5], x = tx)
  tx  <- gsub(pattern = "ltormaxval2", replace = par[6], x = tx)
  tx  <- gsub(pattern = "wscalminval1", replace = par[7], x = tx)
  tx  <- gsub(pattern = "wscalminval2", replace = par[8], x = tx)
  tx  <- gsub(pattern = "parff_minval1", replace = par[9], x = tx)
  tx  <- gsub(pattern = "parff_minval2", replace = par[10], x = tx)
  tx  <- gsub(pattern = "randomval", replace = random, x = tx)
  insname <- paste("./tempins",random,".ins",sep="")
  writeLines(tx, con=insname)
  
  print("Parameter values:")
  print(round(par,2))
  
  # Run model using new ins file
  system(sprintf("./guess /local/job/$SLURM_JOB_ID/%s", insname))
  
  # Read in output from model
  npp <- fread(paste("anpp_",random,".txt",sep=""), header=T) %>%
    dplyr::mutate(Year=Year+860) %>% 
    dplyr::filter(Year>=1991) %>%
    dplyr::rename(Model=Total) %>%
    dplyr::select(Year, Model) 
  b <- merge(NPP,npp, by="Year")
 
  # NEW COST FXN:
  resid <- b %>% dplyr::mutate(resid1=abs((Model-Field))) %>%
    dplyr::summarise(sumresid=sum(resid1))

  # Read in % cover
  cov1 <- fread(paste("fpc_",random,".txt",sep=""), header=T) %>%
    	dplyr::filter(Year>=1133) %>%
      summarise_at(vars(ANGE:PAVI), funs(mean)) %>%
      dplyr::mutate(resid=(abs(ANGE-.6)+abs(PAVI-.4))*3)
  #SSR <- resid %>% dplyr::summarise(SSR=sum(resid2))
  #print(round(SSR,2))
  #return (as.numeric(SSR+lc2*.756)) # weight so annual LAI+FPC=all monthly GPP
  #return (as.numeric(SSR+lc2*.3825)) # weight so annual LAI+FPC=1 yr of monthly GPP
  #return (as.numeric(SSR))
  # NEW cost function- from Hufkens et al. 2016
  return(as.numeric(resid$sumresid+cov1$resid))
}

low <- c(.0001,.0001,60,60,.3,.3,.2,.2,10000,10000) #lower bound
up <- c(.8,.8,240,240,1,1,.6,.6,10000000,10000000) #upper bound for ech parameter (default is inf)

DE1 <- DEoptim(lower=low,upper=up,fn=LPJG, 
               control=DEoptim.control(itermax = 1000, trace=50, steptol=100,
                                       parallelType=1, packages=c("tidyr","dplyr","data.table","zoo"), 
                                       parVar=c("NPP","ins")))

#Description <- "optimized based on monthly gpp and lai, original model but grass is summergreen"

save.image("OptimOut.RData")


