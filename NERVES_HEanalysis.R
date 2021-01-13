
## Name: NERVES_HEanalysis.R
## Date: 08/08/19

## Purpose:
  # Perform NERVES base case economic analysis

## Clear data
rm(list=ls())

## Libs
.libPaths(c( .libPaths(), "/home/b.mhp601/R/libs"))

## Packages
library(parallel)
library(dplyr)
library(pracma)
library(tidyr)
library(purrr)
library(boot)
library(mice)

## Preliminaries
modelname <- "demo_"
date <- Sys.Date()
nsims <- 40 # Number of bootstrap replicates
nsubj <- 157 # Number of subjects
lambda <- 20000 # cost-effectiveness threshold
dir <- "/home/b.mhp601/NERVES/v3/demo/"

## Get predictor matrix
nm_predmatrix <- "PredictorMatrix.csv"
predmat <- as.matrix(read.csv(paste(dir,nm_predmatrix,sep=""),row.names = 1))

## Get functions
source(paste(dir,"NERVES_HEanalysis_fns.R",sep=""))

## Get data
df.NERVES_foranalyis <- read.csv(paste(dir,"analysis_bc1.csv",sep=""),stringsAsFactors = F)

## start clock
tick <- proc.time()

## Do deterministic analysis
main.results <- fn_computeicer(1,
                               data=df.NERVES_foranalyis,
                               predmat=predmat)

## Do bootstrap analysis
boot1.list <- mclapply(X = 1:nsims,  
                       FUN = fn_booticer,
                       data = df.NERVES_foranalyis, 
                       predmat = predmat, 
                       mc.cores = 40)

## Unlist bootstrap results
boot1 <- matrix(unlist(boot1.list),
                ncol = 14,
                byrow = T)

## Save a backup image
#save.image("backup_bc1.Rdata")

## Save deterministic results
saveRDS(main.results, 
        file = paste(dir, "main1_", modelname, date, ".rds", sep=""))

## Save bootstrap results
saveRDS(boot1, 
        file = paste(dir, "boot1_", modelname, date, ".rds", sep=""))

## End
tock <- proc.time() - tick
tock
