#29392
rm(list=ls())
require(Rmpi)
library(tidyverse)
library(statnet)
library(parallel)
library(pbapply)
library(btergm)
library(ergm.terms.contrib)
library(ergm.userterms)
library(ggmap)
library(lubridate)
require(doParallel)
require(foreach)
require(ergm)
#require(Rmpi)
library(ggmap)

load('scratch/final_boot_runs.RData')

form_tech = net_list$TECHNICAL ~ edges +  offset(isolates) + b2degree(1)
# 
m3 = ergm(form_tech,eval.loglik = F,verbose=F,constraints = ~edges,
          control = control.ergm(MCMC.interval = 1500,MCMC.burnin = 10000,
                                 MCMLE.maxit = 20,MCMC.samplesize = 20000,parallel.type="MPI",parallel=4,
                                 Step.MCMC.samplesize = 1000),offset.coef = c(-Inf))


