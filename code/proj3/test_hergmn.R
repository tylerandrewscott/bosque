rm(list=ls())
library(broom)
library(statnet)
library(tidyverse)
library(ergm.terms.contrib)
library(ergm.userterms)
load('scratch/ergm_mcmc_psock_results_absdiff3.RData')

library(btergm)
mat_fin_nearest_other <- log(as.matrix(mat_fin_nearest_other))
mat_man_nearest_other <- log(as.matrix(mat_man_nearest_other))
mat_tech_nearest_other <- log(as.matrix(mat_tech_nearest_other))

test = as.network(tcrossprod(as.sociomatrix(net_list$FINANCIAL)),matrix.type = 'adjacency',directed=F)
bp = net_list$FINANCIAL$gal$bipartite
lapply(list.vertex.attributes(net_list$FINANCIAL),function(va) invisible(test %v% va <<- (net_list$FINANCIAL %v% va)[1:bp]))


# test_form = test ~ edges +  isolates +
#   gwdegree(0.75,fixed=T) + 
#   gwesp(0.25,fixed=T) +
#   nodefactor('Water_Type') + nodefactor('Make_Buy') +
#   nodecov('Log_Acreage') + 
#   nodecov('AGE') + 
#   nodecov('Log_Service_Pop') + 
#   nodecov('Log_Bonds_Outstanding_Per_Connection') + 
#   nodecov('Log_Revenue_Per_Connection') + 
#   nodecov('Viol_Points_5yr') + 
#   # #b1absdiff('Log_Service_Pop') +
#   absdiff('Log_Revenue_Per_Connection') +
#   # #b1absdiff('Log_Bonds_Outstanding_Per_Connection') +
#   absdiff('Viol_Points_5yr') +
#   edgecov(space_mat) + edgecov(tcrossprod(trans_fin_mat)) +
#   nodematch('Same_GCD_GW_User',diff=F,keep = which(grepl('GW',sort(unique(test %v% 'Same_GCD_GW_User')))&grepl('Self',sort(unique(test %v% 'Same_GCD_GW_User')))) )
# 
# testm = ergm(test_form,control = control.ergm(MCMC.burnin = 10000,MCMC.interval = 750,
#                                               MCMC.samplesize = 2000,
#                                               init.method = 'CD',MCMLE.maxit = 42),verbose=F)


library(hergm)
test_form2 = test ~ edges_ij +  
  triangle_ijk + isolates +
  nodefactor('Water_Type') + nodefactor('Make_Buy') +
  nodecov('Log_Acreage') + 
  nodecov('AGE') + 
  nodecov('Log_Service_Pop') + 
  nodecov('Log_Bonds_Outstanding_Per_Connection') + 
  nodecov('Log_Revenue_Per_Connection') + 
  nodecov('Viol_Points_5yr') + 
  # #b1absdiff('Log_Service_Pop') +
  absdiff('Log_Revenue_Per_Connection') +
  # #b1absdiff('Log_Bonds_Outstanding_Per_Connection') +
  absdiff('Viol_Points_5yr') +
  edgecov(space_mat) + edgecov(tcrossprod(trans_fin_mat)) +
  nodematch('Same_GCD_GW_User',diff=F,keep = which(grepl('GW',sort(unique(test %v% 'Same_GCD_GW_User')))&grepl('Self',sort(unique(test %v% 'Same_GCD_GW_User')))) )
library(hergm)
testm2 = hergm(test_form2)

save.image('scratch/hergm_test.RData')