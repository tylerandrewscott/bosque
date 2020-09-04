rm(list=ls())
library(broom)
library(statnet)
library(tidyverse)
library(ergm.terms.contrib)
library(ergm.userterms)
load('scratch/ergm_mcmc_psock_results_absdiff3.RData')

library(ggthemes)
library(scales)


library(btergm)
mat_fin_nearest_other <- log(as.matrix(mat_fin_nearest_other))
mat_man_nearest_other <- log(as.matrix(mat_man_nearest_other))
mat_tech_nearest_other <- log(as.matrix(mat_tech_nearest_other))


fin_net = as.network(tcrossprod(as.sociomatrix(net_list$FINANCIAL)),matrix.type = 'adjacency',directed=F,ignore.eval = T)
bp = net_list$FINANCIAL$gal$bipartite
invisible(lapply(list.vertex.attributes(net_list$FINANCIAL),function(va) fin_net %v% va <<- (net_list$FINANCIAL %v% va)[1:bp]))


fin_form = fin_net ~ edges +  isolates +
  gwdegree(0.75,fixed=T) + 
  gwesp(0.25,fixed=T) +
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
  nodematch('Same_GCD_GW_User',diff=F,keep = which(grepl('GW',sort(unique(fin_net%v% 'Same_GCD_GW_User')))&grepl('Self',sort(unique(fin_net %v% 'Same_GCD_GW_User')))) )+
  offset(degrange(from = (max(degree(fin_net,gmode='graph')) + 1),to=Inf))

fin_mod = ergm(fin_form,offset.coef = -Inf,
            constraints = ~edges,
            control = control.ergm(MCMC.burnin = 10000,MCMC.interval = 500,
                                              MCMC.samplesize = 8000,parallel.type="PSOCK",parallel=4,
                                              init.method = 'CD',MCMLE.maxit = 42),verbose=F)



man_net = as.network(tcrossprod(as.sociomatrix(net_list$MANAGERIAL)),matrix.type = 'adjacency',directed=F)
bp = net_list$MANAGERIAL$gal$bipartite
invisible(lapply(list.vertex.attributes(net_list$MANAGERIAL),function(va) man_net %v% va <<- (net_list$MANAGERIAL %v% va)[1:bp]))


man_form = man_net ~ edges +  isolates +
  gwdegree(0.75,fixed=T) + 
  gwesp(0.25,fixed=T) +
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
  edgecov(space_mat) + edgecov(tcrossprod(trans_man_mat)) +
  nodematch('Same_GCD_GW_User',diff=F,keep = which(grepl('GW',sort(unique(man_net %v% 'Same_GCD_GW_User')))&grepl('Self',sort(unique(man_net %v% 'Same_GCD_GW_User')))) )+
  offset(degrange(from = (max(degree(man_net,gmode='graph')) + 1),to=Inf))

man_mod = ergm(man_form,offset.coef = -Inf,
               constraints = ~edges,
               control = control.ergm(MCMC.burnin = 10000,MCMC.interval = 500,
                                      MCMC.samplesize = 8000,parallel.type="PSOCK",parallel=4,
                                      init.method = 'CD',MCMLE.maxit = 42),verbose=F)


tech_net = as.network(tcrossprod(as.sociomatrix(net_list$TECHNICAL)),matrix.type = 'adjacency',directed=F)
bp = net_list$TECHNICAL$gal$bipartite
invisible(lapply(list.vertex.attributes(net_list$TECHNICAL),function(va) tech_net %v% va <<- (net_list$TECHNICAL %v% va)[1:bp]))


tech_form = tech_net ~ edges +  isolates +
  gwdegree(0.75,fixed=T) + 
  gwesp(0.25,fixed=T) +
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
  edgecov(space_mat) + edgecov(tcrossprod(trans_tech_mat)) +
  nodematch('Same_GCD_GW_User',diff=F,keep = which(grepl('GW',sort(unique(tech_net %v% 'Same_GCD_GW_User')))&grepl('Self',sort(unique(tech_net %v% 'Same_GCD_GW_User')))) )+
  offset(degrange(from = (max(degree(tech_net,gmode='graph')) + 1),to=Inf))

tech_mod = ergm(tech_form,offset.coef = -Inf,
               constraints = ~edges,
               control = control.ergm(MCMC.burnin = 10000,MCMC.interval = 500,
                                      MCMC.samplesize = 8000,parallel.type="PSOCK",parallel=4,
                                      init.method = 'CD',MCMLE.maxit = 42),verbose=F)

save.image('test_unimodal4.RData')