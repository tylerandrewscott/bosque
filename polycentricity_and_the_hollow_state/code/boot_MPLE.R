######## bootstrapped MPLE

# packages required
# library(statnet)
# library(doParallel)
# library(foreach)


##### main function
# ergm.formula: An R formula object, of the form y ~ <model terms>, where y is a network. For details on the possible <model terms>, see ergm-terms
# boot.rep: default=500, the number of networks to be simulated to calculate the bootstrapped MPLE
# mcmc.interval: default=1024, Number of proposals between sampled statistics. Increasing interval will reduces the autocorrelation in the sample, and may increase the precision in estimates by reducing MCMC error, at the expense of time. Set the interval higher for larger networks.
# mcmc.burnin: default=16*1024, Number of proposals before any MCMC sampling is done. It typically is set to a fairly large number
# number.cores: number of cores used in caluclation

boot.MPLE<- function(ergm.formula, boot.rep=500, mcmc.interval=1024, mcmc.burnin=16*1024, mcmc.samplesize=50000,
                     number.cores=1,calc_loglik = TRUE,offset_coef = NULL)
  {

  require(statnet)
  require(doParallel)
  require(foreach)
  require(ergm)
  require(Rmpi)
  
  # check whether multiple cores are necessary
  if(number.cores>1){
    registerDoParallel(cores=number.cores)
  }
  
  # calculate MPLE of network object
  mple <- ergm(ergm.formula, estimate="MPLE",eval.loglik = calc_loglik,offset.coef = offset_coef,
               control = control.ergm(MCMC.burnin = mcmc.burnin,MCMC.samplesize = mcmc.samplesize,
                                      MPLE.samplesize = 200000,MCMC.max.maxedges = 5000,parallel = 10,parallel.type = 'mpi'))
  
  # save network statistics of observed network
  observed.network.stats <- mple$target.stats

  if(calc_loglik)
  {# get mple coefficients
  mple.coef <- summary(mple)[[20]]
  # get number of parameters
  number.parameters <- dim(mple.coef)[1]
  # create a results table. for this take existing table from summary(ergm) and adjust it for our purposes
  boot.results <- mple.coef[, -4]
  boot.results[, 2:3]<-0}
  
  if(!calc_loglik)
  {
    mple.coef = cbind(coef(mple))
  number.parameters <- dim(mple.coef)[1]
  boot.results <- cbind(mple.coef,rep(0,number.parameters),rep(0,number.parameters))
  }

  colnames(boot.results)<- c('est', "2.5%", "97.5%")
  
  # simulate boot.rep networks
  sim.mple <- simulate.ergm(mple, nsim=boot.rep,
                            control=control.simulate.ergm(MCMC.burnin=mcmc.burnin, MCMC.interval=mcmc.interval,
                                                          MCMC.init.maxedges=20000*0.2,MCMC.max.maxedges = 5000)) 
  
  # create empty matrix to save statistics of the simulated networks
  sim.network.stats <- matrix(0, number.parameters, boot.rep)

  ergm.formula <- mple$formula
  # loop (can be parallelized) to calculate the the mple of the simulated networks 
    boot.coef<- foreach(i= 1:boot.rep, .combine=cbind, .packages=c("ergm"))%dopar%{
    # extract covariates
    #covariates <- labels(terms(ergm.formula))
    covariates <- rownames(boot.results)
    # create auxiliary formula
    aux.formula <- sim.mple[[i]]~a+b
   
    # create formula for simulated networks
    #sim.formula<-reformulate(covariates, aux.formula[[2]])
    sim.formula <- reformulate( as.character(ergm.formula)[3],aux.formula[[2]])
    # calculate MPLE of simulated network and get coefficients
    sim.results <- ergm(sim.formula, estimate= "MPLE",eval.loglik = calc_loglik,
                        offset.coef = offset_coef,
                        control = control.ergm(MCMC.burnin = mcmc.burnin,MCMC.samplesize = mcmc.samplesize,
                                               MPLE.samplesize = 100000,MCMC.max.maxedges = 3500,
                                               MCMC.init.maxedges = 2500))
    coeffs<-coef(sim.results)
    # calculate statistics of simulated networks
    sim.network.stats <- sim.results$target.stats
    # return
    c(coeffs, sim.network.stats)
  }

 
  # create empty matrix to store results of the degeneracy test
  if(calc_loglik){degeneracy.test<- cbind(mple.coef, c(1:number.parameters))}
  if(!calc_loglik){degeneracy.test<- cbind(mple.coef,replicate(4,c(1:number.parameters)))}
  colnames(degeneracy.test)<- c("sim > obs", "observed_value", "sim 25%", "sim 50%", "sim 75%")
  degeneracy.test <- as.data.frame(degeneracy.test)

  # empty vector for true/false
  ev<- c()
  # calculate 2.5 percentile and 97.5 percentile and degeneracy results
  for(j in 1:number.parameters){
    # calculate 2.5 percentile and 97.5 percentile of simulated mple
    boot.results[j,2:3]<-quantile(boot.coef[j,], probs = c(0.025, 0.975))
    # proportion the simulated statistic is greater than the observed statistic. if result is close to 0 or 1 -> strong indicator for degeneracy
    # or increase mcmc.interval and mcmc.burnin
    degeneracy.test[j,1]<- length(boot.coef[j+number.parameters, ][boot.coef[j+number.parameters, ] < observed.network.stats[j]])/boot.rep
    degeneracy.test[j,2]<- observed.network.stats[j]
    degeneracy.test[j,3:5] <- quantile(boot.coef[j+number.parameters, ], probs = c(0.25, 0.5, 0.75))
    # if sim > obs is 0 or 1, report TRUE, otherwise false. this is a strong indicator for degeneracy
    ev[j] <- degeneracy.test[j,1]==1 | degeneracy.test[j,1]==0
  }

  # put degeneracy test-matrix and ev-data vector together
  degeneracy.test <- data.frame(degeneracy.test, ev)
  colnames(degeneracy.test)<- c("sim > obs", "observed_value", "sim 25%", "sim 50%", "sim 75%", "degenerate?")
  # create list to return results
  results <- list()
  results[[1]]<- summary(mple)  # MPLE results
  results[[2]]<- boot.coef[1:number.parameters, ]  # sample of bootstrap coefficients
  results[[3]]<- boot.results # bootstrapped MPLE results
  results[[4]]<- boot.coef[(number.parameters+1):(2*number.parameters), ] # network statistics of simulated statistics
  results[[5]]<- degeneracy.test # proportions of simulated statistics that are greater than the observed statistics

  names(results)<- c("MPLE_results", "sample_of_bootstrap_coefficients", "bootstrapped_MPLE_results", "simulated_network_statistics", "degeneracy_check")
   stopImplicitCluster()
   return(results)
}

## Example

# library(ergm)
# data(faux.mesa.high)
# # set seed to assure replicability
# set.seed(1234)
# bmple<- boot.MPLE(ergm.formula=faux.mesa.high~edges+nodematch("Sex")+gwesp(0.25, fixed=TRUE), boot.rep=100, number.cores=2 )
# # MPLE results
# bmple$MPLE_results
# 
# # bootstrapped MPLE results
# bmple$bootstrapped_MPLE_results
# 
# # Degeneracy check
# bmple$degeneracy_check
# if sim > obs is close to 0 or 1 -> strong indicator for degeneracy
# or try if the model performs better when the mcmc.interval and mcmc.burnin are incresed