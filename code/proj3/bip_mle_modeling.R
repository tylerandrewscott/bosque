

library(tidyverse)
library(statnet)

locs = 'input/Individual_matrices/unique_matrices_66/'
file_names = list.files(locs)
matrix_list = lapply(file_names,function(x) read_csv(paste0(locs,x))[,-1])
matrix_list = lapply(matrix_list,function(x) x[1:ncol(x),])
matrix_list = lapply(matrix_list,function(x) as.matrix(x,dimnames =list(colnames(x),colnames(x))))
for (i in 1:length(matrix_list))
{rownames(matrix_list[[i]]) <- colnames(matrix_list[[i]])}

nd = unique(unlist(lapply(matrix_list,colnames)))
dn = list()
dn[[1]]<- nd
dn[[2]]<- nd
ex_matrix_structzeros <- lapply(matrix_list,function(x) is.na(futile.matrix::expand(x,target=dn,default = NA))+0)
ex_matrix_ties <- lapply(matrix_list,function(x) (futile.matrix::expand(x,target=dn,default = 0) > 0)+0)
all_matrix_stack <- Reduce(f = `+`,x = ex_matrix_ties)
baseline_net = ex_matrix_ties[[grep('Home Heating Oil',file_names)]]
network_ex_list = lapply(ex_matrix_ties,function(x) as.network(x,directed=T,matrix.type='adjacency',bipartite=F))
names(network_ex_list) <- file_names
network_list = lapply(matrix_list,function(x) as.network(x,directed=T,matrix.type='adjacency',bipartite=F))
names(network_list) <- file_names

###ONLY BASELINE, DROP
#34. Home Heating Oil
#47. Natural Resources Grants
#48.Catskill Tourissm
network_list <- network_list[!grepl('Home Heating Oil|Natural Resources Grants|Catskills Tourism',names(network_list))]

mod_test <- lapply(network_list,function(x) ergm(x ~ edges + gwidegree(0.25,fixed=T) + 
                                                   gwodegree(0.25,fixed=T) + 
                                                   mutual + gwesp(0.25,fixed=T) + gwnsp(0.25,fixed=T) +
                                                   offset(edgecov(ex_matrix_structzeros[[1]])) +
                                                   offset(edgecov(baseline_net)),estimate = 'MPLE',offset.coef = c(-Inf,Inf)))

summary()
plot(network_list[[50]])

ergm(network_list[[50]] ~ edges + mutual + twopath +gwidegree(0.25,fixed=T) ,control = control.ergm(MCMC.burnin = 20000,
                                                                          MCMC.interval = 1500))
     


control = control.ergm(main.method = 'Stepping',Step.MCMC.samplesize = 300,
                            Step.gridsize = 1000))



degree(network_list[[50]],cmode='indegree')

network_list[[50]]
as.network(baseline_net)

all_rules <- do.call(rbind,lapply(1:length(matrix_list), function(x) matrix_list[[x]] %>% 
                                    as.data.frame() %>%
                                    mutate(from = rownames(.),graph = file_names[x]) %>% gather(key = to,value = tie,-from,-graph) %>% 
                                    filter(from!=to)))

all_rules$graphactor <- factor(paste(all_rules$from,all_rules$graph,sep='.'))
library(INLA)
formula <- tie ~ 1 + f(graph, model="iid") + f(graphactor, model="iid")
result <- inla(formula, family="gaussian", data=all_rules)
result <- inla.hyperpar(result)
summary(result)



data(jsp, package="faraway")
jspr <- jsp[jsp$year==2,]
mjspr <- data.frame(rbind(jspr[,1:6],jspr[,1:6]),subject=factor(rep(c("english","math"),c(953,953))),score=c(jspr$english/100,jspr$math/40))
mjspr$craven <- mjspr$raven-mean(mjspr$raven)
mjspr$school <- factor(mjspr$school)
mjspr$classch <- factor(paste(mjspr$school,mjspr$class,sep="."))
mjspr$classchid <- factor(paste(mjspr$school,mjspr$class,mjspr$id,sep="."))




test = do.call(rbind,lapply(mod_test,coef))[,1:2]


plot(test[,2] ~ test[,1])


graph_list = lapply(network_list,intergraph::asIgraph)

#data.frame(centralization_closeness = sapply(graph_list,function(x) igraph::centr_clo(x)$centralization),
mutuality = sapply(graph_list,function(x) igraph::transitivity(x)))


get_loglik = FALSE
est_method = 'MPLE'
mod_list = lapply(network_list,function(x) ergm(x ~ edges + mutual + #gwidegree(0.75,fixed=T) +
                                                  #gwodegree(0.75,fixed=T) + 
                                                  gwesp(2,fixed=T),
                                                #gwnsp(2,fixed=T),
                                                estimate = 'MPLE',
                                                #eval.loglik =get_loglik,
                                                ~fixedas(present = network_list[[34]])))

do.call(rbind,lapply(network_list,function(x) summary(x ~ edges + mutual + triangle)))

graph_list = lapply(network_list,intergraph::asIgraph)

##base net = 34/47/48
for (i in 1:length(network_list))
{print(i);
  ergm(network_list[[i]]~  edges + mutual + gwidegree(0.75,fixed=T) +
         gwodegree(0.75,fixed=T) + gwesp(2,fixed=T) + gwnsp(2,fixed=T),
       estimate = 'MPLE',constraints =  ~fixedas(present = network_list[[34]]))}








grep('Home Heating Oil|Natural Resources Grants|Catskills Tourism',file_names)
library(GERGM)


library(GERGM)
set.seed(12345)
plot_network(matrix_list[[34]])

which()
tt <- 
  
  base_list = list()
base_list$baseline_net

form_list = ~ edges + mutual(alpha = 0.8)


net = ex_matrix_list[[24]]
library(ergm.count)
table(net)

ergm(net ~ edges + mutual('sum'),reference = ~DiscUnif())
help('ergm-defunct')

test <- gergm(net ~ edges + mutual(alpha=0.8) +,
              #covariate_data = covariate_data_2005,
              number_of_networks_to_simulate = 4000,
              thin = 1/10,
              proposal_variance = 0.05,
              MCMC_burnin = 100,
              seed = 456,
              convergence_tolerance = 0.5,
              network_data_list = base_list)




network_data_list <- list(network_covariate = network_covariate)
formula <- net ~ edges + mutual(alpha = 0.8) +
  netcov("network_covariate")

form_list <- list(f1 = formula)
testl <- parallel_gergm(formula_list = form_list,
                        observed_network_list = net,
                        covariate_data_list = node_level_covariates,
                        network_data_list = network_data_list,
                        cores = 2,
                        number_of_networks_to_simulate = 10000,
                        thin = 1/100,
                        proposal_variance = 0.1,
                        MCMC_burnin = 5000)


base_list$baseline_net




+ sender("log_GDP") + 
  receiver("log_GDP") + nodemix("G8", base = "No")


+ netcov(net_exports_2005) 


network_list[[47]]
network_list[[48]]
table(matrix_list[[1]]
      plot(network_list[[34]])
      degree(network_list[[34]],cmode = 'indegree')
      
      temp =do.call(rbind,lapply(mod_list,coef)) %>% as.data.frame() %>% mutate(m = 1:nrow(.)) %>% gather(param,est,-m)
      
      ggplot(temp,aes(x=m,y=est)) + facet_wrap(~param,scales='free') + geom_point() + theme_bw()
      
      
      arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
                                                               c(3,3,2))>.5, 3 ) )
      
      library(btergm)
      btergm(matrix_list ~ edges + mutual)
      
      
      library(GGally)
      library(ggnetwork)
      
      ggplot(ggnetwork(network_list[[11]],layout = "target"),aes(x=x,y=y,xend=xend,yend=yend)) + geom_nodes() + geom_edges()
      ?fortify.network
      geom_edges
      test = do.call(igraph::union,graph_list)
      library(intergraph)
      plot(asNetwork(test))
      plot(network_list[[12]])
      
      
      coefs = do.call(rbind,lapply(mod_list,function(x) x$coef)) %>% as.data.frame() %>% 
        mutate(game = 1:nrow(.))
      coefs = coefs %>% gather(param,est,-game)
      library(ggthemes)
      
      ggplot(coefs,aes(x=game,y=est,col = ifelse(game<=11,'CC','PG'))) + 
        facet_wrap(~param,nrow=2,scales='free_y') + geom_point() + theme_bw() +
        scale_color_colorblind(name = 'Game type',labels=c('credible commitement','public good')) +
        theme(legend.position = c(0.8,0.2))
      
      
      library(btergm)
      plot(network_list[[1]]) 
      plot(network_list[[2]])
      plot(network_list[[3]])
      plot(network_list[[4]])
      
      degree(network_list[[3]],cmode = 'indegree')
      degree(network_list[[3]],cmode = 'outdegree')
      
      
      test = read_csv(paste0(locs,file_names[64]))
      
      
      
      
      
      lapply(matrix_list,dim)
      net_list = lapply(matrix_list,as.network,matrix.type = 'adjacency',bipartite=F,directed=T)
      
      
      cols = lapply(matrix_list,function(x) x[,1])
      
      
      do.call(intersect,cols)
      