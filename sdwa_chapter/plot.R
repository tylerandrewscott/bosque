
fls <- list.files('sdwa_chapter/state_systems/',full.names = T)
lst <- lapply(fls,readRDS)
library(data.table)
stems <- rbindlist(lst)
stems <- stems[pws_type_code=='CWS',]
library(tidyverse)
dim(stems)
table(stems$population_served_count<10000)/nrow(stems)
summary(stems$population_served_count)
stems_sub <- stems[,.(pwsid,population_served_count)]

#stems_sub$prop = stems_sub$cumsum / sum(stems_sub$population_served_count)
stems_sub <- stems_sub[order(population_served_count),]
stems_sub$cumsum <- cumsum(stems_sub$population_served_count)

stems_sub$index <- 1:nrow(stems_sub)
stems_sub$cumsum_m <- stems_sub$cumsum/1e6
stems_sub$prop <- stems_sub$cumsum / sum(stems_sub$population_served_count)

index_breaks = seq(0,48,8)[-1]
index_breaks <- c(0.1,index_breaks)
library(stringr)
percs <- c(0.01,0.25,0.5,0.75,0.99)
quant_breaks <- quantile(stems_sub$population_served_count,percs)
index_breaks <- quantile(stems_sub$index,percs)
quant_labels <- str_replace(names(quant_breaks),'%','th')
people <- round(quant_breaks)
people[4] <- '2.1k'
people[5] <- '98k'


gg_pop <- ggplot(stems_sub) + 
  ggtitle('U.S. Community Water Systems by size')+
  ylab('Total U.S. population served (million)') + 
  xlab('Pop. percentile (pop.)')+
  theme_bw() + 
  geom_path(aes(x = index,y = cumsum_m)) + 
  geom_point(size = 0.2,alpha = 0.5,aes(x = index,y = cumsum_m))+
  scale_x_continuous(breaks =index_breaks,labels = paste0(quant_labels,'\n','(',people,')'))

ggsave('sdwa_chapter/output/figure_population.png',gg_pop,dpi = 450,width = 6,height = 4, units = 'in')


