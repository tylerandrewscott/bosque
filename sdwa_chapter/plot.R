
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
gg_pop <- ggplot(stems_sub,aes(x = index,y = cumsum_m)) + 
  ggtitle('U.S. Community Water Systems by service population')+
  ylab('Cumulative population served (million)') + 
  xlab('Water system service population')+
  theme_bw() + geom_path() + geom_point(size = 0.2,alpha = 0.5)+
  scale_x_continuous(breaks =index_breaks*1e3,labels =  stems_sub$population_served_count[index_breaks*1e3])
gg_pop
ggsave('sdwa_chapter/output/figure_population.png',gg_pop,dpi = 450,width = 6,height = 4, units = 'in')


stems_sub[100,]
head(stems_sub)


sum(stems$population_served_count)

table(stems$pws_type_code)
