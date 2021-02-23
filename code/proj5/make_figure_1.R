library(data.table)
library(tidyverse)
library(INLA)
library(lubridate)
library(MASS)
library(tidyverse)
library(lubridate)
library(survival)
library(mstate)
library(sf)
library(geojsonsf)
library(data.table)
library(tigris)
library(lwgeom)
library(pbapply)
library(ggthemes)

library(rgeos)
require(spdep)
#setwd('../../')
not_district = '0000000'
tx_query = "https://usdmdataservices.unl.edu/api/StateStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=48&startdate=1/1/2000&enddate=12/28/2020&statisticsType=2/json"
library(jsonlite)
start_year = 2010
tx_dsci = fromJSON(tx_query)
tx_dsci$D0 = as.numeric(tx_dsci$D0);tx_dsci$D1 = as.numeric(tx_dsci$D1);tx_dsci$D2 = as.numeric(tx_dsci$D2)
tx_dsci$D3 = as.numeric(tx_dsci$D3);tx_dsci$D4 = as.numeric(tx_dsci$D4);
tx_dsci = tx_dsci[tx_dsci$StatisticFormatID==1,]
tx_dsci$D0 = as.numeric(tx_dsci$D0);tx_dsci$D1 = as.numeric(tx_dsci$D1);tx_dsci$D2 = as.numeric(tx_dsci$D2)
tx_dsci$D3 = as.numeric(tx_dsci$D3);tx_dsci$D4 = as.numeric(tx_dsci$D4);
tx_dsci$ValidStart = ymd(tx_dsci$ValidStart)
rib_cols = tableau_color_pal(type = 'ordered-sequential',palette = 'Classic Orange')(7)[c(1,2,3,5,6)]
figure1 =  ggplot(data = tx_dsci,aes(x = ValidStart)) +
  # geom_point(aes( y=D0),fill = 'yellow') + 
  geom_ribbon(aes(ymin=0, ymax=D0,fill =  rib_cols[1])) + 
  geom_ribbon(aes(ymin=0, ymax=D1,fill = rib_cols[2])) + 
  geom_ribbon(aes(ymin=0, ymax=D2,fill = rib_cols[3])) + 
  geom_ribbon(aes(ymin=0, ymax=D3,fill = rib_cols[4])) +
  geom_ribbon(aes(ymin=0, ymax=D4,fill = rib_cols[5])) + 
  theme_bw() + scale_y_continuous(name = '% of state land area in status',expand = c(0,0)) + 
  scale_x_date(name = 'Weekly drought status',expand = c(0,0)) + 
  theme(text = element_text(family = 'Times'),legend.position = c(0.92,0.6),axis.title = element_text(size = 12),
        legend.background = element_rect(fill = alpha('white',0.5)))+ 
  ggtitle('Texas statewide drought conditions, 2010 to 2018') + 
  geom_vline(aes(x  = May 4th, 2010))
  scale_fill_identity(labels = c('D4','D3-D4','D2-D4','D1-D4','D0-D4'),guide = 'legend',name = 'Category')
figure1
ggsave(figure1,filename = 'output/proj5/figure1.png',dpi = 500,width=6,height=2.8,units='in')