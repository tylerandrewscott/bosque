library(lubridate)
library(tidyverse)
library(ggthemes)
library(data.table)
library(stringr)
not_district = '0000000'
dir.create('drought_and_debt/output/')
tx_dsci <- readRDS('drought_and_debt/input/dsci_measures.RDS')
tx_dsci <- data.table(tx_dsci)
library(jsonlite)
start_year = 2010
tx_dsci$date <- ymd(tx_dsci$MapDate)
tx_dsci <- tx_dsci[!is.na(date),]
tx_dsci<-tx_dsci[year(tx_dsci$date) %in% 2011:2022,]

tx_dsci <- tx_dsci[order(Name,date),]
tx_dsci$DSCI <- as.numeric(tx_dsci$DSCI)
tx_dsci$Name <- str_remove(tx_dsci$Name,'--.*')
tx_dsci$Name[tx_dsci$Name=='Dallas'] <- 'Dallas/Fort Worth'
tx_dsci$Name <- str_remove(tx_dsci$Name,'\\, TX$')
(figure1A =  ggplot(data = tx_dsci[tx_dsci$Name!='Texas',],aes(x = date)) +
  geom_path(aes(y = DSCI/5,group = Name,colour = Name)) + 
  scale_y_continuous(name = 'severity-coverage index',breaks = c(seq(0,100,20)),labels = c(seq(0,100,20) * 5))+
  scale_color_tableau() + theme_bw() + 
  ggtitle('Urban area drought conditions, 2010 to 2022')+
  scale_x_date(expand = c(0,0),name = 'Weekly drought status') +
  theme(#axis.title.x = element_blank(),
        legend.position = c(0.55,0.70),
        legend.background = element_rect(fill = alpha('white',0.5)),
        legend.title = element_blank()))

txc <- fromJSON('https://usdmdataservices.unl.edu/api/StateStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=48&dx=1&DxLevelThresholdFrom=10&DxLevelThresholdTo=70&startdate=1/1/2000&enddate=1/1/2023&statisticsType=1')
txc$D0 = as.numeric(txc$D0);txc$D1 = as.numeric(txc$D1);txc$D2 = as.numeric(txc$D2)
txc$D3 = as.numeric(txc$D3);txc$D4 = as.numeric(txc$D4);
txc = txc[txc$StatisticFormatID==1,]
txc$D0 = as.numeric(txc$D0);txc$D1 = as.numeric(txc$D1);txc$D2 = as.numeric(txc$D2)
txc$D3 = as.numeric(txc$D3);txc$D4 = as.numeric(txc$D4);
txc$ValidStart = ymd(txc$ValidStart)

rib_cols = tableau_color_pal(type = 'ordered-sequential',palette = 'Classic Orange')(7)[c(1,2,3,5,6)]
figure1B <- ggplot(data = txc[year(txc$ValidStart)>=2010,],aes(x = ValidStart)) +
      #geom_point(aes( y=D0),fill = 'yellow') + 
     geom_ribbon(aes(ymin=0, ymax=D0,fill =  rib_cols[1])) + 
     geom_ribbon(aes(ymin=0, ymax=D1,fill = rib_cols[2])) + 
     geom_ribbon(aes(ymin=0, ymax=D2,fill = rib_cols[3])) + 
     geom_ribbon(aes(ymin=0, ymax=D3,fill = rib_cols[4])) +
     geom_ribbon(aes(ymin=0, ymax=D4,fill = rib_cols[5])) + 
     theme_bw() + scale_y_continuous(name = '% of land area in status',expand = c(0,0)) + 
     scale_x_date(name = 'Weekly drought status',expand = c(0,0)) + 
     theme(text = element_text(family = 'Times'),legend.position = c(0.55,0.6),axis.title = element_text(size = 12),
           legend.background = element_rect(fill = alpha('white',0.5)))+ 
     ggtitle('Statewide (TX) drought conditions, 2010 to 2022') + 
   scale_fill_identity(labels = c('D4','D3-D4','D2-D4','D1-D4','D0-D4'),guide = 'legend',name = 'Category')
library(gridExtra)
grob <- grid.arrange(figure1B,figure1A,ncol = 1)
 
ggsave(grob,filename = 'drought_and_debt/output/figure1.png',width = 7,height = 6,units = 'in',dpi = 400)

