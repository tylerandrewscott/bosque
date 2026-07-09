library(data.table)
library(jsonlite)
library(stringr)
library(pbapply)
library(lubridate)
library(tidyverse)
library(terra)          # replaces retired rgeos/rgdal/maptools/sp
source('code/spatial_helpers.R')  # reproject_sf(), tx_county_adjacency()


mlist <- fread('bosquebox/input/texas_dww/district_master_list.csv')
mlist_dt <- mlist[Type=='C']
mlist_dt$Purchaser = grepl('P',mlist_dt$`Pri. Src. Water Type`) + 0
mlist_dt$Groundwater = grepl('G',mlist_dt$`Pri. Src. Water Type`) + 0
mlist_dt[,`Pri. Src. Water Type`:=NULL]

setnames(mlist_dt,c('Water System No.','Water System Name'),c('PWS_ID','PWS_NAME'))
mlist_dt$PWS_NAME <- stringr::str_remove(stringr::str_extract(mlist_dt$PWS_NAME,"^[A-Z0-9\\s]+"),'\\sF$')

pws_drought_weekly <- readRDS('input/pws_drought_weekly.RDS')
dsci_month_means <- pws_drought_weekly |> 
  mutate(month = floor_date(ymd(DroughtDate),unit ='month')) |>
  group_by(month) |> 
  summarise(avg_DSCI = mean(DSCI))
library(reReg)
library(lubridate)
library(data.table)
pws_drought_weekly$YMD <- ymd(pws_drought_weekly$DroughtDate)
# astart at begining of 2010
min_date <- min(pws_drought_weekly[year(YMD) == 2010,]$YMD)
pws_drought_weekly <-pws_drought_weekly[YMD>=min_date,]
pws_drought_weekly <- pws_drought_weekly[pws_drought_weekly$PWS_ID %in% mlist_dt$PWS_ID,]

pws_drought_weekly <- left_join(pws_drought_weekly,mlist_dt,by = c('PWS_ID' = 'PWS_ID'))

notice <- readRDS('input/combined_restriction_records.RDS')
notice <- notice |> 
  rename(YMD = NOTIFIED_YMD,PWS_ID = `PWS ID`)
notice <- notice[!duplicated(paste(PWS_ID,YMD)),]

# For each row in pws_drought_weekly
# Convert to data.table
setDT(pws_drought_weekly)
setDT(notice)

# Sort both tables by PWS_ID and YMD
setkey(notice, PWS_ID, YMD)
setkey(pws_drought_weekly, PWS_ID, YMD)

# Order by PWS_ID and YMD
notice <- notice[order(PWS_ID,YMD),]

pws_drought_weekly <- pws_drought_weekly[order(PWS_ID,YMD),]

# Rolling join to find most recent notice for each weekly observation
pws_drought_weekly[, RESTRICTION := 0]


pws_drought_weekly[, prior_YMD := shift(YMD, type="lag"), by=PWS_ID]


pws_drought_weekly_notice <- pws_drought_weekly[notice,  RESTRICTION := 1L,
                  on = .(PWS_ID, YMD > YMD, prior_YMD<=YMD)]

pws_drought_weekly_notice[is.na(RESTRICTION), RESTRICTION := 0]


library(survival)
library(lubridate)
first_date <- min(min(pws_drought_weekly_notice$YMD,na.rm = T),min(pws_drought_weekly_notice$prior_YMD,na.rm = T))
# Calculate the decimal date for each PWS_ID
#pws_drought_weekly_notice[, decimal_date := as.numeric(difftime(YMD, min(YMD), units = "days")) / 365.25, by = PWS_ID]
pws_drought_weekly_notice[, decimal_date.t1 := decimal_date(YMD), by = PWS_ID]
pws_drought_weekly_notice[, decimal_date.t0 := decimal_date(prior_YMD), by = PWS_ID]
# Create a Surv object for the Cox model using RESTRICTION as the event indicator

id_id <- readRDS('input/id_crosswalk.rds')
pws_drought_weekly_notice$District_ID <- id_id$District_ID[match(pws_drought_weekly_notice$PWS_ID,id_id$PWS_ID)]
pws_drought_weekly_notice$District_ID[pws_drought_weekly_notice$PWS_ID=='TX2490016'] <- '8492000'
pws_drought_weekly_notice$District_ID[pws_drought_weekly_notice$PWS_ID=='TX0430053'] <- '5952250'
pws_drought_weekly_notice$District_ID[pws_drought_weekly_notice$PWS_ID=='TX0420034'] <- '2312250'
pws_drought_weekly_notice$District_ID[pws_drought_weekly_notice$PWS_ID=='TX1900009'] <- '7585150'
pws_drought_weekly_notice$District_ID[pws_drought_weekly_notice$PWS_ID=='TX2040033'] <- '7492500'
pws_drought_weekly_notice$District_ID[pws_drought_weekly_notice$PWS_ID=='TX2290037'] <- '8070000'
pws_drought_weekly_notice$District_ID[pws_drought_weekly_notice$PWS_ID=='TX2360010'] <- '7634575'
pws_drought_weekly_notice$District_ID[pws_drought_weekly_notice$PWS_ID=='TX1290010'] <- '995951'
pws_drought_weekly_notice$District_ID[pws_drought_weekly_notice$PWS_ID=='TX0940015'] <- '2412188'
pws_drought_weekly_notice$District_ID[pws_drought_weekly_notice$PWS_ID=='TX1650133'] <- '5846750'
pws_drought_weekly_notice$District_ID[pws_drought_weekly_notice$PWS_ID=='TX0200706'] <- '1636654' 


pws_district_weekly <- pws_drought_weekly_notice[!is.na(District_ID),]
pws_district_weekly <- pws_district_weekly[!is.na(decimal_date.t0),]
fin <- readRDS('input/combined_and_lagged_finances.RDS')
fin <- fin[!is.na(DISTRICT_NAME),]

money_cols = grep('GENERAL|ENTERPRISE|Total|Balance|BONDS',colnames(fin),value = T)
fin[,(money_cols):=lapply(.SD,function(x){x/1e3}),.SDcols = money_cols]


replaceNA <-  function(x,y){
  x[is.na(x)] <- y
  return(x)
} 
# Quick ratio
fin$Total_Assets <- (replaceNA(fin$`GENERAL FUND - ASSETS`, 0) + replaceNA(fin$`ENTERPRISE FUND - ASSETS`, 0))
fin$Total_Fund_Balance <- (replaceNA(fin$`GENERAL FUND - FUND BALANCE`, 0) + replaceNA(fin$`ENTERPRISE FUND - NET ASSETS`, 0))
fin$Total_Liabilities <- (replaceNA(fin$`GENERAL FUND - LIABILITIES`, 0) + replaceNA(fin$`ENTERPRISE FUND - LIABILITIES`, 0)) 
fin$Quick_Ratio <-  ifelse(fin$Total_Assets==0 & fin$Total_Liabilities ==0,1,
                           ifelse(fin$Total_Assets > 0 & fin$Total_Liabilities==0,
                                  fin$Total_Assets / {fin$Total_Liabilities+1},fin$Total_Assets / fin$Total_Liabilities))

summary(fin$Total_Assets[fin$Total_Liabilities==0])
fin[fin$Total_Assets==0 & fin$Total_Liabilities==0,]
# Operating Margins ratio
fin$Total_Revenue <- (replaceNA(fin$`GENERAL FUND - TOTAL REVENUES`, 0) + replaceNA(fin$`ENTERPRISE FUND - OPERATING REVENUES`, 0))
fin$Total_Expenditure <- (replaceNA(fin$`GENERAL FUND - TOTAL EXPENDITURES`, 0) + replaceNA(fin$`ENTERPRISE FUND - OPERATING EXPENSES`, 0)) 
fin$Operating_Ratio <-  fin$Total_Revenue/fin$Total_Expenditure 
## one-to-one ratio but boht are zero
fin$Operating_Ratio[fin$Total_Revenue==0&fin$Total_Expenditure==0]<-1
fin$Balance_over_Revenues <- fin$Total_Fund_Balance / fin$Total_Revenue

# long term obligations over yearly revenue
fin$TotalDebtServiceOutstanding <- replaceNA(fin$TotalDebtServiceOutstanding_GO,0) + replaceNA(fin$TotalDebtServiceOutstanding_REV,0) 
fin$TotalDebtServiceOutstanding[fin$TotalDebtServiceOutstanding==0 & fin$`BONDS OUTSTANDING`>0] <- fin$`BONDS OUTSTANDING`[fin$TotalDebtServiceOutstanding==0 & fin$`BONDS OUTSTANDING`>0] 

fin$LTD_over_Revenue <- replaceNA(fin$TotalDebtServiceOutstanding,0) / replaceNA(fin$Total_Revenue,0) 

min(fin$Total_Revenue)
summary(fin$TotalDebtServiceOutstanding)

ggplot(fin[Quick_Ratio<Inf & fin$Total_Assets>0 & fin$Fund_Balance!=0,]) + 
  geom_point(aes(x = log(Quick_Ratio),y=log(Balance_over_Revenues)))

fin_sub <- fin[,.(District_ID,`FISCAL YEAR ENDED`,LTD_over_Revenue,Quick_Ratio,Operating_Ratio,Balance_over_Revenues,Total_Revenue,Total_Expenditure,`BONDS OUTSTANDING`,TotalDebtServiceOutstanding)]
fin_sub$decimal_date.FYE <- decimal_date(ymd(fin_sub$`FISCAL YEAR ENDED`))

fin_sub$join_time <- fin_sub$decimal_date.FYE
pws_district_weekly$join_time <- pws_district_weekly$decimal_date.t0

setkey(fin_sub,District_ID,join_time)
setkey(pws_district_weekly,District_ID,join_time)

pws_district_weekly <- fin_sub[pws_district_weekly,roll = T]

quantile(pws_district_weekly$Quick_Ratio,seq(0.0,0.9,0.1),na.rm = T)

pws_district_weekly <- pws_district_weekly |>
  mutate(Quick_Ratio_Category = factor(case_when(
    Quick_Ratio < .9 ~ "< 0.9",
    Quick_Ratio >= 0.9 & Quick_Ratio < 1.1 ~ "0.9-1.1",
    Quick_Ratio >= 1.1 & Quick_Ratio < 2.5 ~ "1.1-2.5",
    Quick_Ratio >= 2.5 ~ "> 2.5" # & Quick_Ratio < 5 ~ "2.5-5",
    #Quick_Ratio >= 5 & Quick_Ratio < 10 ~ "5-10",
    #Quick_Ratio >= 5 ~ "> 5"
  ), levels = c("< 0.9", "0.9-1.1","1.1-2.5", "> 2.5"), ordered = F))

pws_district_weekly <- pws_district_weekly |>
  mutate(Operating_Ratio_Category = factor(case_when(
    Operating_Ratio < .75 ~ "<.75",
    Operating_Ratio >= 0.75 & Operating_Ratio < 1 ~ "0.75-1",
    Operating_Ratio >= 1 & Operating_Ratio < 1.2 ~ "1-1.2",
    Operating_Ratio >= 1.2 & Operating_Ratio < 1.5 ~ "1.2-1.5",
    Operating_Ratio >= 1.5 ~ "1.5+"
  ), levels = c("<.75", "0.75-1", "1-1.2", "1.2-1.5", "1.5+"), ordered = F))

pws_district_weekly <- pws_district_weekly |>
  mutate(LTD_over_Revenue_Category = factor(case_when(
    LTD_over_Revenue == 0 ~ "0",
    LTD_over_Revenue > 0 & LTD_over_Revenue < 1 ~ '0-1',
    LTD_over_Revenue >= 1 & LTD_over_Revenue < 2.5 ~ "1-2.5",
    LTD_over_Revenue >= 2.5 & LTD_over_Revenue < 5 ~ "2.5-5",
    LTD_over_Revenue >= 5 & LTD_over_Revenue < 10 ~ "5-10",
    LTD_over_Revenue >= 10 ~ "10+"
  ),levels = c("0",'0-1','1-2.5','2.5-5','5-10','10+'),ordered = F))

epa <- fread('bosquebox/input/epa_sdwis/Water System Summary_20250214.csv')
pws_district_weekly$Wholesaler <- (epa$`Is Wholesaler`[match(pws_district_weekly$PWS_ID,epa$`PWS ID`)] =='Y')+0
pws_district_weekly$Pop_Served <- epa$`Population<br> Served Count`[match(pws_district_weekly$PWS_ID,epa$`PWS ID`)]
pws_district_weekly$Pop_Served <- as.numeric(str_remove_all(pws_district_weekly$Pop_Served,'\\,'))
pws_district_weekly$Pop_Served_Cat5 <- epa$`Pop Cat 5`[match(pws_district_weekly$PWS_ID,epa$`PWS ID`)]
pws_district_weekly$Groundwater <- grepl("ground",tolower(epa$`Primary Source`[match(pws_district_weekly$PWS_ID,epa$`PWS ID`)])) + 0

stor <- 'input/storage_connections_data.txt'
stor_dt <- fread(stor)
stor_dt$Value <- as.numeric(stor_dt$Value)
stor_dt$Value[stor_dt$Unit=='GAL'] <- stor_dt$Value[stor_dt$Unit=='GAL']/1e6
stor_dt$Unit <- 'MG'

pws_district_weekly$Total_Storage_MG <- stor_dt$Value[match(pws_district_weekly$PWS_ID,stor_dt$PWS_ID)]
pws_district_weekly$Interconnects <- stor_dt$Num_Interconnections[match(pws_district_weekly$PWS_ID,stor_dt$PWS_ID)]

median_storage_vals <- pws_district_weekly[!duplicated(PWS_ID),median(Total_Storage_MG,na.rm = T),by=.(Pop_Served_Cat5)]

pws_district_weekly$MEDIAN_STORAGE <- is.na(pws_district_weekly$Total_Storage_MG) + 0
pws_district_weekly$Total_Storage_MG[is.na(pws_district_weekly$Total_Storage_MG)] <- median_storage_vals$V1[match(pws_district_weekly$Pop_Served_Cat5[is.na(pws_district_weekly$Total_Storage_MG)],median_storage_vals$Pop_Served_Cat5)]

surv_obj <-  with(pws_district_weekly,Surv(time = decimal_date.t0, time2 = decimal_date.t1, event = RESTRICTION))
summary(pws_district_weekly$Total_Storage_MG + 0.01)

summary(cox_model1)
base_form <- surv_obj ~ log(DSCI+1) + log(Pop_Served+1) + Groundwater + log(Total_Storage_MG) + Interconnects
# Fit the repeated events Cox proportional hazards model-*-*/
cox_model1 <- coxph(update.formula(base_form,~ . + Quick_Ratio_Category),cluster = PWS_ID, data = pws_district_weekly)
cox_model2 <- coxph(update.formula(base_form,~ . + Operating_Ratio_Category),cluster = PWS_ID, data = pws_district_weekly)
cox_model3 <- coxph(update.formula(base_form,~ . + LTD_over_Revenue_Category),cluster = PWS_ID, data = pws_district_weekly)
table(pws_district_weekly$Quick_Ratio_Category)
library(texreg)
screenreg(list(cox_model1,cox_model2,cox_model3))
table(pws_district_weekly$Interconnects,pws_district_weekly$Wholesaler)
table(pws_district_weekly$LTD_over_Revenue_Category)
summary(cox_model2)

pws_district_weekly[order(-Pop_Served),][Interconnects == 0 & Wholesaler == 1,]

colnames(epa)

table(is.na(epa$`Is Wholesaler`[match(pws_district_weekly$PWS_ID,epa$`PWS ID`)]))
table(unique(pws_district_weekly$PWS_ID) %in% unique(dets$`PWS ID`))
table(is.na(dets$Wholesaler[match(pws_district_weekly$PWS_ID,dets$`PWS ID`)]))


table(dets$`DBPR Schedule Category Code`,dets$`Is Wholesaler`)
head(dets)

table(dets$`DBPR Schedule Category`)
table(dets$`DBPR Schedule Category Code`)



dim(fin_sub)


dim(pws_district_weekly)
dim()
head(test)
decimal_date(ymd('2005-09-30'))
infrastructure <- NULL
demos <- readRDS('input/pws_tract_overlaps.RDS')

demos



id_id[,.N,by=.(District_ID)][order(-N)]




dim(fin)
head(fin)







summary(cox_model)
print(citation('survival'),bibtex = T)

# Display the summary of the Cox model
summary(cox_model)




test2 <- test[PWS_ID == 'TX0000001',.(PWS_ID,YMD)]
test2

notice[PWS_ID == 'TX0000001',.(PWS_ID,YMD)]

dim(test)
dim(pws_drought_weekly)

head(test)

# Handle cases where there is no next notice (set to 1 for all dates after last notice)
pws_drought_weekly[notice[is.na(next_YMD)], 
                  RESTRICTION := 1L,
                  on = .(PWS_ID, YMD >= YMD),
                  roll = TRUE]

#mlist_dt$PERIOD <- rep(c('P1','P2','P3'),each = nrow(mlist))


# https://www.twdb.texas.gov/publications/reports/other_reports/doc/Drought-in-Texas-Comparison-1950s-2010s.pdf
### this is when PDSI says drought started (before SPI in Feb 2011)
p1_start <- mdy('08-01-2010')
### this is when SPI says drought ended (after PDSI in Nov 2014)
p1_end <- mdy('03-31-2015')

p3_start <- mdy('09-01-2021')
#p3_end <- mdy('10-31-2024')
p2_start <- p1_end+days(1)
p2_end <- p3_start-days(1)

p3_end <- max(notice$YMD)+days(1)




library(reReg)
library(tidyverse)

head(pws_drought_weekly)
mlist_dt

mlist_dt$`Water System Name` <- stringr::str_remove(mlist_dt$`Water System Name`,'\\sFact.*Summary\\sSheet$')
mlist_dt
#notice <- notice |>
  ### any day within either D period is included ### (<= on both sides)
 # mutate(PERIOD = case_when(YMD>=p1_start&YMD<= p1_end ~ 'P1',
 #                                   YMD>=p3_start&YMD<=p3_end ~ 'P3',
 #                                   T ~ 'P2')) 
#

# find earliest adoption by PWS_ID and PERIOD
#notice <- notice %>% group_by(PWS_ID,PERIOD) %>% summarize(RESTRICTION_DATE = min(YMD))


2+2
mlist_notice <- merge(mlist_dt,notice)
dim(mlist_notice)


mlist_dt <- mlist_dt |> mutate(RESTRICTION_TIME = case_when(
  PERIOD == 'P1' ~ interval(RESTRICTION_DATE,p1_start) %/% days(1),
  PERIOD == 'P3' ~ interval(RESTRICTION_DATE,p3_start) %/% days(1),
  T ~ interval(RESTRICTION_DATE,p1_end + days(1)) %/% days(1)))

#### for same day, recode to day 1 ###
mlist_dt$RESTRICTION_TIME[mlist_dt$RESTRICTION_TIME == 0] <- 1
### abs because the interval produces negative time ###
mlist_dt$RESTRICTION_TIME<-abs(mlist_dt$RESTRICTION_TIME)


notice <- data.table(notice)
notice$YMD <- notice$RESTRICTION_DATE
setkey(notice,'PWS_ID','YMD','PERIOD')
setkey(pws_drought_weekly,'PWS_ID','YMD','PERIOD')

notice$RESTRICTION <- 1
df <- (notice[pws_drought_weekly,])
df$RESTRICTION[is.na(df$RESTRICTION)] <- 0
df <- df[order(PWS_ID,DroughtDate),]
df <- df[YMD>=p1_start,]


# For each PWS_ID and PERIOD, get observations up to and including first restriction
df_subset <- df[order(PWS_ID, DroughtDate), ][, {
  # Find index of first restriction in this group, if any
  first_restrict <- which(RESTRICTION == 1)[1]
  
  if (is.na(first_restrict)) {
    # If no restriction, keep all rows for this drought window
    .SD
  } else {
    # Keep rows up to and including first restriction
    .SD[1:first_restrict]
  }
}, by = .(PWS_ID, PERIOD)]

# Reorder the final dataset
setorder(df_subset, PWS_ID, DroughtDate)
df <- df_subset
df$YMD_dec <- decimal_date(df$YMD)
df <- df |> mutate(time = case_when(PERIOD=='P1'~YMD_dec - decimal_date(p1_start),
                              PERIOD=='P2'~YMD_dec - decimal_date(p2_start),
                              T ~ YMD_dec - decimal_date(p3_start)))

df <- df[order(PWS_ID,time),time0:=lag(time,n = 1),by=.(PWS_ID,PERIOD)]
df$time0[is.na(df$time0)]<-0

restriction_month_year <- notice |> 
  mutate(month = floor_date(RESTRICTION_DATE, unit = "month")) |> 
  group_by(month) |>
  summarise(restr_count = n())

month_year <- merge(restriction_month_year,dsci_month_means,all = T)
month_year$restr_count[is.na(month_year$restr_count)]<-0
month_year <- month_year[month_year$month >= p1_start&month_year$month <=p3_end,]


g_base <- ggplot(data = month_year,
       aes(x = month)) + 
  geom_vline(xintercept = p1_end,lty = 2,col = 'grey50') + 
  geom_vline(xintercept = p2_end,lty = 2,col = 'grey50') + 
  scale_x_date(name = 'Month', 
               labels = function(z) gsub("^0", "", strftime(z, "%m/'%y")),
               #date_labels = "%m/%y", 
               breaks = "year")+
  theme_bw() 

g1 <- g_base + labs(y = '# restrictions issued') + 
  geom_bar(aes(y = restr_count),stat = 'identity') + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),text = element_text(family = 'Times'))
g2 <- g_base + labs(y = 'avg. DSCI') + geom_line(aes(y = avg_DSCI)) + 
  theme(text = element_text(family = 'Times'),
        axis.text.x = element_text(angle= 00,size = 8))

library(cowplot)
grid_plot <- cowplot::plot_grid(g1, 
                   g2, 
                   nrow = 2,align = 'v',
                   labels = "auto",label_fontfamily = 'Times')

# now add the title
title <- ggdraw() + 
  draw_label(
    "Drought level and water use restrictions",
    fontface = 'bold',fontfamily = 'Times',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
grid_with_title <- plot_grid(
  title, grid_plot,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

ggsave(plot = grid_with_title,filename = 'output/figure1_dsci_restrictions.png',dpi = 450,width = 7,height = 7,units = 'in')

library(survival)

surv_obj <- Surv(df$time0,df$time,df$RESTRICTION)
cox_mod <- coxph(surv_obj ~ 1 + #scale(DSCI) + 
                   PERIOD,cluster = PWS_ID,data = df)

summary(cox_mod)

head(df)

df[PERIOD=='P2']
df[,list(.N,mean(RESTRICTION)),by=.(PERIOD)]
summary(cox_mod)
table(is.na(df$restriction))
table(is.na(df$PWS_ID))
table(is.na(df$DSCI))

head(df)

paste(year(df$RESTRICTION_DATE),month(df$RESTRICTION_DATE),sep='_')
df$RESTRICTION_DATE

library(survival)
library(INLA)
#df1 <- df[df$PERIOD=='p1',]
df.test <- df %>% group_by(PERIOD,PWS_ID) %>%
  filter(time == max(time))

surv.drought <- Surv(time = df.test$time,event = df.test$restriction)
km.drought <- survfit(surv.drought ~ -1 + PERIOD,data = df.test)
autoplot(km.drought)

df$DSCI_2SD <- {df$DSCI-mean(df$DSCI)} / {sd(df$DSCI)*2}
surv.time <-  Surv(time2 = df$time,time = df$time0,event = df$restriction)
mod <- coxph(surv.time ~ DSCI_2SD + drought_window,data = df)

library(INLA)


table(is.na(df$time0))
df$intercept <- 1
df_end <- df |> group_by(PWS_ID,drought_window) |> filter(max(time) ==time)

cox_base = inla.coxph(inla.surv(time = time,event = restriction) ~ -1 + intercept + 
                        f(PWS_ID,model="iid",hyper = list(prec = list(param=c(1, 1)))),
                       # f(Primary_CFIPS,model="iid",hyper = list(prec = list(param=c(1, 1)))) +
                      #  f(CFIPS_ID,model="besag",graph="tx.adj",hyper = list(prec = list(param=c(1, 1))))  ,
                      data = df)
cox_base                      

df[is.na(time0),]
control.hazard=list(model="rw1", n.intervals=months2,scale.model = TRUE))




coxinla.vet <- inla(inla.surv(time = time0,time2= time,event = restriction) ~ 
                      1  + DSCI_2SD, data = df,
                    family = "coxph",
                    control.hazard = list(hyper = list(prec = list(param = c(0.001, 0.001)))))




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
#library(esri2sf)

require(spdep)
library(readxl)


#Drought score
#Population served, logged
#Total storage per 1,000 people 
#Number of interconnections, logged 
# Groundwater
#% customers served retail 
#Average daily consumption per1,000 people
#% Democratic vote
#Median household income, logged, #% houses built after 1980, #% rural,#% Black, #% Hispanic, #% four-year college degree

library(readxl)


start_date = mdy('5/4/2010')
end_date = mdy('7/7/2015')
weekly <- seq(start_date,end_date,by='weeks')
start_date_dec = decimal_date(start_date)




tx_restrictions <- readRDS('input/combined_restriction_records.RDS')
tx_restrictions <- tx_restrictions[!duplicated(tx_restrictions[,.(`PWS ID`,Priority,Stage,Population,Connections,Notified)]),]
tx_restrictions$PWS_ID = paste0('TX',tx_restrictions$`PWS ID`)
tx_restrictions[,`PWS ID`:=NULL]
tx_restrictions = tx_restrictions[!is.na(tx_restrictions$PWS_ID),]
tx_restrictions$Restriction = NA
tx_restrictions$Restriction <- tx_restrictions$Mandatory
tx_restrictions$Date_Notified  = ymd(tx_restrictions$Notified)
tx_rest = tx_restrictions
tx_rest = tx_rest[!duplicated(paste(tx_rest$PWS_ID,tx_rest$Date_Notified,tx_rest$STAGE)),]
tx_rest = tx_rest[tx_rest$Restriction==1,]
tx_rest = tx_rest[tx_rest$Date_Notified>=mdy('1/1/2010'),]
tx_rest = tx_rest %>% arrange(PWS_ID,Date_Notified) %>% filter(!duplicated(PWS_ID))
tx_rest = tx_rest[!is.na(tx_rest$PWS_ID),]
tx_rest = tx_rest[tx_rest$Date_Notified < end_date,]
tx_rest$Restriction_Time = decimal_date(tx_rest$Date_Notified) - start_date_dec
id_crosswalk <- readRDS('input/id_crosswalk.RDS')
tx_rest$District_ID <- id_crosswalk$District_ID[match(tx_rest$PWS_ID,id_crosswalk$PWS_ID)]


tx_info <- merge(tx_info,tx_rest,all = T,by='PWS_ID')
tx_info$Restriction_Time[is.na(tx_info$Restriction_Time)] <- decimal_date(end_date) - decimal_date(start_date)
tx_info$Restriction[is.na(tx_info$Restriction)] <- 0
tx_info$Total_Storage_MG[is.na(tx_info$Total_Storage_MG)] <- 0



table(is.na(tx_info$PopulationType_ServiceConnections_Residential))

coxph(Surv(Restriction_Time,Restriction) ~ Owner_Type + Interconnections + log(Total_Storage_MG+0.0001),data = tx_info)

table(tx_info$Owner_Type)
table(is.na(tx_info$Total_Storage_MG))

summary(tx_info$Total_Storage_MG)


tx_rest[,list(.N,min(Notified),sd(Notified)),by=.(District_ID)][order(-N),][N>1,]





library(forcats)
library(tidycensus)

tx_systems =fread('bosquebox/input/SDWA_latest_downloads/SDWA_GEOGRAPHIC_AREAS.csv',stringsAsFactors = F,na.strings = c('','NA'))
tx_systems <- tx_systems[grepl('TX',PWSID),]
setnames(tx_systems,c('PWSID'),c('PWS_ID'))
tx_dww_info = fread('bosquebox/input/texas_dww/texas_master_pws.csv',stringsAsFactors = F,header=T)
tx_dww_info = tx_dww_info[,.(PWS_ID,Primary_Source_Type)]
tx_dww_info$Purchaser = grepl('P',tx_dww_info$Primary_Source_Type) + 0
tx_dww_info$Groundwater = grepl('G',tx_dww_info$Primary_Source_Type) + 0
tx_dww_info[,Primary_Source_Type:=NULL]
tx_systems <- merge(tx_systems,tx_dww_info,on = 'PWS_ID')

tx_systems <- merge(tx_systems,tx_info,all = T)

tx_systems$Residential_Service_Connections = as.numeric(tx_systems$PopulationType_ServiceConnections_Residential)
tx_systems$Residential_Service_Connections[is.na(tx_systems$Residential_Service_Connections)] <- 0
#tx_systems$Ln_Residential_Service_Connections = log(tx_systems$Residential_Service_Connections)
tx_systems$Wholesale_Service_Connections = ifelse(!is.na(as.numeric(tx_systems$PopulationType_ServiceConnections_Wholesale)),as.numeric(tx_systems$PopulationType_ServiceConnections_Wholesale),0)
tx_systems$Wholesale_Service_Connections[is.na(tx_systems$Wholesale_Service_Connections)] <- 0
#tx_systems$Ln_Wholesale_Service_Connections = log(tx_systems$Wholesale_Service_Connections+1)
tx_systems$Nonresidential_Service_Connections = ifelse(!is.na(as.numeric(tx_systems$PopulationType_ServiceConnections_NonResidential)),as.numeric(tx_systems$PopulationType_ServiceConnections_NonResidential),0)
tx_systems$Nonresidential_Service_Connections[is.na(tx_systems$Nonresidential_Service_Connections)] <- 0
tx_systems$Total_Service_Connections = tx_systems$Wholesale_Service_Connections + tx_systems$Nonresidential_Service_Connections + tx_systems$Residential_Service_Connections
consump_units = str_extract(tx_systems$AverageDailyConsump.,'[A-Z]{1,}')
tx_systems$Avg_Consumption_Per_Connection_G = (tx_systems$Average_Daily_Consump_MGD/tx_systems$Total_Service_Connections) * 1e6
tx_systems$Owner_Type = as.character(tx_systems$Owner_Type)
tx_systems = tx_systems[!tx_systems$Owner_Type %in% c('Federal Government','State Government'),]
tx_systems$Owner_Type[tx_systems$PWS_ID%in%c('TX1700784','TX1013113','TX1700589','TX1050019','TX1260016','TX1050038','TX2090005','TX0390019','TX0430037','TX0740031')] <- 'District'

interconnects = fread('bosquebox/input/texas_dww/sales_connections_2020-11-30.csv')
interconnects = interconnects[!duplicated(interconnects),]
interconnects = interconnects[!duplicated(paste(Seller,Buyer)),]
interconnects = interconnects[!paste(Seller,Buyer) %in% paste(Buyer,Seller),]
emergency_interconnects = interconnects[Sale_Type=='E',]
emergency_interconnects = melt(emergency_interconnects,measure.vars = c('Seller','Buyer'))
emergency_interconnects_count <- emergency_interconnects[,.N,by=.(value)]
setnames(emergency_interconnects_count,c('value','N'),c('PWS_ID','Num_Emergency_Interconnections'))
wholesalers = interconnects[Sale_Type%in%c('S','P','O','I'),][!duplicated(Seller),][,(Seller)]
tx_systems$Wholesale <- (tx_systems$PWS_ID %in% wholesalers) + 0
tx_systems = merge(tx_systems,emergency_interconnects_count,on = 'PWS_ID',all.x=T,all.y=F)
tx_systems$Num_Interconnections[is.na(tx_systems$Num_Interconnections)]<-0

#tx_rest = tx_rest %>% group_by(PWS_ID) %>% filter(Time == min(Time))
#tx_systems$Event = NA
#tx_systems$Time = NA
tx_systems$Time = tx_rest$Time[match(tx_systems$PWS_ID,tx_rest$PWS_ID)]
tx_systems$Event = tx_rest$Event[match(tx_systems$PWS_ID,tx_rest$PWS_ID)]
tx_systems$Event[is.na(tx_systems$Event)] <- 0
tx_systems$Time[is.na(tx_systems$Time)] <- decimal_date(end_date) - start_date_dec

tx_systems = tx_systems[is.na(tx_systems$LAST_REPORTED_DATE) | mdy(tx_systems$LAST_REPORTED_DATE)>end_date,]
tx_systems = tx_systems[tx_systems$Owner_Type!='County',]
tx_systems$Owner_Type = as.character(tx_systems$Owner_Type)
tx_systems$Total_Storage_MG[tx_systems$PWS_ID%in%c('TX1110098','TX1300010')] <- NA
#tx_systems = tx_systems[tx_systems$`PWS Type Code`=='CWS',]
tx_systems$start_date <- start_date

#dinfo_dt = read.csv('bosquebox/input/twdd_records/district_list_2019-03-08.csv',stringsAsFactors = F)
id_crosswalk <- readRDS('input/id_crosswalk.RDS')
tx_systems$District_ID <- id_crosswalk$District_ID[match(tx_systems$PWS_ID,id_crosswalk$PWS_ID)]
#### these are hand-coded, fixing errors in the database on the TWDD side
#tx_systems$District_ID[is.na(tx_systems$District_ID)] <- not_district

tx_systems <- tx_systems[!is.na(District_ID),]


tx_rest


tx_systems[,list(max(Event),min(Time),sum(Connections)),by=.(District_ID)]

tx_systems[,mean(Event),by=.(District_ID)][V1!=0&V1!=1,]


tx_rest[PWS_ID %in% tx_systems[District_ID=='7632500',]$PWS_ID,]


audits <- readRDS('input/district_audits.RDS')
audits <- audits[audits$District_ID %in% tx_systems$District_ID & audits$`FISCAL YEAR ENDED` < start_date,]
audits$join_time <- audits$`FISCAL YEAR ENDED`
tx_systems$join_time <- tx_systems$start_date
setkey(audits,District_ID,join_time)
setkey(tx_systems,District_ID,join_time)




audits[tx_systems,]






tx_systems[,sum(Total_Service_Connections,na.rm = T),by=.(District_ID)]









debt <- readRDS('input/district_debt_issuances.RDS')
debt <- debt[FiscalYear==2009,]





head(debt)
table(debt$FiscalYear)


coxph(cluster = tx_sys)





tx_systems$Time



wu <- data.table(readxl::read_excel('input/Intakes-Sales-Cxns_ForPWS.xlsx'))
wu <- wu[!is.na(wu$`TCEQ PWS Code`),]
wu$PWS_ID <- paste0('TX',formatC(wu$`TCEQ PWS Code`,width = 7,flag = '0',mode = 'integer'))

wu[PWS_ID=='TX0250019'&Year==2014,]

tx_systems[tx_systems$PWS_ID=='TX0250019']

wu[,.N,by=.(PWS_ID,Year)]
wu[wu$`TCEQ PWS Code`=='0030007',]
formatC(wu$`TCEQ PWS Code`)
head(wu)
tx_systems[tx_systems$PWS_ID=='TX0030007']
#tx_systems = tx_systems[tx_systems$PWS_ID %in% twd_boundaries$PWS_ID,]





tx_systems[,CWS_Count:=.N,by=.(District_ID)]

tx_systems$District_Type <- dinfo_dt$Type[match(tx_systems$District_ID,dinfo_dt$District_ID)]
tx_systems$District_Type[is.na(tx_systems$District_Type)] <- 'Not a district'
tx_systems$District_Type[tx_systems$District_Type %in% c('WATER CONTROL AND IMPROVEMENT DISTR')] <- 'WCID'
tx_systems$District_Type[tx_systems$District_Type %in% c('MUNICIPAL UTILITY DISTRICT')] <- 'MUD'
tx_systems$District_Type[tx_systems$District_Type %in% c('FRESH WATER SUPPLY DISTRICT')] <- 'FWSD'
tx_systems$District_Type[tx_systems$District_Type %in% c('SPECIAL UTILITY DISTRICT')] <- 'SUD'
tx_systems$District_Type[!tx_systems$District_Type %in% c('SUD','FWSD','MUD','WCID','Not a district')] <- 'Other'
tx_systems$CWS_Count[tx_systems$District_Type == 'Not a district'] <- 1

#tx_systems$HAVE_SHAPEFILE = (tx_systems$PWS_ID %in% twd_boundaries$PWS_ID) + 0

#tx_systems$Geog_Area = as.numeric(st_area(twd_boundaries)[match(tx_systems $PWS_ID,twd_boundaries$PWS_ID)])

stormod = lm(Total_Storage_MG~Total_Service_Connections,data= tx_systems)
tx_systems$Predicted_Total_Storage_MG <- as.numeric(unlist(predict.lm(stormod,newdata = data.frame(Total_Service_Connections = tx_systems$Total_Service_Connections))))
tx_systems$Total_Storage_MG_Impute = ifelse(is.na(tx_systems$Total_Storage_MG),tx_systems$Predicted_Total_Storage_MG,tx_systems$Total_Storage_MG)
# 
tx_systems$
  tx_systems$Total_Storage_MG
# uac = tigris::urban_areas(class='sf')
# uac <- st_transform(uac,albersNA)
# uac <- st_make_valid(uac)
# states = tigris::states(class='sf')
# texas = states[states$STUSPS=='TX',]
# texas <- st_transform(texas,albersNA)
# texas <- st_make_valid(texas)
# uac_tx_inter = st_intersects(uac,texas,sparse = F)
# uac_tx = uac[uac_tx_inter,]
# #uac_all_tx = st_union(uac_tx)
# wd_uac_inters = st_intersects(twd_boundaries,uac_tx)
# urban_overlap = st_intersection(twd_boundaries,uac_tx)
# urban_overlap$Area = st_area(urban_overlap)
# urban_area=urban_overlap %>% group_by(PWS_ID) %>% summarise(Urban_Area = sum(Area))
# saveRDS(urban_area,'scratch/urban_area_proportions.RDS')
urban_area = readRDS('scratch/urban_area_proportions.RDS')
#urban_area = readRDS('scratch/urban_area_proportions.RDS')
tx_systems$Urban_Area = as.numeric(urban_area$Urban_Area[match(tx_systems$PWS_ID,urban_area$PWS_ID)])
tx_systems$Urban_Area_Prop = ifelse(is.na(tx_systems$Urban_Area),0,tx_systems$Urban_Area)/tx_systems$Geog_Area
library(tidycensus)
#k = "b5a388cd6162590fc939335ddc45787bcc61778c"
#tidycensus::census_api_key(k)
#codes = list(Total_Population = "B01003_001",Median_Income='B06011_001',Median_Home_Value='B25077_001',White_Population='B02008_001',Bachelors_Population = 'B07009_005',Median_Year_Structure_Built = 'B25035_001',Total_Homes='B25002_001',Owner_Occupied_Homes = 'B25002_002')
#acs_counties = as.character(tx_county$CNTY_NM[sapply(any_twd,length)>0])
# acs_pull = lapply(2010:2016,function(x) {print(x);tidycensus::get_acs('tract',variables =unlist(codes),
#               state = 'TX',year = x,geometry = F) %>% mutate(Year = x)})
# acs_pull_dt = rbindlist(acs_pull)
# saveRDS(acs_pull_dt,'scratch/raw_acs_tract_data.RDS')
temp_congress = congressional_districts(resolution = "500k", year = 2012,class='sf')
temp_congress <- st_transform(temp_congress,albersNA)
temp_congress <- st_make_valid(temp_congress)
temp_congress=temp_congress[temp_congress$STATEFP=='48',]
temp_congress$Congressional_District = paste0(temp_congress$STATEFP,temp_congress$CD112FP)
twd_congress = st_intersection(twd_boundaries,temp_congress)

congress_overlap_dt  = data.table(PWS_ID = twd_congress$PWS_ID,Congressional_District = twd_congress$Congressional_District,
                                  Prop_Over_District = as.numeric(st_area(twd_congress)/st_area(twd_boundaries)[match(twd_congress$PWS_ID,twd_boundaries$PWS_ID)]))
congress_overlap_dt$Prop_Over_District <- round(congress_overlap_dt$Prop_Over_District,2)
congress_overlap_dt = congress_overlap_dt[Prop_Over_District>0,]

#saveRDS(congress_overlap_dt, 'scratch/congress_overlap_file.RDS')

plot_adopts = tx_systems[tx_systems$Event==1,] 
plot_adopts$Sample = (!is.na(plot_adopts$District_Type) & plot_adopts$District_Type %in% c('MUD')) + 0
figure2 = ggplot(data = plot_adopts) + geom_freqpoly(aes(x = Time+start_year,colour = as.factor(Sample)),binwidth = 1/12) + 
  scale_x_continuous(name = 'Month of restriction adoption',expand=c(0,0)) + 
  scale_y_continuous(name = '# of restrictions adopted',expand = c(0,0)) + 
  ggtitle('First observed mandatory water use restriction adoption') + 
  scale_color_tableau(name = 'Water systems',labels = c('Not in sample','In sample')) + 
  theme_bw() + theme(legend.position = c(0.8,0.5),axis.text = element_text(size = 12),
                     axis.title = element_text(size = 12),legend.title = element_text(size = 12),
                     legend.text = element_text(size =12),text = element_text(family = 'Times'))
ggsave(figure2,filename = 'output/figure2.png',dpi = 500,width=6,height=4,units='in')

tdf = tx_systems
dinfo$District_ID <- as.character(dinfo$District_ID)
tdf = left_join(tdf,dinfo %>% dplyr::select(-PWS_ID))
tdf = data.table(tdf)
tdf = tdf[{is.na(Created)&ymd(`First Reported Date`)<start_date} | mdy(Created)<start_date,]

### make a map of texas
# 
# twd_boundaries$In_Analysis = (twd_boundaries$PWS_ID %in% tdf$PWS_ID) + 0
# #tdf = tdf[!is.na(tdf$District_ID) & !is.na(tdf$PWS_ID),]
# #tdf = tdf[!is.na(tdf$Total_Storage_MG) & !is.na(tdf$Total_Product_MGD),]
# twd_boundaries$In_Analysis = (twd_boundaries$PWS_ID %in% tdf$PWS_ID) + 0
# twd_boundaries$Is_SampleDistrict = (twd_boundaries$PWS_ID %in% tdf$PWS_ID[!is.na(tdf$District_Type)&tdf$District_Type!='Other'])+0
# twd_boundaries$CWS = (twd_boundaries$PWS_ID %in% tdf$PWS_ID[tdf$`PWS Type`=='Community water system']) + 0
# HCS = c('HARRIS','BRAZORIA','GALVESTON','FORT BEND','MONTGOMERY','CHAMBERS','WALLER','LIBERTY','AUSTIN')
# twd_boundaries$In_Houston = (twd_boundaries$PWS_ID %in% tx_systems$PWS_ID[tx_systems$`County Served` %in% stringr::str_to_title(HCS)]) + 0
# twd_boundaries$Adopt = (twd_boundaries$PWS_ID %in% tx_systems$PWS_ID[tx_systems$Event ==1]) + 0
# tx = st_union(tx_county)
# houston_msa =tx_county[tx_county$CNTY_NM %in% str_to_title(HCS),]

# 
# figure2 = ggplot() + 
# geom_sf(data = tx,fill = 'grey80',lwd = 0.2) + 
# geom_sf(data = twd_boundaries[twd_boundaries$CWS==1,],
# aes(fill = as.factor(Is_SampleDistrict)),col = 'grey50',lwd=0.025) + #,colour = as.factor(Is_District))) + 
# scale_fill_colorblind(labels = c('Other ownership','FWSD/MUD/SUD/WCID')) +  
# #scale_colour_colorblind(labels = c('Other ownership','Special District')) + 
# theme_tufte(ticks=F) + 
# theme(legend.text = element_text(size = 12),
# axis.text = element_blank(),text= element_text(family = 'Times'),
# title = element_text(size = 12),legend.position = c(0.2,0.1),
# legend.title = element_blank(),panel.grid = element_line(colour = 'white')) + 
# ggtitle(paste0('Active community water systems in Texas (',start_year,'-present)'))
# ggsave(figure2,filename = 'output/figure2.png',dpi = 500,width=6,height=6,units='in')

library(tigris)
tx_county_sp = tigris::counties(state = 48, class = 'sf')  # sf (modern tigris)
tx_county_sp = reproject_sf(tx_county_sp, albersNA)        # terra-backed; was spTransform()/CRS()
#Create adjacency matrix (poly2nb accepts sf directly)
tx.nb <- poly2nb(tx_county_sp,row.names = tx_county_sp$GEOID,queen=F)
#Convert the adjacency matrix into a file in the INLA format
nb2INLA("tx.adj", tx.nb)
#co_count = tdf %>% filter(IS_DISTRICT==1) %>% group_by(`County Served`) %>% summarise(county_count = n()) %>%
#  arrange(-county_count)
#library(forcats)
#co_count$`County Served` = fct_inorder(as.factor(co_count$`County Served`))
#cox_dt = cox_dt[cox_dt$Date<Sys.Date(),]
#cox_dt = cox_dt[cox_dt$Date<=max(phdi_tx$Date),]
cfips = tigris::fips_codes
cfips$CFIPS = paste0(cfips$state_code,cfips$county_code)
cfips$county = gsub(' County','',cfips$county)
cfips = cfips[cfips$state=='TX',]
tdf$Primary_CFIPS = cfips$CFIPS[match(toupper(tdf$`County Served`),toupper(cfips$county))]
tdf$CFIPS_ID= match(tdf$Primary_CFIPS,tx_county_sp@data$GEOID)


weeks = length(seq.Date(from = mdy('05/04/2010'),to = mdy('07/07/2015'),by = 'week'))
months = round(weeks/4)
months2 = months/2
#months = round(as.numeric({mdy('7/7/2015') - mdy(paste0('1/1/',start_year))} / 30))
tdf$intercept = 1
inla_df = as.data.frame(tdf)
library(INLA)
#table(tdf$Owner_Type,tdf$Event)
inla.setOption( num.threads = 8) 
inla_df <- inla_df[inla_df$HAVE_SHAPEFILE==1,]
#saveRDS(tdf,'scratch/ready_to_coxph.RDS')

cox_base = inla.coxph(inla.surv(time = Time,event = Event) ~ -1 + intercept + 
                        f(PWS_ID,model="iid",hyper = list(prec = list(param=c(1, 1)))) + 
                        f(Primary_CFIPS,model="iid",hyper = list(prec = list(param=c(1, 1)))) +
                        f(CFIPS_ID,model="besag",graph="tx.adj",hyper = list(prec = list(param=c(1, 1))))  ,
                      data = inla_df,
                      control.hazard=list(model="rw1", n.intervals=months2,scale.model = TRUE))

saveRDS(cox_base,'scratch/base_inlacoxph_object.RDS')

cox_dt = cox_base$data %>%  group_by(PWS_ID) %>% arrange(PWS_ID,baseline.hazard.idx) 
cox_dt$Date = date_decimal(start_date_dec + cox_dt$baseline.hazard.time)
cox_dt = data.table(cox_dt,stringsAsFactors = F)

cox_dt$sub_index = 1:nrow(cox_dt)
cox_dt$PWS_ID <- as.character(cox_dt$PWS_ID)

tx_connections = fread('bosquebox/input/twdb_historicalestimates/20170630_Connections.csv')
tx_connections = melt(tx_connections,id = 1:5,measure.vars = as.character(2010:2015))
tx_connections$value <- gsub('\\,','',tx_connections$value)
tx_connections$value <- as.numeric(tx_connections$value)
setnames(tx_connections,c('variable','value','PWS ID'),c('Year','Connections','PWS_ID'))
tx_connections = tx_connections[,c('PWS_ID','Year','Connections')]
tx_population = fread('bosquebox/input/twdb_historicalestimates/20170630_PopServed.csv')
tx_population = melt(tx_population,id = 1:5,measure.vars = as.character(2010:2015))
tx_population$value <- gsub('\\,','',tx_population$value)
tx_population$value <- as.numeric(tx_population$value)
setnames(tx_population,c('variable','value','PWS ID'),c('Year','PopServed','PWS_ID'))
tx_population = tx_population[,c('PWS_ID','Year','PopServed')]
setkey(tx_population,'PWS_ID','Year')
setkey(tx_connections,'PWS_ID','Year')
tx_usage = fread('bosquebox/input/twdb_historicalestimates/20170630_UseEstimate.csv')
tx_usage = melt(tx_usage,id = 1:5,measure.vars = as.character(2010:2015))
tx_usage$value <- gsub('\\,','',tx_usage$value)
tx_usage$value <- as.numeric(tx_usage$value)

setnames(tx_usage,c('variable','value','PWS ID'),c('Year','Usage','PWS_ID'))
tx_usage = tx_usage[,c('PWS_ID','Year','Usage')]
setkey(tx_usage,'PWS_ID','Year')
tx_population = tx_population[tx_connections,][tx_usage,]
tx_population$Year <- as.numeric(as.character(tx_population$Year))
tx_population[is.na(PopServed)|is.na(Connections)|is.na(Usage),PopServed:=NA]
tx_population[is.na(PopServed)|is.na(Connections)|is.na(Usage),Connections:=NA]
tx_population[is.na(PopServed)|is.na(Connections)|is.na(Usage),Usage:=NA]
tx_population$Year = as.numeric(as.numeric(as.character(tx_population$Year)))
tx_population$Usage_Per_Connection = tx_population$Usage/tx_population$Connections
tx_population[order(PWS_ID,Year),Usage_Per_Connection_P1:=lag(Usage_Per_Connection,1),by=.(PWS_ID)]
library(zoo)
setkey(tx_population,'PWS_ID','Year')
cox_dt$Year <- year(cox_dt$Date)
setkey(cox_dt,'PWS_ID','Year')
cox_dt = data.table(left_join(cox_dt,tx_population))
fvals = c('PopServed','Connections','Usage')
cox_dt[order(PWS_ID,Year),(fvals):=lapply(.SD,zoo::na.locf,na.rm=F),by=.(PWS_ID),.SDcols = fvals]
cox_dt[order(PWS_ID,Year),(fvals):=lapply(.SD,zoo::na.locf,na.rm=F,fromLast=T),by=.(PWS_ID),.SDcols = fvals]

cox_dt[,join_time:=decimal_date(Date)]
# 
# phdi = fread('bosquebox/input/drought_index_data/climdiv-phdidv-v1.0.0-20180205.csv',skip = 2)
# phdi = phdi[phdi$State=='Texas',]
# phdi$FIPS_CD= paste0('48',phdi$Region)
# phdi = phdi[phdi$Year>2000,]
# phdi = phdi %>% rename(PHDI = IndexValue)
# phdi$Date <- as.Date(phdi$Date)
# setkey(climatediv_overs,'FIPS_CD')
# setkey(phdi,'FIPS_CD')
# 
# phdi <- phdi[order(FIPS_CD,Date),]
# phdi$PHDI <-as.numeric(phdi$PHDI)
# phdi[, PHDI_3Month_Average := round(rollapplyr(PHDI, 3, mean,fill = NA),3), by = FIPS_CD]
# phdi[, PHDI_6Month_Average := round(rollapplyr(PHDI, 6, mean,fill = NA),3), by = FIPS_CD]
# phdi[, PHDI_12Month_Average := round(rollapplyr(PHDI, 12, mean,fill = NA),3), by = FIPS_CD]
# phdi_overmerge = phdi[climatediv_overs,allow.cartesian=T]
# phdi_vars = c('PHDI','PHDI_3Month_Average','PHDI_6Month_Average','PHDI_12Month_Average')
# phdi_pws = phdi_overmerge[,lapply(.SD,weighted.mean,w = Climate_Div_Weight),by=.(Date,Year,Month,PWS_ID),.SDcols = phdi_vars]
# phdi_pws[,join_time:=decimal_date(Date)]

# phdi_pws[,Date:=NULL]
# phdi_pws = phdi_pws[,c('PWS_ID',phdi_vars,'join_time'),with=F]
# setkey(phdi_pws,PWS_ID,join_time)
# setkey(cox_dt,PWS_ID,join_time)
# cox_dt = phdi_pws[cox_dt,roll = T]
# 
# 
# pdsi = fread('bosquebox/input/drought_index_data/climdiv-pdsidv-v1.0.0-20180205.csv',skip = 2)
# pdsi = pdsi[pdsi$State=='Texas',]
# pdsi$FIPS_CD= paste0('48',pdsi$Region)
# pdsi = pdsi[pdsi$Year>2000,]
# pdsi = pdsi %>% rename(PDSI = IndexValue)
# pdsi$Date <- as.Date(pdsi$Date)
# setkey(climatediv_overs,'FIPS_CD')
# setkey(pdsi,'FIPS_CD')
# 
# pdsi <- pdsi[order(FIPS_CD,Date),]
# pdsi$PDSI <-as.numeric(pdsi$PDSI)
# pdsi[, PDSI_3Month_Average := round(rollapplyr(PDSI, 3, mean,fill = NA),3), by = FIPS_CD]
# pdsi[, PDSI_6Month_Average := round(rollapplyr(PDSI, 6, mean,fill = NA),3), by = FIPS_CD]
# pdsi[, PDSI_12Month_Average := round(rollapplyr(PDSI, 12, mean,fill = NA),3), by = FIPS_CD]
# pdsi_overmerge = pdsi[climatediv_overs,allow.cartesian=T]
# pdsi_vars = c('PDSI','PDSI_3Month_Average','PDSI_6Month_Average','PDSI_12Month_Average')
# pdsi_pws = pdsi_overmerge[,lapply(.SD,weighted.mean,w = Climate_Div_Weight),by=.(Date,Year,Month,PWS_ID),.SDcols = pdsi_vars]
# pdsi_pws[,join_time:=decimal_date(Date)]
# pdsi_pws[,Date:=NULL]
# setkey(pdsi_pws,PWS_ID,join_time)
# setkey(cox_dt,PWS_ID,join_time)
# pdsi_pws = pdsi_pws[,c('PWS_ID',pdsi_vars,'join_time'),with=F]
# cox_dt = pdsi_pws[cox_dt,roll = T]

flist = list.files('bosquebox/input/drought_index_data/','dm_export',full.names = T)
dm_list = lapply(flist,function(x){ 
  temp = fread(x,stringsAsFactors = F,integer64 = 'numeric')
  temp$DSCI = as.numeric(temp$D0) + as.numeric(temp$D1) * 2 + as.numeric(temp$D2) * 3 + as.numeric(temp$D3) * 4 + as.numeric(temp$D4) * 5
  temp %>% dplyr::select(FIPS,ValidStart,ValidEnd,DSCI,None,D0,D1,D2,D3,D4)
})
dm_df = rbindlist(dm_list)
dm_df = dm_df[!duplicated(dm_df),]
dm_temp = dm_df
dm_df = dm_df[dm_df$ValidStart>='2009-01-01',]
setnames(dm_df,'FIPS','CFIPS')
dm_df$CFIPS <- as.character(dm_df$CFIPS)
dm_dt <- dm_df
setnames(dm_dt,'ValidStart','Date')
dm_dt$Date=as.Date(dm_dt$Date)

dm_dt <- dm_dt[order(CFIPS,Date),]
dm_dt[, DSCI_3Month_Average := round(rollapplyr(DSCI, 3, mean,fill = NA),3), by = CFIPS]
dm_dt[, DSCI_6Month_Average := round(rollapplyr(DSCI, 6, mean,fill = NA),3), by = CFIPS]
dm_dt[, DSCI_12Month_Average := round(rollapplyr(DSCI, 12, mean,fill = NA),3), by = CFIPS]
dm_dt[,join_time:=as.Date(Date)]
setkey(dm_dt,CFIPS)
setkey(county_overs,CFIPS)
dsci_overmerge = dm_dt[county_overs,allow.cartesian=TRUE]

dsci_vars = c('DSCI','DSCI_3Month_Average','DSCI_6Month_Average','DSCI_12Month_Average')
dsci_pws = dsci_overmerge[,lapply(.SD,weighted.mean,w = Prop_Over_County),by=.(Date,PWS_ID),.SDcols = dsci_vars]

dsci_pws[,join_time:=decimal_date(Date)]
dsci_pws[,Date:=NULL]
setkey(dsci_pws,PWS_ID,join_time)
setkey(cox_dt,PWS_ID,join_time)
#dsci_pws  = dsci_pws[dsci_pws$PWS_ID %in% cox_dt$PWS_ID,]
dsci_pws = dsci_pws[,c('PWS_ID',dsci_vars,'join_time'),with=F]
cox_dt = dsci_pws[cox_dt,roll = T]
dates = unique(cox_dt$Date)
dates <- ymd(as.Date(dates))
dates = dates[dates < Sys.Date()]
dates = c(seq.Date(from = ymd('2009-01-01'),to = min(dates),'month'),dates)
qpref = 'https://twc.tamu.edu/weather_images/summ/summ'
qsuf = ifelse(year(dates)< 2016 | year(dates)==2016&month(dates)<10 ,'.txt','.csv')
library(rvest)


# NOTE: canonical KBDI assembly now lives in code/02_prep/01_assemble_kbdi.R;
# prefer readRDS(scratch("kbdi_county.RDS")) over re-scraping here. The two
# source formats differ (.txt fixed-width COUNTY MEAN MAX MIN; .csv comma-
# delimited County,Min,Max,Average,Change), so parse each by meaning. The old
# positional mapping silently stored the .csv Min column as KBDI_Avg and broke
# on multi-word counties in the .txt files.
read_kbdi_txt = function(url){
  ln = tryCatch(readr::read_lines(url),error = function(e) NULL); if(is.null(ln)) return(NULL)
  m = stringr::str_match(ln,'^\\s*(.+?)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s*$')
  m = m[!is.na(m[,1]),,drop = FALSE]; if(nrow(m)==0) return(NULL)
  tibble::tibble(County = stringr::str_squish(m[,2]), KBDI_Avg = as.numeric(m[,3]),
                 KBDI_Max = as.numeric(m[,4]), KBDI_Min = as.numeric(m[,5]))
}
read_kbdi_csv = function(url){
  raw = tryCatch(readr::read_csv(url,show_col_types = FALSE,name_repair = 'unique_quiet'),error = function(e) NULL)
  if(is.null(raw) || nrow(raw)==0) return(NULL); names(raw) = tolower(names(raw))
  if(!all(c('county','average','max','min') %in% names(raw))) return(NULL)
  tibble::tibble(County = raw$county, KBDI_Avg = as.numeric(raw$average),
                 KBDI_Max = as.numeric(raw$max), KBDI_Min = as.numeric(raw$min))
}
kbdi_list = pblapply(seq_along(dates),function(x) {
  day = dates[x]
  tab = NULL
  is_txt = grepl('txt',qsuf[x])
  guard = 0
  while(is.null(tab) && guard < 14){
    url = paste0(qpref,gsub('-','',day),qsuf[x])
    tab = if(is_txt) read_kbdi_txt(url) else read_kbdi_csv(url)
    if(is.null(tab)){day = day - days(1); guard = guard + 1}}
  if(is.null(tab)) return(NULL)
  tab %>% mutate(Query_Date = dates[x],KBDI_Date = day)},cl = 1)
kbdi_df = do.call(rbind,kbdi_list)
kbdi_df$County = toupper(kbdi_df$County)
kbdi_df = kbdi_df[kbdi_df$County!='\032',]
kbdi_df$Date = as.Date(kbdi_df$KBDI_Date)
kbdi_df=as.data.table(kbdi_df)
library(zoo)

kbdi_df$CFIPS = as.character(tx_county$GEOID[match(gsub(' ','',toupper(kbdi_df$County)),gsub(' ','',toupper(tx_county$NAME)))])
kbdi_df[order(CFIPS,Date), KBDI_3Month_Average := rollapplyr(KBDI_Avg, 3, mean,fill = NA), by = CFIPS]
kbdi_df[order(CFIPS,Date), KBDI_6Month_Average := rollapplyr(KBDI_Avg, 6, mean,fill = NA), by = CFIPS]
kbdi_df[order(CFIPS,Date), KBDI_12Month_Average := rollapplyr(KBDI_Avg, 12, mean,fill = NA), by = CFIPS]
# NOTE: KBDI assembly is now maintained as a standalone Stage-C step in
# code/02_prep/01_assemble_kbdi.R. Prefer sourcing that and reading the
# result with readRDS(scratch("kbdi_county.RDS")). Bug fixed here: the old line
#   fwrite(kbdi_df,'scratch/kbdi_dt.RDS')
# wrote CSV content under an .RDS name. Use saveRDS + a proper local-scratch path.
if (!dir.exists('scratch')) dir.create('scratch', recursive = TRUE, showWarnings = FALSE)
saveRDS(kbdi_df,'scratch/kbdi_county.RDS')
county_overs$County_Name = gsub(' County$','',fips_codes$county[match(county_overs$CFIPS,paste0(fips_codes$state_code,fips_codes$county_code))])
kbdi_df[,County:=NULL]
setkey(kbdi_df,'CFIPS')
setkey(county_overs,'CFIPS')
kbdi_overmerge = kbdi_df[county_overs,allow.cartesian=TRUE]

kbdi_vars = c('KBDI_Avg','KBDI_3Month_Average','KBDI_6Month_Average','KBDI_12Month_Average')
kbdi_pws = kbdi_overmerge[,lapply(.SD,weighted.mean,w = Prop_Over_County),by=.(Date,PWS_ID),.SDcols = kbdi_vars]
kbdi_pws[,join_time:=decimal_date(Date)]
kbdi_pws[,Date:=NULL]
setkey(kbdi_pws,PWS_ID,join_time)
setkey(cox_dt,PWS_ID,join_time)
#kbdi_pws  = kbdi_pws[kbdi_pws$PWS_ID %in% cox_dt$PWS_ID,]
kbdi_pws = kbdi_pws[,c('PWS_ID',kbdi_vars,'join_time'),with=F]
cox_dt = kbdi_pws[cox_dt,roll = T]


library(INLA)
library(spdep)
library(survival)
library(data.table)
grow_pop_mod = lm(log(PopServed)~Year,data = tx_population[tx_population$PWS_ID%in%cox_dt$PWS_ID[cox_dt$IS_DISTRICT==1],])
grow_connections_mod = lm(log(Connections)~Year,data = tx_population[tx_population$PWS_ID%in%cox_dt$PWS_ID[cox_dt$IS_DISTRICT==1],])
cox_dt$PopulationType_Population_Residential = as.numeric(cox_dt$PopulationType_Population_Residential)
cox_dt$Fill_Pop = round(cox_dt$PopulationType_Population_Residential / ({1+grow_pop_mod$coefficients[2]}^{2018 - cox_dt$Year}))
cox_dt$PopServed = ifelse(is.na(cox_dt$PopServed),cox_dt$Fill_Pop,cox_dt$PopServed)
cox_dt$Fill_Connections = round(cox_dt$Total_Service_Connections / ({1+grow_pop_mod$coefficients[2]}^{2018 - cox_dt$Year}))
cox_dt$Connections = ifelse(is.na(cox_dt$Connections),cox_dt$Fill_Connections,cox_dt$Connections)



acs_pull_dt = readRDS('scratch/raw_acs_tract_data.RDS')
#acs_pull_dt = readRDS('scratch/raw_acs_tract_data.RDS')
setnames(acs_pull_dt,'GEOID','GEOID10')
setkey(tract_overs,'GEOID10')
setkey(acs_pull_dt,'GEOID10','Year')
acs_merge = tract_overs[acs_pull_dt,allow.cartesian=TRUE]
central_vals = acs_merge[variable %in% c('Median_Year_Structure_Built','Median_Home_Value','Median_Income'),]
count_vals = acs_merge[!variable %in% c('Median_Year_Structure_Built','Median_Home_Value','Median_Income'),]
pws_sums = count_vals[,sum(Prop_Of_Tract * estimate),by = .(Year,PWS_ID,variable)]
setnames(pws_sums,'V1','estimate')
pws_sums$PWS_ID <- as.character(pws_sums$PWS_ID)
pws_sums <- pws_sums[PWS_ID %in% cox_dt$PWS_ID,]
pfill = cox_dt[,.(PWS_ID,PopServed,Year)]
pfill = pfill[!duplicated(pfill),]
setkey(pfill,PWS_ID,Year)
setkey(pws_sums,PWS_ID,Year)
pws_sums <- pws_sums[pfill,]
pws_sums <- pws_sums[!is.na(variable),]
pws_sums$Reported_District_Population = pws_sums$PopServed
pws_sums$pop_multiple <- NA
pws_sums$pop_multiple[pws_sums$variable=='Total_Population'] <- pws_sums$estimate[pws_sums$variable=='Total_Population']/pws_sums$Reported_District_Population[pws_sums$variable=='Total_Population']
pws_sums[order(PWS_ID),pop_multiple:=zoo::na.locf(pop_multiple),by=.(PWS_ID)]
pws_sums$Multiplied_Value = pws_sums$estimate*(1/pws_sums$pop_multiple)

pws_census_estimates = dcast(pws_sums,Year + PWS_ID ~ variable,value.var = 'Multiplied_Value')[!is.na(PWS_ID),]
pws_census_estimates$Prop_Bachelors_Degree = pws_census_estimates$Bachelors_Population/pws_census_estimates$Total_Population
pws_census_estimates$Prop_White_Population = pws_census_estimates$White_Population/pws_census_estimates$Total_Population
pws_census_estimates$Prop_Owner_Occupied = pws_census_estimates$Owner_Occupied_Homes/pws_census_estimates$Total_Homes
cval_avgs = central_vals[,weighted.mean(x = estimate,w = Prop_Of_Tract),by=.(Year,variable,PWS_ID)]
pws_census_avgs = dcast(cval_avgs,Year + PWS_ID ~ variable,value.var = 'V1')[!is.na(PWS_ID),]
pws_census_data = merge(pws_census_avgs,pws_census_estimates)
demo_df = pws_census_data
demo_df$PWS_ID <- as.character(demo_df$PWS_ID)
fill_vars = names(demo_df)[sapply(demo_df,is.numeric)]
fill_vars = fill_vars[fill_vars!='Year']
demo_df[order(PWS_ID,Year),(fill_vars):=lapply(.SD,zoo::na.locf,na.rm=F),by = .(PWS_ID),.SDcols = fill_vars]
cox_dt$Year = year(cox_dt$Date)
setkey(demo_df,PWS_ID,Year)
setkey(cox_dt,PWS_ID,Year)

cox_dt = merge(cox_dt,demo_df,on=c('PWS_ID','Year'),all.x = T)#demo_df[cox_dt,]


#congress_overlap_dt = readRDS('scratch/congress_overlap_file.RDS')
house_recs = data.table(readRDS('bosquebox/input/houseAtts_5-2019.RDS'))
house_recs = house_recs[house_recs$state=='Texas',]
setnames(house_recs,'year','Year')
congress_overlap_dt$PWS_ID = as.character(congress_overlap_dt$PWS_ID)
setkey(house_recs,Congressional_District_ID,Year)
setkey(congress_overlap_dt,Congressional_District)
house_overmerge = house_recs[congress_overlap_dt,allow.cartesian=TRUE]
polvars = c('democrat','LCV_lifetime','nominate_dim1')
pol_vals_dt = house_overmerge[,lapply(.SD,weighted.mean,w=Prop_Over_District),by=.(Year,PWS_ID),.SDcols = polvars]
setkey(pol_vals_dt,PWS_ID,Year)
setkey(cox_dt,PWS_ID,Year)
cox_dt = pol_vals_dt[cox_dt,]


#cox_dt$Wholesale = (cox_dt$Prop_Wholesale_Connections>0) + 0
cox_dt$Urban_District = (cox_dt$Urban_Area_Prop>0.5) + 0
cox_dt$intercept = 1

cox_dt$Perc_Nonwhite <- {1-cox_dt$Prop_White_Population} * 100
cox_dt$Perc_College_Grad <- cox_dt$Prop_Bachelors_Degree * 100
cox_dt$Storage_Per_Connection_MG = cox_dt$Total_Storage_MG_Impute/cox_dt$Connections
cox_dt$Storage_Per_Connection_G = cox_dt$Storage_Per_Connection_MG*1000000
cox_dt$District_Age = decimal_date(cox_dt$Date) - decimal_date(mdy(dinfo$Created[match(cox_dt$District_ID,dinfo$District_ID)]))
cox_dt$Median_Home_Value[is.na(cox_dt$Median_Home_Value)] <- median(cox_dt$Median_Home_Value,na.rm=T)
cox_dt$Num_Emergency_Interconnections[is.na(cox_dt$Num_Emergency_Interconnections)]<-0
cox_dt$Has_Emergency_Interconnect <- (cox_dt$Num_Emergency_Interconnections>0)+0

cox_dt$Class <- NA
cox_dt$Class[cox_dt$Connections<=500]<-'Very Small'
cox_dt$Class[cox_dt$Connections>500&cox_dt$Connections<=3300]<-'Small'
cox_dt$Class[cox_dt$Connections>3300&cox_dt$Connections<=10000]<-'Medium'
cox_dt$Class[cox_dt$Connections>10000]<-'Large'

cox_dt$District_Name = as.character(cox_dt$District_Name)
cox_dt$District_ID <- as.character(cox_dt$District_ID)


fiscal_dt = readRDS('scratch/fiscal_data.RDS')
fiscal_dt[,District_Name:=NULL]
fiscal_dt$`FISCAL YEAR ENDED` = ymd(fiscal_dt$`FISCAL YEAR ENDED`)
fiscal_dt$`FISCAL YEAR ENDED`[is.na(fiscal_dt$`FISCAL YEAR ENDED`)] <- mdy(paste0('8/31/',fiscal_dt$FiscalYear[is.na(fiscal_dt$`FISCAL YEAR ENDED`)])) 
fiscal_div = data.table(PWS_ID = cox_dt$PWS_ID,District_ID = cox_dt$District_ID,CWS_Service_Connections = cox_dt$Connections,FiscalYear = cox_dt$Year)
fiscal_div = fiscal_div[!duplicated(fiscal_div)]
fiscal_div = fiscal_div[District_ID!='0000000',]
district_service_connections = fiscal_div[,sum(CWS_Service_Connections),by = .(District_ID,FiscalYear)]
setnames(district_service_connections,'V1','District_Service_Connections')

fiscal_div = fiscal_div [district_service_connections,on = c('District_ID','FiscalYear')]
fiscal_div$CWS_Prop = fiscal_div$CWS_Service_Connections/fiscal_div$District_Service_Connections

setkey(fiscal_dt,'District_ID','FiscalYear')
setkey(fiscal_div,'District_ID','FiscalYear')

findt = merge(fiscal_div,fiscal_dt,all = T)

# aud_vars = unique(c('BONDS OUTSTANDING',"CURRENT ASSESSED VALUATION" ,"DEBT SERVICE TAX LEVIED" , "ENTERPRISE FUND - ASSETS" ,  "ENTERPRISE FUND - LIABILITIES" ,          
#                     "ENTERPRISE FUND - NET ASSETS", "ENTERPRISE FUND - OPERATING EXPENSES", "ENTERPRISE FUND - OPERATING REVENUES" ,   
#                     "GENERAL FUND - ASSETS" , "GENERAL FUND - FUND BALANCE",             
#                     "GENERAL FUND - LIABILITIES"   , "GENERAL FUND - TOTAL EXPENDITURES","GENERAL FUND - TOTAL REVENUES",
#                     grep('Issue_',colnames(fiscal_dt),value=T),grep('Outstanding',colnames(fiscal_dt),value=T),
#                     "GENERAL FUND - LIABILITIES"   , "GENERAL FUND - TOTAL EXPENDITURES","GENERAL FUND - TOTAL REVENUES" ))

aud_vars = names(findt)[grep('TAX|FUND|REV|EXP|ISSUE|DEBT|PRINC|INTER|BONDS',toupper(names(findt)))]
findt[,(aud_vars):=lapply(.SD,function(x) x * CWS_Prop),.SDcols = aud_vars]

cox_dt$join_time = ymd(base::as.Date(cox_dt$Date))
findt$join_time = ymd(findt$`FISCAL YEAR ENDED`)

setkey(cox_dt,District_ID,PWS_ID,join_time)
setkey(findt,District_ID,PWS_ID,join_time)
cox_dt = findt[cox_dt,roll = 730]
#cox_dt = cox_dt[cox_dt$District_ID!='0000000',]

cox_dt$Total_Principal_Outstanding[is.na(cox_dt$Total_Principal_Outstanding)&cox_dt$`BONDS OUTSTANDING`==0] <- 0
#cox_dt$TotalPrincipalOutstanding_REV[is.na(cox_dt$TotalPrincipalOutstanding_REV)&cox_dt$`BONDS OUTSTANDING`==0] <- 0
#cox_dt$TotalPrincipalOutstanding_GO[is.na(cox_dt$TotalPrincipalOutstanding_GO)&cox_dt$`BONDS OUTSTANDING`==0] <- 0
#cox_dt$TotalPrincipalOutstanding_GO[is.na(cox_dt$TotalPrincipalOutstanding_GO)&cox_dt$District_ID%in%debt_dt$District_ID] <- 0
#cox_dt$TotalPrincipalOutstanding_REV[is.na(cox_dt$TotalPrincipalOutstanding_REV)&cox_dt$District_ID%in%debt_dt$District_ID] <- 0
#cox_dt$Total_Principal_Outstanding[is.na(cox_dt$Total_Principal_Outstanding)&cox_dt$District_ID%in%debt_dt$District_ID] <- 0

cox_dt$District_Type <- as.character(cox_dt$District_Type)
cox_dt$District_Type[is.na(cox_dt$District_Type)] <- 'Not a district'
#cox_dt$TotalPrincipalOutstanding_REV[is.na(cox_dt$TotalPrincipalOutstanding_REV)&cox_dt$District_Type=='SUD'] <- cox_dt$`BONDS OUTSTANDING`[is.na(cox_dt$TotalPrincipalOutstanding_REV)&cox_dt$District_Type=='SUD']
#cox_dt$TotalPrincipalOutstanding_GO[is.na(cox_dt$TotalPrincipalOutstanding_REV)&cox_dt$District_Type=='SUD'] <- 0
cox_dt$Total_Principal_Outstanding[is.na(cox_dt$Total_Principal_Outstanding)&cox_dt$District_Type=='SUD'] <- cox_dt$TotalPrincipalOutstanding_REV[is.na(cox_dt$Total_Principal_Outstanding)&cox_dt$District_Type=='SUD']
#cox_dt$TotalPrincipalOutstanding_REV[is.na(cox_dt$TotalPrincipalOutstanding_REV)&!is.na(cox_dt$TotalPrincipalOutstanding_GO)] <- 0
#cox_dt$TotalPrincipalOutstanding_GO[!is.na(cox_dt$TotalPrincipalOutstanding_REV)&is.na(cox_dt$TotalPrincipalOutstanding_GO)] <- 0
#cox_dt$TotalPrincipalOutstanding_REV[cox_dt$District_ID=='3398000'] <- cox_dt$`BONDS OUTSTANDING`[cox_dt$District_ID=='3398000']
cox_dt$Total_Principal_Outstanding[cox_dt$District_ID=='3398000'] <- cox_dt$TotalPrincipalOutstanding_REV[cox_dt$District_ID=='3398000']
#cox_dt$TotalPrincipalOutstanding_GO[cox_dt$District_ID=='3398000'] <- 0
cox_dt$`BONDS OUTSTANDING`[cox_dt$District_ID=='4880000'] <- 0
#cox_dt$TotalPrincipalOutstanding_GO[cox_dt$District_ID=='4880000'] <- 0
#cox_dt$TotalPrincipalOutstanding_REV[cox_dt$District_ID=='4880000'] <- 0
cox_dt$Total_Principal_Outstanding[cox_dt$District_ID=='4880000'] <- 0
cox_dt$`BONDS OUTSTANDING`[cox_dt$District_ID=='7103000'] <- 0
#cox_dt$TotalPrincipalOutstanding_GO[cox_dt$District_ID=='7103000'] <- 0
#cox_dt$TotalPrincipalOutstanding_REV[cox_dt$District_ID=='7103000'] <- 0
cox_dt$Total_Principal_Outstanding[cox_dt$District_ID=='7103000'] <- 0
cox_dt$`BONDS OUTSTANDING`[cox_dt$District_ID=='7322000'] <- 0
#cox_dt$TotalPrincipalOutstanding_GO[cox_dt$District_ID=='7322000'] <- 0
#cox_dt$TotalPrincipalOutstanding_REV[cox_dt$District_ID=='7322000'] <- 0
cox_dt$Total_Principal_Outstanding[cox_dt$District_ID=='7322000'] <- 0

#cox_dt$Total_Principal_Outstanding[is.na(cox_dt$Total_Principal_Outstanding)&!is.na(cox_dt$`BONDS OUTSTANDING`) &cox_dt$`BONDS OUTSTANDING`>0] <- cox_dt$`BONDS OUTSTANDING`[is.na(cox_dt$Total_Principal_Outstanding)&!is.na(cox_dt$`BONDS OUTSTANDING`) & cox_dt$`BONDS OUTSTANDING`>0]
cox_dt$Two_Year_Match = (decimal_date(cox_dt$Date) - decimal_date(cox_dt$`FISCAL YEAR ENDED`) > 1) + 0
cox_dt$Total_Principal_Outstanding = cox_dt$Total_Principal_Outstanding * cox_dt$CWS_Prop
#cox_dt$TotalPrincipalOutstanding_REV = cox_dt$TotalPrincipalOutstanding_REV * cox_dt$CWS_Prop
#cox_dt$TotalPrincipalOutstanding_GO = cox_dt$TotalPrincipalOutstanding_GO * cox_dt$CWS_Prop

#### capacity indicator 1
cox_dt$Total_Revenue = as.numeric(cox_dt$`GENERAL FUND - TOTAL REVENUES`)  +
  as.numeric(cox_dt$`ENTERPRISE FUND - OPERATING REVENUES`)
cox_dt$Enterprise_Revenue = as.numeric(cox_dt$`ENTERPRISE FUND - OPERATING REVENUES`)
cox_dt$General_Revenue = as.numeric(cox_dt$`GENERAL FUND - TOTAL REVENUES`)  

cox_dt$Total_Revenue_Per_Connection = cox_dt$Total_Revenue/cox_dt$Connections
cox_dt$Enterprise_Revenue_Per_Connection = cox_dt$Enterprise_Revenue/cox_dt$Connections
cox_dt$General_Revenue_Per_Connection = cox_dt$General_Revenue/cox_dt$Connections

cox_dt$`TOTAL TAX RATE`[!is.na(cox_dt$District_Name) & cox_dt$District_Name == 'HARRIS COUNTY MUD 86' & cox_dt$Year%in%start_year:2012] <- 0.85
cox_dt$Fee_Based_Revenue =  as.numeric(cox_dt$`GENERAL FUND - TOTAL REVENUES`)  +
  as.numeric(cox_dt$`ENTERPRISE FUND - OPERATING REVENUES`) - as.numeric(cox_dt$`OPERATION & MAINTENANCE TAX LEVIED`)
cox_dt$Fee_Based_Revenue = cox_dt$Fee_Based_Revenue/cox_dt$Connections
##### slush fund
cox_dt$Fund_Balance_Per_Connection = (as.numeric(cox_dt$`GENERAL FUND - FUND BALANCE`)) / cox_dt$Connections
#### debt load
cox_dt$Total_Principal_Outstanding[is.na(cox_dt$Total_Principal_Outstanding)] <- cox_dt$`BONDS OUTSTANDING`
cox_dt$Debt_Per_Connection = cox_dt$Total_Principal_Outstanding/cox_dt$Connections

#cox_dt$Debt_Per_Connection_REV = cox_dt$TotalPrincipalOutstanding_REV/cox_dt$Connections
#cox_dt$Debt_Per_Connection_GO = cox_dt$TotalPrincipalOutstanding_GO/cox_dt$Connections
#cox_dt$Revenue_Backed_Debt <- (cox_dt$TotalPrincipalOutstanding_REV>0) + 0
#cox_dt$TotalDebtServiceOutstanding_REV[is.na(cox_dt$TotalDebtServiceOutstanding_REV)] <- 0
#cox_dt$REV_Debt_Per_Connection = cox_dt$TotalDebtServiceOutstanding_REV/cox_dt$Connections
#cox_dt$REV_Debt_Per_Connection_1k = cox_dt$TotalDebtServiceOutstanding_REV/1000
#### operating expense ratio (OER) 
cox_dt$Total_Expenditure = as.numeric(cox_dt$`GENERAL FUND - TOTAL EXPENDITURES`)  + as.numeric(cox_dt$`ENTERPRISE FUND - OPERATING EXPENSES`)

#cox_dt$Prop_Rev_Deb = cox_dt$TotalPrincipalOutstanding_REV / cox_dt$Total_Principal_Outstanding
#cox_dt$Have_Ent_Rev = (cox_dt$Enterprise_Revenue_Per_Connection_1k>0)+0
#cox_dt$Debt_Service_Tax = (cox_dt$`DEBT SERVICE TAX LEVIED` > 0) + 0
#cox_dt$Any_Debt = (cox_dt$Debt_Per_Connection>0)+0
cox_dt$District_Type <- as.character(cox_dt$District_Type)
#cox_dt$FWSD <- (cox_dt$District_Type %in% 'FWSD') + 0
#cox_dt$WCID <- (cox_dt$District_Type %in% 'WCID') + 0
#cox_dt$SUD <- (cox_dt$District_Type %in% 'SUD') + 0
#cox_dt$Tax_Rate <- cox_dt$`TOTAL TAX RATE`
#cox_dt$Revenue_Backed_Debt <- {cox_dt$TotalPrincipalOutstanding_REV>0} + 0
#cox_dt$Enterprise_Revenue <- {cox_dt$Enterprise_Revenue_Per_Connection>0} + 0
#cox_dt$Yearly_Payment_Per_Connection = cox_dt$Yearly_Debt_Payment/cox_dt$Connections
#cox_dt$Yearly_Payment_To_Revenue = cox_dt$Yearly_Debt_Payment/cox_dt$Total_Revenue
cox_dt$MO_Tax <- (cox_dt$`OPERATIONS & MAINTENANCE TAX RATE`>0) +0
cox_dt$Debt_Tax <- (cox_dt$`DEBT SERVICE TAX RATE`>0) +0
cox_dt$Median_Structure_Age = decimal_date(cox_dt$Date)-cox_dt$Median_Year_Structure_Built

cox_dt$Operating_Ratio = cox_dt$Total_Revenue/cox_dt$Total_Expenditure
cox_dt$Fund_Balance = cox_dt$`GENERAL FUND - FUND BALANCE`
cox_dt$Water_Bond_P1 = (!is.na(cox_dt$Issue_WaterRelated_P1)) + 0
cox_dt$Refund_Bond_P1 = (!is.na(cox_dt$Issue_Refund_P1)) + 0
cox_dt$Expenditure_Per_Connection = cox_dt$Total_Expenditure/cox_dt$Connections
cox_dt$OM_Tax_Over_Expenditure = cox_dt$`OPERATION & MAINTENANCE TAX LEVIED`/cox_dt$Total_Expenditure
cox_dt$Total_Debt_Service_Outstanding[is.na(cox_dt$Total_Debt_Service_Outstanding)] <- 0

cox_dt$Total_Debt_Service_Outstanding = ifelse(cox_dt$Total_Debt_Service_Outstanding==0&!is.na(cox_dt$`BONDS OUTSTANDING`)&cox_dt$`BONDS OUTSTANDING`>0,cox_dt$`BONDS OUTSTANDING`,cox_dt$Total_Debt_Service_Outstanding)


cox_dt[order(PWS_ID,Date),Usage_Per_Connection_P1:=zoo::na.locf0(Usage_Per_Connection_P1,fromLast=T),by = .(PWS_ID)]
cox_dt$`OTHER TAX RATE(S)`[is.na(cox_dt$`OTHER TAX RATE(S)`)] <- 0
cox_dt[,Total_Tax_Rate:=`OPERATIONS & MAINTENANCE TAX RATE`+`DEBT SERVICE TAX RATE`+`OTHER TAX RATE(S)`+`OTHER TAX RATES(S)`]


#tx_usage[grepl('PALOMA LAKE',tx_usage$`Entity Name`),]
#cox_dt[grepl('PALOMA LAKE',District_Name),.(PWS_ID,Usage_Per_Connection,Usage_Per_Connection_P1,Date)]

saveRDS(cox_dt,'scratch/full_panel_data.RDS')

# ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/CONUS_CLIMATE_DIVISIONS.shp.zip
# 
# #dinfo = read_csv('bosquebox/input/twdd_records/district_list_2019-01-03.csv')
# 
# cox_dt$District_Name=  gsub('MUNICIPAL UTILITY DISTRICT','MUD',cox_dt$District_Name)
# cox_dt$District_Type[grepl('MUD$',cox_dt$District_Name)] <- 'MUD'
# cox_dt$District_Name = as.character(cox_dt$District_Name)
# setnames(debt_dt,'FiscalYear','Year')
# setkey(cox_dt,District_ID,Year)
# setkey(debt_dt,District_ID,Year)
# cox_dt = merge(cox_dt,debt_dt,by = c('District_ID','Year'),all.x=T)
# cox_dt$Debt_Match[is.na(cox_dt$Debt_Match)] <- 0








p <- 


df[time==StartTime,]

head(df)

summary(km.drought)
plot(km.drought,col = 1:2,ylab = 'Survival')
km.drought$n.event
table(df.test$restriction,df.test$drought_window)
plot(km.drought)

form <- inla.surv(time, restriction) ~ 1 + drought_window + DSCI_2SD + f(PWS_ID, model = "iid",
                                                       hyper = list(prec = list(param = c(0.001, 0.001))))

# Model fitting
fr.ret <- inla(form, data = df,
               family  = "weibullsurv")
surv.drought <- Surv(df$time, df$restriction)
km.dr <- survfit(surv.drought ~ 1)
km.df.period <- survfit(surv.drought ~ -1 + drought_window, data = df)
table(df$restriction)
str(km.df.period)
autoplot(km.df.period)
names(km.df.period)
ggplot(km.df.period,aes(x = time,y = )
summary(fr.ret)
length(unique(df$PWS_ID))
table(df$restriction)
d1_start
head(df)
table(df$STAGE)

dim(pws_drought_weekly)
dim(notice)
dim(pws_drought_weekly)
table(notice$YEAR)
notice[,.N,by=.(`PWS ID`)][order(-N),]

notice[`PWS ID`=='TX1050099',]
reReg::reSurv()



