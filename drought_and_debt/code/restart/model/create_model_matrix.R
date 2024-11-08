pws_drought_weekly <- readRDS('drought_and_debt/input/pws_drought_weekly.RDS')
library(reReg)
library(lubridate)
library(data.table)
pws_drought_weekly$YMD <- ymd(pws_drought_weekly$DroughtDate)
# astart at begining of 2010
min_date <- min(pws_drought_weekly[year(YMD) == 2010,]$YMD)
pws_drought_weekly <-pws_drought_weekly[YMD>=min_date,]

notice <- readRDS('drought_and_debt/input/combined_restriction_records.RDS')
library(reReg)

# https://www.twdb.texas.gov/publications/reports/other_reports/doc/Drought-in-Texas-Comparison-1950s-2010s.pdf
### this is when PDSI says drought started (before SPI in Feb 2011)
d1_start <- mdy('08-01-2010')
### this is when SPI says drought ended (after PDSI in Nov 2014)
d1_end <- mdy('03-31-2015')

d2_start <- mdy('09-01-2021')
d2_end <- mdy('10-31-2024')
library(tidyverse)
pws_drought_weekly <- pws_drought_weekly |> 
  mutate(drought = case_when(YMD>d1_start&YMD<=d1_end ~ 'd1',
                                                 YMD>d2_start&YMD<=d2_end ~ 'd2',
                                                 T ~ 'none')) 
setnames(notice,'PWS ID','PWD_ID')


notice$YMD <- notice$NOTIFIED_YMD

table(notice$YEAR)
notice[,.N,by=.(`PWS ID`)][order(-N),]

notice[`PWS ID`=='TX1050099',]
reReg::reSurv()



