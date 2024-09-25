### 

packs = c('rgeos','rgdal','sp','maptools','readxl','hhi','spdep','lubridate','stringr','neatRanges',
          'rvest','R.utils','pbapply','jsonlite','tidyverse','data.table','sf','tigris','lwgeom','tidyquant','readr')
need = packs[!packs %in% installed.packages()[,'Package']]
if(!identical(need,character(0))){sapply(need,install.packages)}
sapply(packs,require,character.only = T)
#if(!require(lucr)){remotes::install_github('Ironholds/lucr');require(lucr)}

audits = fread('input/tceq_audits/district_audits.csv')
money = as.vector(which(apply(audits,2,function(x) any(grepl('\\$',x)))))
audits = cbind(audits[,-money,with=F],audits[,lapply(.SD,parse_number),.SDcols = money])
audits$Year = year(mdy(audits$`FISCAL YEAR ENDED`))
audits$`FISCAL YEAR ENDED` <- mdy(audits$`FISCAL YEAR ENDED`)
setnames(audits,"DISTRICT_ID", "District_ID")
setnames(audits,"TOTAL TAX RATE", "Total_Tax_Rate")
audits = audits[, !"YEAR", with=FALSE]  
audits = audits[, !"DISTRICT_NAME", with=FALSE]  
audits$common = paste(audits$District_ID,audits$`FISCAL YEAR ENDED`,sep='_')
audits$zeros = rowSums(audits == 0,na.rm = T)
audits = audits[order(common, -zeros),]
audits = audits[!duplicated(audits, incomparables=FALSE, fromLast=FALSE, by='common'),]
audits$Date = decimal_date(audits$`FISCAL YEAR ENDED`)
audits$Retail_Wastewater = (audits$`WASTEWATER CUST - EQ SINGLE FAMILY UNITS`>0)+0
audits$y = 1
audits$District_ID = as.character(audits$District_ID)
#audits$YEAR <- floor(audits$Date)
#audits = audits[audits$YEAR %in% c(2007:2022),]
#audits$PERIOD= ifelse(floor(audits$Date) %in% c(2009:2011),'2009-2011',ifelse(floor(audits$Date) %in% c(2012:2014),'2012-2014','2015-2017'))
audits$Fund_Balance <- audits$`GENERAL FUND - FUND BALANCE`
audits$Total_Revenue <- audits$`ENTERPRISE FUND - OPERATING REVENUES` + audits$`GENERAL FUND - TOTAL REVENUES`
audits$Total_Expenditure<- audits$`GENERAL FUND - TOTAL EXPENDITURES`+ audits$`ENTERPRISE FUND - OPERATING EXPENSES`
audits$Water_SFU <- audits$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`
audits$Wastewater_SFU <- replace_na(audits$`WASTEWATER CUST - EQ SINGLE FAMILY UNIT`,0) + 
  replace_na(audits$`WASTEWATER CUST - EQ SINGLE FAMILY UNITS`,0)

saveRDS(audits,'drought_and_debt/input/district_audits.RDS')
