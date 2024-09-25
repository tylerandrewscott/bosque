### 

packs = c('rgeos','rgdal','sp','maptools','readxl','hhi','spdep','lubridate','stringr','neatRanges',
          'rvest','R.utils','pbapply','jsonlite','tidyverse','data.table','sf','tigris','lwgeom','tidyquant','readr')
need = packs[!packs %in% installed.packages()[,'Package']]
if(!identical(need,character(0))){sapply(need,install.packages)}
sapply(packs,require,character.only = T)
#if(!require(lucr)){remotes::install_github('Ironholds/lucr');require(lucr)}

district_files <- list.files('input/twdd_records/',pattern = 'district_list',full.names = T)

dinfo_dt <- fread(district_files[which.max(file.info(district_files)$mtime)])
#OLD
#debt = fread('input/tbrb/Debt_Outstanding_By_Local_Government_UPDATE.csv') 
#NEW: just read in from TX opendata website
#https://data.texas.gov/Government-and-Taxes/Debt-Outstanding-by-Local-Government-Searchable-by/6d42-4z7a/data
debt <- fread('https://data.texas.gov/api/views/6d42-4z7a/rows.csv?')

debt = debt[debt$GovernmentType == 'WD',]
debt$GovernmentName <- str_remove(toupper(debt$GovernmentName),"(\\s|-)DEFINED AREA.*")
debt = debt[,list(sum(TotalDebtServiceOutstanding),sum(TotalPrincipalOutstanding)),by=.(GovernmentName,FiscalYear,PledgeType)]
setnames(debt,c('V1','V2'),c('TotalDebtServiceOutstanding','TotalPrincipalOutstanding'))


debt <- dcast(data = debt,GovernmentName + FiscalYear ~ PledgeType,value.var = c('TotalDebtServiceOutstanding','TotalPrincipalOutstanding'))
    
### local issuance data are not as nice, API is limited to 1k at a time
### so instead just use local file downloaded from: https://data.texas.gov/Government-and-Taxes/Local-Issuance/fnjb-etpr/data_preview
iss = fread('input/tbrb/Local_Issuance_20240924.csv')
iss = iss[iss$GovernmentType=='WD'&!is.na(iss$NewMoneyPar),]
iss$GovernmentName <- str_remove(toupper(iss$GovernmentName),"(\\s|-)DEFINED AREA.*")
iss = iss[,.(GovernmentName,NewMoneyPar,FiscalYearIssuance,PledgeType)]
iss <- iss[,sum(NewMoneyPar),by=.(FiscalYearIssuance,GovernmentName,PledgeType)]
setnames(iss,c('FiscalYearIssuance','V1'),c('FiscalYear','NewMoney'))

iss <- dcast(iss,GovernmentName + FiscalYear ~ PledgeType,value.var = 'NewMoney')
setnames(iss,c('GO','REV'),c('NewMoney_GO','NewMoney_REV'))
setkey(iss,GovernmentName,FiscalYear)
setkey(debt,GovernmentName,FiscalYear)

fin <- merge(debt,iss,all = T)


#fin$GovernmentName = toupper(fin$GovernmentName)
fin$District_Name = fin$GovernmentName
fin$District_Name = gsub('(\\s)0(?=[0-9])','\\1\\2',fin$District_Name,perl = T)

fin$District_Name = gsub('(\\s)0(?=[0-9])','\\1\\2',fin$District_Name,perl = T)
fin$District_Name = gsub('\\sNO(\\s[0-9]{1,})','\\1',fin$District_Name,perl = T)


fin$District_Name = gsub(' UD',' UTILITY DISTRICT',fin$District_Name,perl = T)
fin$District_Name = gsub('SPECIAL UTILITY DISTRICT','SUD',fin$District_Name,perl = T)
fin$District_Name = gsub(' ID$',' IRRIGATION DISTRICT',fin$District_Name,perl = T)
fin$District_Name = gsub(' ID ',' IRRIGATION DISTRICT ',fin$District_Name,perl = T)
fin$District_Name = gsub('IRRIG DISTRICT','IRRIGATION DISTRICT',fin$District_Name,perl = T)
fin$District_Name = gsub(' RA$',' RIVER AUTHORITY',fin$District_Name,perl = T)
fin$District_Name = gsub(' AUTH$',' AUTHORITY',fin$District_Name,perl = T)
fin$District_Name = gsub(' WA$',' WATER AUTHORITY',fin$District_Name,perl = T)
fin$District_Name = gsub(' DD$',' DRAINAGE DISTRICT',fin$District_Name,perl = T)
fin$District_Name = gsub(' DD ',' DRAINAGE DISTRICT ',fin$District_Name,perl = T)
fin$District_Name = gsub(' ND$',' NAVIGATION DISTRICT',fin$District_Name,perl = T)
fin$District_Name = gsub(' WD$',' WATER DISTRICT',fin$District_Name,perl = T)
fin$District_Name = gsub('HARRIS-FORT BEND','HARRIS FORT BEND',fin$District_Name,perl = T)
fin$District_Name[grepl('MUD 1$',fin$District_Name)] = ifelse(fin$District_Name[grepl('MUD 1$',fin$District_Name)] %in% dinfo_dt$District_Name,fin$District_Name[grepl('MUD 1$',fin$District_Name)],
                                                                gsub(" 1$",'',fin$District_Name[grepl('MUD 1$',fin$District_Name)]))
fin$District_Name = gsub("ARANSAS COUNTY ND 1","ARANSAS COUNTY NAVIGATION DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("BEEVILLE WSD","BEEVILLE WATER SUPPLY DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("BELL COUNTY WCID 2-LITTLE RIVER","BELL COUNTY WCID 2",fin$District_Name,perl = T)
fin$District_Name = gsub("BELMONT FWSD 1","BELMONT FWSD 1 OF DENTON COUNTY",fin$District_Name,perl = T)
fin$District_Name = gsub("BISTONE MWSD","BISTONE MUNICIPAL WATER SUPPLY DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("BRIGHT STAR-SALEM","BRIGHT STAR SALEM",fin$District_Name,perl=T)
fin$District_Name = gsub("BARKER-CYPRESS MUD","BARKER CYPRESS MUD",fin$District_Name,perl = T)
fin$District_Name = gsub("BRAZORIA-FORT BEND COUNTIES MUD","BRAZORIA-FORT BEND COUNTY MUD 1",fin$District_Name,perl = T)
fin$District_Name = gsub("BROOKSHIRE MWD","BROOKSHIRE MUNICIPAL WATER DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("BRUSHY CREEK MUD-DEFINED AREA","BRUSHY CREEK MUD",fin$District_Name,perl = T)
fin$District_Name = gsub("CANADIAN RIVER MWA","CANADIAN RIVER MUNICIPAL WATER AUTHORITY",fin$District_Name,perl = T)
fin$District_Name = gsub("CARDINAL MEADOWS WCID","CARDINAL MEADOWS IMPROVEMENT DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("CENTRAL WCID","CENTRAL WCID OF ANGELINA COUNTY",fin$District_Name,perl = T)
fin$District_Name = gsub("CHAMPIONS MUD","CHAMPIONS MUNICIPAL UTILITY DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("COMMODORE COVE IRRIGATION DISTRICT","COMMODORE COVE IMPROVEMENT DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("CONROE MMD 1"," CONROE MUNICIPAL MANAGEMENT DISTRICT 1",fin$District_Name,perl = T)
fin$District_Name = gsub("CORYELL CITY WSD","CORYELL CITY WATER SUPPLY DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("CY-CHAMP","CY CHAMP",fin$District_Name,perl = T)
fin$District_Name = gsub("CYPRESS SPINGS SUD","CYPRESS SPRINGS SUD",fin$District_Name,perl = T)
fin$District_Name = gsub("DALLAS COUNTY U&RD","DALLAS COUNTY UTILITY & RECLAMATION DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("DENTON COUNTY FWSD 8A","DENTON COUNTY FWSD 8-A",fin$District_Name,perl = T)
fin$District_Name = gsub("DENTON COUNTY FWSD 8B","DENTON COUNTY FWSD 8-B",fin$District_Name,perl = T)
fin$District_Name = gsub("DENTON COUNTY FWSD 8C","DENTON COUNTY FWSD 8-C",fin$District_Name,perl = T)
fin$District_Name = gsub("DENTON COUNTY RECL & RD$","DENTON COUNTY RECLAMATION & ROAD DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("DUVAL COUNTY C&RD","DUVAL COUNTY CONSERVATION & RECLAMATION DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("EASTLAND COUNTY WSD","EASTLAND COUNTY WATER SUPPLY DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("EL PASO COUNTY WID-TORNILLO","EL PASO COUNTY TORNILLO WID",fin$District_Name,perl = T)
fin$District_Name = gsub("FORT HANCOCK WCID 1","FORT HANCOCK WCID",fin$District_Name,perl = T)
fin$District_Name = gsub("GRAND PRAIRIE METROPOLITAN U&RD","GRAND PRAIRIE METROPOLITAN UTILITY & RECLAMATION DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("GREATER TEXOMA UA","GREATER TEXOMA UTILITY AUTHORITY",fin$District_Name,perl = T)
fin$District_Name = gsub("GREEN VALLEY SUD","GREEN VALLEY SPECIAL UTILITY DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("HARRIS-FORT BEND","HARRIS FORT BEND",fin$District_Name,perl = T)
fin$District_Name = gsub("HUDSPETH COUNTY C&RD 1","HUDSPETH COUNTY CONSERVATION & RECLAMATION DISTRICT 1",fin$District_Name,perl = T)
fin$District_Name = gsub("IRVING FCD SECTION 1","IRVING FLOOD CONTROL DISTRICT SECTION 1",fin$District_Name,perl = T)
fin$District_Name = gsub("IRVING FCD SECTION 3","IRVING FLOOD CONTROL DISTRICT SECTION 3",fin$District_Name,perl = T)
fin$District_Name = gsub("JACKRABBIT ROAD PUD","JACKRABBIT ROAD PUBLIC UTILITY DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("JACKSON COUNTY WCID 2-VANDERBILT","JACKSON COUNTY WCID 2",fin$District_Name,perl = T)
fin$District_Name = gsub("KELLY LANE WCID 1","KELLY LANE WCID 1 OF TRAVIS COUNTY",fin$District_Name,perl = T)
fin$District_Name = gsub("KELLY LANE WCID 2","KELLY LANE WCID 2 OF TRAVIS COUNTY",fin$District_Name,perl = T)
fin$District_Name = gsub("LAKE CITIES MUA","LAKE CITIES MUNICIPAL UTILITY AUTHORITY",fin$District_Name,perl = T)
fin$District_Name = gsub("LAKE VIEW MANAGEMENT & DEVELOPMENT DISTRICT","LAKE VIEW MANAGEMENT AND DEVELOPMENT DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("LIVE OAK CREEK MUD","LIVE OAK CREEK MUD 1 OF TARRANT COUNTY",fin$District_Name,perl = T)
fin$District_Name = gsub("LONE STAR REGIONAL WATER AUTHORITY","LONE STAR RWA",fin$District_Name,perl = T)
fin$District_Name = gsub("LONE STAR REGIONAL WATER AUTHORITY","LONE STAR RWA",fin$District_Name,perl = T)
fin$District_Name = gsub("MACKENZIE MWA","MACKENZIE MUNICIPAL WATER AUTHORITY",fin$District_Name,perl = T)
fin$District_Name = gsub("MATAGORDA COUNTY ND 1","MATAGORDA COUNTY NAVIGATION DISTRICT 1",fin$District_Name,perl = T)
fin$District_Name = gsub("MEEKER MWD","MEEKER MUNICIPAL WATER DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("MORNINGSTAR RANCH MUD","MORNINGSTAR RANCH MUD 1 OF PARKER COUNTY",fin$District_Name,perl = T)
fin$District_Name = gsub("NORTHAMPTON MUD","NORTHAMPTON MUNICIPAL UTILITY DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("NORTHEAST TEXAS MWD","NORTHEAST TEXAS MUNICIPAL WATER DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("OAKMONT PUD","OAKMONT PUBLIC UTILITY DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("PALO PINTO COUNTY MWD 1","PALO PINTO COUNTY MUNICIPAL WATER DISTRICT 1",fin$District_Name,perl = T)
fin$District_Name = gsub("POLK COUNTY FWSD 2","POLK COUNTY FRESH WATER SUPPLY DISTRICT 2",fin$District_Name,perl = T)
fin$District_Name = gsub("PROVIDENCE VILLAGE WCID","PROVIDENCE VILLAGE WCID OF DENTON COUNTY",fin$District_Name,perl = T)
fin$District_Name = gsub("RED RIVER AUTHORITY","RED RIVER AUTHORITY OF TEXAS",fin$District_Name,perl = T)
fin$District_Name = gsub("SEDONA LAKES MUD","SEDONA LAKES MUD 1 OF BRAZORIA COUNTY",fin$District_Name,perl = T)
fin$District_Name = gsub("SPINGS SUD$"," SPRINGS SUD",fin$District_Name,perl = T)
fin$District_Name = gsub("TARRANT REGIONAL WATER DISTRICT","TARRANT REGIONAL WATER DISTRICT A WCID",fin$District_Name,perl = T)
fin$District_Name = gsub("TERRANOVA WEST MUD","TERRANOVA WEST MUNICIPAL UTILITY DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub("TIMBERLAKE IRRIGATION DISTRICT","TIMBERLAKE IRRIGATION DIST",fin$District_Name,perl = T)
fin$District_Name = gsub("TRINITY BAY CD","TRINITY BAY CONSERVATION DISTRICT",fin$District_Name)
fin$District_Name = gsub("TRINITY RIVER AUTHORITY","TRINITY RIVER AUTHORITY OF TEXAS",fin$District_Name)
fin$District_Name = gsub("WALLER COUNTY ROAD IRRIGATION DISTRICT 1","WALLER COUNTY ROAD IMPROVEMENT DISTRICT 1",fin$District_Name)
fin$District_Name = gsub("WALLER COUNTY ROAD IRRIGATION DISTRICT 2","WALLER COUNTY ROAD IMPROVEMENT DISTRICT 2",fin$District_Name)
fin$District_Name = gsub("WEST HARRIS COUNTY REGIONAL WATER AUTHORITY","WEST HARRIS COUNTY RWA",fin$District_Name)
fin$District_Name = gsub("WEST JEFFERSON COUNTY MWD","WEST JEFFERSON COUNTY MUNICIPAL WATER DISTRICT",fin$District_Name)
fin$District_Name = gsub("WEST KEEGANS BAYOU IRRIGATION DISTRICT","WEST KEEGANS BAYOU IMPROVEMENT DISTRICT",fin$District_Name)
fin$District_Name = gsub("WILLIAMSON COUNTY WATER SEWER IRRIG & DRAINAGE DISTRICT 3","WILLIAMSON COUNTY WATER SEWER IRRIGATION AND DRAINAGE DIST 3",fin$District_Name)
fin$District_Name = gsub("WOODLANDS METRO CENTER MUD THE","THE WOODLANDS METRO CENTER MUD",fin$District_Name)
fin$District_Name = gsub("WOODLANDS MUD 2","THE WOODLANDS MUD 2",fin$District_Name)
fin$District_Name = gsub("CANEY CREEK MUD","CANEY CREEK MUD OF MATAGORDA COUNTY",fin$District_Name)
fin$District_Name = gsub("D'ARC","DARC",fin$District_Name,perl = T)
fin$District_Name = gsub("HUNTER'S GLEN MUD","HUNTERS GLEN MUD",fin$District_Name,perl = T)
fin$District_Name = gsub("\\sCONS\\s"," CONSOLIDATED ",fin$District_Name,perl = T)
fin$District_Name = gsub("MOORE'S CROSSING MUD","MOORES CROSSING MUD",fin$District_Name,perl = T)
fin$District_Name = gsub("TRAVIS COUNTY WCID 17.*","TRAVIS COUNTY WCID 17",fin$District_Name,perl = T)
fin$District_Name = gsub("SPORTSMAN'S WORLD MUD","SPORTSMANS WORLD MUD",fin$District_Name,perl = T)
fin$District_Name = gsub("PORT O'CONNOR IRRIGATION DISTRICT","PORT OCONNOR IMPROVEMENT DISTRICT",fin$District_Name,perl = T)
fin$District_Name = gsub('FLYING "L" RANCH PUD','FLYING L PUD',fin$District_Name,perl = T)
fin$District_Name = gsub('TATTOR ROAD MUD','TATTOR ROAD MUNICIPAL DISTRICT',fin$District_Name,perl = T)
fin$District_Name = gsub('POLK COUNTY FRESH WATER SUPPLY DISTRICT 2',"POLK COUNTY FWSD 2",fin$District_Name,perl = T)
fin$District_Name = gsub('(LAKESIDE WCID 2)([A-D])','\\1-\\2',fin$District_Name,perl = T)

indx <- grepl('\\sOF\\s[A-Z]{1,}\\sCOUNTY$',fin$District_Name)

fin$District_Name[indx] <- gsub('\\sOF\\s[A-Z]{1,}\\sCOUNTY$',"",fin$District_Name[indx],perl = T)

fin$NewMoney_GO = replace_na(fin$NewMoney_GO,0)
fin$NewMoney_REV = replace_na(fin$NewMoney_REV,0)
fin$District_ID <- dinfo_dt$District_ID[match(fin$District_Name,dinfo_dt$District_Name)]
fin = fin[!is.na(fin$District_ID),]
saveRDS(fin,'drought_and_debt/input/district_debt_issuances.RDS')


