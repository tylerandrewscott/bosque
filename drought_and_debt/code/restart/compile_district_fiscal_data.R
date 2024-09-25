library(data.table)
library(lubridate)
library(tidyverse)
audits = fread('input/tceq_audits/district_audits.csv')

library(scales)
fix_names = names(audits)[apply(audits,2,function(x) any(grepl('\\$',x)))]
audits[,(fix_names):=lapply(.SD,function(x) gsub('\\$|\\,','',x)),.SDcols=fix_names]
audits[,(fix_names):=lapply(.SD,function(x) {as.numeric(gsub("\\(","-",gsub("\\)","",x)))}),.SDcols=fix_names]
audits$`FISCAL YEAR ENDED` <- mdy(audits$`FISCAL YEAR ENDED`)
audits$FiscalYear = year(audits$`FISCAL YEAR ENDED`)
audits = audits %>% dplyr::select(-YEAR) %>% rename(District_ID = DISTRICT_ID)
audits$common = paste(audits$District_ID,audits$`FISCAL YEAR ENDED`,sep='_')
audits$zeros = rowSums(audits == 0,na.rm = T)
audits = audits %>% arrange(common,-zeros) %>% filter(!duplicated(common)) 
audits = as.data.table(audits)
audits[,`FISCAL YEAR`:=NULL]
audits$DISTRICT_NAME = gsub('%2C',',',gsub('%2E','.',gsub('%26','&',gsub('%2D','-',gsub('%20',' ',gsub('name=','',str_extract(audits$DOC_URL,'name=[^&]+')))))))
audits$District_ID <- as.character(audits$District_ID)


debt = fread('input/tbrb/Debt_Outstanding_By_Local_Government.csv',stringsAsFactors = F,integer64 = 'numeric')
debt = debt[GovernmentType %in% c('WD'),]
debt$District_Name = toupper(debt$GovernmentName)
num_cols = names(debt)[sapply(debt, is.numeric)]
num_cols = num_cols[-1]
debt = debt[,lapply(.SD, sum,na.rm=T),by=.(District_Name,FiscalYear,PledgeType),.SDcols = num_cols]
remove_vars = c('Population','TaxRateMO','TaxRateIS','GODebtToTaxableValue', 'GODebtServiceToTaxableValue','DebtPerCapitaPledgeType')
debt = debt[,(remove_vars):=NULL]
debt_spread = dcast(debt,District_Name + FiscalYear ~ PledgeType,fill = 0,
                    value.var = c('TotalPrincipalOutstanding','TotalInterestOutstanding','TotalDebtServiceOutstanding','TaxableValue','TaxRateTotal'))
debt_dt = debt_spread
debt_dt[,Total_Debt_Service_Outstanding:=TotalDebtServiceOutstanding_GO+TotalDebtServiceOutstanding_REV]
debt_dt[,Total_Principal_Outstanding:=TotalPrincipalOutstanding_GO+TotalPrincipalOutstanding_REV]
debt_dt[,Total_Tax_Rate:=TaxRateTotal_GO + TaxRateTotal_REV]

issue = fread('input/tbrb/Local_Issuance.csv',stringsAsFactors = F,integer64 = 'numeric')
issue = issue[GovernmentType %in% c('WD'),]
setnames(issue,'GovernmentName','District_Name')
issue$District_Name = toupper(issue$District_Name)
issue = issue[,sum(ActualPar),by = .(District_Name,IssuePurpose,FiscalYearIssuance)]
issue$IssuePurpose = paste0('Issue_',issue$IssuePurpose)
issue_dt = dcast(issue,District_Name + FiscalYearIssuance ~ IssuePurpose)
setnames(issue_dt,'FiscalYearIssuance','FiscalYear')
setkey(issue_dt,District_Name,FiscalYear)
setkey(debt_dt,District_Name,FiscalYear)
finance_dt = merge(debt_dt,issue_dt,all=T)
setnames(finance_dt,'District_Name','DISTRICT_NAME')

finance_dt$DISTRICT_NAME = gsub('(\\s)0(?=[0-9])','\\1\\2',finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub('DIST$|DISTR$','DISTRICT',finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub(' MUNICIPAL UTILITY DISTRICT$',' MUD',finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub(' MUNICIPAL UTILITY DISTRICT ',' MUD ',finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub(' UD',' UTILITY DISTRICT',finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub(' MUNICIPAL DISTRICT$',' MUD',finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub('SPECIAL UTILITY DISTRICT','SUD',finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub('WATER CONTROL DISTRICT','WCID',finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub('NAVIGATION DISTRICT','ND',finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub('IRRIGATION DISTRICT|IRRIG DISTRICT','ID',finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub('DRAINAGE DISTRICT','DD',finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub('RIVER AUTHORITY','RA',finance_dt$DISTRICT_NAME,perl = T)


audits$DISTRICT_NAME = gsub('(\\s)0(?=[0-9])','\\1\\2',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub('DIST$|DISTR$','DISTRICT',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub(' MUNICIPAL UTILITY DISTRICT$',' MUD',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub(' MUNICIPAL UTILITY DISTRICT ',' MUD ',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub(' UD',' UTILITY DISTRICT',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub(' MUNICIPAL DISTRICT$',' MUD',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub('SPECIAL UTILITY DISTRICT','SUD',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub('WATER CONTROL DISTRICT','WCID',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub('NAVIGATION DISTRICT','ND',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub('IRRIGATION DISTRICT','ID',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub('DRAINAGE DISTRICT','DD',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub('RIVER AUTHORITY','RA',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub(" OF [A-Z]{1,} COUNTY$","",audits$DISTRICT_NAME,perl=T)

finance_dt$DISTRICT_NAME = gsub("LEANDER TRANSIT ORIENTED DEVELOPMENT DISTRICT (TODD)","LEANDER TODD",finance_dt$DISTRICT_NAME,fixed = T)
finance_dt$DISTRICT_NAME = gsub("OF PARKER COUNTY","",finance_dt$DISTRICT_NAME,fixed = T)
finance_dt$DISTRICT_NAME = gsub("WOODLANDS METRO CENTER MUD THE","THE WOODLANDS METRO CENTER MUD",finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub("WOODLANDS MUD 2$","THE WOODLANDS MUD 2",finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub("BARKER-CYPRESS MUD","BARKER CYPRESS MUD",finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub("COUNTY CONS MUD","COUNTY CONSOLIDATED MUD",finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub(" OF [A-Z]{1,} COUNTY","",finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub("BRUSHY CREEK MUD-DEFINED AREA" ,"BRUSHY CREEK MUD" ,finance_dt$DISTRICT_NAME,fixed = T)
finance_dt$DISTRICT_NAME = gsub("FLYING \"\"L\"\" RANCH PUD"  ,"FLYING L PUD" ,finance_dt$DISTRICT_NAME,fixed = T)
finance_dt$DISTRICT_NAME = gsub("HARRIS-FORT BEND COUNTIES"   ,"HARRIS FORT BEND COUNTIES"  ,finance_dt$DISTRICT_NAME,fixed = T)
finance_dt$DISTRICT_NAME = gsub("SEBASTIAN MUD 1"  ,"SEBASTIAN MUD" ,finance_dt$DISTRICT_NAME,fixed = T)
finance_dt$DISTRICT_NAME = gsub("RED RA"  ,"RED RA OF TEXAS" ,finance_dt$DISTRICT_NAME,fixed = T)
finance_dt$DISTRICT_NAME = gsub("'","",finance_dt$DISTRICT_NAME,perl = T)
finance_dt$DISTRICT_NAME = gsub('(\\s)0(?=[0-9])','\\1\\2',finance_dt$DISTRICT_NAME,perl = T)
 

fiscal_dt = merge(audits,finance_dt,all = T)

fiscal_dt$District_Name = gsub('(\\s)0(?=[0-9])','\\1\\2',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub(' UD',' UTILITY DISTRICT',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub(' MUNICIPAL DISTRICT$',' MUD',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub('SPECIAL UTILITY DISTRICT','SUD',fiscal_dt$District_Name,perl = T)
#fiscal_dt$District_Name = gsub(' ID$',' IRRIGATION DISTRICT',fiscal_dt$District_Name,perl = T)
#fiscal_dt$District_Name = gsub(' ID ',' IRRIGATION DISTRICT ',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub('IRRIG DISTRICT','IRRIGATION DISTRICT',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub(' RA$',' RIVER AUTHORITY',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub(' AUTH$',' AUTHORITY',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub(' WA$',' WATER AUTHORITY',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub(' DD$',' DRAINAGE DISTRICT',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub(' DD ',' DRAINAGE DISTRICT ',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub(' ND$',' NAVIGATION DISTRICT',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub(' WD$',' WATER DISTRICT',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub('HARRIS-FORT BEND','HARRIS FORT BEND',fiscal_dt$District_Name,perl = T)
#fiscal_dt$District_Name[grepl('MUD 1$',fiscal_dt$District_Name)] = ifelse(fiscal_dt$District_Name[grepl('MUD 1$',fiscal_dt$District_Name)] %in% dinfo$District_Name,fiscal_dt$District_Name[grepl('MUD 1$',fiscal_dt$District_Name)],
#                                                                      gsub(" 1$",'',fiscal_dt$District_Name[grepl('MUD 1$',fiscal_dt$District_Name)]))
fiscal_dt$District_Name = gsub("ARANSAS COUNTY ND 1","ARANSAS COUNTY NAVIGATION DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("BEEVILLE WSD","BEEVILLE WATER SUPPLY DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("INVERNESS FOREST ID","INVERNESS FOREST IMPROVEMENT DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("COMMODORE COVE ID","COMMODORE COVE IMPROVEMENT DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("LAZY RIVER ID","LAZY RIVER IMPROVEMENT DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("BELL COUNTY WCID 2-LITTLE RIVER","BELL COUNTY WCID NO 2",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("BELMONT FWSD 1","BELMONT FWSD 1 OF DENTON COUNTY",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("BISTONE MWSD","BISTONE MUNICIPAL WATER SUPPLY DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("BRIGHT STAR-SALEM","BRIGHT STAR SALEM",fiscal_dt$District_Name,perl=T)
fiscal_dt$District_Name = gsub("WICKSON CREEK SPECIAL UD","WICKSON CREEK SUD GRIMES COUNTY",fiscal_dt$District_Name,perl=T)
fiscal_dt$District_Name = gsub("BARKER-CYPRESS MUD","BARKER CYPRESS MUD",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("LEANDER TRANSIT ORIENTED DEVELOPMENT DISTRICT (TODD)","LEANDER TODD",fiscal_dt$District_Name,fixed = T)
fiscal_dt$District_Name = gsub("OF PARKER COUNTY","",fiscal_dt$District_Name,fixed = T)
fiscal_dt$District_Name = gsub("BRAZORIA-FORT BEND COUNTIES MUD","BRAZORIA-FORT BEND COUNTY MUD 1",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("BROOKSHIRE MWD","BROOKSHIRE MUNICIPAL WATER DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("BRUSHY CREEK MUD-DEFINED AREA","BRUSHY CREEK MUD",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("CANADIAN RIVER MWA","CANADIAN RIVER MUNICIPAL WATER AUTHORITY",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("CARDINAL MEADOWS WCID","CARDINAL MEADOWS IMPROVEMENT DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("CENTRAL WCID","CENTRAL WCID OF ANGELINA COUNTY",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("CHAMPIONS MUD","CHAMPIONS MUNICIPAL UTILITY DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("COMMODORE COVE IRRIGATION DISTRICT","COMMODORE COVE IMPROVEMENT DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("CONROE MMD 1"," CONROE MUNICIPAL MANAGEMENT DISTRICT 1",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("CORYELL CITY WSD","CORYELL CITY WATER SUPPLY DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("CY-CHAMP","CY CHAMP",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("CYPRESS SPINGS SUD","CYPRESS SPRINGS SUD",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("DALLAS COUNTY U&RD","DALLAS COUNTY UTILITY & RECLAMATION DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("DENTON COUNTY FWSD 8A","DENTON COUNTY FWSD 8-A",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("DENTON COUNTY FWSD 8B","DENTON COUNTY FWSD 8-B",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("DENTON COUNTY FWSD 8C","DENTON COUNTY FWSD 8-C",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("DENTON COUNTY RECL & RD$","DENTON COUNTY RECLAMATION & ROAD DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("DUVAL COUNTY C&RD","DUVAL COUNTY CONSERVATION & RECLAMATION DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("EASTLAND COUNTY WSD","EASTLAND COUNTY WATER SUPPLY DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("EL PASO COUNTY WID-TORNILLO","EL PASO COUNTY TORNILLO WID",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("FORT HANCOCK WCID 1","FORT HANCOCK WCID",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("GRAND PRAIRIE METROPOLITAN U&RD","GRAND PRAIRIE METROPOLITAN UTILITY & RECLAMATION DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("GREATER TEXOMA UA","GREATER TEXOMA UTILITY AUTHORITY",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("GREEN VALLEY SUD","GREEN VALLEY SPECIAL UTILITY DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("HARRIS-FORT BEND","HARRIS FORT BEND",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("HUDSPETH COUNTY C&RD 1","HUDSPETH COUNTY CONSERVATION & RECLAMATION DISTRICT 1",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("IRVING FCD SECTION 1","IRVING FLOOD CONTROL DISTRICT SECTION 1",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("IRVING FCD SECTION 3","IRVING FLOOD CONTROL DISTRICT SECTION 3",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("JACKRABBIT ROAD PUD","JACKRABBIT ROAD PUBLIC UTILITY DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("JACKSON COUNTY WCID 2-VANDERBILT","JACKSON COUNTY WCID 2",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("KELLY LANE WCID 1","KELLY LANE WCID 1 OF TRAVIS COUNTY",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("KELLY LANE WCID 2","KELLY LANE WCID 2 OF TRAVIS COUNTY",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("LAKE CITIES MUA","LAKE CITIES MUNICIPAL UTILITY AUTHORITY",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("LAKE VIEW MANAGEMENT & DEVELOPMENT DISTRICT","LAKE VIEW MANAGEMENT AND DEVELOPMENT DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("LIVE OAK CREEK MUD","LIVE OAK CREEK MUD 1 OF TARRANT COUNTY",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("LONE STAR REGIONAL WATER AUTHORITY","LONE STAR RWA",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("LONE STAR REGIONAL WATER AUTHORITY","LONE STAR RWA",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("MACKENZIE MWA","MACKENZIE MUNICIPAL WATER AUTHORITY",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("MATAGORDA COUNTY ND 1","MATAGORDA COUNTY NAVIGATION DISTRICT 1",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("MEEKER MWD","MEEKER MUNICIPAL WATER DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("MORNINGSTAR RANCH MUD","MORNINGSTAR RANCH MUD 1 OF PARKER COUNTY",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("NORTHAMPTON MUD","NORTHAMPTON MUNICIPAL UTILITY DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("NORTHEAST TEXAS MWD","NORTHEAST TEXAS MUNICIPAL WATER DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("OAKMONT PUD","OAKMONT PUBLIC UTILITY DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("PALO PINTO COUNTY MWD 1","PALO PINTO COUNTY MUNICIPAL WATER DISTRICT 1",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("POLK COUNTY FWSD 2","POLK COUNTY FRESH WATER SUPPLY DISTRICT 2",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("PROVIDENCE VILLAGE WCID","PROVIDENCE VILLAGE WCID OF DENTON COUNTY",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("RED RIVER AUTHORITY","RED RIVER AUTHORITY OF TEXAS",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("SEDONA LAKES MUD","SEDONA LAKES MUD 1 OF BRAZORIA COUNTY",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("SPINGS SUD$"," SPRINGS SUD",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("TARRANT REGIONAL WATER DISTRICT","TARRANT REGIONAL WATER DISTRICT A WCID",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("TERRANOVA WEST MUD","TERRANOVA WEST MUNICIPAL UTILITY DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("TIMBERLAKE ID","TIMBERLAKE IMPROVEMENT DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("TRINITY BAY CD","TRINITY BAY CONSERVATION DISTRICT",fiscal_dt$District_Name)
fiscal_dt$District_Name = gsub("TRINITY RIVER AUTHORITY","TRINITY RIVER AUTHORITY OF TEXAS",fiscal_dt$District_Name)
fiscal_dt$District_Name = gsub("WALLER COUNTY ROAD IRRIGATION DISTRICT 1","WALLER COUNTY ROAD IMPROVEMENT DISTRICT 1",fiscal_dt$District_Name)
fiscal_dt$District_Name = gsub("WALLER COUNTY ROAD IRRIGATION DISTRICT 2","WALLER COUNTY ROAD IMPROVEMENT DISTRICT 2",fiscal_dt$District_Name)
fiscal_dt$District_Name = gsub("WEST HARRIS COUNTY REGIONAL WATER AUTHORITY","WEST HARRIS COUNTY RWA",fiscal_dt$District_Name)
fiscal_dt$District_Name = gsub("WEST JEFFERSON COUNTY MWD","WEST JEFFERSON COUNTY MUNICIPAL WATER DISTRICT",fiscal_dt$District_Name)
fiscal_dt$District_Name = gsub("WEST KEEGANS BAYOU ID","WEST KEEGANS BAYOU IMPROVEMENT DISTRICT",fiscal_dt$District_Name)
fiscal_dt$District_Name = gsub("WILLIAMSON COUNTY WATER SEWER IRRIG & DRAINAGE DISTRICT 3","WILLIAMSON COUNTY WATER SEWER IRRIGATION AND DRAINAGE DIST 3",fiscal_dt$District_Name)
fiscal_dt$District_Name = gsub("WOODLANDS METRO CENTER MUD THE","THE WOODLANDS METRO CENTER MUD",fiscal_dt$District_Name)
fiscal_dt$District_Name = gsub("WOODLANDS MUD 2","THE WOODLANDS MUD 2",fiscal_dt$District_Name)
fiscal_dt$District_Name = gsub("D'ARC","DARC",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("\\s0([0-9])"," \\1",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("HUNTER'S GLEN MUD","HUNTERS GLEN MUD",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("MOORE'S CROSSING MUD","MOORES CROSSING MUD",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("HARRIS-BRAZORIA COUNTY MUD 509","HARRIS-BRAZORIA COUNTIES MUD 509",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("SPORTSMAN'S WORLD MUD","SPORTSMANS WORLD MUD",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("ROCKWALL COUNTY CONS MUD","ROCKWALL COUNTY CONSOLIDATED MUD 1",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub("PORT O'CONNOR ID","PORT OCONNOR IMPROVEMENT DISTRICT",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub('FLYING "L" RANCH PUD','FLYING L PUD',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub('MCKINNEY MUD',"MCKINNEY MUD 1 OF COLLIN COUNTY",fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub('LEANDER TRANSIT ORIENTED DEVELOPMENT DISTRICT (TODD) MUD','LEANDER TODD MUD 1',fiscal_dt$District_Name,fixed=T)
fiscal_dt$District_Name = gsub('(WCID\\s1)([A-Z]{1})$','\\1-\\2',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub('(WCID\\s2)([A-Z]{1})$','\\1-\\2',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub('(DENTON COUNTY FWSD\\s[0-9]{1,2})([A-Z]{1})$','\\1-\\2',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name = gsub('(HARRIS COUNTY FWSD\\s[0-9]{1,2})([A-Z]{1})$','\\1-\\2',fiscal_dt$District_Name,perl = T)
fiscal_dt$District_Name[grepl('TRAVIS COUNTY WCID 17',fiscal_dt$District_Name)] <- 'TRAVIS COUNTY WCID 17'
fiscal_dt$District_Name[grepl('KENDALL COUNTY WCID 2-A',fiscal_dt$District_Name)] <- 'KENDALL COUNTY WCID 2A'
fiscal_dt$District_Name[grepl("THE WOODLANDS MUD 2" ,fiscal_dt$District_Name)] <- "THE WOODLANDS MUD 1"
fiscal_dt$District_Name[grepl('BELL COUNTY WCID NO 2',fiscal_dt$District_Name)] <- 'BELL COUNTY WCID 2'
fiscal_dt$District_Name[grepl('CYPRESS-KLEIN',fiscal_dt$District_Name)] <- "CYPRESS KLEIN UTILITY DISTRICT"
fiscal_dt$District_Name[grepl('LAMAR COUNTY WSD',fiscal_dt$District_Name)] <- "LAMAR COUNTY WATER SUPPLY DISTRICT"
fiscal_dt$District_Name[grepl("PALO PINTO COUNTY WCID" ,fiscal_dt$District_Name)] <- "PALO PINTO COUNTY WCID 1" 
fiscal_dt$District_Name[grepl("MAURICEVILLE SUD" ,fiscal_dt$District_Name)] <- "MAURICEVILLE MUNICIPAL UTILITY DISTRICT" 

fvars = names(fiscal_dt)[grep('TAX|FUND|REV|EXP|ISSUE|DEBT|PRINC|INTER|BONDS',toupper(names(fiscal_dt)))]
newnames = paste0(fvars,'_P1')
fiscal_dt = fiscal_dt[order(District_ID,FiscalYear),]
fiscal_dt[,(newnames):=lapply(.SD,lag),by = .(District_ID),.SDcols = fvars]


saveRDS(fiscal_dt,'scratch/fiscal_data.RDS')


