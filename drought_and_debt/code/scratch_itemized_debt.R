
#dinfo = read_csv('input/twdd_records/district_list_2019-01-03.csv')
debt = left_join(read_csv('input/tbrb/Debt_Outstanding_By_Issuance_Local.csv') ,
                 read_csv('input/tbrb/Debt_Outstanding_By_Local_Government.csv') %>% 
                   dplyr::select(GovernmentName,FiscalYear,TaxRateTotal,TaxableValue))

debt %>% filter(GovernmentType %in% c('WD','CITY')) %>%
  filter(IssuePurpose == 'WaterRelated')
debt = debt %>% filter(GovernmentType %in% c('WD','CITY')) %>%
  filter(IssuePurpose == 'WaterRelated') %>% dplyr::select(-IssuerName,-IssuePurpose) %>%
  group_by(GovernmentType,GovernmentName,FiscalYear,PledgeType) %>%
  summarise(TotalPrincipalOutstanding = sum(TotalPrincipalOutstanding),
            TotalInterestOutstanding = sum(TotalPrincipalOutstanding),
            TotalDebtServiceOutstanding = sum(TotalDebtServiceOutstanding),
            TaxRateTotal = sum(TaxRateTotal),TaxableValue = sum(TaxableValue))


debt$GovernmentName = toupper(debt$GovernmentName)
debt$District_Name = ifelse(debt$GovernmentType=='WD',debt$GovernmentName,NA)
debt$City_Name = ifelse(debt$GovernmentType=='CITY',debt$GovernmentName,NA)
debt$City_Name = ifelse(!is.na(debt$City_Name) & paste0('CITY OF ',debt$City_Name) %in% tx_systems$PWS_NAME,paste0('CITY OF ',debt$City_Name),debt$City_Name)
debt$City_Name = ifelse(!is.na(debt$City_Name) & paste0('TOWN OF ',debt$City_Name) %in% tx_systems$PWS_NAME,paste0('TOWN OF ',debt$City_Name),debt$City_Name)
debt$City_Name = ifelse(!is.na(debt$City_Name) & paste0('VILLAGE OF ',debt$City_Name) %in% tx_systems$PWS_NAME,paste0('VILLAGE OF ',debt$City_Name),debt$City_Name)
debt$City_Name = ifelse(!is.na(debt$City_Name) & paste0(debt$City_Name,' MUNICIPAL WATER SYSTEM') %in% tx_systems$PWS_NAME,paste0(debt$City_Name,' MUNICIPAL WATER SYSTEM'),debt$City_Name)
debt$City_Name = ifelse(!is.na(debt$City_Name) & paste0(debt$City_Name,' WATER UTILITIES') %in% tx_systems$PWS_NAME,paste0(debt$City_Name,' WATER UTILITIES'),debt$City_Name)
debt$City_Name = ifelse(!is.na(debt$City_Name) & paste0(debt$City_Name,' WATER UTILITY') %in% tx_systems$PWS_NAME,paste0(debt$City_Name,' WATER UTILITY'),debt$City_Name)
debt$City_Name = ifelse(!is.na(debt$City_Name) & paste0(debt$City_Name,' WATER & WASTEWATER') %in% tx_systems$PWS_NAME,paste0(debt$City_Name,' WATER & WASTEWATER'),debt$City_Name)
debt$City_Name = ifelse(!is.na(debt$City_Name) & paste0('CITY OF ',debt$City_Name,' WATER UTILITY DEPT') %in% tx_systems$PWS_NAME,paste0('CITY OF ',debt$City_Name,' WATER UTILITY DEPT'),debt$City_Name)
debt$City_Name = ifelse(!is.na(debt$City_Name) & paste0('CITY OF ',debt$City_Name,' WATER & WASTEWATER') %in% tx_systems$PWS_NAME,paste0('CITY OF ',debt$City_Name,' WATER & WASTEWATER'),debt$City_Name)
debt$City_Name = ifelse(!is.na(debt$City_Name) & paste0('CITY OF ',debt$City_Name,' WATER SYSTEM') %in% tx_systems$PWS_NAME,paste0('CITY OF ',debt$City_Name,' WATER SYSTEM'),debt$City_Name)
debt$City_Name = ifelse(!is.na(debt$City_Name) & paste0('CITY OF ',debt$City_Name,' MUNICIPAL WATER SUPPLY') %in% tx_systems$PWS_NAME,paste0('CITY OF ',debt$City_Name,' MUNICIPAL WATER SUPPLY'),debt$City_Name)

debt$City_Name = ifelse(!is.na(debt$City_Name) & paste0(debt$City_Name,' PUBLIC WATER SYSTEM') %in% tx_systems$PWS_NAME,paste0(debt$City_Name,' PUBLIC WATER SYSTEM'),debt$City_Name)
debt$City_Name = ifelse(!is.na(debt$City_Name) & paste0(debt$City_Name,' PUBLIC UTILITY') %in% tx_systems$PWS_NAME,paste0(debt$City_Name,' PUBLIC UTILITY'),debt$City_Name)
debt$City_Name = ifelse(!is.na(debt$City_Name) & paste0(debt$City_Name,' CITY MUNICIPAL WATER SYSTEM') %in% tx_systems$PWS_NAME,paste0(debt$City_Name,' CITY MUNICIPAL WATER SYSTEM'),debt$City_Name)

debt$City_Name[debt$City_Name == 'EL PASO'] <- "EL PASO WATER UTILITIES PUBLIC SERVICE B"
debt$City_Name[debt$City_Name == 'PECOS CITY'] <- "CITY OF PECOS"
debt$City_Name[debt$City_Name == "MORGAN'S POINT" ] <- "CITY OF MORGANS POINT"
debt$City_Name[debt$City_Name == "SPRING VALLEY" ] <- "CITY OF SPRING VALLEY VILLAGE"
### reno a, reno b, SAWS,

debt$District_Name = gsub('(\\s)0(?=[0-9])','\\1\\2',debt$District_Name,perl = T)
debt$District_Name = gsub(' UD',' UTILITY DISTRICT',debt$District_Name,perl = T)
debt$District_Name = gsub('SPECIAL UTILITY DISTRICT','SUD',debt$District_Name,perl = T)
debt$District_Name = gsub(' ID$',' IRRIGATION DISTRICT',debt$District_Name,perl = T)
debt$District_Name = gsub(' ID ',' IRRIGATION DISTRICT ',debt$District_Name,perl = T)
debt$District_Name = gsub('IRRIG DISTRICT','IRRIGATION DISTRICT',debt$District_Name,perl = T)
debt$District_Name = gsub(' RA$',' RIVER AUTHORITY',debt$District_Name,perl = T)
debt$District_Name = gsub(' AUTH$',' AUTHORITY',debt$District_Name,perl = T)
debt$District_Name = gsub(' WA$',' WATER AUTHORITY',debt$District_Name,perl = T)
debt$District_Name = gsub(' DD$',' DRAINAGE DISTRICT',debt$District_Name,perl = T)
debt$District_Name = gsub(' DD ',' DRAINAGE DISTRICT ',debt$District_Name,perl = T)
debt$District_Name = gsub(' ND$',' NAVIGATION DISTRICT',debt$District_Name,perl = T)
debt$District_Name = gsub(' WD$',' WATER DISTRICT',debt$District_Name,perl = T)
debt$District_Name = gsub('HARRIS-FORT BEND','HARRIS FORT BEND',debt$District_Name,perl = T)
debt$District_Name[grepl('MUD 1$',debt$District_Name)] = ifelse(debt$District_Name[grepl('MUD 1$',debt$District_Name)] %in% dinfo$District_Name,debt$District_Name[grepl('MUD 1$',debt$District_Name)],
                                                                gsub(" 1$",'',debt$District_Name[grepl('MUD 1$',debt$District_Name)]))
debt$District_Name = gsub("ARANSAS COUNTY ND 1","ARANSAS COUNTY NAVIGATION DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("BEEVILLE WSD","BEEVILLE WATER SUPPLY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("BELL COUNTY WCID 2-LITTLE RIVER","BELL COUNTY WCID NO 2",debt$District_Name,perl = T)
debt$District_Name = gsub("BELMONT FWSD 1","BELMONT FWSD 1 OF DENTON COUNTY",debt$District_Name,perl = T)
debt$District_Name = gsub("BISTONE MWSD","BISTONE MUNICIPAL WATER SUPPLY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("BRIGHT STAR-SALEM","BRIGHT STAR SALEM",debt$District_Name,perl=T)
debt$District_Name = gsub("BARKER-CYPRESS MUD","BARKER CYPRESS MUD",debt$District_Name,perl = T)
debt$District_Name = gsub("BRAZORIA-FORT BEND COUNTIES MUD","BRAZORIA-FORT BEND COUNTY MUD 1",debt$District_Name,perl = T)
debt$District_Name = gsub("BROOKSHIRE MWD","BROOKSHIRE MUNICIPAL WATER DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("BRUSHY CREEK MUD-DEFINED AREA","BRUSHY CREEK MUD",debt$District_Name,perl = T)
debt$District_Name = gsub("CANADIAN RIVER MWA","CANADIAN RIVER MUNICIPAL WATER AUTHORITY",debt$District_Name,perl = T)
debt$District_Name = gsub("CARDINAL MEADOWS WCID","CARDINAL MEADOWS IMPROVEMENT DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("CENTRAL WCID","CENTRAL WCID OF ANGELINA COUNTY",debt$District_Name,perl = T)
debt$District_Name = gsub("CHAMPIONS MUD","CHAMPIONS MUNICIPAL UTILITY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("COMMODORE COVE IRRIGATION DISTRICT","COMMODORE COVE IMPROVEMENT DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("CONROE MMD 1"," CONROE MUNICIPAL MANAGEMENT DISTRICT 1",debt$District_Name,perl = T)
debt$District_Name = gsub("CORYELL CITY WSD","CORYELL CITY WATER SUPPLY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("CY-CHAMP","CY CHAMP",debt$District_Name,perl = T)
debt$District_Name = gsub("CYPRESS SPINGS SUD","CYPRESS SPRINGS SUD",debt$District_Name,perl = T)
debt$District_Name = gsub("DALLAS COUNTY U&RD","DALLAS COUNTY UTILITY & RECLAMATION DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("DENTON COUNTY FWSD 8A","DENTON COUNTY FWSD 8-A",debt$District_Name,perl = T)
debt$District_Name = gsub("DENTON COUNTY FWSD 8B","DENTON COUNTY FWSD 8-B",debt$District_Name,perl = T)
debt$District_Name = gsub("DENTON COUNTY FWSD 8C","DENTON COUNTY FWSD 8-C",debt$District_Name,perl = T)
debt$District_Name = gsub("DENTON COUNTY RECL & RD$","DENTON COUNTY RECLAMATION & ROAD DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("DUVAL COUNTY C&RD","DUVAL COUNTY CONSERVATION & RECLAMATION DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("EASTLAND COUNTY WSD","EASTLAND COUNTY WATER SUPPLY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("EL PASO COUNTY WID-TORNILLO","EL PASO COUNTY TORNILLO WID",debt$District_Name,perl = T)
debt$District_Name = gsub("FORT HANCOCK WCID 1","FORT HANCOCK WCID",debt$District_Name,perl = T)
debt$District_Name = gsub("GRAND PRAIRIE METROPOLITAN U&RD","GRAND PRAIRIE METROPOLITAN UTILITY & RECLAMATION DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("GREATER TEXOMA UA","GREATER TEXOMA UTILITY AUTHORITY",debt$District_Name,perl = T)
debt$District_Name = gsub("GREEN VALLEY SUD","GREEN VALLEY SPECIAL UTILITY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("HARRIS-FORT BEND","HARRIS FORT BEND",debt$District_Name,perl = T)
debt$District_Name = gsub("HUDSPETH COUNTY C&RD 1","HUDSPETH COUNTY CONSERVATION & RECLAMATION DISTRICT 1",debt$District_Name,perl = T)
debt$District_Name = gsub("IRVING FCD SECTION 1","IRVING FLOOD CONTROL DISTRICT SECTION 1",debt$District_Name,perl = T)
debt$District_Name = gsub("IRVING FCD SECTION 3","IRVING FLOOD CONTROL DISTRICT SECTION 3",debt$District_Name,perl = T)
debt$District_Name = gsub("JACKRABBIT ROAD PUD","JACKRABBIT ROAD PUBLIC UTILITY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("JACKSON COUNTY WCID 2-VANDERBILT","JACKSON COUNTY WCID 2",debt$District_Name,perl = T)
debt$District_Name = gsub("KELLY LANE WCID 1","KELLY LANE WCID 1 OF TRAVIS COUNTY",debt$District_Name,perl = T)
debt$District_Name = gsub("KELLY LANE WCID 2","KELLY LANE WCID 2 OF TRAVIS COUNTY",debt$District_Name,perl = T)
debt$District_Name = gsub("LAKE CITIES MUA","LAKE CITIES MUNICIPAL UTILITY AUTHORITY",debt$District_Name,perl = T)
debt$District_Name = gsub("LAKE VIEW MANAGEMENT & DEVELOPMENT DISTRICT","LAKE VIEW MANAGEMENT AND DEVELOPMENT DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("LIVE OAK CREEK MUD","LIVE OAK CREEK MUD 1 OF TARRANT COUNTY",debt$District_Name,perl = T)
debt$District_Name = gsub("LONE STAR REGIONAL WATER AUTHORITY","LONE STAR RWA",debt$District_Name,perl = T)
debt$District_Name = gsub("LONE STAR REGIONAL WATER AUTHORITY","LONE STAR RWA",debt$District_Name,perl = T)
debt$District_Name = gsub("MACKENZIE MWA","MACKENZIE MUNICIPAL WATER AUTHORITY",debt$District_Name,perl = T)
debt$District_Name = gsub("MATAGORDA COUNTY ND 1","MATAGORDA COUNTY NAVIGATION DISTRICT 1",debt$District_Name,perl = T)
debt$District_Name = gsub("MEEKER MWD","MEEKER MUNICIPAL WATER DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("MORNINGSTAR RANCH MUD","MORNINGSTAR RANCH MUD 1 OF PARKER COUNTY",debt$District_Name,perl = T)
debt$District_Name = gsub("NORTHAMPTON MUD","NORTHAMPTON MUNICIPAL UTILITY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("NORTHEAST TEXAS MWD","NORTHEAST TEXAS MUNICIPAL WATER DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("OAKMONT PUD","OAKMONT PUBLIC UTILITY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("PALO PINTO COUNTY MWD 1","PALO PINTO COUNTY MUNICIPAL WATER DISTRICT 1",debt$District_Name,perl = T)
debt$District_Name = gsub("POLK COUNTY FWSD 2","POLK COUNTY FRESH WATER SUPPLY DISTRICT 2",debt$District_Name,perl = T)
debt$District_Name = gsub("PROVIDENCE VILLAGE WCID","PROVIDENCE VILLAGE WCID OF DENTON COUNTY",debt$District_Name,perl = T)
debt$District_Name = gsub("RED RIVER AUTHORITY","RED RIVER AUTHORITY OF TEXAS",debt$District_Name,perl = T)
debt$District_Name = gsub("SEDONA LAKES MUD","SEDONA LAKES MUD 1 OF BRAZORIA COUNTY",debt$District_Name,perl = T)
debt$District_Name = gsub("SPINGS SUD$"," SPRINGS SUD",debt$District_Name,perl = T)
debt$District_Name = gsub("TARRANT REGIONAL WATER DISTRICT","TARRANT REGIONAL WATER DISTRICT A WCID",debt$District_Name,perl = T)
debt$District_Name = gsub("TERRANOVA WEST MUD","TERRANOVA WEST MUNICIPAL UTILITY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("TIMBERLAKE IRRIGATION DISTRICT","TIMBERLAKE IRRIGATION DIST",debt$District_Name,perl = T)
debt$District_Name = gsub("TRINITY BAY CD","TRINITY BAY CONSERVATION DISTRICT",debt$District_Name)
debt$District_Name = gsub("TRINITY RIVER AUTHORITY","TRINITY RIVER AUTHORITY OF TEXAS",debt$District_Name)
debt$District_Name = gsub("WALLER COUNTY ROAD IRRIGATION DISTRICT 1","WALLER COUNTY ROAD IMPROVEMENT DISTRICT 1",debt$District_Name)
debt$District_Name = gsub("WALLER COUNTY ROAD IRRIGATION DISTRICT 2","WALLER COUNTY ROAD IMPROVEMENT DISTRICT 2",debt$District_Name)
debt$District_Name = gsub("WEST HARRIS COUNTY REGIONAL WATER AUTHORITY","WEST HARRIS COUNTY RWA",debt$District_Name)
debt$District_Name = gsub("WEST JEFFERSON COUNTY MWD","WEST JEFFERSON COUNTY MUNICIPAL WATER DISTRICT",debt$District_Name)
debt$District_Name = gsub("WEST KEEGANS BAYOU IRRIGATION DISTRICT","WEST KEEGANS BAYOU IMPROVEMENT DISTRICT",debt$District_Name)
debt$District_Name = gsub("WILLIAMSON COUNTY WATER SEWER IRRIG & DRAINAGE DISTRICT 3","WILLIAMSON COUNTY WATER SEWER IRRIGATION AND DRAINAGE DIST 3",debt$District_Name)
debt$District_Name = gsub("WOODLANDS METRO CENTER MUD THE","THE WOODLANDS METRO CENTER MUD",debt$District_Name)
debt$District_Name = gsub("WOODLANDS MUD 2","THE WOODLANDS MUD 2",debt$District_Name)
debt$District_Name = gsub("D'ARC","DARC",debt$District_Name,perl = T)
debt$District_Name = gsub("HUNTER'S GLEN MUD","HUNTERS GLEN MUD",debt$District_Name,perl = T)
debt$District_Name = gsub("MOORE'S CROSSING MUD","MOORES CROSSING MUD",debt$District_Name,perl = T)
debt$District_Name = gsub("SPORTSMAN'S WORLD MUD","SPORTSMANS WORLD MUD",debt$District_Name,perl = T)
debt$District_Name = gsub("PORT O'CONNOR IRRIGATION DISTRICT","PORT OCONNOR IMPROVEMENT DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub('FLYING "L" RANCH PUD','FLYING L PUD',debt$District_Name,perl = T)

debt_sd = debt[debt$GovernmentType=='WD',]
debt_sd$District_ID = dinfo$District_ID[match(debt_sd$District_Name,dinfo$District_Name)]
debt_sd = debt_sd %>% arrange(FiscalYear) %>% group_by(GovernmentType,District_Name,District_ID,FiscalYear,PledgeType,GovernmentType) %>% summarise_if(is.numeric,sum,na.rm=T)
debt_sd_spread = Reduce(full_join,list(
  debt_sd %>%
    dplyr::select(District_Name,FiscalYear,PledgeType,TotalPrincipalOutstanding) %>% 
    group_by(District_Name,FiscalYear,PledgeType) %>%
    mutate(TotalPrincipalOutstanding = sum(TotalPrincipalOutstanding)) %>% 
    spread(PledgeType,TotalPrincipalOutstanding,fill=0) %>% rename(GO_Principal_Outstanding = GO,REV_Principal_Outstanding = REV),
  debt_sd %>%
    dplyr::select(District_Name,FiscalYear,PledgeType,TotalInterestOutstanding) %>% 
    group_by(District_Name,FiscalYear,PledgeType) %>%
    mutate(TotalInterestOutstanding = sum(TotalInterestOutstanding)) %>% 
    spread(PledgeType,TotalInterestOutstanding,fill=0) %>% rename(GO_Interest_Outstanding = GO,REV_Interest_Outstanding = REV),
  debt_sd %>%
    dplyr::select(District_Name,FiscalYear,PledgeType,TotalDebtServiceOutstanding) %>% 
    group_by(District_Name,FiscalYear,PledgeType) %>%
    mutate(TotalDebtServiceOutstanding= sum(TotalDebtServiceOutstanding)) %>% 
    spread(PledgeType,TotalDebtServiceOutstanding,fill=0) %>% rename(GO_Service_Outstanding = GO,REV_Service_Outstanding = REV),
  debt_sd %>%
    dplyr::select(District_Name,FiscalYear,PledgeType,TaxRateTotal) %>% 
    group_by(District_Name,FiscalYear,PledgeType) %>%
    mutate(TaxRateTotal = sum(TaxRateTotal)) %>% 
    spread(PledgeType,TaxRateTotal,fill=0) %>% dplyr::select(-REV) %>% rename(TaxRate = GO),
  debt_sd %>%
    dplyr::select(District_Name,FiscalYear,PledgeType,TaxableValue) %>% 
    group_by(District_Name,FiscalYear,PledgeType) %>%
    mutate(TaxableValue = sum(TaxableValue)) %>% 
    spread(PledgeType,TaxableValue,fill = 0) %>% dplyr::select(-REV) %>% rename(TaxableValue = GO)))


debt_city = debt[debt$GovernmentType=='CITY',]
debt_city$PWS_ID = tx_systems$PWS_ID[match(debt_city$City_Name,tx_systems$PWS_NAME)]
debt_city = debt_city %>% arrange(FiscalYear) %>% group_by(GovernmentType,City_Name,PWS_ID,FiscalYear,PledgeType,GovernmentType) %>% summarise_if(is.numeric,sum,na.rm=T)

debt_city_spread = Reduce(full_join,list(
  debt_city %>%
    dplyr::select(City_Name,PWS_ID,FiscalYear,PledgeType,TotalPrincipalOutstanding) %>% 
    group_by(City_Name,PWS_ID,FiscalYear,PledgeType) %>%
    mutate(TotalPrincipalOutstanding = sum(TotalPrincipalOutstanding)) %>% 
    spread(PledgeType,TotalPrincipalOutstanding,fill=0) %>% rename(GO_Principal_Outstanding = GO,REV_Principal_Outstanding = REV),
  debt_city %>%
    dplyr::select(City_Name,PWS_ID,FiscalYear,PledgeType,TotalInterestOutstanding) %>% 
    group_by(City_Name,PWS_ID,FiscalYear,PledgeType) %>%
    mutate(TotalInterestOutstanding = sum(TotalInterestOutstanding)) %>% 
    spread(PledgeType,TotalInterestOutstanding,fill=0) %>% rename(GO_Interest_Outstanding = GO,REV_Interest_Outstanding = REV),
  debt_city %>%
    dplyr::select(City_Name,PWS_ID,FiscalYear,PledgeType,TotalDebtServiceOutstanding) %>% 
    group_by(City_Name,PWS_ID,FiscalYear,PledgeType) %>%
    mutate(TotalDebtServiceOutstanding= sum(TotalDebtServiceOutstanding)) %>% 
    spread(PledgeType,TotalDebtServiceOutstanding,fill=0) %>% rename(GO_Service_Outstanding = GO,REV_Service_Outstanding = REV),
  debt_city %>%
    dplyr::select(City_Name,PWS_ID,FiscalYear,PledgeType,TaxRateTotal) %>% 
    group_by(City_Name,PWS_ID,FiscalYear,PledgeType) %>%
    mutate(TaxRateTotal = sum(TaxRateTotal)) %>% 
    spread(PledgeType,TaxRateTotal,fill=0) %>% dplyr::select(-REV) %>% rename(TaxRate = GO),
  debt_city %>%
    dplyr::select(City_Name,PWS_ID,FiscalYear,PledgeType,TaxableValue) %>% 
    group_by(City_Name,PWS_ID,FiscalYear,PledgeType) %>%
    mutate(TaxableValue = sum(TaxableValue)) %>% 
    spread(PledgeType,TaxableValue,fill = 0) %>% dplyr::select(-REV) %>% rename(TaxableValue = GO)))

debt_sd_spread$Principal_Outstanding = debt_sd_spread$GO_Principal_Outstanding+debt_sd_spread$REV_Principal_Outstanding
debt_sd_spread$Service_Outstanding = debt_sd_spread$GO_Service_Outstanding+debt_sd_spread$REV_Service_Outstanding
debt_sd_spread$Interest_Outstanding = debt_sd_spread$GO_Interest_Outstanding+debt_sd_spread$REV_Interest_Outstanding
debt_sd_spread$Primary_Debt_Type = ifelse(debt_sd_spread$GO_Service_Outstanding>debt_sd_spread$REV_Service_Outstanding,'GO','REV')

debt_city_spread$Principal_Outstanding = debt_city_spread$GO_Principal_Outstanding + debt_city_spread$REV_Principal_Outstanding
debt_city_spread$Service_Outstanding = debt_city_spread$GO_Service_Outstanding+debt_city_spread$REV_Service_Outstanding
debt_city_spread$Interest_Outstanding = debt_city_spread$GO_Interest_Outstanding+debt_city_spread$REV_Interest_Outstanding
debt_city_spread$Primary_Debt_Type = ifelse(debt_city_spread$GO_Service_Outstanding>debt_city_spread$REV_Service_Outstanding,'GO','REV')

debt_combined = plyr::rbind.fill(debt_sd_spread,debt_city_spread) #%>% dplyr::select(District_Name)

