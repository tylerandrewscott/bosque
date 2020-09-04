
dinfo = read_csv('input/twdd_records/district_list_2019-01-03.csv')
debt = read_csv('input/tbrb/Debt_Outstanding_By_Local_Government.csv')
debt = debt[debt$GovernmentType=='WD',]
debt = debt %>% group_by(GovernmentName,PledgeType) %>% arrange(GovernmentName,PledgeType,FiscalYear) %>% 
  mutate(Interest_Payment = TotalInterestOutstanding - lag(TotalInterestOutstanding)) %>%
  rename( District_Name = GovernmentName)
debt$District_Name = toupper(debt$District_Name)
test = unique(debt$District_Name)
test = gsub('(?!\\s)0','',test,perl = T)
test = gsub(' UD',' UTILITY DISTRICT',test,perl = T)
test = gsub('SPECIAL UTILITY DISTRICT','SUD',test,perl = T)
test = gsub(' ID$',' IRRIGATION DISTRICT',test,perl = T)
test = gsub(' ID ',' IRRIGATION DISTRICT ',test,perl = T)
test = gsub('IRRIG DISTRICT','IRRIGATION DISTRICT',test,perl = T)
test = gsub(' RA$',' RIVER AUTHORITY',test,perl = T)
test = gsub(' AUTH$',' AUTHORITY',test,perl = T)
test = gsub(' WA$',' WATER AUTHORITY',test,perl = T)
test = gsub(' DD$',' DRAINAGE DISTRICT',test,perl = T)
test = gsub(' DD ',' DRAINAGE DISTRICT ',test,perl = T)
test = gsub(' ND$',' NAVIGATION DISTRICT',test,perl = T)
test = gsub(' WD$',' WATER DISTRICT',test,perl = T)
test = gsub('HARRIS-FORT BEND','HARRIS FORT BEND',test,perl = T)
dinfo$District_Name = gsub("([1-4])-([A-J]{1})","\\1\\2",dinfo$District_Name,perl=T)
test[grepl('MUD 1$',test)] = ifelse(test[grepl('MUD 1$',test)] %in% dinfo$District_Name,test[grepl('MUD 1$',test)],
                                    gsub(" 1$",'',test[grepl('MUD 1$',test)]))
test = gsub("BRIGHT STAR-SALEM","BRIGHT STAR SALEM",test)
test = gsub("TRINITY BAY CD","TRINITY BAY CONSERVATION DISTRICT",test)
####Check progress
table(test %in% dinfo$District_Name)
####what is still not in there?
test[!test %in% dinfo$District_Name]
##Master webpage
#http://www14.tceq.texas.gov/iwud/dist/index.cfm?fuseaction=ListDistricts&Command=list