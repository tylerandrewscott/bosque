#$ sudo docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
#$ sudo docker ps
library(parallel)
library(rvest)
library(tidyverse)
library(RSelenium)

search_ids = read_csv('input/tceq_audits/district_info.csv')

temp = read_csv('code/upwork/WIDE (1).csv')
temp$HOW_CHOSEN = temp$`Number of Directors:`
temp$`Way Chosen:`[is.na(temp$HOW_CHOSEN)]
temp$ACTIVITY = ifelse(grepl('[0-9]',temp$`Business Phone:`),temp$`Activity Status:`, temp$`Business Phone:` )
temp$TYPE = ifelse(grepl('[0-9]',temp$`Business Phone:`),temp$`Type:`,  temp$`Activity Status:` )
temp = temp %>% filter(ACTIVITY == 'ACTIVE')
temp = temp %>% rename(SYSTEM_ID = `District:`)

temp = left_join(search_ids,temp %>% select(-`Financial Status:`,-`Activity Status:`))


temp = temp %>% filter(`Primary County:` %in% c('HARRIS','FORT BEND','BRAZORIA','MONTGOMERY','LIBERTY','WALLER','CHAMBERS','AUSTIN','GALVESTON')) %>%
  filter(!is.na(HOW_CHOSEN))

temp$HOW_CHOSEN[temp$HOW_CHOSEN=='Elected by Precinct'] <- 'Elected'


functions_df = read_csv('input/tceq_audits/district_functions.csv')

test = left_join(temp,functions_df)

test = test %>% filter(TYPE %in% c("SPECIAL UTILITY DISTRICT","WATER CONTROL AND IMPROVEMENT DISTR","MUNICIPAL UTILITY DISTRICT",
                           "FRESH WATER SUPPLY DISTRICT")) %>% 
  mutate(TAX.BOND.AUTHORITY = ifelse(!is.na(TAX.BOND.AUTHORITY),'TAX.BOND','NO.TAX')) %>%
  mutate(RETAIL.WASTEWATER = ifelse(!is.na(RETAIL.WASTEWATER),'WASTEWATER','NO.WASTEWATER'))
       
test$RETAIL.WASTEWATER           
library(knitr)
table(test$HOW_CHOSEN,test$TAX.BOND.AUTHORITY,test$TYPE)
table(test$TAX.BOND.AUTHORITY,test$TYPE)
table(test$HOW_CHOSEN)






table(test$RETAIL.WASTEWATER)


614/(614+45)

613/(613+46)


library(rvest)
b = 'http://www14.tceq.texas.gov/iwud/dist/index.cfm?fuseaction=DetailDistrict&ID=10682&command=list&name=ADDICKS%20UTILITY%20DISTRICT'
b %>% read_html() %>% html_nodes('table:nth-child(17) .iwud') %>% html_text()

temp %>% group_by(TYPE,HOW_CHOSEN,
                  
                  
                  ) %>% summarise(n()) %>% filter(TYPE %in% c('MUNICIPAL UTILITY DISTRICT'))





unique(temp$TYPE)



temp[!grepl('[0-9]',`Business Phone:`),6:ncol(temp)] = temp[!grepl('[0-9]',`Business Phone:`),5:ncol(temp)]

%>% rename(NAME = X2) %>%
  mutate(,
         TYPE = ifelse(grepl('[0-9]',`Business Phone:`),`Type:`,  `Activity Status:` ))
temp[,ncol(temp)]
table(temp$`Way Chosen:`)
head(temp)

temp$`Type:`[grepl('[0-9]',temp$`Financial Status:`)]


grep('[A-Z]',temp$`Financial Status:`,value=T,invert=T)
table(is.na(temp$`Financial Status:`))
temp$`Type:`[is.na(temp$`Financial Status:`)]

table(temp$`Way Created:`)

table(temp$ACTIVITY)
head(temp)

table(temp$`Date Received:`)
head(temp[,-c(2,3,1)])
table(temp$`Activity Status:`)
%>% filter(`Activity Status:`=='')


base_site = "http://www14.tceq.texas.gov/iwud/reports/index.cfm?fuseaction=mainreportform&TableId=257"
#length(system_summary_urls)

require(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4445L
                      , browserName = "firefox")
remDr$open()
remDr$navigate(base_site)
webElem <- remDr$findElement(using = 'name', value = "fuseaction")
webElem$setElementAttribute(attributeName = 'fuseaction',value = 'ERPT0001')
subElem <- remDr$findElement(using = 'css',value = 'input')
subElem$clickElement()

remDr$findElement(using = 'name',value = 'districtnumber')

numElem <- remDr$findElement(using = 'name',value = 'districtnumber')
numElem$sendKeysToElement(list('985500'))


runElem <- remDr$findElement(using = 'css',value = 'input+ input')
runElem$clickElement()

test = runElem$findElements(using = 'css',value = 'td')
test[[5]]$getElementText()



webElem <- remDr$findElement(using = 'name', value = "fuseaction")
  webElem$sendKeysToElement(list('985500'))



> webElem$getElementAttribute("class")
[[1]]
[1] "gsfi lst-d-f"

> webElem$getElementAttribute("id")
[[1]]


sess = html_session(base_site)
fm = html_form(sess)[[1]]
fm <- set_values(fm,fuseaction = 'ERPT0001')
sess = submit_form(sess,fm)
fm2 = html_form(sess)[[1]]
fm2 <- set_values(fm2,districtnumber = '985500')

sess2 = submit_form(session = sess,form = fm2)


sess2 %>% read_html() %>% html_nodes('td')

