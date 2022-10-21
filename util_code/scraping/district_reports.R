#$ sudo docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
#$ sudo docker ps
library(parallel)
library(rvest)
library(tidyverse)
library(RSelenium)
#empty_df = read_csv('test.csv')
search_ids = read_csv('input/tceq_audits/district_info.csv') %>% select(NAME,SYSTEM_ID)
search_ids[1:10,]
base_site = "http://www14.tceq.texas.gov/iwud/reports/index.cfm?fuseaction=mainreportform&TableId=257"
#length(system_summary_urls)
sess = html_session(base_site)
fm = html_form(sess)[[1]]
fm <- set_values(fm,fuseaction = 'ERPT0001')
sess = submit_form(sess,fm)
fm2 = html_form(sess)[[1]]
fm2 <- set_values(fm2,districtnumber = '985500')
sess2 = submit_form(session = sess,form = fm2,submit = 'submit')

write_csv(search_ids,'input/scratch/district_ids.csv')

rvest::
?follow_link

sess2 %>% 

follow_link(sess2,i = 9)
back(sess2)

?follow_link

sess2 %>% read_html() %>% html_nodes('td')

driver = webdriver.Chrome(chrome_options=chrome_options)  # Optional argument, if not specified will search path.
base_page = 'http://www14.tceq.texas.gov/iwud/reports/index.cfm?fuseaction=mainreportform&TableId=257'
driver.get(base_page)
Select(driver.find_element_by_name('fuseaction')).select_by_value('ERPT0001')
driver.find_element_by_css_selector('input').click()
test = driver.find_element_by_name('districtnumber')
test.send_keys('985500')





s2 = submit_form(sess,fm2)
s2 %>% read_html() %>% html_text(trim=T)



?set_values
prefix = 'http://dww2.tceq.texas.gov/DWW/JSP/'
page_links = base_site %>% read_html() %>% html_nodes('a') 
data_sheet_urls = page_links[grepl('Summary',page_links %>% html_text(trim=T))] %>% html_attr('href')

temp = do.call(rbind,mclapply(data_sheet_urls,function(x) 
{temp_tds =  gsub(' ','',paste0(prefix,x)) %>% read_html() %>% html_nodes('td');
data.frame(System = str_extract(x,'TX[0-9]{7}'),
           Position = as.character(temp_tds[!is.na(temp_tds %>% html_attr('width')) & (temp_tds %>% html_attr('width')) == '25%'] %>% html_text(trim=T)),
           NAME = as.character(temp_tds[!is.na(temp_tds %>% html_attr('width')) & (temp_tds %>% html_attr('width')) == '35%'] %>% html_text(trim=T)))},
mc.cores=8,mc.cleanup=T))

test = temp %>% mutate(
                Position = stri_replace_all_regex(Position,"\n|\t|\r|&nbsp",""),
                NAME = stri_replace_all_regex(NAME,"\n|\t|\r|&nbsp","")) %>%
  filter(!is.na(NAME))

write_csv(test,'input/texas_dww/personnel_records.csv')



head(temp)
remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445L, browserName = "firefox")
remDr$open()
remDr$navigate(base_site)
operator_css_selector = 'table:nth-child(18)'
summarysheet_link_elements = remDr$findElements(using='css',"font a")
#empty_df = tibble()
system_number_elements <- remDr$findElements(using='css',"#AutoNumber7 td:nth-child(1) a")
system_ids = unlist(lapply(system_number_elements,function(x) x$getElementText()))
system_summary_urls = lapply(summarysheet_link_elements,function(x) x$getElementAttribute('href'))




temp %>% mutate(System = str_extract(System,'TX[0-9]{7}'),
                                      Position = stri_replace_all_regex(Position,"\n|\t|\r|&nbsp",""),
                                      NAME = stri_replace_all_regex(NAME,"\n|\t|\r|&nbsp","")) %>%
  filter(!is.na(NAME))

test_tds =  unlist(system_summary_urls)[3] %>% read_html() %>% html_nodes('td')

data.frame(System = unlist(system_summary_urls)[3],
           Position = as.character(test_tds[!is.na(test_tds %>% html_attr('width')) & (test_tds %>% html_attr('width')) == '25%'] %>% html_text(trim=T)),
           NAME = as.character(test_tds[!is.na(test_tds %>% html_attr('width')) & (test_tds %>% html_attr('width')) == '35%'] %>% 
                                 html_text(trim=T)))

grep('WOFFORD',system_officials_df$NAME,value=T)

library(stringr)
library(stringi)

head(system_officials_df,10)

  
head(system_officials_df$NAME,10)

table(grepl('CAMPBELL, CURTIS',test$NAME))

test[grepl('DWYER',test$NAME),]



library(magrittr)
system_operators_table_list = mclapply(1:length(system_summary_urls),function(x) 
  unlist(system_summary_urls)[x] %>% read_html() %>% rvest::html_node(operator_css_selector) %>% html_table() %>% mutate(SYSTEM = system_ids[x]),mc.cores=4,mc.cleanup=T)

save.image('test_texas_system_operators.RData')
load('test_texas_system_operators.RData')


system_operators_df = do.call(rbind,system_operators_table_list[!sapply(system_operators_table_list,function(x) x$X1[1]=='No Licensing Data for this PWS')])
nosystem_operators_df = do.call(rbind,system_operators_table_list[sapply(system_operators_table_list,function(x) x$X1[1]=='No Licensing Data for this PWS')])

system_operators_df = cbind(system_operators_df[grepl('License',system_operators_df$X1),] %>% select(-X3,-X1) %>% rename(LICENSE_HOLDER = X2),
             system_operators_df[!grepl('License',system_operators_df$X1),] %>% select(-SYSTEM) %>% rename(CLASSIFICATION = X2,LICENSE_NUM = X3))

system_operators_df = full_join(system_operators_df ,nosystem_operators_df %>% rename(LICENSE_HOLDER = X1))

#write.csv(system_operators_df,'Google Drive/bosque/input/texas_dww/licensed_operators_4_19_2017.csv')

system_officials_df = do.call(rbind,mclapply(1:length(system_summary_urls),function(x) 
{temp_tds =  unlist(system_summary_urls)[x] %>% read_html() %>% html_nodes('td');
data.frame(Position = temp_tds[!is.na(temp_tds %>% html_attr('width')) & (temp_tds %>% html_attr('width')) == '25%'] %>% html_text(),
                     NAME = temp_tds[!is.na(temp_tds %>% html_attr('width')) & (temp_tds %>% html_attr('width')) == '35%'] %>% html_text())},
mc.cores=8,mc.cleanup=T))


system_officials_df



temp_tds = (unlist(system_summary_urls)[1] %>% read_html() %>% html_nodes('td'))
temp_df = data.frame(Position = temp_tds[!is.na(temp_tds %>% html_attr('width')) & (temp_tds %>% html_attr('width')) == '25%'] %>% html_text(),
NAME = temp_tds[!is.na(temp_tds %>% html_attr('width')) & (temp_tds %>% html_attr('width')) == '35%'] %>% html_text())




[(unlist(system_summary_urls)[1] %>% read_html() %>% html_nodes('td') %>%
                                                                       html_attr('width')) == '25%']



for (i in 1:length(summarysheet_link_elements))
{print(i)
  temp_num =   unlist(system_number_elements[[i]]$getElementText())
  if (temp_num %in% empty_df$System){next}
  summarysheet_elements[[i]]$clickElement()
  webElem<-remDr$findElements(using = 'css', "td")
  job = unlist(sapply(webElem[sapply(webElem,function(x) x$getElementAttribute('width')) == '25%'],function(x) x$getElementText()))
  
  if(length(job)==0)
  {summarysheet_elements[[i]]$clickElement();
    webElem<-remDr$findElements(using = 'css', "td");
    job = unlist(sapply(webElem[sapply(webElem,function(x) x$getElementAttribute('width')) == '25%'],function(x) x$getElementText()))}
  person = unlist(sapply(webElem[sapply(webElem,function(x) x$getElementAttribute('width')) == '35%'],function(x) x$getElementText()))
  temp_df = tibble(System = temp_num, job = job, person = person)
  empty_df = rbind(empty_df,temp_df)
  write_csv(empty_df,'test.csv')
  #remDr$navigate(austin_county)
  #summarysheet_elements <- remDr$findElements(using='css',"font a")
  #system_number_elements <- remDr$findElements(using='css',"#AutoNumber7 td:nth-child(1) a")
  remDr$goBack()
  summarysheet_elements <- remDr$findElements(using='css',"font a")
  system_number_elements <- remDr$findElements(using='css',"#AutoNumber7 td:nth-child(1) a")
}


owner_type_css = 'table:nth-child(21) tr+ tr td'
contact_table_css = 'table:nth-child(10) > tbody td:nth-child(2) , table:nth-child(10) > tbody td:nth-child(1) , br+ table th:nth-child(2) , br+ table tr+ tr th:nth-child(1)'
x = 3
unlist(system_summary_urls)[x] %>% read_html() %>% rvest::html_node(owner_type_css) %>% html_text()
unlist(system_summary_urls)[x] %>% read_html() %>% rvest::html_nodes(contact_table_css) %>% html_text()


%>% mutate(SYSTEM = system_ids[x])


test




lapply(unlist(system_summary_urls)[1:3],function(u)
  {remDr$navigate(u)
  operator_tables <<- append(operator_tables,(remDr$findElement(using = 'css',operator_css_selector))$getElementText())
})




?html_attr

rvest::(remDr$findElement(using = 'css',operator_css_selector))



remDr$navigate(unlist(system_summary_urls)[1])
(remDr$findElement(using = 'css',operator_css_selector))$


operator_tables[[1]]


system_operators_raw = data.frame(system_id = system_ids, operator_tables = unlist(operator_tables))

operator_tables


op_tables[[1]]$getElementAttribute('text')

unlist(system_summary_urls)[1:10]


op_tables
remDr$findElements(using = 'css', operator_css_selector)

lapply(remDr$findElements(using = 'css', operator_css_selector),function(x)
  print(x$getElementAttribute('text')))


for (i in 1:10)
{print(i)
  summarysheet_link_elements[[i]]$clickElement()
  op_table = remDr$findElements(using = 'css', operator_css_selector)
print(op_table)
remDr$goBack()
summarysheet_link_elements <- remDr$findElements(using='css',"font a")
system_number_elements <- remDr$findElements(using='css',"#AutoNumber7 td:nth-child(1) a")
}

op_table

for (i in 1:length(summarysheet_link_elements))
{print(i)
  temp_num =   unlist(system_number_elements[[i]]$getElementText())
  if (temp_num %in% empty_df$System){next}
  summarysheet_elements[[i]]$clickElement()
webElem<-remDr$findElements(using = 'css', "td")
job = unlist(sapply(webElem[sapply(webElem,function(x) x$getElementAttribute('width')) == '25%'],function(x) x$getElementText()))

if(length(job)==0)
{summarysheet_elements[[i]]$clickElement();
  webElem<-remDr$findElements(using = 'css', "td");
  job = unlist(sapply(webElem[sapply(webElem,function(x) x$getElementAttribute('width')) == '25%'],function(x) x$getElementText()))}
person = unlist(sapply(webElem[sapply(webElem,function(x) x$getElementAttribute('width')) == '35%'],function(x) x$getElementText()))
temp_df = tibble(System = temp_num, job = job, person = person)
empty_df = rbind(empty_df,temp_df)
write_csv(empty_df,'test.csv')
#remDr$navigate(austin_county)
#summarysheet_elements <- remDr$findElements(using='css',"font a")
#system_number_elements <- remDr$findElements(using='css',"#AutoNumber7 td:nth-child(1) a")
remDr$goBack()
summarysheet_elements <- remDr$findElements(using='css',"font a")
system_number_elements <- remDr$findElements(using='css',"#AutoNumber7 td:nth-child(1) a")
}





