#$ sudo docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
#$ sudo docker ps
library(parallel)
library(rvest)
library(tidyverse)

library(stringr)
library(stringi)
#empty_df = read_csv('test.csv')

library(lubridate)

if(any(list.files('scraped_input/tceq_audits/') == 'district_audit_reference_sheet.csv'))
{doc_df = read_csv('scraped_input/tceq_audits/district_audit_reference_sheet.csv') %>% mutate(DOC_ID = as.character(DOC_ID),
                                                                                      DISTRICT_ID = as.character(DISTRICT_ID))}
if(!any(list.files('scraped_input/tceq_audits/') == 'district_audit_reference_sheet.csv'))
{doc_df = data.frame()}

audit_links = 'https://www14.tceq.texas.gov/iwud/document/index.cfm?fuseaction=ListDocumentsByType&COMMAND=LIST&DocType=Audit-District'
audit_session = rvest::html_session(audit_links)
audit_session
go_on = TRUE
while(go_on)
{
go_on = FALSE
css = 'form+ table .iwud'
td = audit_session %>% read_html() %>% html_nodes(css = css)
links = matrix(paste0('https://www14.tceq.texas.gov',td %>% html_nodes("a") %>% html_attr('href')),ncol=2,byrow=T)
text = td %>% html_text(trim=T) %>% matrix(.,ncol=6,byrow=T)
temp_df = data.frame(text,links) %>% rename(DISTRICT_NAME = X1,DISTRICT_ID = X2,DOC_ID = X4,DATE_SUBMITTED = X5,
                                            DISTRICT_URL = X1.1,DOC_URL = X2.1) %>% 
  dplyr::select(-X6,-X3) %>% mutate(DISTRICT_ID = gsub('[A-Za-z]| ','',DISTRICT_ID))
if(all(temp_df$DOC_ID %in% doc_df$DOC_ID)){break}
temp_df = temp_df %>% filter(!DOC_ID %in% doc_df$DOC_ID)
if(nrow(doc_df)==0){doc_df = temp_df}
if(nrow(doc_df)!=0){doc_df = full_join(doc_df,temp_df)}
if(any((audit_session  %>% read_html() %>% html_nodes('img') %>% html_attr('alt')) == 'Next page'))
{
audit_session <- audit_session %>% follow_link(i = 
                    intersect(which(audit_session  %>% read_html() %>% html_nodes('img') %>% html_attr('alt') == 'Next page'),
                      which(audit_session  %>% read_html() %>% html_nodes('img') %>% html_attr('name') == 'next')))
go_on = TRUE}}



audit_links = 'https://www14.tceq.texas.gov/iwud/document/index.cfm?fuseaction=ListDocumentsByType&COMMAND=LIST&DocType=Audit'
audit_session = rvest::html_session(audit_links)
go_on = TRUE
while(go_on)
{go_on = FALSE
css = 'form+ table .iwud'
td = audit_session %>% read_html() %>% html_nodes(css = css)
links = matrix(paste0('https://www14.tceq.texas.gov',td %>% html_nodes("a") %>% html_attr('href')),ncol=2,byrow=T)
text = td %>% html_text(trim=T) %>% matrix(.,ncol=6,byrow=T)
temp_df = data.frame(text,links) %>% rename(DISTRICT_NAME = X1,DISTRICT_ID = X2,DOC_ID = X4,DATE_SUBMITTED = X5,
                                            DISTRICT_URL = X1.1,DOC_URL = X2.1) %>% 
  dplyr::select(-X6,-X3) %>% mutate(DISTRICT_ID = gsub('[A-Za-z]| ','',DISTRICT_ID))
if(all(temp_df$DOC_ID %in% doc_df$DOC_ID)){break}
temp_df = temp_df %>% filter(!DOC_ID %in% doc_df$DOC_ID)
if(nrow(doc_df)==0){doc_df = temp_df}
if(nrow(doc_df)!=0){doc_df = full_join(doc_df,temp_df)}
if(any((audit_session  %>% read_html() %>% html_nodes('img') %>% html_attr('alt')) == 'Next page'))
{
  audit_session <- audit_session %>% follow_link(i = 
                                                   intersect(which(audit_session  %>% read_html() %>% html_nodes('img') %>% html_attr('alt') == 'Next page'),
                                                             which(audit_session  %>% read_html() %>% html_nodes('img') %>% html_attr('name') == 'next')))
  go_on = TRUE}}

write_csv(doc_df,'scraped_input/tceq_audits/district_audit_reference_sheet.csv')


rm(list=ls())
if(any(list.files('scraped_input/tceq_audits/') == 'district_audits.csv'))
{audit_df = read_csv('scraped_input/tceq_audits/district_audits.csv',trim_ws = T)
for (c in colnames(audit_df))
{audit_df[[c]] <- as.character(audit_df[[c]])}}

if(!any(list.files('scraped_input/tceq_audits/') == 'district_audits.csv'))
{audit_df = data.frame()}
library(pbapply)
doc_df = read_csv('scraped_input/tceq_audits/district_audit_reference_sheet.csv')
new_audits = doc_df %>% filter(!DOC_ID %in% audit_df$DOC_ID)

if(nrow(new_audits)!=0){
audit_list = pblapply(1:nrow(new_audits),function(i) as.character(new_audits$DOC_URL[i]) %>% read_html() %>% html_nodes(css = 'td.iwud') %>% html_text(trim=T) %>% matrix(.,ncol=2,byrow=T) %>%
    as.data.frame(.,stringsAsFactors = FALSE) %>% spread(V1,V2) %>% 
    mutate(DOC_URL = new_audits$DOC_URL[i],DOC_ID = new_audits$DOC_ID[i],
           DISTRICT_ID = new_audits$DISTRICT_ID[i],
           DISTRICT_NAME = new_audits$DISTRICT_NAME[i],
           DISTRICT_URL = new_audits$DOC_URL[i],DATE_SUBMITTED = new_audits$DATE_SUBMITTED[i]))
new_audit_df <- invisible(Reduce(full_join,audit_list))
for (c in colnames(new_audit_df))
{new_audit_df[[c]] <- as.character(new_audit_df[[c]])}}


if(nrow(new_audits)!=0){
if(nrow(audit_df)==0)
{audit_df = new_audits}
if(nrow(audit_df!=0))
{audit_df = full_join(audit_df,new_audit_df)}
}
write_csv(audit_df,paste('scraped_input/tceq_audits/district_audits.csv',sep='_'))

require(data.table)

