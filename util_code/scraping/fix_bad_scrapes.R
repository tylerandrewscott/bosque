setwd('Google Drive/bosque/')
library(tidyverse)

master_ref = read_csv('input/tceq_audits/master_financial_doc_reference.csv')

a = read_csv('input/tceq_audits/audit_doc_scrapes.csv') %>% select(-`X1`,-YEAR) %>%
  filter(DOC_ID %in% master_ref$DOC_ID) %>%
  rename(`OTHER TAX RATE(S)` = `OTHER TAX RATES(S)`)

ar = read_csv('input/tceq_audits/audit_report_doc_scrapes.csv') %>% 
  select(-`X1`,-`FISCAL YEAR`) %>% filter(DOC_ID %in% master_ref$DOC_ID) %>%
  rename(`WASTEWATER CUST - EQ SINGLE FAMILY UNITS` = `WASTEWATER CUST - EQ SINGLE FAMILY UNIT`)

a$SYSTEM_NAME = master_ref$`District Name`[match(a$DOC_ID,master_ref$DOC_ID)]
a$DOC_URL = master_ref$DOC_URL[match(a$DOC_ID,master_ref$DOC_ID)]
ar$SYSTEM_NAME = master_ref$`District Name`[match(ar$DOC_ID,master_ref$DOC_ID)]
ar$DOC_URL = master_ref$DOC_URL[match(ar$DOC_ID,master_ref$DOC_ID)]

aud = full_join(a,ar) %>% filter(!duplicated(.))

aud$SYSTEM_NAME[aud$DOC_ID==378102]


write_csv(aud,'input/tceq_audits/yearly_district_audits.csv')

