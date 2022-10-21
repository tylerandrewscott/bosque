#!usr/bin/env python

import time


from selenium import webdriver
from bs4 import BeautifulSoup
import pandas as pd
from selenium import webdriver
from selenium.webdriver import Chrome
from selenium.webdriver.chrome.options import Options

chrome_options = Options()
chrome_options.add_argument('--dns-prefetch-disable')
#driver = Chrome(chrome_options=options)
phantom_options = Options()
phantom_options.add_argument('--dns-prefetch-disable')
empty_dww = pd.DataFrame()
empty_district_info = pd.DataFrame()
empty_audit_info = pd.DataFrame()
#driver = webdriver.Chrome(chrome_options=chrome_options)  # Optional argument, if not specified will search path.
driver = webdriver.PhantomJS()
base_page = 'http://www14.tceq.texas.gov/iwud/dist/index.cfm?fuseaction=ListDistricts&COMMAND=LIST&compress=N&StartName=&ID=&RegionCode=&DistTypeCode=&CreationTypeCode=&DistFunctionTypeCode=&CountyCode=&FinancialStatus=&ActivityStatus=&ListStart='
driver.get(base_page)
#CountyCode
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as EC

wd_info_call = '.iwuddata:nth-child(2)'
wd_info_header = '.iwudheading:nth-child(1)'
#for county in houston_counties:
count = 0
#select_county = Select(driver.find_element_by_css_selector('#CountyCode'))
#select_county.select_by_value(str(8))
#select_status = Select(driver.find_element_by_css_selector('#ActivityStatus'))
#select_status.select_by_value(str('A'))
driver.find_elements_by_css_selector('input')[1].click()
css_find_wds = "table+ table .iwud a"
css_find_number = '.iwud+ .iwud:nth-child(3)'
css_find_status = '.iwud~ .iwud+ .iwud'
css_to_click_more = "table+ table tr:nth-child(1) a:nth-child(1) img"
audit_vars_css = 'td.iwud:nth-child(1)'
audit_vals_css = '.iwud+ td'
get_doc_id = 'table:nth-child(6) tr:nth-child(1) .iwuddata'

System = [];Status = []; ID = []
#### Grab basic district info
count = 0

for system in range(9000):
    #print('system' + str(system))
    temp_wd = driver.find_elements_by_css_selector(css_find_wds)
    temp_status = driver.find_elements_by_css_selector(css_find_status)
    temp_number = driver.find_elements_by_css_selector(css_find_number)
    System.append(temp_wd[count].text)
    Status.append(temp_status[count].text)
    ID.append(temp_number[count].text)
    water_systems = driver.find_elements_by_css_selector(css_find_wds)
    #for systems in range(len(water_systems)):
    temp_name = driver.find_elements_by_css_selector(css_find_wds)[count].text
    print(temp_name)
    temp_id = driver.find_elements_by_css_selector(css_find_number)[count].text
    driver.find_elements_by_css_selector(css_find_wds)[count].click()
    info_vars = []
    info = []
    for el in driver.find_elements_by_css_selector(wd_info_call):
        info.append(el.text)
    for el in driver.find_elements_by_css_selector(wd_info_header):
        info_vars.append(el.text)
    i = 0
    for v in range(len(info_vars)):
        if info_vars[v] == '':
            i = i + 1
            info_vars[v] = str(i)
            #print(info_vars);print(info)
    #print(info_vars)
    #print(info)
    temp = pd.DataFrame(info).T
    temp.columns = [info_vars]
    temp['NAME'] = temp_name
    temp['WDD_ID'] = temp_id
    #for dup in range(sum(temp.columns.duplicated())):
    while sum(temp.columns.duplicated())>0:
        temp = temp.drop(temp.columns[temp.columns.duplicated()][0],axis=1)
        #temp = temp.drop('Address:', axis=1)
        #temp = temp.columns.drop_duplicates()
        #print(temp_info)
        #if empty_dww.shape[0] !=0 & any(empty_dww['Name'] == temp_name):
        #    driver.back()
        #    continue  join(temp_info,how='outer')
        #print(temp.columns)
    empty_district_info = pd.concat([empty_district_info,temp], axis=0, join='outer', ignore_index=True)
    empty_district_info.to_csv('../../Input/tceq_audits/system_info_detail.csv')
        #empty_district_info = empty_district_info.append(temp,how='outer')
        #empty_district_info =  = empty_district_info.append(temp)
        #grouped = empty_district_info.groupby(level=0)
        #empty_district_info = grouped.last()
    time.sleep(.25)
    driver.back()
    if len(temp_wd)<49 & count == len(temp_wd):
        break
    if count == 49:
        driver.find_element_by_css_selector(css_to_click_more).click()
        count = 0
        continue
    count = count + 1
#system_list_df = pd.DataFrame({'Name': System, 'Status': Status,'ID': ID,'County': 'AUSTIN'})
#system_list_df.to_csv('../Input/tceq_dww/system_info_basic.csv')
