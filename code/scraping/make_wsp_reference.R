library(rvest)
system_links = 'http://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county=All&WaterSystemType=C&SourceWaterType=All&SampleType=null&begin_date=1%2F1%2F2000&end_date=8%2F23%2F2016&action=Search+For+Water+Systems'
temp = read_html(system_links)
table_tag = temp %>% html_nodes('body') %>% html_nodes('table') 
tt = html_table(table_tag[[3]],fill=TRUE,header=T)
tt$`Water System Name` = gsub(' Fact.*','',tt$`Water System Name`)
prefix = 'http://dww2.tceq.texas.gov/DWW/JSP/'
links = temp %>% html_nodes("a") %>% html_attr("href")
links = grep('WaterSystemDetail',links,value=T)
tt$System_Detail_Page = paste0(prefix,links)
tt$System_Detail_Page = gsub(' ','',tt$System_Detail_Page)
write.csv(tt,'input/wsp_base_reference.csv')


test = read_html(tt$System_Detail_Page[1])
head(tt)
head(tt$`Water System No.`)
library(dplyr)
test = tt[1:10,] %>% rowwise() %>% do(as.data.frame(population_served = read_html(.$System_Detail_Page) %>% 
                                        html_nodes('br+ table tr:nth-child(6) td:nth-child(2) p') %>%
                                        html_text(trim=T),
                                        contact = read_html(.$System_Detail_Page) %>% 
                                          html_nodes('table:nth-child(9) tr~ tr+ tr td+ td') %>%
                                          html_text(trim=T)))

tt$System_Detail_Page[4]
test = read_html(tt$System_Detail_Page[4]) %>% html_text()
library(stringr)


str_extract(test,"(?:[A-Z]{1,}), TX.*[0-9]{5}-[0-9]{0,4}")
str_extract()

class(test)

  
  html_nodes('table:nth-child(9) tr~ tr+ tr td:nth-child(2)') 

  html_text(trim=T))


head(test)

head(tt)

tt$System_Detail_Page[4]
df <- expand.grid(x = 1:3, y = 3:1)
df %>% rowwise() %>% do(i = seq(.$x, .$y))
.Last.value %>% summarise(n = length(i))
.Last.value

plyr::adply(.data = tt, .margins=1,.fun = function(x) {read_html()} read_html(x=))

test = plyr::ddply(tt[1:10,],"System_Detail_Page",function(x) {read_html(x) %>% 
        html_nodes('br+ table tr:nth-child(6) td:nth-child(2) p') %>% html_text(trim=T)})



dd = test %>% html_nodes('br+ table tr:nth-child(6) td:nth-child(2) p') %>% html_text(trim=T)

dd
?html_text
dd[1]
gsub(' ','',tt$System_Detail_Page)




