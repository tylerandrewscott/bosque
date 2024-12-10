library(tidyverse)
# Read in Merritt Data Utilities text file from input subdirectory
merritt_data <- read.delim("sdwa_chapter/input/Merritt Data - Utilities.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

merritt_data |> filter(Credit.Sector=='Water / Sewer',!is.na(Current.Liabilities),!is.na(Cash...Short.Term.Investments),!is.na(Net.Accounts.Receivable)) -> merritt_data
#merritt_data$Cash...Short.Term.Investments[is.na(merritt_data$Cash...Short.Term.Investments)] <- 0
#merritt_data$Net.Accounts.Receivable[is.na(merritt_data$Net.Accounts.Receivable)] <- 0
#merritt_data$Current.Liabilities[is.na(merritt_data$Current.Liabilities)] <- 0
# Calculate quick ratio 

# for now, filter out any cases where current.liabilities = 0
merritt_data |> filter(Current.Liabilities!=0) -> merritt_data

merritt_data |> mutate_if(is.numeric,function(x) x * 1e3) -> merritt_data

merritt_data$Quick.Ratio <-  {merritt_data$Cash...Short.Term.Investments +
                merritt_data$Net.Accounts.Receivable}/merritt_data$Current.Liabilities

#filter out quick ratio that has ablsolute value greater than 10
merritt_data |> filter(abs(Quick.Ratio)<10) -> merritt_data

# Convert fiscal year end dates to year using lubridate
library(lubridate)
merritt_data$Year <- year(mdy(merritt_data$Fiscal.Year.End))
library(ggridges)


qr_over_1 = merritt_data |>
  group_by(Year) |>
  summarise(over1 = paste0(round(mean(Quick.Ratio>1) * 100,1),'% > 1'))

library(forcats)
p <- merritt_data |> 
  ggplot(aes(x=Quick.Ratio, y=fct_rev(factor(Year)),fill = as.factor(after_stat(x) > 1))) +
  geom_density_ridges_gradient(alpha=0.5) +
  labs(x="Quick Ratio for water/sewer utilities", y="Year") +
  geom_vline(xintercept = 1,lty = 2,colour = 'grey20') +
  theme_minimal() +
  scale_fill_manual(values = c('#E15759','#79706E')) + 
  annotate('text',size = 3,x = 2.3,color = 'white',y = .75 + as.numeric(fct_rev(as.factor(sort(unique(merritt_data$Year))))),label = qr_over_1$over1) + 
  ggtitle("Quick Ratio Distribution by Year, 2012-2021") + 
  guides(fill = 'none')
p
ggsave("sdwa_chapter/output/quick_ratio_dist.png", p, width=6, units="in", dpi=450)
library(data.table)


merritt_data <- merritt_data |>
  mutate(operating.ratio = Total.Operating.Rev / Total.Operating.Exp)

p2 <- merritt_data |> 
  ggplot(aes(y = operating.ratio,
             x = Total.Operating.Exp+1,
             color = operating.ratio>1)) + 
  theme_bw() + 
  ggtitle("Operating Ratio Distribution by Year, 2012-2021") + 
  scale_x_log10(name = 'Operating Exp. ($)',
           breaks = c(1e5,1e7,1e9), 
          labels = c('$100k','$10M','$1B')) + 
  geom_point(pch = 19,alpha = 0.25) +
  scale_color_manual(values = c('#E15759','grey30')) + 
  geom_hline(yintercept = 1,lty = 2,col = 'grey50')+
  guides(color = 'none')+
  scale_y_continuous(limits = c(NA,6),name = 'Operating Revenue/Expenses')


ggsave("sdwa_chapter/output/medterm_operating_ratio.png", p2, width=6, units="in", dpi=450)
library(data.table)
p3 <- ggplot(merritt_data, aes(y={Long.Term.Debt+1}/Total.Assets, x=Total.Assets,col = 1 > {Long.Term.Debt+1}/Total.Assets )) +
  geom_point(alpha=0.15,pch = 19) +
  theme_bw() +
  scale_color_manual(values = c('#E15759','grey30')) + 
  guides(colour = 'none') + 
  ggtitle("Debt to Assets by Year, 2012-2021") +
  scale_x_log10(name = 'Total Assets ($)',
                labels = c('$1M','$100M','$10B'),
                breaks = c(1e6,1e8,1e10)) +
  scale_y_continuous(name = 'Total Debt/Total Assets')

ggsave("sdwa_chapter/output/debt_assets_scatter.png", p3, width=6,units="in", dpi=450)


mean(1 < merritt_data$Long.Term.Debt/merritt_data$Total.Assets,na.rm = T)
merritt_data$Operating.Ratio <- (merritt_data$Total.Operating.Rev/merritt_data$Total.Operating.Exp)



summary(merritt_data$Long.Term.Debt)
table(is.na(merritt_data$Total.Operating.Exp),is.na(merritt_data$Total.Water.Sales.Operating.Rev))
merritt_data |> filter()
table(merritt_data$Year)
table(merritt_data$Current.Liabilities==0)
summary(merritt_data$Quick.Ratio)

summary(merritt_data$Quick.Ratio)
summary(merritt_data$Current.Liabilities)