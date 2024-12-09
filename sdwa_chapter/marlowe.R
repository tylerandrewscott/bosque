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

names(merritt_data)
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


names(merritt_data)
merritt_data |>
  mutate(operating.margin = Total.Water.Sales.Operating.Rev + Total.Sewer.Sales.Operating.Rev - )

merritt_data$
p2 <- ggplot(merritt_data, aes(x=log10({Long.Term.Debt+1}), y=log10(Total.Assets+1))) +
  geom_point(alpha=0.25,pch = 19,fill = NA) +
  labs(x="Long Term Debt ($)", y="Total Assets ($)") +
  theme_minimal() +
  ggtitle("Water and Sewer Utilities: Long Term Debt vs Total Assets") +
  scale_x_log10(labels = function(x) round(10^x,2)) +
  scale_y_log10(labels = function(y) round(10^y,2))

ggsave("sdwa_chapter/output/debt_assets_scatter.png", p2, width=6,height = 6, units="in", dpi=450)




table(merritt_data$Year)
table(merritt_data$Current.Liabilities==0)
summary(merritt_data$Quick.Ratio)

summary(merritt_data$Quick.Ratio)
summary(merritt_data$Current.Liabilities)