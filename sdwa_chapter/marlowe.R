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


merritt_data$Quick.Ratio <-  {merritt_data$Cash...Short.Term.Investments +
                merritt_data$Net.Accounts.Receivable}/merritt_data$Current.Liabilities

#filter out quick ratio that has ablsolute value greater than 10
merritt_data |> filter(abs(Quick.Ratio)<10) -> merritt_data

# Convert fiscal year end dates to year using lubridate
library(lubridate)
merritt_data$Year <- year(mdy(merritt_data$Fiscal.Year.End))
library(ggridges)


p <- ggplot(merritt_data, aes(x=Quick.Ratio, y=factor(Year))) +
  geom_density_ridges(alpha=0.5) +
  labs(x="Quick Ratio", y="Year") +
  theme_minimal() +
  ggtitle("Water and Sewer Utilities: Quick Ratio Distribution by Year, 2012-2021")

ggsave("sdwa_chapter/output/quick_ratio_dist.png", p, width=6, units="in", dpi=450)


log10(0)
summary(merritt_data$Total.Assets)
summary(merritt_data$Long.Term.Debt)
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