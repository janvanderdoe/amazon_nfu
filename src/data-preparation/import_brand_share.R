library(tidyverse)
library(janitor)
library(tibble)

#Retrieve names csv files with shares (it's per year)
temp = list.files(pattern="*.csv")

#Save as individual objects
for (i in 1:length(temp)) assign(paste("data", i, sep="."), read.csv(temp[i]))
data_brands <- list(data.1, data.2, data.3, data.4, data.5, data.6, data.7, data.8, data.9, data.10, data.11, data.12, data.13)

#Transpose
for (i in 1:length(data_brands)) assign(paste("data", i, sep="."), t(as.data.frame(data_brands[i])))
data_brands <- list(data.1, data.2, data.3, data.4, data.5, data.6, data.7, data.8, data.9, data.10, data.11, data.12, data.13)

#Use dates as columns
for (i in 1:length(data_brands)) assign(paste("data", i, sep="."), as.data.frame(data_brands[i]) %>% row_to_names(row_number = 1))
data_brands <- list(data.1, data.2, data.3, data.4, data.5, data.6, data.7, data.8, data.9, data.10, data.11, data.12, data.13)

#Brands are as rownames. Make it a separate column instead
for (i in 1:length(data_brands)) assign(paste("data", i, sep="."), tibble::rownames_to_column(as.data.frame(data_brands[i]),"brand"))
data_brands <- list(data.1, data.2, data.3, data.4, data.5, data.6, data.7, data.8, data.9, data.10, data.11, data.12, data.13)

#Merge all 13 files together
brand_merged <- as.data.frame(Reduce(function(x,y) merge(x,y, by="brand", all=TRUE), data_brands))

#Clean column names with dates
names(brand_merged) <- gsub("X", "", names(brand_merged))
names(brand_merged) <- gsub(".x", "", names(brand_merged))
brand_merged <- brand_merged %>% select(-contains(".y"))

#Make it longer to make it suitable for merger
brand_merged_long <- brand_merged %>% pivot_longer(cols = !brand, names_to = "date", values_to = "market_share", values_drop_na = TRUE)

#Transformation
brand_merged_long$market_share <- as.numeric(brand_merged_long$market_share)
brand_merged_long$date_trans <- as.Date(paste0(brand_merged_long$date,".15"), "%Y.%m.%d")
brand_merged_long$brand <- tolower(brand_merged_long$brand)

#Anything before April 2010 is 0, so remove
brand_merged_long <- brand_merged_long %>% filter(date_trans >= as.Date("2010-04-01", "%Y-%m-%d"))

#inspection
brand_merged_long %>% ggplot(aes(date_trans, market_share, color=brand)) +
  geom_line() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n'%y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ggtitle("Brand share development") +
  xlab("Time")

#Saving
write.csv(brand_merged_long, "../../gen/temp/brand_shares.csv")
