library(tidyverse)
library(readxl)
library(lubridate)
library(lme4)

data_usa <- read.csv("../../gen/temp/data_overall2.csv", sep = ";")

brands <- read.csv("../../gen/output/brand_corrected.csv", sep = ";")

#Transforming variable
data_usa$date_trans <- as.Date(data_usa$date_trans)

data_usa$iphone <- ifelse(data_usa$brand_overall == "apple", 1, 0)

data_usa$listed_since_date <- as.Date(as.POSIXct((data_usa$listed_since+21564000)*60, origin="1970-01-01"))

data_usa$list_time_days <- as.numeric(data_usa$date_trans - data_usa$listed_since_date)

#Merging brand dataset
data_usa <- data_usa %>%
  mutate(month = floor_date(as.Date(date_trans), "month"), brand = brand_overall) %>%
  left_join(brands %>% mutate(month = floor_date(as.Date(date_trans), "month")) %>% select(month, brand, market_share), by = c("month", "brand")) %>%
  select(!c(brand, month, date))

data_usa$market_share_with_zero <- data_usa$market_share
data_usa$market_share_with_zero[is.na(data_usa$market_share_with_zero)] <- 0

#Adding variables
data_usa$diff_rating_abs <- abs(data_usa$diff_rating)

#uniqueness _variant_parent_sku
uniq_by_parent <- data_usa %>% group_by(asin_from_link) %>% count()
names(uniq_by_parent) <- c("asin_from_link", "uniq_parent")
data_usa <- data_usa %>% left_join(uniq_by_parent, by = "asin_from_link")

uniq_by_variant_parent <- data_usa %>% group_by(variant, asin_from_link) %>% summarize(uniq_variant_parent = n()/mean(uniq_parent))
data_usa <- data_usa %>% left_join(uniq_by_variant_parent, by = c("variant", "asin_from_link"))

##Uniqueness variant_brand
count_brand_variant <- data_usa %>% group_by(brand_overall, variant) %>% summarize(brand_variant_count = n())
count_brand <- data_usa %>% group_by(brand_overall) %>% summarize(brand_count = n())
uniq_by_variant_brand <- count_brand_variant %>% left_join(count_brand, by = "brand_overall") %>% mutate(uniq_variant_brand = brand_variant_count/brand_count) %>% select(c(1,2,5))
data_usa <- data_usa %>% left_join(uniq_by_variant_brand, by=c("brand_overall", "variant"))


##time since first review
oldest_review <- data_usa %>% group_by(asin) %>% summarize(oldest_review = min(date_trans_correct))
data_usa <- data_usa %>% left_join(oldest_review, by="asin")

data_usa <- data_usa %>% mutate(time_since_oldest_review = date_trans - oldest_review)

data_usa <- data_usa %>% select(-c(X, Unnamed..0, usa, date_trans))
data_usa <- data_usa %>% mutate_all(na_if, "")

#Median splits on main variables
data_usa$median_color_parent_time <- ifelse(data_usa$uniq_color_parent_time >= median(data_usa$uniq_color_parent_time), "low color uniqueness", "high color uniqueness")
data_usa$median_avg_rating <- ifelse(data_usa$avg_rating >= median(data_usa$avg_rating), "low avg", "high avg")
data_usa$median_market_share <- ifelse(data_usa$market_share_with_zero >= median(data_usa$market_share_with_zero, na.rm = TRUE), "low brand uniqueness", "high brand uniqueness")

#Divide uniqueness by 100 to make it on a scale from 0 to 1
data_usa$market_share_with_zero <- data_usa$market_share_with_zero/100
data_usa$market_share <- data_usa$market_share/100

#Mean centering 
data_usa$mc_market_share <- data_usa$market_share - mean(data_usa$market_share)
data_usa$mc_uniq_color <- data_usa$uniq_color_parent_time - mean(data_usa$uniq_color_parent_time)

#Take inverse
data_usa$inv_market_share_with_zero <- 1- data_usa$market_share_with_zero
data_usa$inv_uniq_color <- 1- data_usa$uniq_color_parent_time

#Mean center inverse
data_usa$inv_mc_market_share <- data_usa$inv_market_share_with_zero - mean(data_usa$inv_market_share_with_zero, na.rm = T)
data_usa$inv_mc_uniq_color <- data_usa$inv_uniq_color - mean(data_usa$inv_uniq_color)


#Mean centering alternative calculation
data_usa$inv_variant_parent <- 1- data_usa$uniq_variant_parent
data_usa$inv_mc_variant_parent <- data_usa$inv_variant_parent - mean(data_usa$inv_variant_parent, na.rm = T)

write.csv(data_usa, "../../gen/output/amazon_usa_clean.csv")
