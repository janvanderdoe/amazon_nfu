library(tidyverse)
library(readxl)
library(lubridate)
library(lme4)

Sys.setlocale(category = 'LC_ALL','English_United States.1250')

data <- read.csv("../../gen/temp/amazon_complete_dataset_0_1000.csv", sep = ";")
colors <- read_excel("../../data/opvallendheid_kleuren_smartphone.xlsx")

#Data cleaning

data[data == 'none'] <- NA
data[data == -1] <- NA
data$helpful[is.na(data$helpful)] <- 0

#Transformation

data$date_trans_correct <- as.Date(data$date_trans, "%d-%m-%Y")

data['rating_float'] <- as.numeric(unlist(data['rating_float']))

#Adding columns

data$usa <- ifelse(data$country == "the United States", 1, 0)

data_usa <- data %>% filter(usa == 1)
data_usa$helpful <- as.numeric(data_usa$helpful)
#UNIQUENESS VARIABLES
##Uniquen
uniq_by_variant_overall <- data_usa %>% group_by(variant) %>% summarize(uniq_variant_overall = n()/count(data_usa))
data_usa <- merge(data_usa, uniq_by_variant_overall, by = "variant")
data_usa['uniq_variant_overall'] <- unlist(data_usa['uniq_variant_overall'])

##uniqueness_brand_overall
uniq_by_brand_overall <- data_usa %>% group_by(brand_overall) %>% summarize(uniq_brand_overall = n()/count(data_usa))
uniq_by_brand_overall['uniq_brand_overall'] <- unlist(uniq_by_brand_overall['uniq_brand_overall'])
data_usa <-  data_usa %>% left_join(uniq_by_brand_overall, by = "brand_overall")

##uniqueness sku_parent_sku
uniq_by_parent <- data_usa %>% group_by(parent_asin) %>% count()
names(uniq_by_parent) <- c("parent_asin", "uniq_parent")
data_usa <- data_usa %>% left_join(uniq_by_parent, by = "parent_asin")
uniq_by_sku_parent <- data_usa %>% group_by(version_asin) %>% summarize(uniq_sku_parent = n()/mean(uniq_parent)) %>% filter(version_asin != "")
data_usa <- data_usa %>% left_join(uniq_by_sku_parent, by = "version_asin")
data_usa$uniq_sku_parent <- replace(data_usa$uniq_sku_parent, which(data_usa$uniq_sku_parent <0), NA)

#uniqueness _variant_parent_sku
uniq_by_variant_parent <- data_usa %>% group_by(variant, parent_asin) %>% summarize(uniq_variant_parent = n()/mean(uniq_parent))
data_usa <- data_usa %>% left_join(uniq_by_variant_parent, by = c("variant", "parent_asin"))

##Uniqueness variant_brand
count_brand_variant <- data_usa %>% group_by(brand_overall, variant) %>% summarize(brand_variant_count = n())
count_brand <- data_usa %>% group_by(brand_overall) %>% summarize(brand_count = n())
uniq_by_variant_brand <- count_brand_variant %>% left_join(count_brand, by = "brand_overall") %>% mutate(uniq_variant_brand = brand_variant_count/brand_count) %>% select(c(1,2,5))
data_usa <- data_usa %>% left_join(uniq_by_variant_brand, by=c("brand_overall", "variant"))

#Variants
count_variants <- data_usa %>% group_by(parent_asin) %>% distinct(variant) %>% count() %>% rename(count_variant_parent = n)
data_usa <- data_usa %>% left_join(count_variants, by = "parent_asin")

#colors
data_usa <- data_usa %>% left_join(colors, by = "variant")

##time since first review
oldest_review <- data_usa %>% group_by(version_asin) %>% summarize(oldest_review = min(date_trans_correct))
data_usa <- data_usa %>% left_join(oldest_review, by="version_asin")

data_usa <- data_usa %>% mutate(time_since_oldest_review = date_trans_correct - oldest_review)

data_usa <- data_usa %>% select(-c(X, Unnamed..0, usa, date_trans))
data_usa <- data_usa %>% mutate_all(na_if, "")
write.csv(data_usa, "../../gen/output/amazon_usa_clean.csv")
