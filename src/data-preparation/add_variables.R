library(tidyverse)
library(readxl)
library(lubridate)
library(lme4)

data <- read.csv("../../gen/temp/amazon_complete_dataset_0_1000.csv", sep = ";")
keepa <- read_excel("../../data/KeepaExport_all_variants.xlsx", range = cell_cols("K:M"))
colors <- read_excel("../../data/opvallendheid_kleuren_smartphone.xlsx")
keepa <- keepa[,c(1,3)]

#Data cleaning

data[data == 'none'] <- NA
data[data == -1] <- NA
data$helpful[is.na(data$helpful)] <- 0

#Merge datasets
data <- data %>% left_join(keepa, by = c("version_asin" =  "ASIN"))
#Adding columns

data$date_trans_correct <- as.Date(data$date_trans, "%d-%m-%Y")

data['rating_float'] <- as.numeric(unlist(data['rating_float']))

uniqueness <- data %>% group_by(variant) %>% summarize(uniqueness = 1- n()/count(data))

data <- merge(data, uniqueness, by = "variant")

data['uniqueness_all'] <- unlist(data['uniqueness'])

#data$iphone <- ifelse(data$Brand == "Apple", 1, 0)

data$usa <- ifelse(data$country == "the United States", 1, 0)

#data <- data %>% mutate(pronoun_length = pronoun/length)

data_usa <- data %>% filter(usa == 1)

uniqueness_by_variant <- data_usa %>% group_by(variant) %>% summarize(uniqueness_variant = 1- n()/count(data_usa))

data_usa <- merge(data_usa, uniqueness_by_variant, by = "variant")

data_usa['uniqueness_variant'] <- unlist(data_usa['uniqueness_variant'])
data_usa['uniqueness'] <- unlist(data_usa['uniqueness'])

##uniqueness_brand_overall
uniqueness_by_brand <- data_usa %>% group_by(brand_overall) %>% summarize(uniqueness_brand = n()/count(data_usa))
names(uniqueness_by_brand) <- c('brand_overall', 'uniqueness_brand')
data_usa <-  data_usa %>% left_join(uniqueness_by_brand, by = "brand_overall")
data_usa['uniqueness_brand'] <- unlist(data_usa['uniqueness_brand'])

##uniqueness sku_parent_sku
uniqueness_by_parent <- data_usa %>% group_by(parent_asin) %>% count()
names(uniqueness_by_parent) <- c("parent_asin", "uniqueness_parent")
data_usa <- data_usa %>% left_join(uniqueness_by_parent, by = "parent_asin")
uniqueness_by_sku <- data_usa %>% group_by(version_asin) %>% summarize(uniqueness_sku = 1-n()/mean(uniqueness_parent))
data_usa <- data_usa %>% left_join(uniqueness_by_sku, by = "version_asin")
data_usa$uniqueness_sku <- replace(data_usa$uniqueness_sku, which(data_usa$uniqueness_sku <0), NA)

#uniqueness _variant_parent_sku
uniqueness_by_variant_parent_sku <- data_usa %>% group_by(variant, parent_asin) %>% summarize(uniqueness_variant_parent_sku = n()/mean(uniqueness_parent))
data_usa <- data_usa %>% left_join(uniqueness_by_variant_parent_sku, by = c("variant", "parent_asin"))

##Uniqueness variant_brand
count_brand_variant <- data_usa %>% group_by(brand_overall, variant) %>% summarize(brand_variant_count = n())
count_brand <- data_usa %>% group_by(brand_overall) %>% summarize(brand_count = n())
uniqueness_brand_variant <- count_brand_variant %>% left_join(count_brand, by = "brand_overall") %>% mutate(uniqueness_brand_variant = brand_variant_count/brand_count) %>% select(c(1,2,5))
data_usa <- data_usa %>% left_join(uniqueness_brand_variant, by=c("brand_overall", "variant"))

#Variants
count_variants <- data_usa %>% group_by(parent_asin) %>% distinct(variant) %>% count() %>% rename(count_variant_parent = n)
data_usa <- data_usa %>% left_join(count_variants, by = "parent_asin")

##colors
data_usa <- data_usa %>% left_join(colors, by = "variant")

data_usa_iphone <- data_usa %>% filter(iphone == 1)

data_usa$helpful <- as.numeric(data_usa$helpful)

##time since first review
oldest_review <- data_usa %>% group_by(version_asin) %>% summarize(oldest_review = min(date_trans_correct))
data_usa <- data_usa %>% left_join(oldest_review, by="version_asin")

data_usa <- data_usa %>% mutate(time_since_oldest_review = date_trans_correct - oldest_review)

write.csv(data_usa, "../../gen/output/amazon_usa_clean.csv", sep = ";")