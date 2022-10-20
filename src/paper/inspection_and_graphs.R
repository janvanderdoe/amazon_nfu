library(tidyverse)

data_usa <- read.csv("../../gen/output/amazon_usa_clean.csv")
#overview table

data_usa %>% group_by(variant) %>% summarize(number_reviews =  n(), perc_variant= n()/count(data_usa), pronoun_length = mean(pronoun_length, na.rm=TRUE), length_review = mean(length), , number_skus = n_distinct(asin_url), mean_rating = mean(rating_float, na.rm = TRUE), price = mean(price_new, na.rm = TRUE)/100, out_of_stock = mean(oos_new, na.rm = TRUE), renewed = mean(renewed == "yes"), mean(spec_mentions)) %>% 
  arrange(desc(perc_variant))

#NAs
data_usa %>% summarise_all(~ sum(is.na(.)))

#Correlation matrix uniquenesses
cor(data_usa %>% select(uniqueness_brand,uniqueness_variant_parent_sku, uniqueness_sku, uniqueness_variant), use = "complete.obs")

#Variant trend
data %>%
  #mutate(date_lub = dmy(date_trans_correct)) %>% 
  count(variant, month = floor_date(date_trans_correct , "month")) %>% 
  ggplot(aes(month, n, color=variant)) +
  geom_line() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n'%y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

#Variant by brand
data_usa %>% ggplot(aes(x=brand_overall,fill=variant)) +
  geom_bar(stat = "count", position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#Variant rarity graph
data_usa %>% ggplot(aes(uniqueness_brand,uniqueness_sku, color=brand_overall)) +
  geom_point() +
  scale_x_log10()
##By review
data %>% count(variant) %>% arrange(desc(n)) %>% ggplot(aes(variant, n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ggtitle("Variant by review")

##By SKU
data %>% group_by(variant) %>% summarize(SKUs = n_distinct(asin_url)) %>% arrange(desc(SKUs)) %>% ggplot(aes(variant, SKUs)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ggtitle("Variant by SKU")

#uSA
data %>% group_by(country) %>% summarize(uniqueness = mean(uniqueness_all), n(), number_skus = n_distinct(asin_url)) %>% arrange(desc(uniqueness))

data %>% group_by(country) %>% count() %>% ggplot(aes(country, n)) +
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90, hjust=1))
#Brand

data %>% group_by(brand) %>% summarize(n = n(), share = n()/count(data),perc_renewed = mean(renewed=="yes"), price = mean(price90, na.rm = TRUE)) %>% arrange(desc(n)) %>% print(n = Inf)

data %>% group_by(brand) %>% count() %>% top_n(10) %>% mutate(brand = fct_reorder(brand, desc(n))) %>% ggplot(aes(brand, n)) +
  geom_col()

#PRONOUN LENGTH
theme_set(theme_bw(16))
##By variant
data_usa %>% group_by(variant) %>% summarize(pronoun_length = mean(pronoun_length)) %>% mutate(variant = fct_reorder(variant, desc(pronoun_length))) %>% ggplot(aes(variant,pronoun_length)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))