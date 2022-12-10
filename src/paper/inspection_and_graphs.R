library(tidyverse)
library(lubridate)

theme_set(theme_bw(16))

data_usa <- read.csv("../../gen/temp/data_overall2.csv", sep = ";")

data_usa$market_share_with_zero <- data_usa$market_share
data_usa$market_share_with_zero[is.na(data_usa$market_share_with_zero)] <- 0

data_usa %>% group_by(median_variant_parent_time) %>% summarize(mean(diff_rating_abs, na.rm = TRUE))
#overview table

data_usa %>% group_by(variant) %>% summarize(number_reviews =  n(), perc_variant= n()/count(data_usa), pronoun_length = mean(pronoun_length, na.rm=TRUE), length_review = mean(length), , number_skus = n_distinct(asin_url), mean_rating = mean(rating_float, na.rm = TRUE), price = mean(price_new, na.rm = TRUE)/100, out_of_stock = mean(oos_new, na.rm = TRUE), renewed = mean(renewed == "yes"), mean(spec_mentions)) %>% 
  arrange(desc(perc_variant))

#Brands dataset overview
data_usa %>% group_by(brand_overall) %>% count() %>% arrange(desc(n)) %>% ggplot(aes(brand_overall, n)) +
  geom_segment( aes(x=brand_overall, xend=brand_overall, y=0, yend=n), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("Brand") +
  ylab("Number of reviews")

#variant variation by brand
data_usa %>%  group_by(brand_overall, median_variant_parent) %>% count() %>%
  pivot_wider(names_from = median_variant_parent, values_from = n) %>%
  mutate(ratio = `high NFU`/(`low NFU`+`high NFU`)) %>% print(n=Inf)

#NAs
data_usa %>% summarise_all(~ sum(is.na(.)))

#Deviation from mean
data_usa %>% ggplot(aes(uniq_color_parent_time, diff_rating_abs)) +
  geom_point() +
  geom_smooth()

data_usa %>% ggplot(aes(market_share, diff_rating_abs)) +
  geom_point() +
  geom_smooth()

#Main effects plot
data_usa %>% ggplot() +
  geom_bar(aes(median_color_parent_time, diff_rating_abs), position = "dodge", stat = "summary", fun.y = "mean") +
  facet_grid(~median_market_share) +
  facet_grid(~median_market_share) 
  xlab("uniqueness of color") +
  ylab("absolute difference in rating") +
  geom_text(aes(label = diff_rating_abs), vjust = -0.2)


data_usa %>% ggplot() +
  geom_bar(aes(median_color_parent_time, diff_rating_abs), position = "dodge", stat = "summary", fun.y = "mean")+
  geom_label()

#Rating distribution
data_usa %>% ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.5)

data_usa %>% ggplot(aes(diff_rating_abs)) +
  geom_histogram(binwidth = 0.2)

data_usa %>% ggplot(aes(diff_rating)) +
  geom_boxplot()

data_usa %>% ggplot(aes(as.character(rating), diff_rating)) +
  geom_boxplot()

#Variant trend
data_usa %>% filter(variant %in% c('black', 'blue', 'gray', 'gold', 'white')) %>% 
  #mutate(date_lub = dmy(date_trans_correct)) %>% 
  count(variant, month = floor_date(date_trans_correct , "month")) %>% 
  ggplot(aes(month, n, color=variant)) +
  geom_line() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n'%y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ggtitle("Development of top 5 color popularity") +
  xlab("Number of reviews")

#Variant by brand
data_usa %>% ggplot(aes(x=brand_overall,fill=variant)) +
  geom_bar(stat = "count", position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

##By review
data_usa %>% count(variant) %>% arrange(desc(n)) %>% ggplot(aes(variant, n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ggtitle("Variant by review")

##By SKU
data_usa %>% group_by(variant) %>% summarize(SKUs = n_distinct(asin_url)) %>% arrange(desc(SKUs)) %>% ggplot(aes(variant, SKUs)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ggtitle("Variant by SKU")

#Brand
data_usa %>% group_by(brand_overall) %>% summarize(n = n(), share = n()/count(data),perc_renewed = mean(renewed=="yes"), price = mean(price90, na.rm = TRUE)) %>% arrange(desc(n)) %>% print(n = Inf)

data %>% group_by(brand_overall) %>% count() %>% top_n(10) %>% mutate(brand_overall = fct_reorder(brand_overall, desc(n))) %>% ggplot(aes(brand_overall, n)) +
  geom_col()

#Main effects
data_usa <- data_usa %>% left_join(
  data_usa %>% group_by(median_color_parent_time, median_market_share) %>% summarize(sd(diff_rating_abs, na.rm = TRUE)),
  by = c("median_color_parent_time", "median_market_share"))

data_usa <- data_usa %>% rename(sd = "sd(diff_rating_abs, na.rm = TRUE)")

ggplot(data_usa) +
  geom_bar( aes(x=median_market_share, y=diff_rating_abs), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=median_market_share, ymin=diff_rating_abs-sd, ymax=diff_rating_abs+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)
