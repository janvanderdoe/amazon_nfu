library(tidyverse)
library(lubridate)
library(hutils)
library(data.table)
library(corrr)
library(rcompanion)
library(car)

setwd("~/Git projects/amazon_nfu/src/paper")
data_usa <- read.csv("../../gen/output/amazon_usa_clean.csv", sep = ",", row.names = 1)

data_usa %>% group_by(median_variant_parent_time) %>% summarize(mean(diff_rating_abs, na.rm = TRUE))
#overview table

data_usa %>% group_by(variant) %>% summarize(number_reviews =  n(), perc_variant= n()/count(data_usa), pronoun_length = mean(pronoun_length, na.rm=TRUE), length_review = mean(length), , number_skus = n_distinct(asin_url), mean_rating = mean(rating_float, na.rm = TRUE), price = mean(price_new, na.rm = TRUE)/100, out_of_stock = mean(oos_new, na.rm = TRUE), renewed = mean(renewed == "yes"), mean(spec_mentions)) %>% 
  arrange(desc(perc_variant))


##Brands graph but with others category
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

count_data_usa <- data_usa %>% group_by(brand_overall) %>% count()
data_usa_dt <- data.table(brand = as.vector(count_data_usa$brand_overall), count = as.vector(count_data_usa$n))
brand_sum <- data_usa_dt %>% mutate_other("brand", var.weight = "count", n=5) %>% .[] %>% group_by(brand) %>% summarize(sum(count))
brand_sum_dt <- data.table(brand = firstup(as.vector(brand_sum$brand)), count = brand_sum$`sum(count)`)
brand_sum_dt$brand <- factor(brand_sum_dt$brand, levels = rev(c("Apple", "Samsung", "Google", "Motorola", "Blu ", "Other")))
brand_sum_dt %>% ggplot(aes(x=brand, count)) +
  geom_segment( aes(x=brand, xend=brand, y=0, yend=count), color="gray", size = 2) +
  geom_point( color="black", size=6, alpha=1) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold"),
    plot.title = element_text(hjust = 0.5, size=14,face="bold")
  ) +
  xlab("Brand") +
  ylab("Number of reviews") +
  ggtitle("Number of reviews for each brand")


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

f2 <- function(row_variant, row_date) nrow(data_usa[(data_usa['date'] <= row_date)&(data_usa['variant'] == row_variant),])/nrow(data_usa[(data_usa['date'] <= row_date),])

f1 <- function(variant) ifelse(variant %in% c('black', 'blue', 'gray', 'gold'), str_to_title(variant), "Others")
data_usa$top_color <- unlist(lapply(data_usa$variant, f1))
#Variant trend
data_usa$date <- as.Date(data_usa$date)
data_usa$topcolor <- ifelse(data_usa$variant )
data_usa %>% 
  filter(date >= as.Date("2010-01-01")) %>% 
  #mutate(date_lub = dmy(date_trans_correct)) %>% 
  arrange(desc(top_color)) %>% 
  count(top_color, month = floor_date(date , "year")) %>% 
  ggplot(aes(month, n, fill=top_color)) +
  geom_col(position = "fill") +
  scale_x_date(date_breaks = "12 months", date_labels = "%b\n'%y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust = 0.5, size=14,face="bold")) +
  ggtitle("Development of top 4 color popularity") +
  xlab("Year") +
  ylab("Number of reviews") +
  xlim(as.Date("2010-12-31"), as.Date("2022-12-31")) +
  scale_fill_grey(name = "Color") 

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

#deviation vs average rating
data_usa %>% ggplot(aes(rating, diff_rating_abs)) +
  geom_point() +
  geom_smooth()

data_usa %>% ggplot(aes(list_time_days, diff_rating_abs, color = median_color_parent_time)) +
  geom_smooth()

#Correlation matrix
data_usa %>% select(uniq_color_parent_time, market_share_with_zero) %>% correlate(use = "complete.obs")

#Normality check (but not necessary)
plotNormalHistogram(data_usa$price_new)

#Comparison of means
## Using facet wrap
data_usa %>% filter(!is.na(median_color_parent_time)) %>% group_by(median_market_share, median_color_parent_time) %>%
  summarise(mean.uniq = mean(diff_rating_abs, na.rm = TRUE),
            sd.uniq = sd(diff_rating_abs, na.rm = TRUE),
            n.uniq = n()) %>%
  mutate(se.uniq = sd.uniq / sqrt(n.uniq),
         lower.ci.uniq = mean.uniq - qt(1 - (0.05 / 2), n.uniq - 1) * se.uniq,
         upper.ci.uniq = mean.uniq + qt(1 - (0.05 / 2), n.uniq - 1) * se.uniq) %>% 
  ggplot(aes(median_color_parent_time, mean.uniq)) + geom_col(fill="gray", alpha=1) +
  facet_wrap(~median_market_share) +
  geom_errorbar( aes(x=median_color_parent_time, ymin=lower.ci.uniq, ymax=upper.ci.uniq), width=0.4, colour="black", alpha=0.8, size=0.8) +
  #facet_wrap(~median_color_parent_time) +
  geom_text(aes(label = round(mean.uniq, 3)), vjust = 2) +
  ylab("Absolute deviation rating") +
  theme(axis.ticks.x=element_blank())

##Using colors
data_usa %>% filter(!is.na(median_color_parent_time)) %>% group_by(median_market_share, median_color_parent_time) %>%
  summarise(mean.uniq = mean(diff_rating_abs, na.rm = TRUE),
            sd.uniq = sd(diff_rating_abs, na.rm = TRUE),
            n.uniq = n()) %>%
  mutate(se.uniq = sd.uniq / sqrt(n.uniq),
         lower.ci.uniq = mean.uniq - qt(1 - (0.05 / 2), n.uniq - 1) * se.uniq,
         upper.ci.uniq = mean.uniq + qt(1 - (0.05 / 2), n.uniq - 1) * se.uniq) %>% 
  ggplot(aes(median_market_share, mean.uniq, fill = median_color_parent_time)) + geom_col(alpha=1, position = "dodge") +
  geom_errorbar( aes(x=median_market_share, ymin=lower.ci.uniq, ymax=upper.ci.uniq), position = position_dodge(width = 0.9), width=0.4, colour="black", alpha=0.8, size=0.8) +
  #facet_wrap(~median_color_parent_time) +
  geom_text(aes(label = round(mean.uniq, 3)), vjust = -1, hjust = 0.55, position = position_dodge(width = 0.85)) +
  ylab("Absolute deviation rating") +
  theme(axis.ticks.x=element_blank()) +
  scale_fill_manual(NULL, values = c("high color uniqueness" = "#666666", "low color uniqueness" = "#D3D3D3")) +
  theme_classic()  +
  theme(axis.text=element_text(size=12),
        axis.title.y=element_text(size=12),
        plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.title.x = element_blank(),
        legend.text = element_text(size=12)) +
  ggtitle("Rating deviation color vs brand uniqueness")
#Rating distribution
data_usa %>% ggplot(aes(x=rating_keepa)) +
  geom_histogram(alpha=0.5, binwidth = 0.5, color = "black", size =1) +
  ggtitle("Average rating distribution") +
  xlab("Average rating of reviews at the time the review was written") +
  ylab('Count') +
  theme(
    plot.title = element_text(hjust = 0.5, size=14,face="bold"),
    axis.title.x = element_text(hjust = 0.5, size=14),
    axis.title.y = element_text(hjust = 0.5, size=14)
  )

data_usa %>% ggplot(aes(x=diff_rating)) +
  geom_histogram(alpha=0.5, binwidth = 0.5, color = "black", size =1) +
  ggtitle("Histogram of difference in rating") +
  xlab("Deviation (Average rating -/- reviewer's rating)") +
  ylab('Number of reviews') +
  theme(
    plot.title = element_text(hjust = 0.5, size=14,face="bold"),
    axis.title.x = element_text(hjust = 0.5, size=14),
    axis.title.y = element_text(hjust = 0.5, size=14)
  ) +
  scale_x_continuous(breaks = seq(-4, 4, by = 1))

#Development over time
data_usa %>% filter(list_time_days >= 0, list_time_days < 365*2, !is.na(median_color_parent_time)) %>%
  ggplot(aes(list_time_days, diff_rating_abs, color = median_color_parent_time, linetype = median_color_parent_time)) + geom_smooth(size = 1) +
  scale_color_manual(NULL, values = c("#666666", "#444444")) +
  scale_linetype_manual(NULL, values= c("dashed", "solid")) + 
  xlab("Number of days since product introduction") +
  ylab("Absolute difference in rating") +
  ggtitle('Development of deviation in rating of \n color uniqueness over 2 years') + 
  theme(
    plot.title = element_text(hjust = 0.5, size=14,face="bold"),
    axis.title.x = element_text(hjust = 0.5, size=12),
    axis.title.y = element_text(hjust = 0.5, size=12)
  )
#Rating deviation
data_usa %>% filter(!is.na(median_color_parent_time)) %>% ggplot(aes(as.factor(rating_amazon), rating_keepa, color = median_color_parent_time)) +
  geom_boxplot()

#iphone
data_usa %>% filter(!is.na(median_color_parent_time)) %>% ggplot(aes(as.factor(iphone), diff_rating_abs, fill = median_color_parent_time)) +
  geom_boxplot()

data_usa$iphone <- as.factor(data_usa$iphone)
data_usa %>% filter(brand_overall %in% c("apple", "samsung")) %>%  filter(!is.na(median_color_parent_time)) %>% group_by(iphone, median_color_parent_time) %>%
  summarise(mean.uniq = mean(diff_rating_abs, na.rm = TRUE),
            sd.uniq = sd(diff_rating_abs, na.rm = TRUE),
            n.uniq = n()) %>%
  mutate(se.uniq = sd.uniq / sqrt(n.uniq),
         lower.ci.uniq = mean.uniq - qt(1 - (0.05 / 2), n.uniq - 1) * se.uniq,
         upper.ci.uniq = mean.uniq + qt(1 - (0.05 / 2), n.uniq - 1) * se.uniq) %>% 
  ggplot(aes(iphone, mean.uniq, fill = median_color_parent_time)) + geom_col(alpha=1, position = "dodge") +
  geom_errorbar( aes(x=iphone, ymin=lower.ci.uniq, ymax=upper.ci.uniq), position = position_dodge(width = 0.9), width=0.4, colour="black", alpha=0.8, size=0.8) +
  #facet_wrap(~median_color_parent_time) +
  geom_text(aes(label = round(mean.uniq, 3)), vjust = -1, hjust = 0.55, position = position_dodge(width = 0.85)) +
  ylab("Absolute deviation rating") +
  theme(axis.ticks.x=element_blank()) +
  scale_fill_manual(NULL, values = c("high color uniqueness" = "#666666", "low color uniqueness" = "#D3D3D3")) +
  theme_classic()  +
  theme(axis.text=element_text(size=12),
        axis.title.y=element_text(size=12),
        plot.title = element_text(hjust = 0.5, size=14,face="bold"),
        axis.title.x = element_blank(),
        legend.text = element_text(size=12)) +
  ggtitle("Rating deviation color vs brand uniqueness")
