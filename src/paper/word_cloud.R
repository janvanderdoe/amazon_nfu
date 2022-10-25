data_usa <- read.csv("../../gen/output/amazon_usa_clean.csv")

library(tidytext)
library(tidyverse)

#Word count by variant
word_count <- data_usa %>% unnest_tokens(input = review, output = word) %>% 
  count(variant, word)
data_usa %>% group_by(variant) %>% unnest_tokens(input = review, output = word) %>% filter(word == "different") %>% count(variant, word) %>%
  left_join(data_usa %>% group_by(variant) %>% unnest_tokens(input = review, output = word) %>% summarize(count = n()), by = "variant") %>% mutate(ratio = n/count) %>% arrange(desc(ratio))
word_count
stop_words

word_count_no_sp <- word_count %>% anti_join(stop_words)

top_n_by_variant_long <- word_count_no_sp %>% group_by(variant) %>% arrange(desc(n), .by_group=TRUE) %>% slice_max(n, n=20, with_ties = FALSE)
top_n_by_variant_long %>% print(n=Inf)

variants <- unique(top_n_by_variant_long$variant)
variants <- variants[!variants %in% c("cyan", "cream")]
temp = 1:21
for (v in variants) {
  variant_word <- top_n_by_variant_long %>% filter(variant == !!v) %>% select(word)
  
  names(variant_word) <- c("n", v)
  temp = cbind(temp, rbind(variant_word[2], sum(top_n_by_variant_long %>% filter(variant == !!v) %>% ungroup() %>% select(n))))
}

temp
#Median
subset_no_iphone$uni_median <- ifelse(subset_no_iphone$uniq_variant_parent >= median(subset_no_iphone$uniq_variant_parent), "low NFU", "high NFU")

word_count_median <- subset_no_iphone %>% unnest_tokens(input = review, output = word) %>% 
  count(uni_median, word)
word_count_median_no_sp <- word_count_median %>% anti_join(stop_words)
top_n_by_median_long <- word_count_median_no_sp %>% group_by(uni_median) %>% arrange(desc(n), .by_group=TRUE) %>% slice_max(n, n=100, with_ties = FALSE)

temp = 1:101
for (v in c("low NFU","high NFU")) {
  median_word <- top_n_by_median_long %>% filter(uni_median == !!v) %>% select(word)
  
  names(median_word) <- c("n", v)
  temp = cbind(temp, rbind(median_word[2], sum(top_n_by_median_long %>% filter(uni_median == !!v) %>% ungroup() %>% select(n))))
}
#names(temp) <- c("ranking", "high NFU", "low NFU")
temp

write.csv2(temp, "../../gen/paper/median_split_uniq_variant_parent.csv")

#brands
word_count_brand <- data_usa %>% unnest_tokens(input = review, output = word) %>% 
  count(brand_overall, word)
word_count_brand_no_sp <- word_count_brand %>% anti_join(stop_words)
top_n_by_brand_long <- word_count_brand_no_sp %>% group_by(brand_overall) %>% arrange(desc(n), .by_group=TRUE) %>% slice_max(n, n=100, with_ties = FALSE)

brands <- unique(top_n_by_brand_long$brand_overall)
brands <- data_usa %>% group_by(brand_overall) %>% count() %>% arrange(desc(n)) %>% head(n=10) %>% select(brand_overall)
brands <- brands %>% filter(brand_overall != "")

temp = 1:100
for (v in brands) {
  brand_word <- top_n_by_brand_long %>% filter(brand_overall == !!v) %>% select(word)
  
  names(brand_word) <- c("n", v)
  temp = cbind(temp, brand_word[2])
}

temp





subset_iphone <- data_usa %>% filter(brand_overall == 'apple')

subset_iphone %>% group_by(variant) %>% summarize(mentions = mean(brand_mention), n()) %>% arrange(desc(mentions))

subset_no_iphone <- data_usa %>% filter(brand_overall != 'apple')

subset_no_iphone %>% group_by(variant) %>% summarize(mentions = mean(brand_mention), n()) %>% arrange(desc(mentions))

subset_iphone %>% unnest_tokens(input = review, output = word) %>% filter(word == "iphone") %>% count(variant, word) %>%
  left_join(subset_iphone %>% group_by(variant) %>% unnest_tokens(input = review, output = word) %>% summarize(count = n()), by = "variant") %>% mutate(ratio = n/count) %>% arrange(desc(ratio))

subset_iphone <- subset_iphone %>% mutate(brand_mention = str_count(review, "iphone")/str_count(review, "\\S+"))
