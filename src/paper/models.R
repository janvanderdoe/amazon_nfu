library(tidyverse)
library(lme4)

data_usa <- read.csv("../../gen/output/amazon_usa_clean.csv")

summary(lm(pronoun_length ~ uniq_variant_parent, data_usa))

summary(lm(pronoun_length ~ length + price_new + uniq_variant_overall + uniq_sku_parent + uniq_brand_overall + uniq_variant_brand + iphone + oos_new + review_count + renewed + iphone + listed_since + time_since_oldest_review, data_usa))

summary(lmer(pronoun_length ~ uniq_sku_parent + ( uniq_sku_parent | uniq_brand_overall), data_usa))

#Forward stepwise regression pronoun length
min_model = lm(pronoun_length ~ price_new * uniq_variant_overall* uniq_brand_overall * review_count * iphone * oos_new *renewed, data_usa)
max_model <- formula(pronoun_length ~ 1, data_usa)
#fwd.model = step(min_model, direction="backward", scope=max_model)
#LENGTH
##By variant
data_usa %>% group_by(variant) %>% summarize(length = mean(length)) %>% mutate(variant = fct_reorder(variant, desc(length))) %>% ggplot(aes(variant,length)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
##By brand
data_usa %>% group_by(brand) %>% summarize(length = mean(length), n=n()) %>% slice_max(n, n=10) %>% mutate(brand = fct_reorder(brand, desc(length))) %>% ggplot(aes(brand,length)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

##Highly sig. model
summary(lm(length ~ price_new * uniq_variant_overall * review_count + uniq_brand_overall + iphone + oos_new + oos_used + renewed, data_usa))

summary(lm(length ~ price_new * uniq_variant_overall + review_count + uniq_brand_overall + iphone + oos_new + oos_used + renewed, data_usa))

#Star rating
data_usa %>% group_by(variant) %>% summarize(rating = mean(rating_float, na.rm=TRUE)) %>% mutate(variant = fct_reorder(variant, desc(rating))) %>% ggplot(aes(variant,rating)) +
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))
summary(lm(rating_float ~ price_new + uniq_variant_overall*uniq_brand_overall + review_count + iphone + oos_new +renewed, data_usa))

summary(lm(rating_float ~ review_count, data))

#Spec mention
summary(lm(spec_mentions ~ uniq_variant_overall + uniq_variant_brand + iphone + uniq_variant_overall + uniq_sku_parent + log(uniq_brand_overall) + price_new + oos_new+ review_count + listed_since, data_usa))
summary(lm(spec_mentions ~ price_new + renewed, data_usa_iphone))
summary(lm(spec_mentions ~ uniq_variant_overall*uniq_brand_overall + price_new + time_since_oldest_review + iphone +oos_new + oos_used + renewed, data_usa))

#Helpful
summary(lm(helpful ~ price_new + uniq_sku_parent + uniq_variant_overall*uniq_brand_overall + review_count + iphone + oos_new, data_usa))

##Brand mention
data_usa %>% group_by(variant) %>% summarize(brand_mentions = mean(brand_mention), n=n()) %>% slice_max(n, n=10) %>% mutate(variant = fct_reorder(variant, desc(brand_mentions))) %>% ggplot(aes(variant,brand_mentions)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
summary(lm(brand_mention ~ uniq_variant_parent*uniq_brand_overall, data_usa))
summary(lm(brand_mention ~ uniq_variant_overall*uniq_brand_overall + price_new + time_since_oldest_review + iphone +oos_new + oos_used + renewed, data_usa))
summary(lm(brand_mention ~ uniq_variant_overall*uniq_sku_parent + price_new + time_since_oldest_review, data_usa_iphone))

summary(lm(brand_mention ~ length + price_new + uniq_variant_overall*uniq_brand_overall*uniq_sku_parent + oos_new + renewed + review_count + renewed + iphone + listed_since, data_usa))

##recom
summary(lm(recom ~ uniq_variant_parent + uniq_brand_overall+ uniq_variant_brand + spec_mentions, data_usa))


###Random effects
summary(lmer(brand_mention ~ uniq_variant_brand + (uniq_variant_brand | uniq_parent), data_usa))
summary(lmer(brand_mention ~ uniq_variant_overall + (uniq_variant_overall | count_variant_parent), data_usa))

#Uniqueness on vars
summary(lm(uniq_variant_overall ~ length + pronoun_length + helpful + rating_float + spec_mentions + brand_mention + price_new + uniq_sku_parent + uniq_brand_overall + review_count + iphone + oos_new, data_usa))
summary(lm(uniq_sku_parent ~ length + pronoun_length + helpful + rating_float + spec_mentions + price_new + brand_mention + uniq_variant_overall + uniq_brand_overall + review_count + iphone + oos_new, data_usa))
summary(lm(uniq_brand_overall ~ length + pronoun_length + helpful + rating_float + spec_mentions + price_new + brand_mention + uniq_variant_overall + uniq_sku_parent + review_count + iphone + oos_new, data_usa))
summary(lm(uniq_variant_parent  ~ length + pronoun_length + helpful + rating_float + spec_mentions + price_new + brand_mention + uniq_brand_overall + uniq_variant_overall + uniq_sku_parent + review_count + iphone + oos_new + time_since_oldest_review + uniq_parent, data_usa))
