library(tidyverse)
library(lme4)

data_usa <- read.csv("../../gen/output/amazon_usa_clean.csv")

summary(lm(pronoun_length ~ uniqueness_variant, data_usa))

summary(lm(pronoun_length ~ length + price_new + uniqueness_variant + uniqueness_sku + uniqueness_brand + uniqueness_brand_variant + iphone + oos_new + review_count + renewed + iphone + listed_since + time_since_oldest_review, data_usa))

summary(lmer(pronoun_length ~ uniqueness_sku + (uniqueness_sku | uniqueness_brand), data_usa))

#Forward stepwise regression pronoun length
min_model = lm(pronoun_length ~ price_new * uniqueness_variant*uniqueness_brand * review_count * iphone * oos_new *renewed, data_usa)
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
summary(lm(length ~ price_new * uniqueness_variant * review_count + uniqueness_brand + iphone + oos_new + oos_used + renewed, data_usa))

summary(lm(length ~ price_new * uniqueness_variant_parent_sku + review_count + uniqueness_brand + iphone + oos_new + oos_used + renewed, data_usa))

#Star rating
data_usa %>% group_by(variant) %>% summarize(rating = mean(rating_float, na.rm=TRUE)) %>% mutate(variant = fct_reorder(variant, desc(rating))) %>% ggplot(aes(variant,rating)) +
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))
summary(lm(rating_float ~ price_new + uniqueness_variant*uniqueness_brand + review_count + iphone + oos_new +renewed, data_usa))

summary(lm(rating_float ~ review_count, data))

#Spec mention
summary(lm(spec_mentions ~ uniqueness_variant_parent_sku + uniqueness_brand_variant + iphone + uniqueness_variant + uniqueness_sku + log(uniqueness_brand) + price_new + oos_new+ review_count + listed_since, data_usa))
summary(lm(spec_mentions ~ price_new + renewed, data_usa_iphone))
summary(lm(spec_mentions ~ uniqueness_variant_parent_sku*uniqueness_brand + price_new + time_since_oldest_review + iphone +oos_new + oos_used + renewed, data_usa))

#Helpful
summary(lm(helpful ~ price_new + uniqueness_sku + uniqueness_variant*uniqueness_brand + review_count + iphone + oos_new, data_usa))

##Brand mention
data_usa %>% group_by(variant) %>% summarize(brand_mentions = mean(brand_mention), n=n()) %>% slice_max(n, n=10) %>% mutate(variant = fct_reorder(variant, desc(brand_mentions))) %>% ggplot(aes(variant,brand_mentions)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
summary(lm(brand_mention ~ uniqueness_variant_parent_sku*uniqueness_brand + price_new + time_since_oldest_review + iphone +oos_new + oos_used + renewed, data_usa))
summary(lm(brand_mention ~ uniqueness_variant_parent_sku*uniqueness_sku + price_new + time_since_oldest_review, data_usa_iphone))

summary(lm(brand_mention ~ length + price_new + uniqueness_variant*uniqueness_brand*uniqueness_sku + oos_new + renewed + review_count + renewed + iphone + listed_since, data_usa))

###Random effects
summary(lmer(brand_mention ~ uniqueness_brand_variant + (uniqueness_brand_variant | uniqueness_parent), data_usa))
summary(lmer(brand_mention ~ uniqueness_variant_parent_sku + (uniqueness_variant_parent_sku | count_variant_parent), data_usa))

#Uniqueness on vars
summary(lm(uniqueness_variant ~ length + pronoun_length + helpful + rating_float + spec_mentions + brand_mention + price_new + uniqueness_sku + uniqueness_brand + review_count + iphone + oos_new, data_usa))
summary(lm(uniqueness_sku ~ length + pronoun_length + helpful + rating_float + spec_mentions + price_new + brand_mention + uniqueness_variant + uniqueness_brand + review_count + iphone + oos_new, data_usa))
summary(lm(uniqueness_brand ~ length + pronoun_length + helpful + rating_float + spec_mentions + price_new + brand_mention + uniqueness_variant + uniqueness_sku + review_count + iphone + oos_new, data_usa))
summary(lm(uniqueness_variant_parent_sku  ~ length + pronoun_length + helpful + rating_float + spec_mentions + price_new + brand_mention + uniqueness_brand + uniqueness_variant + uniqueness_sku + review_count + iphone + oos_new + time_since_oldest_review + uniqueness_parent, data_usa))
