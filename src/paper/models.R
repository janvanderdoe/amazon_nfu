library(tidyverse)
library(lme4)
library(car)
library(lmtest)
library(caret)
library(AID)
library(XLConnect)

data_usa <- read.csv("../../gen/output/amazon_usa_clean.csv", sep = ";")

data_usa$market_share_with_zero <- data_usa$market_share_with_zero/100
data_usa$market_share <- data_usa$market_share/100

data_usa %>% group_by(median_market_share, median_color_parent_time) %>%
  summarise(mean.uniq = mean(diff_rating_abs, na.rm = TRUE),
            sd.uniq = sd(diff_rating_abs, na.rm = TRUE),
            n.uniq = n()) %>%
  mutate(se.uniq = sd.uniq / sqrt(n.uniq),
        lower.ci.uniq = mean.uniq - qt(1 - (0.05 / 2), n.uniq - 1) * se.uniq,
        upper.ci.uniq = mean.uniq + qt(1 - (0.05 / 2), n.uniq - 1) * se.uniq)

#Simple model
summary(lm(diff_rating_abs ~ uniq_color_parent_time * market_share_with_zero, data_usa))

#Complete model
model <- lm(diff_rating_abs ~ uniq_color_parent_time * market_share_with_zero + iphone + oos_new + reviews + renewed + price_new, data_usa)
summary(model)
#Check for multicollinearity
vif(model) #multicollinearity brands
##Correlation plot to check offender
round(cor(data_usa2 %>% select(uniq_color_parent_time, market_share_with_zero, iphone, oos_new, reviews, price_new), use = "complete.obs"),2)

model2 <- lm(diff_rating_abs ~ uniq_color_parent_time * market_share_with_zero + oos_new + reviews + renewed + price_new, data_usa3)
summary(model2)
writeWorksheetToFile("../../gen/audit/model2.xlsx", 
                     data = summary(model2), 
                     sheet = "summary", 
                     header = TRUE,
                     clearSheets = TRUE)
vif(model2) #removing iPhone solves issue

#Check for heteroscedicity
autoplot(model2)

bptest(model2)
##Trying to solve this by transforming the DV
data_usa2 <- data_usa[!is.na(data_usa$diff_rating_abs),]
trans_diff_rating <- BoxCoxTrans(data_usa2$diff_rating_abs+1e-10)
print(trans_diff_rating)
data_usa2 <- cbind(data_usa2, trans_diff_rating = predict(trans_diff_rating, data_usa2$diff_rating_abs))

model3 <- lm(trans_diff_rating ~ uniq_color_parent_time * market_share_with_zero + oos_new + reviews + renewed + price_new, data_usa2)
summary(model3)
autoplot(model3)

bptest(model3)
out <- boxcoxnc(data_usa2[['diff_rating_abs']]+1e-10, method = "sw", lambda = seq(-2,2,0.0001), verbose = F, plot = F)

##Did not work. Try weighted ols instead
data_usa3 <- data_usa2[complete.cases(data_usa2 %>% select(diff_rating_abs, uniq_color_parent_time, market_share_with_zero, oos_new, reviews, renewed, price_new)),]
model4 <- lm(diff_rating_abs ~ uniq_color_parent_time * market_share_with_zero + oos_new + reviews + renewed + price_new, data_usa3)
wt <- 1 / lm(abs(model2$residuals) ~ model2$fitted.values)$fitted.values^2
model5 <- lm(diff_rating_abs ~ uniq_color_parent_time * market_share_with_zero + oos_new + reviews + renewed + price_new, data_usa3, weights = wt)
autoplot(model5)
bptest(model5)
model6 <- lm(trans_diff_rating ~ uniq_color_parent_time * market_share_with_zero + oos_new + reviews + renewed + price_new, data_usa3, weights = wt)
bptest(model6)

#Better model fit, but heteroscedasticity
wt1 <- 1 / (model2$residuals)^2
model7 <- lm(trans_diff_rating ~ uniq_color_parent_time * market_share_with_zero + oos_new + reviews + renewed + price_new, data_usa3, weights = wt1)

#Independence of residual values
durbinWatsonTest(model3)

summary(lm(pronoun_length ~ uniq_variant_parent, data_usa))

summary(lm(pronoun_length ~ length + price_new + uniq_variant_overall + uniq_sku_parent + uniq_brand_overall + uniq_variant_brand + iphone + oos_new + review_count + renewed + iphone + listed_since + time_since_oldest_review, data_usa))

summary(lmer(pronoun_length ~ uniq_sku_parent + ( uniq_sku_parent | uniq_brand_overall), data_usa))

summary(lm(pronoun_length ~ length + price_new + uniq_variant_parent*time_since_oldest_review + iphone + oos_new + review_count + renewed + iphone + listed_since, data_usa))
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
summary(lm(uniq_variant_parent_time  ~ length + pronoun_length + helpful + rating_float + spec_mentions + price_new + brand_mention + + review_count + iphone + oos_new + time_since_oldest_review, data_usa))
summary(lm(uniq_variant_parent  ~ length + pronoun_length + helpful + rating_float + spec_mentions + price_new + brand_mention + + review_count + iphone + oos_new + time_since_oldest_review, data_usa))
