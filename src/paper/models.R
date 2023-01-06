library(tidyverse)
library(car) #used for vif
library(broom) #used as input
library(texreg) #comparing and saving models
library(writexl) #saving
library(caret) #boxcox
library(ggfortify) #autoplot
library(lmtest) #bp test

setwd("~/Git projects/amazon_nfu/src/paper")
data_usa <- read.csv("../../gen/output/amazon_usa_clean2.csv", sep = ",", row.names = 1)

#Only complete observations
data_usa <- data_usa[complete.cases(data_usa %>% select(diff_rating_abs, uniq_color_parent_time, market_share_with_zero, oos_new, reviews, renewed, price_new)),]

#Descriptive statistics
rel_cols <- c("diff_rating_abs", "inv_mc_market_share", "inv_mc_uniq_color", "oos_new", "reviews", "price_new", "rating_keepa", "rating_amazon", "renewed_num", "list_time_days") 

data_summary <- c("variable", "mean", "median", "sd", "min", "max")
for(col in rel_cols) {
  col <- c(col, sd(data_usa[[col]], na.rm = T), mean(data_usa[[col]], na.rm = TRUE), median(data_usa[[col]], na.rm = TRUE), min(data_usa[[col]], na.rm = TRUE), max(data_usa[[col]], na.rm = TRUE))
  data_summary <- cbind(data_summary, col)
}
t(data_summary)

#Correlation matrix
data_usa$renewed_num <- ifelse(data_usa$renewed == "yes", 1, 0)
round(cor(data_usa %>% select(diff_rating_abs, inv_mc_uniq_color, inv_mc_uniq_color_data, inv_mc_market_share, oos_new, reviews, price_new, rating_keepa, renewed_num, list_time_days), use = "complete.obs"),2)

cor(data_usa$uniq_color_data_time, data_usa$uniq_color_parent_time, use = "complete.obs")
# t tests
t.test(diff_rating_abs ~ median_market_share, data_usa %>% filter(median_color_parent_time == "low color uniqueness"))
t.test(diff_rating_abs ~ median_color_parent_time, data_usa)

#MAIN MODEL
orig_model <- lm(diff_rating_abs ~ inv_mc_uniq_color * inv_mc_market_share + oos_new + log(reviews) + price_new  + rating_keepa + renewed + list_time_days, data_usa)
write.table(tidy(orig_model), "../../gen/paper/orig_model.csv", dec = ".", sep = ";") #export for table
summary(orig_model)

#Check for multicollinearity
write.csv2(round(as.data.frame(vif(orig_model)),3), "../../gen/paper/vif_output.csv") #no multicollinearity

#Check for heteroscedasticity
bptest(orig_model) # sig, meaning hetero
autoplot(orig_model)

##Trying to solve it by transforming the y
trans_diff_rating <- BoxCoxTrans(data_usa$diff_rating_abs+1e-10, na.rm = T) #lambda is 0.3

orig_model_trans <- lm(diff_rating_abs^0.3 ~ inv_mc_uniq_color * inv_mc_market_share + oos_new + log(reviews) + price_new  + rating_keepa + renewed + list_time_days, data_usa)
summary(orig_model_trans)
autoplot(orig_model_trans) #did not solve

##Using a WLS instead
wt <- 1 / lm(abs(orig_model$residuals) ~ orig_model$fitted.values)$fitted.values^2

orig_model_wt <- lm(diff_rating_abs ~ inv_mc_uniq_color * inv_mc_market_share + oos_new + log(reviews) + price_new  + rating_keepa + renewed + list_time_days, data_usa, weights = wt)
summary(orig_model_wt)
autplot(orig_model_wt)

#Testing autocorrelation
durbinWatsonTest(orig_model) #1.82, which is okay

#ALTERNATIVE MODELS
model_no_zero <- lm(diff_rating_abs ~ inv_mc_uniq_color * inv_mc_excl_zero + oos_new + log(reviews) + price_new  + rating_keepa + renewed + list_time_days, data_usa)
summary(model_no_zero)

model_alt_uniq <- lm(diff_rating_abs ~ inv_mc_uniq_color_data * inv_mc_market_share + oos_new + log(reviews) + price_new  + rating_keepa + renewed + list_time_days, data_usa)
summary(model_alt_uniq)

model_neg_dev <- lm(diff_rating_abs ~ inv_mc_uniq_color * inv_mc_market_share + oos_new + log(reviews) + price_new  + rating_keepa + renewed + list_time_days, data_usa %>% filter(diff_rating < 0))
summary(model_neg_dev)

model_pos_dev <- lm(diff_rating_abs ~ inv_mc_uniq_color * inv_mc_market_share + oos_new + log(reviews) + price_new  + rating_keepa + renewed + list_time_days, data_usa %>% filter(diff_rating >= 0))
summary(model_pos_dev)

model_variants <- lm(diff_rating_abs ~ inv_mc_uniq_color * inv_mc_market_share + oos_new + log(reviews) + price_new  + rating_keepa + renewed + list_time_days + variant, data_usa)
summary(model_variants)
wordreg(model_variants, "../../gen/audit/model_variants.doc", digits = 3)

model_android <- lm(diff_rating_abs ~ inv_mc_uniq_color * inv_mc_market_share + oos_new + log(reviews) + price_new  + rating_keepa + renewed + list_time_days, data_usa %>% filter(brand_overall == "apple"))
summary(model_android)

#Overview models
screenreg(list(orig_model, model_no_zero, model_alt_uniq, model_android, model_variants, model_neg_dev, model_pos_dev), digits = 3)

#Export
wordreg(list(orig_model, model_no_zero, model_alt_uniq, model_android, model_variants, model_neg_dev, model_pos_dev), 
        custom.model.names = c('orig', 'nozero', 'alt_col', 'android', 'variants', 'neg_dev', 'pos_dev'),
        digits = 3, "../../models.doc", single.row = TRUE)
