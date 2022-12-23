library(tidyverse)
library(lme4)
library(car)
library(lmtest)
library(caret)
library(AID)
library(XLConnect)
library(ggfortify)

data_usa <- read.csv("../../gen/output/amazon_usa_clean.csv", sep = ",")

#Only complete observations
data_usa <- data_usa[complete.cases(data_usa %>% select(diff_rating_abs, uniq_color_parent_time, market_share_with_zero, oos_new, reviews, renewed, price_new)),]

#Descriptive statistics
rel_cols <- c("diff_rating_abs", "inv_mc_market_share", "inv_mc_uniq_color", "oos_new", "reviews", "price_new", "rating", "iphone", "renewed", "list_time_days") 

data_summary <- c("variable", "sd", "mean", "median", "min", "max")
for(col in rel_cols) {
  col <- c(col, sd(data_usa[[col]], na.rm = T), mean(data_usa[[col]], na.rm = TRUE), median(data_usa[[col]], na.rm = TRUE), min(data_usa[[col]], na.rm = TRUE), max(data_usa[[col]], na.rm = TRUE))
  data_summary <- cbind(data_summary, col)
}
data_summary
data_usa %>% group_by(median_market_share, median_color_parent_time) %>%
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

#Simple model
summary(lm(diff_rating_abs ~ uniq_color_parent_time * market_share_with_zero, data_usa))

#Complete model
model <- lm(diff_rating_abs ~ uniq_color_parent_time + market_share_with_zero + iphone + oos_new + reviews + renewed + price_new, data_usa)
summary(model)
#Check for multicollinearity
vif(model) #multicollinearity brands
##Correlation plot to check offender
round(cor(data_usa %>% select(diff_rating_abs, inv_mc_market_share, inv_mc_uniq_color, oos_new, reviews, price_new, rating, renewed_num, iphone, list_time_days), use = "complete.obs"),2)

model2 <- lm(diff_rating_abs ~ uniq_color_parent_time * market_share_with_zero + oos_new + reviews + renewed + price_new, data_usa)
summary(model2)



model_inv <- lm(diff_rating_abs ~ inv_uniq_color * inv_market_share + oos_new + reviews + renewed + price_new, data_usa)
summary(model_inv)

XLConnect::writeWorksheetToFile("../../gen/audit/model9.xlsx", 
                     data = summary(data_usa), 
                     sheet = "summary", 
                     header = TRUE,
                     clearSheets = TRUE)
vif(model2) #removing iPhone solves issue

#Check for heteroscedicity
autoplot(model2)

bptest(model2) #sig, meaning hetero

##Trying to solve this by transforming the DV
trans_diff_rating <- BoxCoxTrans(data_usa$diff_rating_abs+1e-10)
print(trans_diff_rating)
data_usa <- cbind(data_usa, trans_diff_rating = predict(trans_diff_rating, data_usa$diff_rating_abs))

model3 <- lm(trans_diff_rating ~ uniq_color_parent_time * market_share_with_zero + oos_new + reviews + renewed + price_new, data_usa)
summary(model3)
autoplot(model3)

bptest(model3)
out <- boxcoxnc(data_usa[['diff_rating_abs']]+1e-10, method = "sw", lambda = seq(-2,2,0.0001), verbose = F, plot = F)

##Did not work. Try weighted ols instead
model4 <- lm(diff_rating_abs ~ uniq_color_parent_time * market_share_with_zero + oos_new + reviews + renewed + price_new, data_usa)
wt <- 1 / lm(abs(model2$residuals) ~ model2$fitted.values)$fitted.values^2
model5 <- lm(diff_rating_abs ~ uniq_color_parent_time * market_share_with_zero + oos_new + reviews + renewed + price_new, data_usa, weights = wt)
autoplot(model5)
bptest(model5)

model6 <- lm(trans_diff_rating ~ uniq_color_parent_time * market_share_with_zero + oos_new + reviews + renewed + price_new, data_usa, weights = wt)
bptest(model6)

#Better model fit, but heteroscedasticity
wt1 <- 1 / (model5$residuals)^2
model7 <- lm(trans_diff_rating ~ uniq_color_parent_time * market_share_with_zero + oos_new + reviews + renewed + price_new, data_usa, weights = wt1)

#Independence of residual values
durbinWatsonTest(model3)

model8 <- lm(diff_rating_abs ~ inv_mc_market_share * inv_mc_uniq_color + oos_new + reviews + price_new + rating + I(rating^2) + iphone + renewed, data_usa)
summary(model8)

wt8 <- 1 / lm(abs(model8$residuals) ~ model8$fitted.values)$fitted.values^2

model9 <- lm(diff_rating_abs ~ inv_mc_market_share * inv_mc_uniq_color + oos_new + log(reviews) + price_new  + I(rating^2) + renewed + iphone + list_time_days, data_usa)
summary(model9)


autoplot(model9)
bptest(model8)
durbinWatsonTest(model8)