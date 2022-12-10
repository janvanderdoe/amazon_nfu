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
model <- lm(diff_rating_abs ~ uniq_color_parent_time + market_share_with_zero + iphone + oos_new + reviews + renewed + price_new, data_usa)
summary(model)
#Check for multicollinearity
vif(model) #multicollinearity brands
##Correlation plot to check offender
round(cor(data_usa %>% select(uniq_color_parent_time, market_share_with_zero, iphone, oos_new, reviews, price_new), use = "complete.obs"),2)

model2 <- lm(diff_rating_abs ~ uniq_color_parent_time * market_share_with_zero + oos_new + reviews + renewed + price_new, data_usa)
summary(model2)



model_inv <- lm(diff_rating_abs ~ inv_uniq_color * inv_market_share + oos_new + reviews + renewed + price_new, data_usa)
summary(model_inv)

writeWorksheetToFile("../../gen/audit/model2.xlsx", 
                     data = summary(model2), 
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
