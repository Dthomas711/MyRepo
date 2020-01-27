library(tidyverse)
library(GGally)
bank_data <- bank

null_bank_model = lm(balance ~ duration + campaign, data = bank_data)

full_bank_model = lm(balance ~ ., data = bank_data)
anova(null_bank_model, full_bank_model)

graph <- lm(balance ~ duration + campaign, data = bank_data)


ggplot(bank_data, aes(x = duration + campaign, y = balance)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+ scale_x_log10()+scale_y_log10()
