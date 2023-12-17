# week 6: HOML chap.2

library(dplyr) # data manipulation
library(ggplot2) # graphics
library(rsample) # resampling procedures
library(caret) # resampling and model training
library(h2o) # resampling and model training


# data
library(AmesHousing)
ames <- AmesHousing::make_ames()

# split training and test
# random sampling
set.seed(1)
index_1 <- createDataPartition(ames$Sale_Price, p = 0.7,
                               list = FALSE)
train_1 <- ames[index_1, ]
test_2 <- ames[-index_1, ]


# stratified sampling

(Sale_Price ~ Neighborhood + Year_Sold,
         data = ames)
