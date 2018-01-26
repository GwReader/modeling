setwd("C:/")  ## 경로지정
dir()

######## 1. preprocessing
#install.packages("dplyr")
library(dplyr)


apoe = read.csv("apoe.csv") %>%
  select(ApoE4, M_41_60_ca, Composite_4rois)

apoe = apoe[complete.cases(apoe), ]

apoe$ApoE4 = as.factor(apoe$ApoE4)
  levels(apoe$ApoE4) = c("+", "-")  # Apoe4 라벨 바꾸기

str(apoe) ; summary(apoe) ; colSums(is.na(apoe))


######## 2. plotting

library(ggplot2)
library(scales)

## i. stacked plot

ggplot(apoe, aes(x=M_41_60_ca, y=Composite_4rois, fill = ApoE4)) +
  geom_bar(stat="identity")


## ii. relative stacked plot

ggplot(apoe, aes(x=M_41_60_ca, y=Composite_4rois, fill = ApoE4)) +
  geom_bar(stat="identity", position = "fill")


## iii. plot in separated sections

ggplot(apoe, aes(x=M_41_60_ca, y=Composite_4rois, fill = M_41_60_ca)) +
  geom_bar(stat="identity") + facet_grid(~ApoE4)


## iv. labeling

ggplot(apoe, aes(x=M_41_60_ca, y=Composite_4rois, fill = M_41_60_ca)) +
  geom_bar(stat="identity") + facet_grid(~ApoE4) +
  labs(x = "X_label", y = "Y_label") +          ## 축 라벨
  guides(fill=F)          ## 범주 없애기
