library(dbplyr)
library(readr)

train_data = read_csv("~/Descargas/competitive-data-science-predict-future-sales/sales_train.csv.gz")

summarize(train_data)