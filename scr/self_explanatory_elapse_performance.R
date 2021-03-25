library(APMtools)
elapse <- read.csv("../test_EXPANSE_NO2/data/rawData/AIRBASE_NO2_2010_variables.csv",
                   encoding = "utf-8")
error_matrix(elapse$NO2_2010, elapse$PREDNO2LURFULL)
View(elapse)
