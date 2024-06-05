rm(list=ls());
Emigration <- gzfile("Emigration_agegroup.gz", "r")
Emigration <- read.csv(Emigration)
Emigration$age <- sub("^Y", "", Emigration$age)
Emigration$age <- sub("_LT5", "0-5", Emigration$age)
Emigration$age <- sub("_GE85", "85-", Emigration$age)
Emigration$age <- sub("-.*", "", Emigration$age)
Emigration$age <- as.numeric(Emigration$age)
Emigration <- arrange(Emigration, age)
Immigration <- gzfile("Immigration_agegroup.gz", "r")
Immigration <- read.csv(Immigration)
Immigration$age <- sub("^Y", "", Immigration$age)
Immigration$age <- sub("_LT5", "0-5", Immigration$age)
Immigration$age <- sub("_GE85", "85-", Immigration$age)
Immigration$age <- sub("-.*", "", Immigration$age)
Immigration$age <- as.numeric(Immigration$age)
Immigration <- arrange(Immigration, age)
data1 <- Immigration  #DATA_RC.csv  
data2 <- Emigration #Sending Country
req <- c("partner", "age", "sex", "geo", "TIME_PERIOD", "OBS_VALUE")
data1 <- data1[, req]
data2 <- data2[, req]
data1 <- data1[data1$TIME_PERIOD >= 2009 & data1$TIME_PERIOD <= 2015, ]
data2 <- data2[data2$TIME_PERIOD >= 2009 & data2$TIME_PERIOD <= 2015, ]
df1x <- expand.grid(partner = unique(data1$partner), age = unique(data1$age), sex = unique(data1$sex), geo = unique(data1$partner), TIME_PERIOD = 2009:2015)
df1x$OBS_VALUE <- NA
df2x <- expand.grid(partner = unique(data2$partner), age = unique(data2$age), sex = unique(data2$sex), geo = unique(data2$partner), TIME_PERIOD = 2009:2015)
df2x$OBS_VALUE <- NA

df1 <- merge(df1x, data1, by = c("partner", "age", "sex", "geo", "TIME_PERIOD"), all.x = TRUE)
df1 <- df1[order(df1$age, df1$sex, df1$TIME_PERIOD), ]

df1 <- df1[,-6]
names(df1)[names(df1) == "OBS_VALUE.y"] <- "OBS_VALUE"


df2 <- merge(df2x, data2, by = c("partner", "age", "sex", "geo", "TIME_PERIOD"), all.x = TRUE)


df2 <- df2[order(df2$age, df2$sex, df2$TIME_PERIOD), ]

df2 <- df2[,-6]
names(df2)[names(df2) == "OBS_VALUE.y"] <- "OBS_VALUE"
df1 <- df1 %>%
  mutate(OBS_VALUE = ifelse(geo == partner, 0, OBS_VALUE))
df2 <- df2 %>%
  mutate(OBS_VALUE = ifelse(geo == partner, 0, OBS_VALUE))

library(tidyverse)
library(dplyr)
library(tidyr)
df1_f <- df1 %>%
  pivot_wider(names_from = TIME_PERIOD, values_from = OBS_VALUE)
df2_f <- df2 %>%
  pivot_wider(names_from = TIME_PERIOD, values_from = OBS_VALUE)
df1_f <- df1_f %>%
  arrange(sex,  partner,geo, age)
df2_f <- df2_f %>%
  arrange(sex,  geo,partner, age)
#df1_f <- df1_f[!(df1_f$partner == 'NO' | df1_f$geo == 'NO'), ]
#df2_f <- df2_f[!(df2_f$partner == 'NO' | df2_f$geo == 'NO'), ]
data_ODAS_RC <- df1_f[,-c(1:4)]
data_ODAS_SC <- df2_f[,-c(1:4)]

#data_ODAS_RC[is.na(data_ODAS_RC)]<-0.1 
#data_ODAS_SC[is.na(data_ODAS_SC)]<-0.1

##################################################################
write.csv(data_ODAS_RC, "data_ODAS_RC.csv")
write.csv(data_ODAS_SC, "data_ODAS_SC.csv")

