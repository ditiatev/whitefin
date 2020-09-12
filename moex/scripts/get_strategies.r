get_strategies <- function() {
# Setup ===========================================================================================

library(dplyr)
setwd("C:/R/Fin/whitefin/moex")

# Read csv =========================================================================================


### _RTS =====
df_2008_2013_RTS_10_14 <- read.csv("./data/2008-2013_RTS_10-14.csv", encoding = "UTF-8", sep = ";")
df_2014_2018_RTS_10_14 <- read.csv("./data/2014-2018_RTS_10-14.csv", encoding = "UTF-8", sep = ";")
df_2019_2020_RTS_10_14 <- read.csv("./data/2019-2020_RTS_10-14.csv", encoding = "UTF-8", sep = ";")

df_2008_2013_RTS_14_18 <- read.csv("./data/2008-2013_RTS_14-18.csv", encoding = "UTF-8", sep = ";")
df_2014_2018_RTS_14_18 <- read.csv("./data/2014-2018_RTS_14-18.csv", encoding = "UTF-8", sep = ";")
df_2019_2020_RTS_14_18 <- read.csv("./data/2019-2020_RTS_14-18.csv", encoding = "UTF-8", sep = ";")

df_RTS_18_22           <- read.csv("./data/dailystrategydata.csv", encoding = "UTF-8")

### _SI =====
df_2008_2013_SI_10_14 <- read.csv("./data/2008-2013_Si_10-14.csv", encoding = "UTF-8", sep = ";")
df_2014_2018_SI_10_14 <- read.csv("./data/2014-2018_Si_10-14.csv", encoding = "UTF-8", sep = ";")
df_2019_2020_SI_10_14 <- read.csv("./data/2019-2020_Si_10-14.csv", encoding = "UTF-8", sep = ";")

df_2008_2013_SI_14_18 <- read.csv("./data/2008-2013_Si_14-18.csv", encoding = "UTF-8", sep = ";")
df_2014_2018_SI_14_18 <- read.csv("./data/2014-2018_Si_14-18.csv", encoding = "UTF-8", sep = ";")
df_2019_2020_SI_14_18 <- read.csv("./data/2019-2020_Si_14-18.csv", encoding = "UTF-8", sep = ";")

df_2008_2013_SI_18_22 <- read.csv("./data/2008-2013_Si_18-22.csv", encoding = "UTF-8", sep = ";")
df_2014_2018_SI_18_22 <- read.csv("./data/2014-2018_Si_18-22.csv", encoding = "UTF-8", sep = ";")
df_2019_2020_SI_18_22 <- read.csv("./data/2019-2020_Si_18-22.csv", encoding = "UTF-8", sep = ";")

# Selected columns ==================================================================================

### _RTS =====
vNColToSelect <- c(2,9,11,17,18,26,27,29,31,33)

df_2008_2013_RTS_10_14 <- df_2008_2013_RTS_10_14[,vNColToSelect]
df_2014_2018_RTS_10_14 <- df_2014_2018_RTS_10_14[,vNColToSelect]
df_2019_2020_RTS_10_14 <- df_2019_2020_RTS_10_14[,vNColToSelect]
df_2008_2013_RTS_14_18 <- df_2008_2013_RTS_14_18[,vNColToSelect]
df_2014_2018_RTS_14_18 <- df_2014_2018_RTS_14_18[,vNColToSelect]
df_2019_2020_RTS_14_18 <- df_2019_2020_RTS_14_18[,vNColToSelect]
df_RTS_18_22           <- df_RTS_18_22[,c(2,8,10,15,16,24,25,27,29,31)]

### _SI =====
df_2008_2013_SI_10_14 <- df_2008_2013_SI_10_14[,vNColToSelect]
df_2014_2018_SI_10_14 <- df_2014_2018_SI_10_14[,vNColToSelect]
df_2019_2020_SI_10_14 <- df_2019_2020_SI_10_14[,vNColToSelect]
df_2008_2013_SI_14_18 <- df_2008_2013_SI_14_18[,vNColToSelect]
df_2014_2018_SI_14_18 <- df_2014_2018_SI_14_18[,vNColToSelect]
df_2019_2020_SI_14_18 <- df_2019_2020_SI_14_18[,vNColToSelect]
df_2008_2013_SI_18_22 <- df_2008_2013_SI_18_22[,vNColToSelect]
df_2014_2018_SI_18_22 <- df_2014_2018_SI_18_22[,vNColToSelect]
df_2019_2020_SI_18_22 <- df_2019_2020_SI_18_22[,vNColToSelect]

# Named columns ==================================================================================

vColNames <- c("position","date","entryprice","timeclose","exitprice","bartime","barprofit","profit","MAE","MFE")

### _RTS =====
names(df_2008_2013_RTS_10_14) <- vColNames
names(df_2014_2018_RTS_10_14) <- vColNames
names(df_2019_2020_RTS_10_14) <- vColNames
names(df_2008_2013_RTS_14_18) <- vColNames
names(df_2014_2018_RTS_14_18) <- vColNames
names(df_2019_2020_RTS_14_18) <- vColNames
names(df_RTS_18_22)           <- vColNames

### _SI =====
names(df_2008_2013_SI_10_14) <- vColNames
names(df_2014_2018_SI_10_14) <- vColNames
names(df_2019_2020_SI_10_14) <- vColNames
names(df_2019_2020_SI_14_18) <- vColNames
names(df_2008_2013_SI_14_18) <- vColNames
names(df_2014_2018_SI_14_18) <- vColNames
names(df_2008_2013_SI_18_22) <- vColNames
names(df_2014_2018_SI_18_22) <- vColNames
names(df_2019_2020_SI_18_22) <- vColNames

# United diff periods =========================================================================

### _RTS ===========================================

### ____10_14 =====
df_2008_2013_RTS_10_14 <- df_2008_2013_RTS_10_14 %>%
        mutate_all(as.character)
df_2014_2018_RTS_10_14 <- df_2014_2018_RTS_10_14 %>%
        mutate_all(as.character)
df_2019_2020_RTS_10_14 <- df_2019_2020_RTS_10_14 %>%
        mutate_all(as.character)

df_RTS_10_14 <- rbind(df_2008_2013_RTS_10_14, df_2014_2018_RTS_10_14, df_2019_2020_RTS_10_14)

### ____14_18 =====
df_2008_2013_RTS_14_18 <- df_2008_2013_RTS_14_18 %>%
        mutate_all(as.character)
df_2014_2018_RTS_14_18 <- df_2014_2018_RTS_14_18 %>%
        mutate_all(as.character)
df_2019_2020_RTS_14_18 <- df_2019_2020_RTS_14_18 %>%
        mutate_all(as.character)

df_RTS_14_18 <- rbind(df_2008_2013_RTS_14_18, df_2014_2018_RTS_14_18, df_2019_2020_RTS_14_18)

### _SI ============================================

### ____10_14 =====
df_2008_2013_SI_10_14 <- df_2008_2013_SI_10_14 %>%
        mutate_all(as.character)
df_2014_2018_SI_10_14 <- df_2014_2018_SI_10_14 %>%
        mutate_all(as.character)
df_2019_2020_SI_10_14 <- df_2019_2020_SI_10_14 %>%
        mutate_all(as.character)
df_SI_10_14 <- rbind(df_2008_2013_SI_10_14, df_2014_2018_SI_10_14, df_2019_2020_SI_10_14)

### ____14_18 =====
df_2008_2013_SI_14_18 <- df_2008_2013_SI_14_18 %>%
        mutate_all(as.character)
df_2014_2018_SI_14_18 <- df_2014_2018_SI_14_18 %>%
        mutate_all(as.character)
df_2019_2020_SI_14_18 <- df_2019_2020_SI_14_18 %>%
        mutate_all(as.character)
df_SI_14_18 <- rbind(df_2008_2013_SI_14_18, df_2014_2018_SI_14_18, df_2019_2020_SI_14_18)

### ____18_22 =====
df_2008_2013_SI_18_22 <- df_2008_2013_SI_18_22 %>%
        mutate_all(as.character)
df_2014_2018_SI_18_22 <- df_2014_2018_SI_18_22 %>%
        mutate_all(as.character)
df_2019_2020_SI_18_22 <- df_2019_2020_SI_18_22 %>%
        mutate_all(as.character)
df_SI_18_22 <- rbind(df_2008_2013_SI_18_22, df_2014_2018_SI_18_22, df_2019_2020_SI_18_22)


# MUTATE DATA ======================================================================================

### _RTS =====
df_RTS_10_14 <- df_RTS_10_14 %>%
        mutate(date = as.Date(date, format = '%d.%m.%Y')) %>%
        mutate(date = as.POSIXct(date, format = '%d.%m.%Y')) %>%
        mutate(profit = gsub("%", "", profit)) %>%
        mutate(profit = gsub(",", ".", profit)) %>%
        mutate(profit = as.numeric(profit)) %>%
        mutate(year = as.factor(year(date))) %>%
        mutate(month = as.factor(month(date))) %>%
        mutate(wday = as.factor(wday(date, week_start = 1))) %>%
        mutate(timeclose = hms(timeclose)) %>%
        mutate(tradestop = case_when(timeclose$hour == 13 & timeclose$minute > 57  ~ F, T ~ T)) %>%
        mutate(strategy = "RTS_10_14")
df_RTS_10_14 <- na.omit(df_RTS_10_14)

df_RTS_14_18 <- df_RTS_14_18 %>%
        mutate(date = as.Date(date, format = '%d.%m.%Y')) %>%
        mutate(date = as.POSIXct(date, format = '%d.%m.%Y')) %>%
        mutate(profit = gsub("%", "", profit)) %>%
        mutate(profit = gsub(",", ".", profit)) %>%
        mutate(profit = as.numeric(profit)) %>%
        mutate(year = as.factor(year(date))) %>%
        mutate(month = as.factor(month(date))) %>%
        mutate(wday = as.factor(wday(date, week_start = 1))) %>%
        mutate(timeclose = hms(timeclose)) %>%
        mutate(tradestop = case_when((timeclose$hour == 17 & timeclose$minute > 57) | (timeclose$hour < 14)  ~ F, T ~ T)) %>%
        mutate(strategy = "RTS_14_18")
df_RTS_14_18 <- na.omit(df_RTS_14_18)

df_RTS_18_22 <- df_RTS_18_22 %>%
        mutate(date = as.Date(date, format = '%d.%m.%Y')) %>%
        mutate(date = as.POSIXct(date, format = '%d.%m.%Y')) %>%
        mutate(profit = gsub("%", "", profit)) %>%
        mutate(profit = gsub(",", ".", profit)) %>%
        mutate(profit = as.numeric(profit)) %>%
        mutate(year = as.factor(year(date))) %>%
        mutate(month = as.factor(month(date))) %>%
        mutate(wday = as.factor(wday(date, week_start = 1))) %>%
        mutate(timeclose = hms(timeclose)) %>%
        mutate(tradestop = case_when(timeclose$hour == 22 ~ F, T ~ T)) %>%
        mutate(strategy = "RTS_18_22")
df_RTS_18_22 <- na.omit(df_RTS_18_22)

### _SI =====
df_SI_10_14 <- df_SI_10_14 %>%
        mutate(date = as.Date(date, format = '%d.%m.%Y')) %>%
        mutate(date = as.POSIXct(date, format = '%d.%m.%Y')) %>%
        mutate(profit = gsub("%", "", profit)) %>%
        mutate(profit = gsub(",", ".", profit)) %>%
        mutate(profit = as.numeric(profit)) %>%
        mutate(year = as.factor(year(date))) %>%
        mutate(month = as.factor(month(date))) %>%
        mutate(wday = as.factor(wday(date, week_start = 1))) %>%
        mutate(timeclose = hms(timeclose)) %>%
        mutate(tradestop = case_when(timeclose$hour == 13 & timeclose$minute > 57  ~ F, T ~ T)) %>%
        mutate(strategy = "SI_10_14")
df_SI_10_14 <- na.omit(df_SI_10_14)

df_SI_14_18 <- df_SI_14_18 %>%
        mutate(date = as.Date(date, format = '%d.%m.%Y')) %>%
        mutate(date = as.POSIXct(date, format = '%d.%m.%Y')) %>%
        mutate(profit = gsub("%", "", profit)) %>%
        mutate(profit = gsub(",", ".", profit)) %>%
        mutate(profit = as.numeric(profit)) %>%
        mutate(year = as.factor(year(date))) %>%
        mutate(month = as.factor(month(date))) %>%
        mutate(wday = as.factor(wday(date, week_start = 1))) %>%
        mutate(timeclose = hms(timeclose)) %>%
        mutate(tradestop = case_when((timeclose$hour == 17 & timeclose$minute > 57) | (timeclose$hour < 14) ~ F, T ~ T)) %>%
        mutate(strategy = "SI_14_18")
df_SI_14_18 <- na.omit(df_SI_14_18)

df_SI_18_22 <- df_SI_18_22 %>%
        mutate(date = as.Date(date, format = '%d.%m.%Y')) %>%
        mutate(date = as.POSIXct(date, format = '%d.%m.%Y')) %>%
        mutate(profit = gsub("%", "", profit)) %>%
        mutate(profit = gsub(",", ".", profit)) %>%
        mutate(profit = as.numeric(profit)) %>%
        mutate(year = as.factor(year(date))) %>%
        mutate(month = as.factor(month(date))) %>%
        mutate(wday = as.factor(wday(date, week_start = 1))) %>%
        mutate(timeclose = case_when(timeclose == "" ~ "21:58:00", T ~ timeclose)) %>%
        mutate(timeclose = hms(timeclose)) %>%
        mutate(tradestop = case_when((timeclose$hour == 21 & timeclose$minute > 57) | (timeclose$hour < 18)  ~ F, T ~ T)) %>%
        mutate(strategy = "SI_18_22")
df_SI_18_22 <- na.omit(df_SI_18_22)
 
df_strategy <- do.call("rbind", list(df_RTS_10_14, df_RTS_14_18, df_RTS_18_22,
                                     df_SI_10_14,  df_SI_14_18,  df_SI_18_22))

df_strategy
}