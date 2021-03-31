get_strategies <- function() {
# Setup ===========================================================================================

library(dplyr)
setwd("C:/R/Fin/whitefin/moex")

# Read csv =========================================================================================


### _StopLoss_RTS =====
df_2008_2013_RTS_10_14 <- read.csv("./data/strategy/stop_loss_interday/2008-2013_RTS_10-14.csv", encoding = "UTF-8", sep = ";")
df_2014_2018_RTS_10_14 <- read.csv("./data/strategy/stop_loss_interday/2014-2018_RTS_10-14.csv", encoding = "UTF-8", sep = ";")
df_2019_2020_RTS_10_14 <- read.csv("./data/strategy/stop_loss_interday/2019-2020_RTS_10-14.csv", encoding = "UTF-8", sep = ";")

df_2008_2013_RTS_14_18 <- read.csv("./data/strategy/stop_loss_interday/2008-2013_RTS_14-18.csv", encoding = "UTF-8", sep = ";")
df_2014_2018_RTS_14_18 <- read.csv("./data/strategy/stop_loss_interday/2014-2018_RTS_14-18.csv", encoding = "UTF-8", sep = ";")
df_2019_2020_RTS_14_18 <- read.csv("./data/strategy/stop_loss_interday/2019-2020_RTS_14-18.csv", encoding = "UTF-8", sep = ";")

df_RTS_18_22           <- read.csv("./data/strategy/stop_loss_interday/dailystrategydata.csv", encoding = "UTF-8")

### _StopLoss_Intervals =====

### _RTS
vDfNames_Int <- c()
l_Intervals <- list()
vPath <- "./data/strategy/intervals/RTS_TS/10_14/"
vFiles <- list.files(vPath)

for(vFile in vFiles) {
        vDfName <- paste0("df_SL_10_14_",gsub("\\(0,","Int",vFile))
        vDfName <- gsub("\\s+"," ",vDfName)
        vDfName <- gsub("\\s","_",vDfName)
        vDfName <- gsub(" |-","_",gsub("\\.|csv|\\)","",vDfName))
        vDfNames_Int[length(vDfNames_Int)+1] <- vDfName
        l_Intervals[[vDfName]] <- read.csv(paste0(vPath,vFile), encoding = "UTF-8", sep = ";")
}

vPath <- "./data/strategy/intervals/RTS_TS/14_18/"
vFiles <- list.files(vPath)

for(vFile in vFiles) {
        vDfName <- paste0("df_SL_14_18_",gsub("\\(0,","Int",vFile))
        vDfName <- gsub("\\s+"," ",vDfName)
        vDfName <- gsub("\\s","_",vDfName)
        vDfName <- gsub(" |-","_",gsub("\\.|csv|\\)","",vDfName))
        vDfNames_Int[length(vDfNames_Int)+1] <- vDfName
        l_Intervals[[vDfName]] <- read.csv(paste0(vPath,vFile), encoding = "UTF-8", sep = ";")
}

vPath <- "./data/strategy/intervals/RTS_TS/18_22/"
vFiles <- list.files(vPath)

for(vFile in vFiles) {
        vDfName <- paste0("df_SL_18_22_",gsub("\\(0,","Int",vFile))
        vDfName <- gsub("\\s+"," ",vDfName)
        vDfName <- gsub("\\s","_",vDfName)
        vDfName <- gsub(" |-","_",gsub("\\.|csv|\\)","",vDfName))
        vDfNames_Int[length(vDfNames_Int)+1] <- vDfName
        l_Intervals[[vDfName]] <- read.csv(paste0(vPath,vFile), encoding = "UTF-8", sep = ";")
}

### _SI

vPath <- "./data/strategy/intervals/SI_TS/10_14/"
vFiles <- list.files(vPath)

for(vFile in vFiles) {
        vDfName <- paste0("df_SL_10_14_SI",gsub("\\(0,","Int",vFile))
        vDfName <- gsub("\\s+"," ",vDfName)
        vDfName <- gsub("\\s","_",vDfName)
        vDfName <- gsub("\\.|csv|\\)|RTS","",vDfName)
        vDfName <- gsub(" |-","_",vDfName)
        vDfNames_Int[length(vDfNames_Int)+1] <- vDfName
        l_Intervals[[vDfName]] <- read.csv(paste0(vPath,vFile), encoding = "UTF-8", sep = ";")
}

vPath <- "./data/strategy/intervals/SI_TS/14_18/"
vFiles <- list.files(vPath)

for(vFile in vFiles) {
        vDfName <- paste0("df_SL_14_18_SI",gsub("\\(0,","Int",vFile))
        vDfName <- gsub("\\s+"," ",vDfName)
        vDfName <- gsub("\\s","_",vDfName)
        vDfName <- gsub("\\.|csv|\\)|RTS","",vDfName)
        vDfName <- gsub(" |-","_",vDfName)
        vDfNames_Int[length(vDfNames_Int)+1] <- vDfName
        l_Intervals[[vDfName]] <- read.csv(paste0(vPath,vFile), encoding = "UTF-8", sep = ";")
}

vPath <- "./data/strategy/intervals/SI_TS/18_22/"
vFiles <- list.files(vPath)

for(vFile in vFiles) {
        vDfName <- paste0("df_SL_18_22_SI",gsub("\\(0,","Int",vFile))
        vDfName <- gsub("\\s+"," ",vDfName)
        vDfName <- gsub("\\s","_",vDfName)
        vDfName <- gsub("\\.|csv|\\)|RTS","",vDfName)
        vDfName <- gsub(" |-","_",vDfName)
        vDfNames_Int[length(vDfNames_Int)+1] <- vDfName
        l_Intervals[[vDfName]] <- read.csv(paste0(vPath,vFile), encoding = "UTF-8", sep = ";")
}
#######

### _StopLoss_SI =====
df_2008_2013_SI_10_14 <- read.csv("./data/strategy/stop_loss_interday/2008-2013_Si_10-14.csv", encoding = "UTF-8", sep = ";")
df_2014_2018_SI_10_14 <- read.csv("./data/strategy/stop_loss_interday/2014-2018_Si_10-14.csv", encoding = "UTF-8", sep = ";")
df_2019_2020_SI_10_14 <- read.csv("./data/strategy/stop_loss_interday/2019-2020_Si_10-14.csv", encoding = "UTF-8", sep = ";")

df_2008_2013_SI_14_18 <- read.csv("./data/strategy/stop_loss_interday/2008-2013_Si_14-18.csv", encoding = "UTF-8", sep = ";")
df_2014_2018_SI_14_18 <- read.csv("./data/strategy/stop_loss_interday/2014-2018_Si_14-18.csv", encoding = "UTF-8", sep = ";")
df_2019_2020_SI_14_18 <- read.csv("./data/strategy/stop_loss_interday/2019-2020_Si_14-18.csv", encoding = "UTF-8", sep = ";")

df_2008_2013_SI_18_22 <- read.csv("./data/strategy/stop_loss_interday/2008-2013_Si_18-22.csv", encoding = "UTF-8", sep = ";")
df_2014_2018_SI_18_22 <- read.csv("./data/strategy/stop_loss_interday/2014-2018_Si_18-22.csv", encoding = "UTF-8", sep = ";")
df_2019_2020_SI_18_22 <- read.csv("./data/strategy/stop_loss_interday/2019-2020_Si_18-22.csv", encoding = "UTF-8", sep = ";")

### _TakeProfit =====
vDfNames_TakeProfit <- c()
l_TakeProfit <- list()
vTakeProfitFiles <- list.files("./data/strategy/take_profit_intraday/")
for(vTakeProfitFile in vTakeProfitFiles) {
        vDfName <- paste0("df_TP",gsub(" |-","_",gsub("\\(|\\.|csv|\\)","",vTakeProfitFile)))
        vDfNames_TakeProfit[length(vDfNames_TakeProfit)+1] <- vDfName
        l_TakeProfit[[vDfName]] <- read.csv(paste0("./data/strategy/take_profit_intraday/",vTakeProfitFile), encoding = "UTF-8", sep = ";")
}

# Selecte columns ==================================================================================

### _StopLoss_RTS =====
vNColToSelect <- c(2,9,11,17,18,26,27,29,31,33)

df_2008_2013_RTS_10_14 <- df_2008_2013_RTS_10_14[,vNColToSelect]
df_2014_2018_RTS_10_14 <- df_2014_2018_RTS_10_14[,vNColToSelect]
df_2019_2020_RTS_10_14 <- df_2019_2020_RTS_10_14[,vNColToSelect]
df_2008_2013_RTS_14_18 <- df_2008_2013_RTS_14_18[,vNColToSelect]
df_2014_2018_RTS_14_18 <- df_2014_2018_RTS_14_18[,vNColToSelect]
df_2019_2020_RTS_14_18 <- df_2019_2020_RTS_14_18[,vNColToSelect]
df_RTS_18_22           <- df_RTS_18_22[,c(2,8,10,15,16,24,25,27,29,31)]

### _StopLoss_SI =====
df_2008_2013_SI_10_14 <- df_2008_2013_SI_10_14[,vNColToSelect]
df_2014_2018_SI_10_14 <- df_2014_2018_SI_10_14[,vNColToSelect]
df_2019_2020_SI_10_14 <- df_2019_2020_SI_10_14[,vNColToSelect]
df_2008_2013_SI_14_18 <- df_2008_2013_SI_14_18[,vNColToSelect]
df_2014_2018_SI_14_18 <- df_2014_2018_SI_14_18[,vNColToSelect]
df_2019_2020_SI_14_18 <- df_2019_2020_SI_14_18[,vNColToSelect]
df_2008_2013_SI_18_22 <- df_2008_2013_SI_18_22[,vNColToSelect]
df_2014_2018_SI_18_22 <- df_2014_2018_SI_18_22[,vNColToSelect]
df_2019_2020_SI_18_22 <- df_2019_2020_SI_18_22[,vNColToSelect]

### _TakeProfit =====
for (vDfName in vDfNames_TakeProfit) {
        l_TakeProfit[[vDfName]] <- l_TakeProfit[[vDfName]][,vNColToSelect]     
}

### _StopLoss_Intervals =====
for (vDfName in vDfNames_Int) {
        l_Intervals[[vDfName]] <- l_Intervals[[vDfName]][,vNColToSelect]
}


# Name columns ==================================================================================

vColNames <- c("position","date","entryprice","timeclose","exitprice","bartime","barprofit","profit","MAE","MFE")

### _StopLoss_RTS =====
names(df_2008_2013_RTS_10_14) <- vColNames
names(df_2014_2018_RTS_10_14) <- vColNames
names(df_2019_2020_RTS_10_14) <- vColNames
names(df_2008_2013_RTS_14_18) <- vColNames
names(df_2014_2018_RTS_14_18) <- vColNames
names(df_2019_2020_RTS_14_18) <- vColNames
names(df_RTS_18_22)           <- vColNames

### _StopLoss_SI =====
names(df_2008_2013_SI_10_14) <- vColNames
names(df_2014_2018_SI_10_14) <- vColNames
names(df_2019_2020_SI_10_14) <- vColNames
names(df_2019_2020_SI_14_18) <- vColNames
names(df_2008_2013_SI_14_18) <- vColNames
names(df_2014_2018_SI_14_18) <- vColNames
names(df_2008_2013_SI_18_22) <- vColNames
names(df_2014_2018_SI_18_22) <- vColNames
names(df_2019_2020_SI_18_22) <- vColNames

### _TakeProfit =====
for (vDfName in vDfNames_TakeProfit) {
        names(l_TakeProfit[[vDfName]]) <- vColNames
}

### _StopLoss_Intervals =====
for (vDfName in vDfNames_Int) {
        names(l_Intervals[[vDfName]]) <- vColNames
}


# Unite diff periods =========================================================================

### _StopLoss_RTS ===========================================

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

### _StopLoss_SI ============================================

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

### _TakeProfit =====
for (vDfName in vDfNames_TakeProfit) {
        l_TakeProfit[[vDfName]] <- l_TakeProfit[[vDfName]] %>%
                mutate_all(as.character)
        l_TakeProfit[[vDfName]]$strategy <- gsub("df_|2019_2020|2008_2013|2014_2018","",vDfName)
}
df_TP <- dplyr::bind_rows(l_TakeProfit)

### _StopLoss_Intervals =====
for (vDfName in vDfNames_Int) {
        l_Intervals[[vDfName]] <- l_Intervals[[vDfName]] %>%
                mutate_all(as.character)
        l_Intervals[[vDfName]]$strategy <- gsub("df_|2019_2020|2008_2013|2014_2018|2014_2020|2010_2013|2019_2020","",vDfName)
}
df_intervals <- dplyr::bind_rows(l_Intervals)


# Mutate sep data ======================================================================================

### _StopLoss_RTS =====
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
        mutate(strategy = "RTS_18_22") %>%
        mutate(entryprice = gsub('[^0-9.]','',entryprice)) %>%
        mutate(exitprice = gsub('[^0-9.]','',exitprice))
df_RTS_18_22 <- na.omit(df_RTS_18_22)

### _StopLoss_SI =====
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

### _TakeProfit =====
df_TP <- df_TP %>%
        filter(!is.na(timeclose)) %>%
        mutate(date = as.Date(date, format = '%d.%m.%Y')) %>%
        mutate(date = as.POSIXct(date, format = '%d.%m.%Y')) %>%
        mutate(profit = gsub("%", "", profit)) %>%
        mutate(profit = gsub(",", ".", profit)) %>%
        mutate(profit = as.numeric(profit)) %>%
        mutate(year = as.factor(year(date))) %>%
        mutate(month = as.factor(month(date))) %>%
        mutate(wday = as.factor(wday(date, week_start = 1))) %>%
        mutate(timeclose = hms(timeclose))

df_TP$hstart <- unlist(strsplit(df_TP$strategy, "_"))[(as.numeric(row.names(df_TP))-1)*3+as.numeric(row.names(df_TP))+2]
df_TP$hstop <- unlist(strsplit(df_TP$strategy, "_"))[(as.numeric(row.names(df_TP))-1)*3+as.numeric(row.names(df_TP))+3]
df_TP$hstop  <- as.numeric(df_TP$hstop)
df_TP$hstart <- as.numeric(df_TP$hstart)

df_TP <- df_TP %>%
        mutate(tradestop = case_when((timeclose$hour == (hstop-1) & timeclose$minute > 57) | (timeclose$hour < hstart) ~ F, T ~ T))
df_TP <- na.omit(df_TP)

drops <- c("hstart","hstop")
df_TP <- df_TP[ , !(names(df_TP) %in% drops)]

### _StopLoss_Intervals =====

df_intervals <- df_intervals %>%
        filter(!is.na(timeclose)) %>%
        mutate(date = as.Date(date, format = '%d.%m.%Y')) %>%
        mutate(date = as.POSIXct(date, format = '%d.%m.%Y')) %>%
        mutate(profit = gsub("%", "", profit)) %>%
        mutate(profit = gsub(",", ".", profit)) %>%
        mutate(profit = as.numeric(profit)) %>%
        mutate(year = as.factor(year(date))) %>%
        mutate(month = as.factor(month(date))) %>%
        mutate(wday = as.factor(wday(date, week_start = 1))) %>%
        mutate(timeclose = hms(timeclose))

df_intervals$strategy <- gsub("\\_+","_",df_intervals$strategy)
df_intervals$hstart <- unlist(strsplit(df_intervals$strategy, "_"))[(as.numeric(row.names(df_intervals))-1)*4+as.numeric(row.names(df_intervals))+1]
df_intervals$hstop <- unlist(strsplit(df_intervals$strategy, "_"))[(as.numeric(row.names(df_intervals))-1)*4+as.numeric(row.names(df_intervals))+2]
df_intervals$hstop  <- as.numeric(df_intervals$hstop)
df_intervals$hstart <- as.numeric(df_intervals$hstart)

df_intervals <- df_intervals %>%
        mutate(tradestop = case_when((timeclose$hour == (hstop-1) & timeclose$minute > 57) | (timeclose$hour < hstart) | (timeclose$hour == hstop)~ F, T ~ T))
df_intervals <- na.omit(df_intervals)

drops <- c("hstart","hstop")
df_intervals <- df_intervals[ , !(names(df_intervals) %in% drops)]


# Unite strategies ==== 
df_strategy <- do.call("rbind", list(df_RTS_10_14, df_RTS_14_18, df_RTS_18_22,
                                     df_SI_10_14,  df_SI_14_18,  df_SI_18_22,
                                     df_TP,df_intervals))

# Mutate united data ====
df_strategy <- df_strategy %>%
        mutate(exitprice = gsub(" ", "", exitprice),
               entryprice = gsub(" ", "", entryprice)) %>%
        mutate(exitprice = case_when(exitprice == "" ~ "0", T ~ exitprice)) %>%
        mutate(entryprice = case_when(entryprice == "" ~ "0", T ~ entryprice)) %>%
        mutate(strategy = as.factor(strategy),
               position = as.factor(position),
               entryprice = as.numeric(sub(",", ".", entryprice, fixed = TRUE)),
               exitprice = as.numeric(sub(",", ".", exitprice, fixed = TRUE)),
               bartime = as.numeric(sub(",", ".", bartime, fixed = TRUE)),
               barprofit = as.numeric(sub(",", ".", barprofit, fixed = TRUE)),
               MAE = as.numeric(sub(",", ".", gsub("%","",MAE), fixed = TRUE)),
               MFE = as.numeric(sub(",", ".", gsub("%","",MFE), fixed = TRUE))) %>%
        select(strategy,date,profit,tradestop,position,entryprice,
               exitprice,bartime,barprofit,MAE,MFE,year,month,wday) %>%
        mutate(year = as.factor(as.numeric(as.character(year))),
               tradestop = as.integer(tradestop))

df_strategy

}