get_WorkSet <- function(vStrategies = NaN) {
        
        ############################################################
        # STRATEGISE
        ############################################################

        #####
        # to create united df with all strategis from separete files
        #####
        # source("./scripts/get_strategies.r")
        # df_strategy <- get_strategies()
        # write.csv(x=df_strategy, file="./data/strategy/strategy.csv", row.names = F)
        
        
        #####
        # read already created file
        #####
        library(dplyr)
        setwd("C:/R/Fin/whitefin/moex")
        df_strategy <- read.csv("./data/strategy/strategy.csv",
                                colClasses = c("factor","POSIXct","numeric","integer", "factor",
                                               rep("numeric",6),rep("factor",3)))
        
        if (!is.nan(vStrategies)) {
                df_daily <- df_strategy %>%
                        filter(strategy %in% vStrategies)
                
                df_daily$index <- row.names(df_daily)
                df_daily$strategy <- as.factor(as.character(df_daily$strategy))
                df_daily$date <- as.Date(df_daily$date)
                }
        
        ############################################################
        # MOEX
        ############################################################
        
        #####
        # get data from Moex server
        #####
        
        # source("./scripts/get_turnover_moex.r")
        # df_moex <- get_data_from_moex(startDate = "2012-01-01",endDate = "2020-03-31")
        # write.csv(x=df_moex, file="./data/moex.csv")
        
        # source("./scripts/get_index_moex.r")
        # l_index_moex <- get_index_moex(startDate = "2008-01-01", endDate = "2020-03-31")
        # df_index_moex <- l_index_moex[[1]]
        # df_index_moex_readme <- l_index_moex[[2]]
        # write.csv(x=df_index_moex, file="./data/index_moex_20080101_20200331.csv")
        # write.csv(x=df_index_moex_readme, file="./data/index_moex_readme_20080101_20200331.csv")
        
        #####
        # get data from local file
        #####
        
        #df_turnover_moex     <- read.csv("./data/moex.csv")
        df_index_moex        <- read.csv("./data/index_moex_20080101_20200331.csv")
        #df_index_moex_readme <- read.csv("./data/index_moex_readme_20080101_20200331.csv")
        
        # choose interesting values
        vColNamesMoex <- names(df_index_moex)
        vColNamesMoexRTS <- grep(("RTSI|IMOEX|MOEX10|MOEXFN|tradedate"), vColNamesMoex, value = T)
        vColNamesMoexRTS <- c(vColNamesMoexRTS,c("value.RTStn", "capitalization.RTStn", 
                                                 "value.RTStl", "capitalization.RTStl",
                                                 "value.RTS2",  "capitalization.RTS2",  
                                                 "value.RTSch", 
                                                 "value.RTSeu", "capitalization.RTSeu", 
                                                 "value.RTSfn", "capitalization.RTSfn",
                                                 "value.RTSin", "capitalization.RTSin", 
                                                 "value.RTSmm", "capitalization.RTSmm",
                                                 "value.RTSSIB","capitalization.RTSSIB",
                                                 "value.RTScr", "capitalization.RTScr",
                                                 "value.RTSog", "capitalization.RTSog",
                                                 "capitalization.RTSTR",
                                                 "capitalization.RTSTRN",
                                                 "capitalization.RTSTRR"))
        
        df_index_moexRTS <- df_index_moex[,vColNamesMoexRTS]
        drops <- c(grep(("RTSIN"), vColNamesMoexRTS, value = T),
                   "capitalization.IMOEX",
                   "capitalization.MOEX10",
                   "capitalization.MOEXFN")
        df_index_moexRTS <- df_index_moexRTS[ , !(names(df_index_moexRTS) %in% drops)]
        df_index_moexRTS[sapply(df_index_moexRTS, is.infinite)] <- NA
        df_index_moexRTS$tradedate <- as.Date(df_index_moexRTS$tradedate)
        
        ############################################################
        # Merge data
        ############################################################
        
        df_daily <- merge(df_daily, df_index_moexRTS, by.x = "date", by.y = "tradedate", all.x = TRUE)
        colnames(df_daily) <- gsub("\\.", "_",colnames(df_daily))
        
        df_daily
        
}