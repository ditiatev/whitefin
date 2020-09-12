get_index_moex <- function(startDate = "2015-01-01", endDate = "2015-01-31") {
        
        library(jsonlite)
        library(dplyr)

                # empty df_market
        df_market <- data.frame(matrix(ncol = 16, nrow = 0))
        x <- c("boardid","secid","tradedate","shortname","name",
               "close","open","high","low","value",
               "duration","yield","decimals","capitalization",
               "currencyid","divisor")
        colnames(df_market) <- x
        df_market <- df_market %>%
                mutate_all(as.character)
        
        # set dates
        dates <- seq(as.Date(startDate), as.Date(endDate), by = "1 days")
        for (date in dates) {
                # get data
                http <- paste0("https://iss.moex.com/iss/history/engines/stock/markets/index/securities.json?date=",
                               as.Date(date,origin = "1970-01-01"))
                jsonData <- fromJSON(http)
                
                data <- jsonData[["history"]][["data"]]
                if (is.null(data) == T) next
                if (nrow(data.frame(data)) == 0) next
                df_data <- data.frame(data)
                names(df_data) <- tolower(jsonData[["history"]][["columns"]])
                
                df_data <- df_data %>%
                        mutate_all(as.character)
                
                df_market <- rbind(df_market, df_data)
        }
        
        df_market$tradedate <- as.POSIXct(df_market$tradedate, origin = "1970-01-01")
        
        df_market$close          <- as.numeric(sub(",", ".", df_market$close, fixed = TRUE))
        df_market$open           <- as.numeric(sub(",", ".", df_market$open, fixed = TRUE))
        df_market$high           <- as.numeric(sub(",", ".", df_market$high, fixed = TRUE))
        df_market$low            <- as.numeric(sub(",", ".", df_market$low, fixed = TRUE))
        df_market$value          <- as.numeric(sub(",", ".", df_market$value, fixed = TRUE))
        df_market$capitalization <- as.numeric(sub(",", ".", df_market$capitalization, fixed = TRUE))
        
        df_market$diffHL <- (df_market$high - df_market$low)/df_market$open
        df_market$diffOC <- (df_market$open - df_market$close)/df_market$open
        df_market$diffOL <- (df_market$open - df_market$low)/df_market$open
        df_market$diffHO <- (df_market$high - df_market$open)/df_market$open
        
        columns <- c("diffHL", "diffOC", "diffOL", "diffHO", "value", "capitalization", "tradedate", "secid")
        df_marketR <- reshape(df_market[,columns],
                              v.names = c("diffHL", "diffOC", "diffOL", "diffHO", "value", "capitalization"),
                              idvar = c("tradedate"),
                              timevar = c("secid"),
                              direction = "wide")
        df_index_readme <- df_market %>%
                group_by(secid) %>%
                summarise(secid = secid[1], shortname = shortname[1], fullname = name[1])
        
        list(df_marketR,df_index_readme)
}