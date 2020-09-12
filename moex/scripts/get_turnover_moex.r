get_turnover_moex <- function(startDate = "2015-01-01", endDate = "2015-01-31") {

library(jsonlite)
dates <- seq(as.Date(startDate), as.Date(endDate), by = "1 days")
df_market <- data.frame(name = character(),
                        val = character(),
                        valusd = character(),
                        numtrades = character(),
                        date = Date())

for (date in dates) {
        # get data
        http <- paste0("https://iss.moex.com/iss/turnovers.json?date=",as.Date(date,origin = "1970-01-01"))
        jsonData <- fromJSON(http)
        
        # turnovers
        turnovers <- jsonData[["turnovers"]][["data"]]
        if (is.null(turnovers) == T) next
        if (nrow(data.frame(turnovers)) != 0) {
                df_turnovers <- data.frame(turnovers)[, c(1, 3:5)]
                names(df_turnovers) <- c("name", "val", "valusd", "numtrades")
                df_turnovers <- na.omit(df_turnovers)
        }
        
        # turnoverssectors
        turnoverssectors <- jsonData[["turnoverssectors"]][["data"]]
        if (is.null(turnoverssectors) == T) next
        if (nrow(data.frame(turnoverssectors)) != 0) {
                df_turnoverssectors <- data.frame(turnoverssectors)[, c(1, 4:6)]
                names(df_turnoverssectors) <- c("name", "val", "valusd", "numtrades")
                df_turnoverssectors <- na.omit(df_turnoverssectors)
                
                # united
                df_un <- rbind(df_turnovers, df_turnoverssectors)
        } else {df_un <- df_turnovers}


        if (nrow(df_un) == 0) next
        df_un$name <- as.character(df_un$name)
        df_un$val <- as.character(df_un$val)
        df_un$valusd <- as.character(df_un$valusd)
        df_un$numtrades <- as.character(df_un$numtrades)
        df_un$date <- as.Date(date,origin = "1970-01-01")
        
        df_market <- rbind(df_market, df_un)
}

df_market$val       <- as.numeric(sub(",", ".", df_market$val, fixed = TRUE))
df_market$valusd    <- as.numeric(sub(",", ".", df_market$valusd, fixed = TRUE))
df_market$numtrades <- as.numeric(sub(",", ".", df_market$numtrades, fixed = TRUE))
df_market$date      <- as.POSIXct(df_market$date, origin = "1970-01-01")

df_marketR <- reshape(df_market,
        v.names = c("val", "valusd", "numtrades"),
        idvar = c("date"),
        timevar = c("name"),
        direction = "wide")

rm(df_market)

df_marketR
}