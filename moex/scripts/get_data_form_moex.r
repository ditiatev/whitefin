library(jsonlite)


dates <- seq(as.Date("2015-01-01"), as.Date("2015-01-31"), by = "1 days")
df_market <- data.frame(name = character(),
                        val = character(),
                        valusd = character(),
                        numtrades = character(),
                        date = Date())

for (date in dates) {
        # get data
        http <- paste0("https://iss.moex.com/iss/turnovers.json?date=",date)
        jsonData <- fromJSON(http)
        
        # into matrix
        turnovers <- jsonData[["turnovers"]][["data"]]
        turnoverssectors <- jsonData[["turnoverssectors"]][["data"]]
        
        # into df
        df_turnovers <- data.frame(turnovers)[, c(1, 3:5)]
        names(df_turnovers) <- c("name", "val", "valusd", "numtrades")
        df_turnovers <- na.omit(df_turnovers)
        
        df_turnoverssectors <- data.frame(turnoverssectors)[, c(1, 4:6)]
        names(df_turnoverssectors) <- c("name", "val", "valusd", "numtrades")
        df_turnoverssectors <- na.omit(df_turnoverssectors)
        
        # united
        df_un <- rbind(df_turnovers, df_turnoverssectors)
        
        df_un$name <- as.character(df_un$name)
        df_un$val <- as.character(df_un$val)
        df_un$valusd <- as.character(df_un$valusd)
        df_un$numtrades <- as.character(df_un$numtrades)
        df_un$date <- date
        
        df_market <- rbind(df_market, df_un)
        }

df_market$val       <- as.numeric(sub(",", ".", df_market$val, fixed = TRUE))
df_market$valusd    <- as.numeric(sub(",", ".", df_market$valusd, fixed = TRUE))
df_market$numtrades <- as.numeric(sub(",", ".", df_market$numtrades, fixed = TRUE))

df_marketR <- reshape(df_market,
        v.names = c("val", "valusd", "numtrades"),
        idvar = c("date"),
        timevar = c("name"),
        direction = "wide")


