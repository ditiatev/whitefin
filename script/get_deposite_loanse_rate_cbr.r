
get_deposite_ruble_30_cbr <- function(...) {
        
        #seatings
        library(xlsx)
        library(lubridate)
        library(dplyr)
        setwd("C:/R/Fin/whitefin")
        
        # download file from www.cbr.ru
        download_file <- function(locDestFile = "./data/deposits_30_e.xlsx",
                                  fileUrl = "https://www.cbr.ru/vfs/eng/statistics/pdko/int_rat/deposits_30_e.xlsx",
                                  freshTimeMin = 24*60) {

                if (!file.exists(locDestFile)) {
                        download.file(fileUrl,
                                      destfile = locDestFile,
                                      mode = "wb")
                } else {
                        holdTimeMin <- as.numeric(difftime(Sys.time(),file.info(locDestFile)$mtime,units = "mins"))
                        if (holdTimeMin > freshTimeMin) {
                                download.file(fileUrl,
                                              destfile = locDestFile,
                                              mode = "wb")
                        }
                }
                        
                        
        }
        download_file(...)
        
        # read file
        startRow <- 5
        df_dep <- read.xlsx("./data/deposits_30_e.xlsx",
                            sheetIndex = 1,
                            header = T,
                            startRow = startRow,
                            encoding = "UTF-8")
        
        # check file format / rename colomns
        col_rename <- function(df_dep) {
                coln_file <- colnames(df_dep)
                coln_file_formate <-
                        c(
                                "NA.",
                                "demand.deposits..",
                                "up.to.30.days..including.demand.deposits.",
                                "up.to.30.days..except.demand.deposits.",
                                "X31.to.90.days",
                                "X91.to.180.days",
                                "X181.days.to.1.year",
                                "up.to.1.year..including.demand.deposits.",
                                "up.to.1.year..except.demand.deposits",
                                "X1.to.3.years",
                                "over.3.years",
                                "over.1.year",
                                "up.to.30.days..including.demand.deposits..1",
                                "X31.to.90.days.1",
                                "X91.to.180.days.1",
                                "X181.days.to.1.year.1",
                                "up.to.1.year..including.demand.deposits..1",
                                "X1.to.3.years.1",
                                "over.3.years.1",
                                "over.1.year.1"
                        )
                coln_df_formate <-
                        c(
                                "date",
                                "ind_demand",
                                "ind_upto30daysincldemand",
                                "ind_upto30daysexcdemand",
                                "ind_X31to90days",
                                "ind_X91to180days",
                                "ind_X181daysto1year",
                                "ind_upto1yearincldemand",
                                "ind_upto1yearexcdemand",
                                "ind_X1to3years",
                                "ind_over3years",
                                "ind_over1year",
                                "firm_upto30daysincldemand",
                                "firm_X31to90days",
                                "firm_X91to180days",
                                "firm_X181daysto1year",
                                "firm_upto1yearincludingdemanddeposits",
                                "firm_X1to3years",
                                "firm_over3years",
                                "firm_over1year"
                        )
                
                if (identical(coln_file, coln_file_formate)) {
                        colnames(df_dep) <- coln_df_formate
                } else {
                        stop(" invalid file formate. Check 'https://www.cbr.ru/vfs/eng/statistics/pdko/int_rat/deposits_30_e.xlsx'.")
                }
                
                df_dep
        }
        
        # transform data
        df_dep <- col_rename(df_dep)
        df_dep <- df_dep %>%
                slice(1:(n()-1)) %>%
                mutate(date = parse_date_time(date, orders = c("bdy", "bY")))
        df_dep$date <- as.POSIXlt(df_dep$date)        
        df_dep$year <- df_dep$date$year + 1900
        df_dep$mon <- df_dep$date$mon + 1
        
        # return
        df_dep
}

get_loanse_ruble_30_cbr <- function(...) {
        
        #seatings
        library(xlsx)
        library(lubridate)
        library(dplyr)
        setwd("C:/R/Fin/whitefin")
        
        get_ind_loanse_ruble_30_cbr <- function(...) {

                # download file from www.cbr.ru
                download_file <- function(locDestFile = "./data/loans_ind_30_e.xlsx",
                                          fileUrl = "https://www.cbr.ru/vfs/eng/statistics/pdko/int_rat/loans_ind_30_e.xlsx",
                                          freshTimeMin = 24*60) {
                        
                        if (!file.exists(locDestFile)) {
                                download.file(fileUrl,
                                              destfile = locDestFile,
                                              mode = "wb")
                        } else {
                                holdTimeMin <- as.numeric(difftime(Sys.time(),file.info(locDestFile)$mtime,units = "mins"))
                                if (holdTimeMin > freshTimeMin) {
                                        download.file(fileUrl,
                                                      destfile = locDestFile,
                                                      mode = "wb")
                                }
                        }
                        
                        
                }
                download_file(...)
                
                # read file
                startRow <- 5
                loans_ind_30_e <- read.xlsx(
                        "./data/loans_ind_30_e.xlsx",
                        sheetIndex = 1,
                        header = T,
                        startRow = startRow,
                        encoding = "UTF-8"
                )
                
                # check file format / rename colomns
                col_rename <- function(loans_ind_30_e) {
                        coln_file <- colnames(loans_ind_30_e)
                        coln_file_formate <-
                                c(
                                        "NA.",
                                        "up.to.30.days..including.call.loans." ,
                                        "X31.to.90.days",
                                        "X91.to.180.days",
                                        "X181.days.to.1.year",
                                        "up.to.1.year..including.call.loans.",
                                        "X1.to.3.years",
                                        "over.3.years",
                                        "over.1.year",
                                        "up.to.30.days..including.call.loans..1",
                                        "X31.to.90.days.1",
                                        "X91.to.180.days.1",
                                        "X181.days.to.1.year.1",
                                        "up.to.1.year..including.call.loans..1",
                                        "X1.to.3.years.1",
                                        "over.3.years.1",
                                        "over.1.year.1"
                                )
                        coln_df_formate <-
                                c(
                                        "date",
                                        "ind_upto30daysinclcallloans" ,
                                        "ind_X31to90days",
                                        "ind_X91to180days",
                                        "ind_X181daysto1year",
                                        "ind_upto1yearinclcallloans",
                                        "ind_X1to3years",
                                        "ind_over3years",
                                        "ind_over1year",
                                        "ind_upto30daysinclcallloans_cars",
                                        "ind_X31to90days_cars",
                                        "ind_X91to180days_cars",
                                        "ind_X181daysto1year_cars",
                                        "ind_upto1yearinclcallloans_cars",
                                        "ind_X1to3years_cars",
                                        "ind_over3years_cars",
                                        "ind_over1year_cars"
                                )
                        
                        if (identical(coln_file, coln_file_formate)) {
                                colnames(loans_ind_30_e) <- coln_df_formate
                        } else {
                                stop(
                                        " invalid file formate. Check 'https://www.cbr.ru/vfs/eng/statistics/pdko/int_rat/loans_ind_30_e.xlsx'."
                                )
                        }
                        
                        loans_ind_30_e
                }
                
                # transform data
                loans_ind_30_e <- col_rename(loans_ind_30_e)
                
                loans_ind_30_e <- loans_ind_30_e %>%
                        slice(1:(n() - 1)) %>%
                        mutate(date = parse_date_time(date, orders = c("bdy", "bY")))
                
                # return
                loans_ind_30_e
        }
        
        get_firm_loanse_ruble_30_cbr <- function(...) {

                # download file from www.cbr.ru
                download_file <- function(locDestFile = "./data/loans_nonfin_30_e.xlsx",
                                          fileUrl = "https://www.cbr.ru/vfs/eng/statistics/pdko/int_rat/loans_nonfin_30_e.xlsx",
                                          freshTimeMin = 24*60) {
                        
                        if (!file.exists(locDestFile)) {
                                download.file(fileUrl,
                                              destfile = locDestFile,
                                              mode = "wb")
                        } else {
                                holdTimeMin <- as.numeric(difftime(Sys.time(),file.info(locDestFile)$mtime,units = "mins"))
                                if (holdTimeMin > freshTimeMin) {
                                        download.file(fileUrl,
                                                      destfile = locDestFile,
                                                      mode = "wb")
                                }
                        }
                        
                        
                }
                download_file(...)
                
                # read file
                startRow <- 5
                loans_firm_30_e <- read.xlsx(
                        "./data/loans_nonfin_30_e.xlsx",
                        sheetIndex = 1,
                        header = T,
                        startRow = startRow,
                        encoding = "UTF-8"
                )
                
                # check file format / rename colomns
                col_rename <- function(loans_firm_30_e) {
                        coln_file <- colnames(loans_firm_30_e)
                        coln_file_formate <-
                                c(
                                        "NA.",
                                        "up.to.30.days..including.call.loans." ,
                                        "X31.to.90.days",
                                        "X91.to.180.days",
                                        "X181.days.to.1.year",
                                        "up.to.1.year..including.call.loans.",
                                        "X1.to.3.years",
                                        "over.3.years",
                                        "over.1.year",
                                        "up.to.30.days..including.call.loans..1",
                                        "X31.to.90.days.1",
                                        "X91.to.180.days.1",
                                        "X181.days.to.1.year.1",
                                        "up.to.1.year..including.call.loans..1",
                                        "X1.to.3.years.1",
                                        "over.3.years.1",
                                        "over.1.year.1"
                                )
                        coln_df_formate <-
                                c(
                                        "date",
                                        "firm_upto30daysinclcallloans" ,
                                        "firm_X31to90days",
                                        "firm_X91to180days",
                                        "firm_X181daysto1year",
                                        "firm_upto1yearinclcallloans",
                                        "firm_X1to3years",
                                        "firm_over3years",
                                        "firm_over1year",
                                        "firm_upto30daysinclcallloans_cars",
                                        "firm_X31to90days_cars",
                                        "firm_X91to180days_cars",
                                        "firm_X181daysto1year_cars",
                                        "firm_upto1yearinclcallloans_cars",
                                        "firm_X1to3years_cars",
                                        "firm_over3years_cars",
                                        "firm_over1year_cars"
                                )
                        
                        if (identical(coln_file, coln_file_formate)) {
                                colnames(loans_firm_30_e) <- coln_df_formate
                        } else {
                                stop(
                                        " invalid file formate. Check 'https://www.cbr.ru/vfs/eng/statistics/pdko/int_rat/loans_nonfin_30_e.xlsx'."
                                )
                        }
                        
                        loans_firm_30_e
                }
                
                # transform data
                loans_firm_30_e <- col_rename(loans_firm_30_e)
                
                loans_firm_30_e <- loans_firm_30_e %>%
                        slice(1:(n() - 1)) %>%
                        mutate(date = parse_date_time(date, orders = c("bdy", "bY")))
                
                # return
                loans_firm_30_e
        }
        
        df_ind <- get_ind_loanse_ruble_30_cbr()
        df_firm <- get_firm_loanse_ruble_30_cbr()
        
        df_loanse <- merge(x = df_ind, y = df_firm, by = "date", all = T)
        df_loanse$date <- as.POSIXlt(df_loanse$date)
        df_loanse$year <- df_loanse$date$year + 1900
        df_loanse$mon <- df_loanse$date$mon + 1
        
        # return
        df_loanse
}



