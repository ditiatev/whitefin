makePrColForStrategy <- function(df_strategy, vColNames, sliding_depth = 30, vColReturn = NaN) {
        library(dplyr)
        for (vN in c(1,1:sliding_depth)) {
                
                for (vColName in vColNames) {
                        vColNamePrN <- paste0(vColName,"_Pr",vN)
                        df_strategy[,vColNamePrN] <- c(rep(NA,vN), head(df_strategy[,vColName],-vN))
                }
                
                # # get first N rows for each group
                # df_daily_na <- df_strategy %>%
                #         group_by(strategy) %>%
                #         slice_max(order_by = desc(date), n = vN)
                # 
                # for (vColName in vColNames) {
                #         vColNamePrN <- paste0(vColName,"_Pr",vN)
                #         df_daily_na[,vColNamePrN] <- NA
                # }
                # 
                # # get index of NA rows
                # index_na <- which(df_strategy$index %in% as.numeric(df_daily_na$index))
                # # get data witout NA rows
                # df_daily_nona <- df_strategy[-index_na,]
                # # united NA and NoNA
                # df_strategy <- dplyr::bind_rows(list(df_daily_na, df_daily_nona))
                # # sort
                # df_strategy <- df_strategy %>%
                #         arrange(strategy, date)
        }
        
        if (!is.nan(vColReturn)[1]) {
                vColNamesReturn <- paste0(vColNames,"_Pr",vColReturn)
        } else {
                vColNamesReturn <- paste0(vColNames,"_Pr",1:sliding_depth)
        }
        
        df_strategy[,vColNamesReturn]
}
