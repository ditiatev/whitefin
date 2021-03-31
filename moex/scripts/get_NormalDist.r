get_NormalDist <- function(df_daily, vColNames = "profit", sliding_depth = 30, slidind_return = NA) {
        
        setwd("C:/R/Fin/whitefin/moex")
        source("./scripts/makePrColForStrategy.r")
        l_df <- list()

        for (vColName in vColNames) {
                # Previos value
                df_daily_pr <- makePrColForStrategy(
                        df_strategy = df_daily,
                        vColNames = vColName,
                        sliding_depth = sliding_depth
                )
                df_daily <- cbind(df_daily,df_daily_pr)
                
                # culculate mean & sd
                for (n in 2:sliding_depth) {
                        df_daily[, paste0(vColName, "_mean_Pr_", n)] <-
                                apply(df_daily[, paste0(vColName, "_Pr", 1:n)], 1, mean, na.rm = F)
                        df_daily[, paste0(vColName, "_sd_Pr_",  n)] <-
                                apply(df_daily[, paste0(vColName, "_Pr", 1:n)], 1, sd, na.rm = F)
                }
                
                # return only slidind_return columns
                if (is.na(slidind_return)) {
                        col_names <- c(
                                paste0(vColName, '_mean_Pr_', 2:sliding_depth),
                                paste0(vColName, '_sd_Pr_', 2:sliding_depth)
                        )
                } else {
                        col_names <- c(
                                paste0(vColName, '_mean_Pr_', slidind_return),
                                paste0(vColName, '_sd_Pr_', slidind_return)
                        )
                }
                
                # put into list
                l_df[vColName] <- list(df_daily[,col_names])
        }

                df <- do.call("cbind", l_df)
                colnames(df) <- sub('.*\\.', '', colnames(df))
                
        return(df)
        
}