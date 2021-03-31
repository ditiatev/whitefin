makeNextColForStrategy <- function(df_strategy, vColNames, sliding_depth = 30) {
        len <- nrow(df_strategy)

        for (vN in 1:sliding_depth) {
                
                for (vColName in vColNames) {
                        vColNameNextN <- paste0(vColName,"_Next",vN)
                        df_strategy[,vColNameNextN] <- c(tail(df_strategy[,vColName],len-vN),rep(NA,vN))
                }
        }
        
        return(df_strategy[,paste0(vColName,"_Next",1:sliding_depth)])
}