get_NormalDistForvard <- function(df_daily, vColName = "profit", sliding_depth = 30) {
        
        setwd("C:/R/Fin/whitefin/moex")
        source("./scripts/makeNextColForStrategy.r")
        
        # Previos value
        df_daily <- makeNextColForStrategy(df_strategy = df_daily,
                                           vColNames = vColName,
                                           sliding_depth = sliding_depth)
        
        for (n in 2:sliding_depth) {
                df_daily[,paste0(vColName,"_mean_Next_",n)] <- apply(df_daily[,paste0(vColName,"_Next",1:n)],1,mean,na.rm = F)
                df_daily[,paste0(vColName,"_sd_Next_",  n)] <- apply(df_daily[,paste0(vColName,"_Next",1:n)],1,sd,na.rm = F)
        }
        
        col_names <- c(paste0(vColName,'_mean_Next_',2:sliding_depth),paste0(vColName,'_sd_Next_',2:sliding_depth))
        
        return(df_daily[,col_names])
        
}