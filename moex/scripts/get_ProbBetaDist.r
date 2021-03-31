get_ProbBetaDist <- function(df_daily, vColName = "tradestop", sliding_depth = 30) {
        
        setwd("C:/R/Fin/whitefin/moex")
        source("./scripts/makePrColForStrategy.r")
        
        # Previos value
        df_daily <- makePrColForStrategy(df_strategy = df_daily,
                                         vColNames = vColName,
                                         sliding_depth = sliding_depth)
        
        for (n in 2:sliding_depth) {
                df_daily[,paste0(vColName,"_MA",n)] <- apply(df_daily[,paste0(vColName,"_Pr",1:n)],1,sum,na.rm = F)
        }
        
        mean_rbeta <- function(x, col_name) {
                # calculating sliding depth
                n <- as.integer(sub(paste0(vColName,"_MA"),"",col_name))
                # return mean of rbeta distribution
                shape1 = as.integer(x[col_name])
                shape2 = n-as.integer(x[col_name])
                if (is.na(shape1)|is.nan(shape1)) {return(NA)}
                mean(rbeta(1000, shape1 = shape1, shape2 = shape2))
        }
        
        # Probability for each sliding 
        for (col_name in paste0(vColName,"_MA",2:sliding_depth)) {
                df_daily[,paste0("prob_",col_name)] <- apply(df_daily[,], 1, FUN = function(x) mean_rbeta(x,col_name))  
        }
        
        # Summary result
        lProb <- list()
        # mean
        for (n in 3:sliding_depth) {
                vProbTS_mean <- apply(df_daily[,paste0("prob_",vColName,"_MA",2:n)],1,mean,na.rm = F)
                vProbTS_mean[is.nan(vProbTS_mean)] <- NA
                lProb[paste0('mean_',n)] <- list(vProbTS_mean)
        }
        # sd
        for (n in 3:sliding_depth) {
                vProbTS_sd   <- apply(df_daily[,paste0("prob_",vColName,"_MA",2:n)],1,sd,na.rm = F)
                vProbTS_sd[is.nan(vProbTS_sd)] <- NA
                lProb[paste0('sd',n)] <- list(vProbTS_sd)
                
        }
        dfProb <- data.frame(t(matrix(unlist(lProb), nrow=length(lProb), byrow=T)))
        colnames(dfProb) <- c(paste0('mean_',3:sliding_depth),paste0('sd_',3:sliding_depth))
        
        return(dfProb)

}