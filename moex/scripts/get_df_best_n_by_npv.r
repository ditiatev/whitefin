get_df_best_n_by_npv <- function(df_train = df_train, vProfitNext = vProfitNext) {

l_npv <- list()
n_max <- 0
NPV_max <- 0
for (n in 1:(vProfitNext-1)) {
        df_train[,'profit_pred'] <- apply(df_train[, paste0('profit_pred', "_Pr", 0:n)], 1, sum, na.rm = T)
        
        min_pred <- min(df_train$profit_pred, na.rm = T)
        max_pred <- max(df_train$profit_pred, na.rm = T)
        lev_profit_preds <- seq(from = min_pred, to = max_pred, length.out= 1000)
        
        max_profit_by_day <- 0
        max_lev_profit_pred <- 0
        for (lev_profit_pred in lev_profit_preds[1:999]) {
                index <- which(df_train$profit_pred > lev_profit_pred)
                profit_by_day <- sum(df_train[index,"profit"], na.rm = T)
                if (profit_by_day > max_profit_by_day) {
                        max_profit_by_day <- profit_by_day
                        max_lev_profit_pred <- lev_profit_pred
                }
        }
        
        index <- which(df_train$profit_pred > max_lev_profit_pred)
        df_train[,"trade_index"] <- 'NOT TRADE'
        df_train[index,"trade_index"] <- 'TRADE'
        
        df_train_summary <- df_train %>%
                mutate(profit_model = if_else(trade_index == 'TRADE', profit, 0)) %>%
                group_by(year, month) %>%
                summarise(profit_model = sum(profit_model))
        
        NPV <- mean(culculate_NPV(df_train_summary$profit_model))
        l_npv[length(l_npv)+1] <- list(c('n' = n, 'npv' = NPV, 'lev_prof' = max_lev_profit_pred))
        
        if (NPV > NPV_max) {
                NPV_max <- NPV
                n_max <- n
                best_lev_profit_pred <- max_lev_profit_pred
        }
}

df_npv <- as.data.frame(do.call(rbind,l_npv))
df_npv <- df_npv %>%
        arrange(desc(npv)) %>%
        head(3)

return(df_npv)
}