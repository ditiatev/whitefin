get_primary_summary <- function(pred, y, point = NA) {
        df <- data.frame('pred' = pred, 'profit' = y)
        
        if (is.na(point)) {
                vMin <- quantile(df$pred, 0.2, type = 1)
                vMax <- quantile(df$pred, 0.2, type = 1)
        } else {
                vMin <- point; 
                vMax <- point;
        }
        
        min_index <- which(df$pred <= vMin)
        max_index <- which(df$pred >= vMax)
        
        min_index_down0 <- which((df$pred <= vMin) & (df$profit < 0))
        min_index_up0   <- which((df$pred >= vMax) & (df$profit > 0))
        
        profit_min       <- mean(df[min_index,'profit'], na.rm = T)
        profit_max       <- mean(df[max_index,'profit'], na.rm = T)
        profit_min_down0 <- mean(df[min_index_down0,'profit'], na.rm = T)
        profit_max_up0   <- mean(df[min_index_up0,'profit'], na.rm = T)
        profit           <- mean(df[,'profit'], na.rm = T)
        
        primary_summary <- c(profit_min,profit_min_down0,profit_max,profit_max_up0,profit)
        return(primary_summary)
}