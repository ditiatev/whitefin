normalize_df <- function(df, vColNames, method = 'sd') {
        normalize_column <- function(x)
        {
                return((x- min(x, na.rm = T)) /(max(x, na.rm = T)-min(x, na.rm = T)))
        }
        
        normalize_column_sd <- function(x)
        {
                x_sd   <- sd(x, na.rm = T)
                x_mean <- mean(x, na.rm = T)
                min_x  <- x_mean-x_sd*3
                return((x-min_x)/(6*x_sd))
        }
        
        if (method == 'sd') {
                
                for (vColName in vColNames) {
                        df[,vColName] <- normalize_column_sd(df[,vColName])
                }
                
        } else {
                
                for (vColName in vColNames) {
                        df[,vColName] <- normalize_column(df[,vColName])
                }
                
        }

        
        return(df)
}