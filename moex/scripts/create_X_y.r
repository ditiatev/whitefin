create_X_y <- function(df = df, 
                       train_index = train_index, 
                       test_index = test_index, 
                       vVars = vVars,
                       normalization = TRUE) {
        
        # get away outliers for train set
        df_train <- df[train_index,]
        for (vVar in vVars) {
                var <- df_train[,vVar]
                vMin <- quantile(var, 0.015, type = 1)
                vMax <- quantile(var, 0.985, type = 1)
                index <- which((df_train[,vVar] < vMin) | (df_train[,vVar] > vMax))
                df_train[index,vVar] <- NA
        }
        index <- complete.cases(df_train)
        df_train <- df_train[index,]
        
        X_train <- df_train[,vVars]
        y_train <- df_train[,c('profit')]
        date_train <- df_train[,c('date')]
        
        X_test  <- df[test_index,vVars]
        y_test  <- df[test_index,c('profit')]
        date_test  <- df[test_index,c('date')]
        
        if (normalization == TRUE) {
                
                normalizeXtrainXtest <-
                        function(X_train, X_test) {
                                
                                vColNames <- colnames(X_train)
                                for (vColName in vColNames) {
                                        
                                        x_sd   <- sd(X_train[,vColName], na.rm = T)
                                        x_mean <- mean(X_train[,vColName], na.rm = T)
                                        min_x  <- x_mean - x_sd * 3
                                        
                                        X_train[, vColName] <- ((X_train[,vColName] - min_x) / (6 * x_sd))
                                        X_test[, vColName]  <- ((X_test[,vColName] - min_x) / (6 * x_sd))
                                }

                                return(list('X_train' = X_train, 
                                            'X_test' = X_test))
                        }
                
                X <- normalizeXtrainXtest(X_train,X_test)
                X_train <- X$X_train
                X_test  <- X$X_test
        }
        
        l <- list('X_train' = X_train,
                  'y_train' = y_train,
                  'date_train' = date_train,
                  'X_test' = X_test,
                  'y_test' = y_test,
                  'date_test' = date_test)
        
        return(l)
}
