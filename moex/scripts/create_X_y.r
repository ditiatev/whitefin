create_X_y <- function(df = df, 
                       train_index = train_index, 
                       test_index = test_index, 
                       vVar1 = vVar1, 
                       vVar2 = vVar2,
                       normalization = TRUE) {
        
        X_train <- df[train_index,c(vVar1,vVar2)]
        y_train <- df[train_index,c('profit')]
        X_test  <- df[test_index,c(vVar1,vVar2)]
        y_test  <- df[test_index,c('profit')]
        
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
                  'X_test' = X_test,
                  'y_test' = y_test)
        
        return(l)
}
