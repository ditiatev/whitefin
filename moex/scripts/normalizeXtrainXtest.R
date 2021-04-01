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