create_lxy_train1_train2 <- function(df_profitPr = df_profitPr,
                                     date = date,
                                     vVars = vVars,
                                     section_length = 21,
                                     normalization = TRUE) {
        
        create_X_y <- function(df = df, 
                               train_index = train_index, 
                               test_index = test_index, 
                               vVars = vVars,
                               normalization = normalization) {
                

                # new start ################################
                
                #
                ### old
                # X_train <- df[train_index,vVars]
                # y_train <- df[train_index,c('profit')]
                # date_train <- df[train_index,c('date')]
                ### old
                #
                
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
                # new end ################################
                
                
                
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
        
        get_separate_index <- function(df, section_length = section_length) {
                # form index
                double_SL <- section_length*2
                n_row <- nrow(df)
                
                vlen <- ceiling(n_row/double_SL)
                vStart <- ((1:vlen)*double_SL-(double_SL-1)); 
                vEnd <- ((1:vlen)*double_SL-(double_SL-1))+(section_length-1)
                
                index_1 <- c(); index_2 <- c()
                for (x in 1:vlen) {
                        index_1 <- c(index_1,seq(from = vStart[x], to = vEnd[x]))
                        index_2 <- c(index_2,seq(from = vStart[x]+section_length, to = vEnd[x]+section_length))
                }
                index_1 <- index_1[index_1 <= n_row]
                index_2 <- index_2[index_2 <= n_row]
                
                return(list('index_1' = index_1,
                            'index_2' = index_2))
        }
        
        df <- na.omit(df_profitPr[c(vVars,'date')])
        df <- merge(df,df_profitPr[,c('date','profit')], all.x = TRUE, by = 'date')
        index_train <- df$date < date
        
        df_split_train <- df[index_train,]
        l_index <- get_separate_index(df = df_split_train, section_length = 21)
        
        l_train_1 <- create_X_y(df = df_split_train,
                                train_index = l_index$index_1,
                                test_index = l_index$index_2,
                                vVars = vVars,
                                normalization = normalization)
        
        l_train_2 <- create_X_y(df = df_split_train,
                                train_index = l_index$index_2,
                                test_index = l_index$index_1,
                                vVars = vVars,
                                normalization = normalization)
        
        l <- list('l_train_1' = l_train_1,
                  'l_train_2' = l_train_2)
        
        return(l)
}