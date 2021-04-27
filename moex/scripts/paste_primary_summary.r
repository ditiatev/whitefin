paste_primary_summary <- function(date,
                                  df_variabels,
                                  df_profit) {
        
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
                        normalizeXtrainXtest <- function(X_train, X_test) {
                                
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
                
                return(l)}
        
        get_primary_summary <- function(pred, y) {
                df <- data.frame('pred' = pred, 'profit' = y)
                vMin <- quantile(df$pred, 0.2, type = 1)
                VMax <- quantile(df$pred, 0.2, type = 1)
                
                min_index <- which(df$pred <= vMin)
                max_index <- which(df$pred >= VMax)
                
                min_index_down0 <- which((df$pred <= vMin) & (df$profit < 0))
                min_index_up0   <- which((df$pred >= VMax) & (df$profit > 0))
                
                profit_min       <- mean(df[min_index,'profit'], na.rm = T)
                profit_max       <- mean(df[max_index,'profit'], na.rm = T)
                profit_min_down0 <- mean(df[min_index_down0,'profit'], na.rm = T)
                profit_max_up0   <- mean(df[min_index_up0,'profit'], na.rm = T)
                profit           <- mean(df[,'profit'], na.rm = T)
                
                primary_summary <- c(profit_min,profit_min_down0,profit_max,profit_max_up0,profit)
                return(primary_summary)
        }
        
        get_separate_index <- function(df, section_length = 21) {
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
                return(list('index_1' = index_1, 'index_2' = index_2))
        }
        
        paste_primary_summary <- function(z,
                                          df_variabels,
                                          df_profit) {
                vVar1 <- df_variabels[z, "V1"]
                vVar2 <- df_variabels[z, "V2"]
                df <- na.omit(df_profit[c(vVar1, vVar2, 'profit', 'date')])
                
                l_index <- get_separate_index(df = df, section_length = 21)
                
                # train models
                l <- create_X_y(df = df,
                                train_index = l_index$index_1,
                                test_index = l_index$index_2,
                                vVar1 = vVar1,
                                vVar2 = vVar2)
                knn_train <- FNN::knn.reg(train=l$X_train, y=l$y_train, test=l$X_test, k=100)
                ps100_1 <- get_primary_summary(pred = knn_train[["pred"]], y = l$y_test)
                
                l <- create_X_y(df = df,
                                train_index = l_index$index_2,
                                test_index = l_index$index_1,
                                vVar1 = vVar1,
                                vVar2 = vVar2)       
                knn_train <- FNN::knn.reg(train=l$X_train, y=l$y_train, test=l$X_test, k=100)
                ps100_2 <- get_primary_summary(pred = knn_train[["pred"]], y = l$y_test)
                
                ps <- c(vVar1,vVar2,ps100_1,ps100_2)
                names(ps) <- c('vVar1','vVar2',
                               'profit_min100_1','profit_min100_down0_1',
                               'profit_max100_1','profit_max100_up0_1','profit100_1',
                               'profit_min100_2','profit_min100_down0_2',
                               'profit_max100_2','profit_max100_up0_2','profit100_2')
                return(ps)
        }
        
        
        ###
        ###
        ###
        len = nrow(df_variabels)
        for (z in 1:len) {
        #foreach(z = 1:len, .packages="dplyr") %dopar% {
                ps = paste_primary_summary(z = z,
                                           df_variabels = df_variabels,
                                           df_profit = df_profit)
                ps
        }
}