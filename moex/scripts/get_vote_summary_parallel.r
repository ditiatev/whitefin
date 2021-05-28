get_vote_summary_parallel <- function(df_profitPr = df_profitPr,
                                      l_str_save = l_str_save,
                                      core = 3, 
                                      paralleling = TRUE) {
        
        get_vote_summary <- function(df_profitPr = df_profitPr,
                                     l_str_save = l_str_save,
                                     vVoteIndex = vVoteIndex) {
                
                create_df_vote <- function() {
                        df_vote <- data.frame("var1" = "var1",
                                              "var2" = "var2",
                                              "prob0_up" = 0,
                                              "prob0_down" = 0,
                                              "k_down" = 0,
                                              "k_up" = 0)
                        df_vote$var1 <- as.character(df_vote$var1)
                        df_vote$var2 <- as.character(df_vote$var2)
                        return(df_vote)
                }
                
                create_vVars <- function(df_topVars = df_topVars,
                                         index = index) {
                        
                        vVar1 <- df_topVars[index,"vVar1"][[1]]
                        vVar2 <- df_topVars[index,"vVar2"][[1]]
                        vVars <- c(vVar1,vVar2)
                        
                        return(vVars)
                }
                
                create_X_y <- function(df = df, 
                                       train_index = train_index, 
                                       test_index = test_index, 
                                       vVars = vVars,
                                       normalization = TRUE) {
                        
 
                        
                        # new start ################################
                        
                        #
                        ### old
                        # df_train   <- na.omit(df[train_index,c(c(vVars,'date','profit'))])
                        # 
                        # X_train    <- df_train[,vVars]
                        # y_train    <- df_train[,c('profit')]
                        # date_train <- df_train[,c('date')]
                        ### old
                        #
                        
                        # get away outliers for train set
                        df_train <- na.omit(df[train_index,c(c(vVars,'date','profit'))])
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
                        
                        
                        
                        df <- df[!is.infinite(rowSums(df[vVars])),]
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
                
                create_lxy <- function(df_profitPr = df_profitPr,
                                       date = date,
                                       vVars = vVars,
                                       days_lag = days_lag) {
                        
                        df <- na.omit(df_profitPr[c(vVars,'date')])
                        df <- df[!is.infinite(rowSums(df[vVars])),]
                        df <- merge(df,df_profitPr[,c('date','profit')],by = 'date',all.x = T)
                        
                        index_train <- which(df$date < date)
                        index_train <- index_train[1:(length(index_train)-days_lag+2)]
                        index_predict <- which(df$date == date)
                        
                        if (length(index_predict) == 0) {return(NA)}
                        
                        df_split_train <- df[index_train,]
                        l_index <- get_separate_index(df = df_split_train, section_length = 21)
                        
                        l_predict <- create_X_y(df = df,
                                                train_index = index_train,
                                                test_index = index_predict,
                                                vVars = vVars)
                        
                        l_train_1 <- create_X_y(df = df_split_train,
                                                train_index = l_index$index_1,
                                                test_index = l_index$index_2,
                                                vVars = vVars)
                        
                        l_train_2 <- create_X_y(df = df_split_train,
                                                train_index = l_index$index_2,
                                                test_index = l_index$index_1,
                                                vVars = vVars)
                        
                        l <- list('l_predict' = l_predict,
                                  'l_train_1' = l_train_1,
                                  'l_train_2' = l_train_2)
                        
                        return(l)
                }
                
                get_prob0_summary <- function(l) {
                        
                        l_predict = l$l_predict
                        
                        knn_train  <- FNN::knn.reg(train=l_predict$X_train, 
                                                   y=l_predict$y_train, 
                                                   test=l_predict$X_train, 
                                                   k=100)
                        df_train <- data.frame('pred' = knn_train[["pred"]], 'profit' = l_predict$y_train)
                        
                        knn_test  <- FNN::knn.reg(train=l_predict$X_train, 
                                                  y=l_predict$y_train, 
                                                  test=l_predict$X_test, k=100)
                        
                        vPred <- knn_test[["pred"]]
                        vPredIndex <- (df_train$pred >= vPred)
                        vPredIndex_up0 <- (df_train$pred >= vPred) & (df_train$profit > 0)
                        vPredIndex_down0 <- (df_train$pred < vPred) & (df_train$profit < 0)
                        
                        prob0_up <- sum(df_train[vPredIndex,"profit"])/sum(df_train[vPredIndex_up0,"profit"])
                        prob0_down <- sum(df_train[!vPredIndex,"profit"])/sum(df_train[vPredIndex_down0,"profit"]) 
                        
                        prob0 <- c(prob0_up,prob0_down,vPred)
                        names(prob0) <- c("prob0_up","prob0_down","vPred")
                        
                        return(prob0)
                }
                
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
                
                get_scater_prob0_summary <- function(l, vPred) {
                        
                        l_train_1 <- l$l_train_1
                        knn_train <- FNN::knn.reg(train=l_train_1$X_train,
                                                  y=l_train_1$y_train,
                                                  test=l_train_1$X_test,
                                                  k=100)
                        ps100_1 <- get_primary_summary(pred = knn_train[["pred"]], y = l_train_1$y_test, point = vPred)
                        
                        l_train_2 <- l$l_train_2
                        knn_train <- FNN::knn.reg(train=l_train_2$X_train,
                                                  y=l_train_2$y_train,
                                                  test=l_train_2$X_test, 
                                                  k=100)
                        ps100_2 <- get_primary_summary(pred = knn_train[["pred"]], y = l_train_2$y_test, point = vPred)
                        
                        ps <- as.numeric(c(ps100_1,ps100_2))
                        names(ps) <- c('profit_min100_1','profit_min100_down0_1',
                                       'profit_max100_1','profit_max100_up0_1','profit100_1',
                                       'profit_min100_2','profit_min100_down0_2',
                                       'profit_max100_2','profit_max100_up0_2','profit100_2')
                        k_down <- sum(c(ps["profit_min100_1"],ps["profit_min100_2"]), na.rm = T
                        )/sum(c(ps["profit_min100_down0_1"],ps["profit_min100_down0_2"]), na.rm = T)
                        k_up   <- sum(c(ps["profit_max100_1"],ps["profit_max100_2"]), na.rm = T
                        )/sum(c(ps["profit_max100_up0_1"],ps["profit_max100_up0_2"]), na.rm = T)
                        
                        if (is.nan(k_down) | is.na(k_down)) {k_down <- 0}
                        if (is.nan(k_up) | is.na(k_up)) {k_up <- 0}
                        
                        k <- c(k_down,k_up)
                        names(k) <- c('k_down','k_up')
                        
                        return(k)
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
                        
                        return(list('index_1' = index_1,
                                    'index_2' = index_2))
                }
                
                
                get_df_vote_m <- function(df_profitPr = df_profitPr,
                                          df_topVars = df_topVars,
                                          x = x,
                                          days_lag = days_lag,
                                          MA = topVars_name) {
                        
                        date <- df_profitPr[x,"date"]
                        df_vote <- create_df_vote()
                        
                        for (index in 1:nrow(df_topVars)) {
                                
                                vVars <- create_vVars(df_topVars = df_topVars, index = index)
                                
                                l <- create_lxy(df_profitPr = df_profitPr,
                                                date = date, vVars = vVars,
                                                days_lag = days_lag)
                                if (is.na(l)) {next}
                                
                                df_vote[index,"var1"] <- vVars[1]
                                df_vote[index,"var2"] <- vVars[2]
                                
                                prob0_summary <- get_prob0_summary(l = l)
                                df_vote[index,"prob0_up"] <- prob0_summary["prob0_up"]
                                df_vote[index,"prob0_down"] <- prob0_summary["prob0_down"]
                                #undebug(get_scater_prob0_summary)
                                scater_prob0_summary <- get_scater_prob0_summary(l = l, 
                                                                                 vPred = prob0_summary["vPred"])
                                df_vote[index,"k_down"] <- scater_prob0_summary["k_down"]
                                df_vote[index,"k_up"] <- scater_prob0_summary["k_up"]
                        }
                        
                        df_vote[sapply(df_vote, is.infinite)] <- NA
                        
                        df_vote_m <- df_vote %>%
                                summarise(MA = MA,
                                          prob0_up_sd = sd(prob0_up, na.rm = T),
                                          prob0_down_sd = sd(prob0_down, na.rm = T),
                                          scater_prob0_up = sum(k_up*prob0_up, na.rm = T)/sum(k_up, na.rm = T),
                                          scater_prob0_down = sum(k_down*prob0_down, na.rm = T)/sum(k_down, na.rm = T),
                                          prob0_up = mean(prob0_up, na.rm = T),
                                          prob0_down = mean(prob0_down, na.rm = T),
                                          date = date,
                                          n = n())
                        
                        return(df_vote_m)
                }
                
                
                
                ###
                ###
                topVars_names <- names(l_str_save$l_topVars)
                len = length(topVars_names)
                
                
                ###
                if (paralleling == TRUE) {
                foreach(z = 1:len, .packages="dplyr") %dopar% {
                        topVars_name <- topVars_names[z]
                        
                        df_topVars <- l_str_save$l_topVars[topVars_name][[1]]
                        df_profitPr[,'profit'] <- l_str_save$df_profit_forvard[,paste0('profit_mean_Next_',topVars_name)]
                        days_lag <- as.numeric(topVars_name)+1
                        
                        df_vote_m <- get_df_vote_m(df_profitPr = df_profitPr,
                                                   df_topVars = df_topVars,
                                                   x = vVoteIndex,
                                                   days_lag = days_lag,
                                                   MA = topVars_name)
                        #df_vote_m['MA'] <- topVars_name
                        }
                } else {
                        l_vote_m <- list()
                        for(z in 1:len) {
                                topVars_name <- topVars_names[z]
                                
                                df_topVars <- l_str_save$l_topVars[topVars_name][[1]]
                                df_profitPr[,'profit'] <- l_str_save$df_profit_forvard[,paste0('profit_mean_Next_',topVars_name)]
                                days_lag <- as.numeric(topVars_name)+1
                                
                                df_vote_m <- get_df_vote_m(df_profitPr = df_profitPr,
                                                           df_topVars = df_topVars,
                                                           x = vVoteIndex,
                                                           days_lag = days_lag,
                                                           MA = topVars_name)
                                
                                l_vote_m[z] <- list(df_vote_m)
                        }
                        l_vote_m
                }
                
        }
        
       vVoteIndex <- which(df_profitPr$date == (l_str_save$work_date[1])) 
        if (paralleling == TRUE) {
        
        cl <- parallel::makeCluster(core)
        doParallel::registerDoParallel(cl)
        l_vote_summary <- tryCatch(get_vote_summary(df_profitPr = df_profitPr,
                                                    l_str_save = l_str_save,
                                                    vVoteIndex = vVoteIndex), 
                                   error = function(e) print(e))
        parallel::stopCluster(cl)
        
        } else {
                l_vote_summary <- get_vote_summary(df_profitPr = df_profitPr,
                                                   l_str_save = l_str_save,
                                                   vVoteIndex = vVoteIndex)
                }
        
        df_vote_summary  <- as.data.frame(do.call(rbind,l_vote_summary))
        df_vote_summary
}