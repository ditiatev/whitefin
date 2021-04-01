paste_summary <- function(X_train = X_train, 
                          X_test = X_test, 
                          y_train = y_train, 
                          y_test = y_test) {
        
        get_summary <- function(pred, y, pred_train) {
                df <- data.frame('pred' = pred, 'profit' = y)
                
                min25 <- quantile(pred_train, 0.25, type = 1)
                max25 <- quantile(pred_train, 0.75, type = 1)
                
                df <- df %>%
                        mutate(
                                up_0 = case_when(profit <= 0 ~ 0,
                                                 profit > 0 ~ 1),
                                gr = case_when(pred <= min25 ~ 'min25',
                                               pred >= max25 ~ 'max25',
                                               TRUE ~ 'middle')
                        ) %>%
                        group_by(gr) %>%
                        summarise(up_0 = mean(up_0),
                                  mean_profit = mean(profit))
                df <- as.data.frame(df)
                
                df[1, 'profit'] <- max25
                df[3, 'profit'] <- min25
                return(df[c(1, 3), ])
        }
        
        knn_train <- knn.reg(train=X_train,y=y_train,test=X_train,k=nk)
        knn_test  <- knn.reg(train=X_train,y=y_train,test=X_test, k=nk)
        
        train_summary <- get_summary(pred = knn_train[["pred"]], y = y_train, pred_train = knn_train[["pred"]])
        test_summary <- get_summary(pred = knn_test[["pred"]], y = y_test, pred_train = knn_train[["pred"]])
        
        df_current_summary <- data.frame(
                'k'   = nk,
                'date'   = date,
                'var1'  = as.character(vVar1),
                'var2'  = as.character(vVar2))
        df_current_summary$var1 <- as.character(df_current_summary$var1)
        df_current_summary$var2 <- as.character(df_current_summary$var2)
        
        # positive summary
        df_current_summary[1,'p_up_0_train']  = train_summary[1, 'up_0']
        df_current_summary[1,'p_up_0_test'] = test_summary[1, 'up_0']
        df_current_summary[1,'p_mean_profit_train'] = train_summary[1, 'mean_profit']
        df_current_summary[1,'p_mean_profit_test'] = test_summary[1, 'mean_profit']
        # negative summary
        df_current_summary[1,'n_up_0_train']  = 1 - train_summary[2, 'up_0']
        df_current_summary[1,'n_up_0_test'] = 1 - test_summary[2, 'up_0']
        df_current_summary[1,'n_mean_profit_train'] = train_summary[2, 'mean_profit']
        df_current_summary[1,'n_mean_profit_test'] = test_summary[2, 'mean_profit']
        
        return(df_current_summary)
}