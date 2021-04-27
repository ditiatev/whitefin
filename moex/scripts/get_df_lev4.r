get_df_lev4 <- function(l_strategies = l_strategies, df_date_lev4, vTraid_index = 7) {
        
        df_profit_lev4_all <- df_date_lev4 
        
        for (x in 1:length(l_strategies)) {
                
                df_profit_lev4 <- l_strategies[[x]]
                names(df_profit_lev4) <- c("strategy","date","profit",
                                           'first_next20','second_next20','third_next20',
                                           'first_next25','second_next25','third_next25',
                                           'first_next30','second_next30','third_next30',
                                           'first_next35','second_next35','third_next35')
                strategy <- as.character(vStrategies[x])
                
                df_profit_lev4 <- df_profit_lev4 %>%
                        mutate(# looking to the best model
                                index_next20 = if_else(first_next20 > 0, 2, 0),
                                index_next25 = if_else(first_next25 > 0, 2, 0),
                                index_next30 = if_else(first_next30 > 0, 2, 0),
                                index_next35 = if_else(first_next35 > 0, 2, 0),
                                
                                # secure bad by the next two best model
                                index_next20 = if_else((second_next20+third_next20) == 0, index_next20-1, index_next20),
                                index_next25 = if_else((second_next25+third_next25) == 0, index_next25-1, index_next25),
                                index_next30 = if_else((second_next30+third_next30) == 0, index_next30-1, index_next30),
                                index_next35 = if_else((second_next35+third_next35) == 0, index_next35-1, index_next35),
                                
                                # secure potential profit by the next two best model
                                index_next20 = if_else(((second_next20>0) & (third_next20>0)), 1+index_next20, index_next20),
                                index_next25 = if_else(((second_next25>0) & (third_next25>0)), 1+index_next25, index_next25),
                                index_next30 = if_else(((second_next30>0) & (third_next30>0)), 1+index_next30, index_next30),
                                index_next35 = if_else(((second_next35>0) & (third_next35>0)), 1+index_next35, index_next35),
                                
                                traid_index = index_next20+index_next25+index_next30+index_next35) %>%
                        select(date,profit,traid_index)
                
                train_index <- df_profit_lev4$date < vDates[1]
                npv_m <- mean(culculate_NPV(df_profit_lev4[((df_profit_lev4$traid_index < 0) & train_index),"profit"]))
                npv_0 <- mean(culculate_NPV(df_profit_lev4[((df_profit_lev4$traid_index >= 0) & train_index),"profit"]))
                npv_1 <- mean(culculate_NPV(df_profit_lev4[((df_profit_lev4$traid_index >= 1) & train_index),"profit"]))
                npv_2 <- mean(culculate_NPV(df_profit_lev4[((df_profit_lev4$traid_index >= 2) & train_index),"profit"]))
                npv_3 <- mean(culculate_NPV(df_profit_lev4[((df_profit_lev4$traid_index >= 3) & train_index),"profit"]))
                npv_4 <- mean(culculate_NPV(df_profit_lev4[((df_profit_lev4$traid_index >= 4) & train_index),"profit"]))
                npv_5 <- mean(culculate_NPV(df_profit_lev4[((df_profit_lev4$traid_index >= 5) & train_index),"profit"]))
                npv_6 <- mean(culculate_NPV(df_profit_lev4[((df_profit_lev4$traid_index >= 6) & train_index),"profit"]))
                npv_7 <- mean(culculate_NPV(df_profit_lev4[((df_profit_lev4$traid_index >= 7) & train_index),"profit"]))
                npv_8 <- mean(culculate_NPV(df_profit_lev4[((df_profit_lev4$traid_index >= 8) & train_index),"profit"]))
                npv_9 <- mean(culculate_NPV(df_profit_lev4[((df_profit_lev4$traid_index >= 9) & train_index),"profit"]))
                npv_10 <- mean(culculate_NPV(df_profit_lev4[((df_profit_lev4$traid_index >= 10) & train_index),"profit"]))
                npv_11 <- mean(culculate_NPV(df_profit_lev4[((df_profit_lev4$traid_index >= 11) & train_index),"profit"]))
                npv_12 <- mean(culculate_NPV(df_profit_lev4[((df_profit_lev4$traid_index == 12) & train_index),"profit"]))
                
                df_profit_lev4 <- df_profit_lev4 %>%
                        mutate(npv_index = case_when(traid_index < 0 ~ npv_m,
                                                     traid_index == 0 ~ npv_0,
                                                     traid_index == 1 ~ npv_1,
                                                     traid_index == 2 ~ npv_2,
                                                     traid_index == 3 ~ npv_3,
                                                     traid_index == 4 ~ npv_4,
                                                     traid_index == 5 ~ npv_5,
                                                     traid_index == 6 ~ npv_6,
                                                     traid_index == 7 ~ npv_7,
                                                     traid_index == 8 ~ npv_8,
                                                     traid_index == 9 ~ npv_9,
                                                     traid_index == 10 ~ npv_10,
                                                     traid_index == 11 ~ npv_11,
                                                     traid_index == 12 ~ npv_12))
                names(df_profit_lev4) <- c('date',
                                           paste0("profit_",strategy),
                                           paste0("traid_index_",strategy),
                                           paste0("npv_index_",strategy))
                
                df_profit_lev4_all <- merge(df_profit_lev4_all, df_profit_lev4, all.x = T, by = 'date')
        }
        
        
        for(vStrategy in vStrategies) {
                index <- df_profit_lev4_all[,paste0("traid_index_",vStrategy)] >= vTraid_index
                df_profit_lev4_all[,paste0("profit_traid_",vStrategy)] <- 0
                df_profit_lev4_all[,paste0("npv_traid_",vStrategy)] <- 0
                df_profit_lev4_all[index,paste0("profit_traid_",vStrategy)] <- df_profit_lev4_all[index,paste0("profit_",vStrategy)]
                df_profit_lev4_all[index,paste0("npv_traid_",   vStrategy)] <- df_profit_lev4_all[index,paste0("npv_index_",vStrategy)]
        }
        df_profit_lev4_all[,'npv_traid'] <- apply(df_profit_lev4_all[,paste0("npv_traid_",vStrategies)], 1, sum, na.rm = T)
        df_profit_lev4_all[,paste0("npv_traid_",vStrategies)] <- df_profit_lev4_all[,paste0("npv_traid_",vStrategies)]/df_profit_lev4_all[,'npv_traid']
        
        df_profit_lev4_all[is.na(df_profit_lev4_all)] <- 0
        
        for(vStrategy in vStrategies) {
                df_profit_lev4_all[,paste0("profit_npv_traid_",vStrategy)] <- df_profit_lev4_all[,paste0("npv_traid_",vStrategy)]*df_profit_lev4_all[,paste0("profit_",vStrategy)]
        }
        
        df_profit_lev4_all[,'profit_model'] <- apply(df_profit_lev4_all[,paste0("profit_npv_traid_",vStrategies)], 1, sum, na.rm = T)
        df_profit_lev4_all[,'profit_avarage'] <- apply(df_profit_lev4_all[,paste0("profit_",vStrategies)], 1, mean, na.rm = T)
        df_profit_lev4_all[,'traid_index'] <- apply(df_profit_lev4_all[,paste0("npv_traid_",vStrategies)], 1, sum, na.rm = T)
        df_profit_lev4_all$year <- year(df_profit_lev4_all$date)
        df_profit_lev4_all$month <- month(df_profit_lev4_all$date)
        
        columsName <- c('date','year','month','traid_index','profit_model','profit_avarage',paste0("profit_",vStrategies),paste0("npv_traid_",vStrategies))
        df_profit_lev4_all[,columsName]
}