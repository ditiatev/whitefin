count_lev3 <- function(l_str_save = l_str_save, only_workdate = NA) {
        # local functions
        get_df_lev3 <- function() {
                df_lev2_train <- l_str_save$l_lev2$train[[MA]]
                df_lev2_test <- l_str_save$l_lev2$test[which(l_str_save$l_lev2$test$MA == MA), c("date","profit_pred")]
                df_lev2 <- rbind(df_lev2_train,df_lev2_test)
                df_lev2 <- df_lev2[df_lev2$date <= l_str_save$work_date,]
                
                df_lev3 <- makePrColForStrategy(df_strategy = df_lev2,
                                                vColNames = 'profit_pred',
                                                sliding_depth = (as.numeric(MA)-1))
                
                df_lev3$date <- df_lev2$date
                df_lev3$profit_pred_Pr0 <- df_lev2$profit_pred
                df_lev3 <- merge(df_lev3, l_str_save$df_profit[,c("date","profit")], all.x = T, by = 'date')
                df_lev3$year  <- year(df_lev3$date)
                df_lev3$month <- month(df_lev3$date)
                
                return(df_lev3)
        }
        
        check_only_wordate <- function() {
                if (is.na(only_workdate)) {
                        if (is.null(l_str_save['df_lev3'][[1]])) {
                                only_workdate = FALSE
                        } else {
                                only_workdate = TRUE}
                }
                return(only_workdate)
        }
        
        update_last_work_date_profit_inside_l_str_save <- function() {
                # update last_work_date profit
                if (only_workdate == TRUE) {
                        last_work_date_profit_index <- which(l_str_save$df_profit$date == l_str_save$last_work_date)
                        last_work_date_profit <- l_str_save$df_profit[last_work_date_profit_index,'profit']
                        last_work_date_profit_index <- which(l_str_save$df_lev3$date == l_str_save$last_work_date)
                        l_str_save$df_lev3[last_work_date_profit_index,'profit'] <- last_work_date_profit
                }
        }
        
        get_df_npv <- function() {
                
                df_lev3_train <- df_profit_lev3[which(df_lev3$date < l_str_save$work_date),]
                l_npv <- list()
                for (x in 1:3) {
                        l_npv[x] <- list(get_df_best_n_by_npv(df_train = df_lev3_train, vProfitNext = as.numeric(MA)))
                }
                df_npv <- do.call(rbind,l_npv)
                df_npv <- df_npv %>%
                        group_by(n) %>%
                        summarise(lev_prof = mean(lev_prof),
                                  npv_sum = sum(npv),
                                  npv = mean(npv)) %>%
                        arrange(desc(npv_sum)) %>%
                        head(3)
                
                return(df_npv)
        }
        
        update_df_lev3 <- function() {        
                for (x in 1:nrow(df_npv)) {
                        n        <- df_npv[x,"n"][[1]]
                        lev_prof <- df_npv[x,"lev_prof"][[1]]
                        npv      <- df_npv[x,"npv"][[1]]
                        
                        v_npv_Name   <- paste0('npv_profit_pred_next_',MA,'_n_',n)
                        v_Pr_n_Names <- paste0('profit_pred', "_Pr", 0:n)
                        df_lev3[,'profit_pred'] <- apply(df_lev3[,v_Pr_n_Names], 1, sum, na.rm = T)
                        
                        traid_index    <- (df_lev3$profit_pred >= lev_prof) 
                        if (x == 1) {k=2} else {k=1}
                        df_lev3[,v_npv_Name] <- -1*k
                        df_lev3[df_lev3$profit_pred == 0,v_npv_Name] <- NA
                        df_lev3[traid_index,v_npv_Name] <- 1*k
                }
                
                col_names <- names(df_lev3)[-grep("_Pr", names(df_lev3))]
                col_names <- setdiff(col_names, c('year','month','profit_pred'))
                df_lev3 <- df_lev3[,col_names]
                colnames(df_lev3) <- c('date','profit','npv1','npv2','npv3')
                df_lev3 <- df_lev3 %>%
                        mutate(vote = npv1+npv2+npv3,
                               MA = MA)
                return(df_lev3)
        }
        
        # body code
        only_workdate <- check_only_wordate()
        
        if (only_workdate == TRUE) {
                last_work_date_profit_index <- which(l_str_save$df_profit$date == l_str_save$last_work_date)
                last_work_date_profit <- l_str_save$df_profit[last_work_date_profit_index,'profit']
                last_work_date_profit_index <- which(l_str_save$df_lev3$date == l_str_save$last_work_date)
                l_str_save$df_lev3[last_work_date_profit_index,'profit'] <- last_work_date_profit
        }


        if (is.null(l_str_save['l_npv'][[1]])) {
                l_npv <- list()
                l_npv$date <- l_str_save$work_date
                update_npv <- TRUE
        } else {
                date_diff <- l_str_save$work_date - l_str_save$l_npv$date
                if (date_diff > 30) {
                        l_npv <- list()
                        l_npv$date <- l_str_save$work_date
                        update_npv <- TRUE
                } else {
                        l_npv <- l_str_save$l_npv
                        update_npv <- FALSE
                }
        }
        
        l_lev3 <- list()
        for (MA in names(l_str_save$l_lev2$train)) {
                
                # df_lev3
                df_lev3 <- get_df_lev3()
                
                # df_npv
                if (update_npv == TRUE) {
                        df_npv <- get_df_npv()
                        l_npv[MA] <- list(df_npv)
                } else {
                        df_npv <- l_str_save[["l_npv"]][[MA]]
                }
                
                
                if (only_workdate == TRUE) {
                        df_lev3 <- df_lev3[df_lev3$date == l_str_save$work_date,]
                        }
                if (nrow(df_lev3) == 0) {next}
                
                df_lev3 <- update_df_lev3()
                l_lev3[MA] <- list(df_lev3)
                
        }
        
        if (length(l_lev3) == 0) {return(list('df_lev3' = l_str_save$df_lev3,
                                              'l_npv' = l_npv))}
                
        df_lev3 <- do.call(rbind,l_lev3)
        df_lev3 <- df_lev3 %>%
                group_by(date) %>%
                summarise(profit = profit[1],
                          vote = sum(vote))
        
        if (only_workdate == TRUE) {
                df_lev3 <- rbind(l_str_save$df_lev3[,c("date","profit","vote")],df_lev3)
        }
        
        l_vote_level <- list()
        for (vote_level in unique(df_lev3$vote)) {
                
                df_lev3_by_years <- df_lev3 %>%
                        filter(vote <= vote_level) %>%
                        group_by(year(date),month(date)) %>%
                        summarise(total_profit = sum(profit, na.rm = T), 
                                  mean_profit = mean(profit, na.rm = T), 
                                  n = n())
                l_vote_level[as.character(vote_level)] <- mean(culculate_NPV(df_lev3_by_years$total_profit))
        }
        df_vote_level <- data.frame(npv_n = do.call(rbind,l_vote_level))
        df_vote_level$vote <- as.numeric(rownames(df_vote_level))
        df_lev3_vote <- merge(df_lev3,df_vote_level,all.x = T, by = 'vote')
        
        l_vote_level <- list()
        for (vote_level in unique(df_lev3$vote)) {
                df_lev3_by_years <- df_lev3 %>%
                        filter(vote >= vote_level) %>%
                        group_by(year(date),month(date)) %>%
                        summarise(total_profit = sum(profit, na.rm = T), 
                                  mean_profit = mean(profit, na.rm = T), 
                                  n = n())
                l_vote_level[as.character(vote_level)] <- mean(culculate_NPV(df_lev3_by_years$total_profit))
        }
        df_vote_level <- data.frame(npv_p = do.call(rbind,l_vote_level))
        df_vote_level$vote <- as.numeric(rownames(df_vote_level))
        df_lev3_vote <- merge(df_lev3_vote,df_vote_level,all.x = T, by = 'vote')
        
        df_lev3_vote$npv <- df_lev3_vote$npv_n*df_lev3_vote$npv_p
        #df_lev3_vote$npv <- sqrt(abs(df_lev3_vote$npv))*sign(df_lev3_vote$npv)
        df_lev3_vote[(is.nan(df_lev3_vote$npv)), "npv"] <- 0
        #df_lev3_vote[(df_lev3_vote$npv < 0),"npv"] <- 0
        df_lev3_vote <- df_lev3_vote %>%
                arrange(date)
        
        if (only_workdate == TRUE) {
                df_lev3_vote <- rbind(l_str_save$df_lev3,tail(df_lev3_vote[,c("date","profit","npv","vote")],1))
        }
        
        return(list('df_lev3' = df_lev3_vote[,c("date","profit","npv","vote")],
                    'l_npv' = l_npv))
        
}