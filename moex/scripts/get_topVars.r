get_topVars <- function(l_vars, quan_min_max = 0.8, quan_min_max_scater = 0.8) {
        
        df_search_vars <- as.data.frame(do.call(rbind,l_vars))
        
        df_search_vars$kMinDiff_1 <- df_search_vars$profit_min100_1/df_search_vars$profit_min100_down0_1
        df_search_vars$kMinDiff_2 <- df_search_vars$profit_min100_2/df_search_vars$profit_min100_down0_2
        df_search_vars$kMaxDiff_1 <- df_search_vars$profit_max100_1/df_search_vars$profit_max100_up0_1
        df_search_vars$kMaxDiff_2 <- df_search_vars$profit_max100_2/df_search_vars$profit_max100_up0_2
        
        min_1 <- quantile(df_search_vars$profit_min100_1, 1-quan_min_max, type = 1)
        min_2 <- quantile(df_search_vars$profit_min100_2, 1-quan_min_max, type = 1)
        max_1 <- quantile(df_search_vars$profit_max100_1, quan_min_max, type = 1)
        max_2 <- quantile(df_search_vars$profit_max100_2, quan_min_max, type = 1)
        
        kmin_1 <- quantile(df_search_vars$kMinDiff_1, quan_min_max_scater, type = 1)
        kmin_2 <- quantile(df_search_vars$kMinDiff_2, quan_min_max_scater, type = 1)
        kmax_1 <- quantile(df_search_vars$kMaxDiff_1, quan_min_max_scater, type = 1)
        kmax_2 <- quantile(df_search_vars$kMaxDiff_2, quan_min_max_scater, type = 1)
        
        index <- which(((df_search_vars$profit_max100_1 > max_1) &  (df_search_vars$profit_max100_2 > max_2) &
                               (df_search_vars$kMaxDiff_1 > kmin_1) & (df_search_vars$kMaxDiff_2 > kmin_2)) |
                               
                               ((df_search_vars$profit_min100_1 < min_1) & (df_search_vars$profit_min100_2 < min_2) &
                               (df_search_vars$kMinDiff_1 > kmax_1) & (df_search_vars$kMinDiff_2 > kmax_2))
                       )
        get_topVars <- df_search_vars[index,]
        get_topVars <- get_topVars %>%
                mutate(k = kMinDiff_1+kMinDiff_2+kMaxDiff_1+kMaxDiff_2) %>%
                arrange(desc(k)) %>%
                head(100)
        
        return(get_topVars)
}