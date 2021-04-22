convert_l_var_to_df_var <- function(l_var) {
        df_search_vars <- as.data.frame(do.call(rbind,l_var_parallel))
        df_search_vars$vVar1 <- as.character(df_search_vars$vVar1)
        df_search_vars$vVar2 <- as.character(df_search_vars$vVar2)
        df_search_vars$profit_min100_1 <- as.numeric(as.character(df_search_vars$profit_min100_1))
        df_search_vars$profit_min100_2 <- as.numeric(as.character(df_search_vars$profit_min100_2))
        df_search_vars$profit_max100_1 <- as.numeric(as.character(df_search_vars$profit_max100_1))
        df_search_vars$profit_max100_2 <- as.numeric(as.character(df_search_vars$profit_max100_2))
        df_search_vars$profit_min100_down0_1 <- as.numeric(as.character(df_search_vars$profit_min100_down0_1))
        df_search_vars$profit_min100_down0_2 <- as.numeric(as.character(df_search_vars$profit_min100_down0_2))
        df_search_vars$profit_max100_up0_1 <- as.numeric(as.character(df_search_vars$profit_max100_up0_1))
        df_search_vars$profit_max100_up0_2 <- as.numeric(as.character(df_search_vars$profit_max100_up0_2))
        df_search_vars$profit100_1 <- as.numeric(as.character(df_search_vars$profit100_1))
        df_search_vars$profit100_2 <- as.numeric(as.character(df_search_vars$profit100_2))
        
        return(df_search_vars)
}