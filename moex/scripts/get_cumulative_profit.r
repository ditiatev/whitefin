get_cumulative_profit <- function(df_lev4 = df_lev4,k = 1) {
        pr_year = min(df_lev4$year)
        l_money_by_year <- list()
        for (x in 1:nrow(df_lev4)) {
                if (df_lev4[x,"year"] != pr_year) {
                        l_money_by_year[pr_year] <- list(c('year' = pr_year,
                                                           'money' = money-100,
                                                           'money_model' = money_model-100))
                        money <- 100
                        money_model <- 100
                }
                pr_year = df_lev4[x,"year"]
                
                money_model <- (k*money_model*df_lev4[x,"profit_model"]/100+money_model)
                money <- (k*money*df_lev4[x,"profit_avarage"]/100+money)
                
                df_lev4[x,'money_model'] <- money_model
                df_lev4[x,'money'] <- money
        }
        df_money_by_year <- data.frame(do.call(rbind,l_money_by_year))
        df_money_by_year$diff = df_money_by_year$money_model - df_money_by_year$money
        df_money_by_year
}