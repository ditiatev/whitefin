get_level_value <- function(df, vVars) {
    df_level <- get_NormalDist(df_daily = df,
                               vColNames = vVars,
                               sliding_depth = 21,
                               slidind_return = 21)
    df <- cbind(df,df_level)
    for (vVar in vVars) {
        df[,paste0(vVar,'_level')] <- df[,vVar]/df[,paste0(vVar,'_mean_Pr_21')]
        index <- which(is.nan(df[,paste0(vVar,'_level')]))
        df[index,paste0(vVar,'_level')] <- NA
    }
    return(df[,paste0(vVars,'_level')])
}