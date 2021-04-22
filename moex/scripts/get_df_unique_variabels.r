get_df_unique_variabels <- function(vVariables, deep = 2, mix = TRUE) {
        df_unique_variabels <- as.data.frame(t(combn(vVariables,deep)))
        df_unique_variabels$V1 <- as.character(df_unique_variabels$V1)
        df_unique_variabels$V2 <- as.character(df_unique_variabels$V2)
        
        if (mix == TRUE) {
                mix_variables_index <- sample(row.names(df_unique_variabels), 
                                              size = nrow(df_unique_variabels))
                df_unique_variabels <- df_unique_variabels[mix_variables_index,]
                row.names(df_unique_variabels) <- NULL
        }
        df_unique_variabels$calculated <- 0
        
        df_unique_variabels
}