get_df_unique_variabels <- function(vVariables, newVariables = NA, deep = 2, mix = TRUE) {
        if (is.na(newVariables)) {
                df_unique_variabels <- as.data.frame(t(combn(vVariables,deep)))
                df_unique_variabels$V1 <- as.character(df_unique_variabels$V1)
                df_unique_variabels$V2 <- as.character(df_unique_variabels$V2)
        } else {
                df_unique_variabels <- as.data.frame(t(combn(newVariables,deep)))
                df_unique_variabels$V1 <- as.character(df_unique_variabels$V1)
                df_unique_variabels$V2 <- as.character(df_unique_variabels$V2)
                
                add_unique_variabels <- expand.grid(vVariables,newVariables)
                colnames(add_unique_variabels) <- c('V1','V2')
                add_unique_variabels$V1 <- as.character(add_unique_variabels$V1)
                add_unique_variabels$V2 <- as.character(add_unique_variabels$V2)
                
                df_unique_variabels <- rbind(df_unique_variabels,add_unique_variabels)
        }
        
        if (mix == TRUE) {
                mix_variables_index <- sample(row.names(df_unique_variabels), 
                                              size = nrow(df_unique_variabels))
                df_unique_variabels <- df_unique_variabels[mix_variables_index,]
                row.names(df_unique_variabels) <- NULL
        }
        df_unique_variabels$calculated <- 0
        
        df_unique_variabels
}
