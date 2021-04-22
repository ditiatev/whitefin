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