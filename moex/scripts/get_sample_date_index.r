get_sample_date_index <- function(vDates, sample_size = 5) {
        
        date_index <- 1:length(vDates)
        
        if (length(date_index) < sample_size) {
                sample_size <- length(date_index)
        }
        
        sample_date_index <- sample(date_index, size = sample_size)
        
        return(list('vDatesSample' = vDates[sample_date_index],
                    'vDatesLeft' = vDates[-sample_date_index]))
}