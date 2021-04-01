create_vector_dates <- function(startYear = 2017,
                                endYear = 2019) {
        vDates <- c(ymd('19700101'))
        for (vyear in startYear:endYear) {
                for (vmonth in 1:12) {
                        if (vmonth < 10) {
                                date <- ymd(paste0(vyear, "0", vmonth, "01"))
                        } else {
                                date <- ymd(paste0(vyear, vmonth, "01"))
                        }
                        vDates <- c(vDates , date)
                }
        }
        return(vDates[2:length(vDates)])
}