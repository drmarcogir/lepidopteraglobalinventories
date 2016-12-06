#@@@@@@@@@@@@@@@@@@@@@@@@@ 
#@@  Calculate trends (%)
#@@@@@@@@@@@@@@@@@@@@@@@@@

trendsmg1 <- function(inputdf, start1, end1, start2, end2) {
    results <- NULL
    # list of countries
    country.l <- unique(countrydat$countrycode)
    country.l <- country.l[country.l != ""]
    for (i in 1:length(country.l)) {
        tmp <- subset(inputdf, countrycode == country.l[i] & year >= start1 & year <= 
            end1)
        if (dim(tmp)[1] == 0) {
            next
        } else {
            tmp1 <- subset(inputdf, countrycode == country.l[i] & year >= start2 & 
                year <= end2)
        }
        if (dim(tmp1)[1] == 0) {
            next
        } else {
            total <- sum(tmp$counter) + sum(tmp1$counter)
            p1 <- sum(tmp$counter)/total * 100
            p2 <- sum(tmp1$counter)/total * 100
            change <- p2 - p1
            resdf <- data.frame(trend = change, countrycode = country.l[i])
            results <- rbind(resdf, results)
        }
    }
    return(results)
}

