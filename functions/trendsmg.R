#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
#@@ Calculate trends by country 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

trendsmg <- function(inputdf, min, max) {
    results <- NULL
    # list of countries
    country.l <- unique(countrydat$countrycode)
    country.l <- country.l[country.l != ""]
    for (i in 1:length(country.l)) {
        tmp <- subset(inputdf, countrycode == country.l[i] & year >= min & year <= 
            max)
        if (dim(tmp)[1] < 10) {
            next
        } else {
            trend <- summary(glm(counter ~ year, data = tmp, family = poisson))$coefficients[2, 
                1]
            resdf <- data.frame(trend = trend, countrycode = country.l[i])
            results <- rbind(resdf, results)
        }
    }
    return(results)
}
