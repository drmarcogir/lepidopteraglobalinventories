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

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
#@@ Create dataframe for plotting 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

dfcreatemg <- function(inputdf = NULL, inputmap) {
    if (!is.null(inputdf)) {
        tmpmap <- merge(inputmap, inputdf, by.x = c("ISO2"), by.y = c("countrycode"))
        tmpmap@data$id = rownames(tmpmap@data)
        tmpmap.points = fortify(tmpmap, region = "id")
        tmpmap.df = join(tmpmap.points, tmpmap@data, by = "id")
        
    } else {
        inputmap@data$id = rownames(inputmap@data)
        inputmap.points = fortify(inputmap, region = "id")
        tmpmap.df = join(inputmap.points, inputmap@data, by = "id")
    }
    return(tmpmap.df)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
#@@ Create discrete legend for plotting 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

legrank <- function(inputdf, ntiles, colname, inraster = NULL) {
    if (!is.null(inraster)) {
        griddf <- data.frame(coordinates(inraster), as.data.frame(inraster))
        griddf <- na.exclude(griddf)
        colnames(griddf)[3] <- c("cat")
        inputdf <- merge(griddf, inputdf, all.x = T)
    }
    # subset NAs
    nas <- inputdf[is.na(inputdf[, colname]), ]
    # exclude NAs
    inputdf1 <- inputdf[!is.na(inputdf[, colname]), ]
    # produce ranking
    inputdf1$rank <- ntile(inputdf1[, colname], n = ntiles)
    # create categorical labels
    inputdf1 <- orderBy(~rank, data = inputdf1)
    rank.l <- unique(inputdf1$rank)
    results <- NULL
    for (i in 1:length(rank.l)) {
        # extract relevant rows
        tmp <- subset(inputdf1, rank == rank.l[i])
        labminmax1 <- paste("[", round(min(tmp[, colname]), digits = 2), ";", round(max(tmp[, 
            colname]), digits = 2), "]", sep = "")
        tmp$rankd <- labminmax1
        results <- rbind(tmp, results)
        
    }
    results$rankd <- as.factor(results$rankd)
    # create labels for NA data frame
    nas$rank <- NA
    nas$rankd <- NA
    results1 <- rbind(results, nas)
    return(results1)
}

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

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
#@@ Calculate inventory incompletness  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

compsac <- function(inputdf, inraster) {
    # reproject
    coordinates(inputdf) <- ~decimallongitude + decimallatitude
    proj4string(inputdf) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    inputdf1 <- spTransform(inputdf, CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
    # extract categories
    cats <- raster::extract(inraster, inputdf1)
    # insert category number
    coor <- as.data.frame(coordinates(inputdf1))
    colnames(coor) <- c("lonm", "latm")
    inputdf2 <- data.frame(coor, as.data.frame(inputdf1), cats)
    inputdf <- subset(inputdf2, !is.na(cats))
    inputdf$counter <- 1
    # aggregate data
    tmp <- aggregate(counter ~ cats + species, data = inputdf, FUN = sum)
    # get rid of empty names
    tmp1 <- tmp[!tmp$species %in% "", ]
    # unique list of cells
    cat.l <- unique(tmp1$cats)
    results <- foreach(x = 1:length(cat.l), .errorhandling = c("remove")) %dopar% 
        {
            tmpcat <- subset(tmp1, cats == cat.l[x])
            # reshape
            tmpcat1 <- spread(tmpcat, species, counter)
            tmpcat1[is.na(tmpcat1)] <- 0
            # exclude second column (cats)
            tmpcat2 <- tmpcat1[2:dim(tmpcat1)[2]]
            # max for each row
            Max = sum(tmpcat2[1, ])
            # cutoff to establish last 10%
            cutoff = round(Max - ((Max/100) * 10))
            # rarefaction of cutoff
            rar.cutoff = rarefy(tmpcat2[1, ], cutoff, se = FALSE, MARGIN = 1)
            # rarefaction of max
            rar.max = rarefy(tmpcat2[1, ], Max, se = FALSE, MARGIN = 1)
            # predictor
            X = c(cutoff, Max)
            # response
            Y = c(rar.cutoff, rar.max)
            # linear model
            mod = lm(Y ~ X)
            slope <- try(summary(mod)$coefficients[2, 1])
            data.frame(cat = cat.l[x], slope = slope)
        }
    results1 <- do.call("rbind", results)
    inrasterdf <- na.exclude(data.frame(coordinates(inraster), as.data.frame(inraster)))
    colnames(inrasterdf)[3] <- c("cat")
    results2 <- merge(inrasterdf, results1, all.x = T)
    return(results2)
    
}

