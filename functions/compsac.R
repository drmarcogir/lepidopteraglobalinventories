#' Calculate inventory incompletness using species accumulation curves
#' @inputdf = input dataframe containing incidence records for species
#' @inraster = raster grid providing the resolution of the analyes
 

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
