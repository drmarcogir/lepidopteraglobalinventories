
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
