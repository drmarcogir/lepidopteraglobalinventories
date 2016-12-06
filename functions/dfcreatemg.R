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

