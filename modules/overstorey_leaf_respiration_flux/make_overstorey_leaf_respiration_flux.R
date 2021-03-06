make_overstorey_leaf_respiration_flux <- function() {
    ### read in MAESPA GPP output
    inDF <- read.csv("data/MAESPA_output/maespa.year.ring.csv")
    
    ### swap ring characters
    inDF$Ring <- gsub("R1","1", inDF$Ring)
    inDF$Ring <- gsub("R2","2", inDF$Ring)
    inDF$Ring <- gsub("R3","3", inDF$Ring)
    inDF$Ring <- gsub("R4","4", inDF$Ring)
    inDF$Ring <- gsub("R5","5", inDF$Ring)
    inDF$Ring <- gsub("R6","6", inDF$Ring)
    
    outDF <- summaryBy(Ra~year+Ring, data=inDF, FUN=mean, keep.names=T, na.rm=T)
    
    # Only use data period 2012-2016
    outDF <- outDF[outDF$year<="2016",]
    
    outDF$Date <- as.Date(paste0(outDF$year, "-01-01"), format = "%Y-%m-%d")
    
    colnames(outDF) <- c("year", "Ring", "Rfoliage", "Date")
    
    return(outDF)
}