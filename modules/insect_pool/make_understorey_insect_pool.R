make_understorey_insect_pool <- function(c_frac){
    

    ### pitfall to collect ground-dwelling arthopods
    myDF1 <- read.csv("data/EucFACE_data/FACE_P0051_RA_ARTHROPODS-2_L1_20131101-20150114.csv")
    
    ## suction sampling to collect understorey arthropods
    myDF2 <- read.csv("data/EucFACE_data/FACE_P0051_RA_ARTHROPODS-3_L1_20131101-20150114.csv")

    myDF1 <- myDF1[,c("RUN", "RING", "PLOT", "GROUP", "ABUNDANCE", "WEIGHT.MG.")]
    myDF2 <- myDF2[,c("Run", "Ring", "Plot", "Group", "Abundance", "Weight.mg.")]
    colnames(myDF1) <- colnames(myDF2) <- c("Run", "Ring", "Plot", "Group", "Abundance", "Weight.mg.")
    
    ## add method
    myDF1$Method <- "pitfall"
    myDF2$Method <- "suction"
    
    myDF <- rbind(myDF1, myDF2)
    
    # correct date information
    myDF$Date <- paste0("01-", as.character(myDF$Run))
    myDF$Date <- gsub("-", "/", myDF$Date)
    myDF$Date <- as.Date(myDF$Date, format="%d/%b/%y")
    
    # sum across plot
    myDF.sum <- summaryBy(Weight.mg.~Date+Ring+Plot+Method, FUN=sum, data=myDF, keep.names=T, na.rm=T)

    # need a plot area to convert to per ground area
    pitfall_area <- pi * 0.045^2
    suction_area <- 1.0
    myDF.sum$area <- ifelse(myDF.sum$Method=="pitfall", pitfall_area, suction_area)
    
    myDF.sum$weight.mg.C <- myDF.sum$Weight.mg. * c_frac
    myDF.sum$weight.mg.C.m2 <- myDF.sum$weight.mg.C / myDF.sum$area
    
    ### only use suction data
    myDF.sum <- subset(myDF.sum, Method=="suction")
    
    ### pitfall sampling over 2 week period
    myDF.sum$weight <- ifelse(myDF.sum$Method=="pitfall", myDF.sum$weight.mg.C.m2 / 14, myDF.sum$weight.mg.C.m2)

    ### sum across methods to get a total arthropod biomass 
    myDF.sum2 <- summaryBy(weight~Date+Ring+Plot, FUN=sum, data=myDF.sum, keep.names=T, na.rm=T)
    
    # average across plot to get per ring mass, g C m-2
    myDF.avg <- summaryBy(weight~Date+Ring, FUN=mean, data=myDF.sum2, keep.names=T, na.rm=T)
    myDF.avg$weight <- myDF.avg$weight / 1000
    
    outDF <- myDF.avg
    
    # sum by date
    out <- summaryBy(weight~Ring+Date, FUN=sum, data=outDF, keep.names=T)
    
    ### out
    out <- out[,c("Date", "Ring", "weight")]
    colnames(out) <- c("Date", "Ring", "insect_pool")
    
    
    ### return
    return(out)
}

