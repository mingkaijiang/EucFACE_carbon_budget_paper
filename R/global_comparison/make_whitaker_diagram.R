make_whitaker_diagram <- function() {
    
    ################################### Prepare MAT and MAP, globally forests
    ### read in global biome and climate data at CRU grids
    myDF <- read.csv("data/support_data/biome_temp_prec_full_1991_2012.csv")
    
    ### create a subset DF
    myDF <- subset(myDF, BIOME <= 14 & BIOME > 0)
    glob.climDF <- myDF[,c("BIOME", "temp_annual_mean", "prec_annual_sum")]
    colnames(glob.climDF) <- c("Biome", "MAT", "MAP")
    
    ################################### Prepare MAT and MAP, FACE sites
    ### prepare storage DF
    faceDF <- data.frame(c("EucFACE", "DukeFACE", "ORNLFACE", "AspenFACE", "PopFACE",
                           "WebFACE", #"AmazonFACE", 
                           "BiForFACE", #"SwedFACE",
                           "FlakalidenWTC"), 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(faceDF) <- c("Site", "Biome", "Lat", "Lon", "MAT", "MAP", "Age", "soilN", "soilP", "NPP", 
                          "MAT_sd", "MAP_sd", "soilN_sd", "soilP_sd", "NPP_sd")
    faceDF$Biome <- c(15:22)
    
    faceDF$Lat[faceDF$Site=="EucFACE"] <- -33.36
    faceDF$Lon[faceDF$Site=="EucFACE"] <- 150.44
    
    faceDF$Lat[faceDF$Site=="DukeFACE"] <- 35.97
    faceDF$Lon[faceDF$Site=="DukeFACE"] <- -79.08
    
    faceDF$Lat[faceDF$Site=="ORNLFACE"] <- 35.9
    faceDF$Lon[faceDF$Site=="ORNLFACE"] <- -84.33
    
    faceDF$Lat[faceDF$Site=="AspenFACE"] <- 45.68
    faceDF$Lon[faceDF$Site=="AspenFACE"] <- -89.63
    
    faceDF$Lat[faceDF$Site=="PopFACE"] <- 42.22
    faceDF$Lon[faceDF$Site=="PopFACE"] <- 11.48
    
    faceDF$Lat[faceDF$Site=="WebFACE"] <- 47.33
    faceDF$Lon[faceDF$Site=="WebFACE"] <- 7.36
    
    faceDF$Lat[faceDF$Site=="BiForFACE"] <- 52.801
    faceDF$Lon[faceDF$Site=="BiForFACE"] <- -2.301
    
    #faceDF$Lat[faceDF$Site=="AmazonFACE"] <- -2.596
    #faceDF$Lon[faceDF$Site=="AmazonFACE"] <- -60.208
    
    #faceDF$Lat[faceDF$Site=="SwedFACE"] <- 57.167
    #faceDF$Lon[faceDF$Site=="SwedFACE"] <- 14.783
    
    faceDF$Lat[faceDF$Site=="FlakalidenWTC"] <- 64.07
    faceDF$Lon[faceDF$Site=="FlakalidenWTC"] <- 19.27
    
    
    ### cuedata
    forcDF <- read.csv("data/support_data/ForC_selected_data.csv")
    
    ### selected ANPP, with understorey
    forcDF1 <- subset(forcDF, variable.name=="ANPP_2_C")
    tmpDF1 <- subset(forcDF, variable.name=="ANPP_2_OM")
    tmpDF1$mean <- tmpDF1$mean * 0.5
    forcDF1 <- rbind(forcDF1, tmpDF1)
    
    ### select ANPP, with understorey
    forcDF2 <- subset(forcDF, variable.name=="ANPP_1_C")
    tmpDF2 <- subset(forcDF, variable.name=="ANPP_1_OM")
    tmpDF2$mean <- tmpDF2$mean * 0.5
    forcDF2 <- rbind(forcDF2, tmpDF2)
    
    ### selected NPP, with understorey
    forcDF3 <- subset(forcDF, variable.name=="NPP_2_C")
    tmpDF3 <- subset(forcDF, variable.name=="NPP_2_OM")
    tmpDF3$mean <- tmpDF3$mean * 0.5
    forcDF3 <- rbind(forcDF3, tmpDF3)
    
    ### select NPP, with understorey
    forcDF4 <- subset(forcDF, variable.name=="NPP_1_C")
    tmpDF4 <- subset(forcDF, variable.name=="NPP_1_OM")
    tmpDF4$mean <- tmpDF4$mean * 0.5
    forcDF4 <- rbind(forcDF4, tmpDF4)
    
    ### fit linear relationship between NPP and age
    forcDF1$stand.age <- as.numeric(as.character(forcDF1$stand.age))
    forcDF2$stand.age <- as.numeric(as.character(forcDF2$stand.age))
    forcDF3$stand.age <- as.numeric(as.character(forcDF3$stand.age))
    forcDF4$stand.age <- as.numeric(as.character(forcDF4$stand.age))
    
    forcDF1 <- forcDF1[forcDF1$stand.age < 500, ]
    forcDF2 <- forcDF1[forcDF2$stand.age < 500, ]
    forcDF3 <- forcDF1[forcDF3$stand.age < 500, ]
    forcDF4 <- forcDF1[forcDF4$stand.age < 500, ]
    
    forcDF1$mean <- forcDF1$mean * 100
    forcDF2$mean <- forcDF2$mean * 100
    forcDF3$mean <- forcDF3$mean * 100
    forcDF4$mean <- forcDF4$mean * 100
    
    fit.npp1 <- lm(mean ~ stand.age, data=forcDF1)
    fit.npp2 <- lm(mean ~ stand.age, data=forcDF2)
    fit.npp3 <- lm(mean ~ stand.age, data=forcDF3)
    fit.npp4 <- lm(mean ~ stand.age, data=forcDF4)
    
    ### useful information
    r2value1 <- round(summary(fit.npp1)$r.squared,3)
    pvalue1 <- round(summary(fit.npp1)$coefficients[,4][2], 3)
    
    r2value2 <- round(summary(fit.npp3)$r.squared,3)
    pvalue2 <- round(summary(fit.npp3)$coefficients[,4][2], 3)
    
    r2value3 <- round(summary(fit.npp3)$r.squared,3)
    pvalue3 <- round(summary(fit.npp3)$coefficients[,4][2], 3)
    
    r2value4 <- round(summary(fit.npp4)$r.squared,3)
    pvalue4 <- round(summary(fit.npp4)$coefficients[,4][2], 3)
    
    
    ### prepare input files - EucFACE
    ### Read input - climate
    DF1 <- read.csv("data/met_data/met_air_flux_data_daily.csv")
    DF2 <- read.csv("data/met_data/rainfall_data_daily.csv")
    names(DF1)[1] <- names(DF2)[1] <- "Date"
    myDF <- merge(DF1, DF2, by.x="Date", all=T)
    colnames(myDF) <- c("Date", "Tair", "RH", "PAR", "Pressure", "Wind", "Rain")
    myDF$year <- year(myDF$Date)
    myDF <- subset(myDF, year > 2012 & year < 2019)
    euc.t <- summaryBy(Tair ~ year, FUN=mean, data=myDF, keep.names=T)
    euc.p <- summaryBy(Rain ~ year, FUN=sum, data=myDF, keep.names=T)
    
    ### Read in put - npp
    DF1 <- tables_by_ring_predicted$npp[,c("term", "Ring_2", "Ring_3", "Ring_6")]
    euc.npp <- colSums(DF1[DF1$term%in%c("Leaf NPP", "Stem NPP", "Fine Root NPP",
                                         "Coarse Root NPP", "Bole Root NPP",
                                         "Other NPP", "Understorey NPP",
                                         "Leaf consumption"), 2:4]) 
    
    ### prepare input files - DukeFACE NPP
    DF1 <- read.csv("data/support_data/other_FACE/DUKE_a.csv")
    dkDF <- summaryBy(fc_npp~treatment_fertilisation, data=DF1, FUN=c(mean,sd), keep.names=T)
    
    ### prepare input files - ORNLFACE NPP
    DF1 <- read.csv("data/support_data/other_FACE/ORNL_a.csv")
    orDF <- summaryBy(fc_npp~co2, data=DF1, FUN=c(mean,sd), keep.names=T)
    
    
    ### prepare input files - ORNLFACE NPP
    DF1 <- read.csv("data/support_data/other_FACE/RHIN_a.csv")
    asDF <- summaryBy(fc_npp~co2, data=DF1, FUN=c(mean,sd), keep.names=T)
    
    ### prepare input files - PopFACE NPP
    pop.npp.m <- (1132 + 1437 + 1130)/3
    pop.npp.sd <- sqrt((57^2 + 31^2 + 35^2)/3)
    
    
    
    ### assign values - eucFACE
    faceDF$MAT[faceDF$Site=="EucFACE"] <- mean(euc.t$Tair)
    faceDF$MAP[faceDF$Site=="EucFACE"] <- mean(euc.p$Rain)
    faceDF$Age[faceDF$Site=="EucFACE"] <- 90
    faceDF$soilN[faceDF$Site=="EucFACE"] <- NA
    faceDF$soilP[faceDF$Site=="EucFACE"] <- NA
    faceDF$NPP[faceDF$Site=="EucFACE"] <- mean(euc.npp)
    
    faceDF$MAT_sd[faceDF$Site=="EucFACE"] <- sd(euc.t$Tair)
    faceDF$MAP_sd[faceDF$Site=="EucFACE"] <- sd(euc.p$Rain)
    faceDF$soilN_sd[faceDF$Site=="EucFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="EucFACE"] <- NA
    faceDF$NPP_sd[faceDF$Site=="EucFACE"] <- sd(euc.npp)
    
    ### assign values - DukeFACE
    faceDF$MAT[faceDF$Site=="DukeFACE"] <- 14.8
    faceDF$MAP[faceDF$Site=="DukeFACE"] <- 1081
    faceDF$Age[faceDF$Site=="DukeFACE"] <- 13
    faceDF$soilN[faceDF$Site=="DukeFACE"] <- 0.079
    faceDF$soilP[faceDF$Site=="DukeFACE"] <- NA
    faceDF$NPP[faceDF$Site=="DukeFACE"] <- dkDF$fc_npp.mean
    
    faceDF$MAT_sd[faceDF$Site=="DukeFACE"] <- 0.6
    faceDF$MAP_sd[faceDF$Site=="DukeFACE"] <-168
    faceDF$soilN_sd[faceDF$Site=="DukeFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="DukeFACE"] <- NA
    faceDF$NPP_sd[faceDF$Site=="DukeFACE"] <- dkDF$fc_npp.sd
    
    ### assign values - ORNLFACE
    faceDF$MAT[faceDF$Site=="ORNLFACE"] <- 14.8
    faceDF$MAP[faceDF$Site=="ORNLFACE"] <- 1221
    faceDF$Age[faceDF$Site=="ORNLFACE"] <- 10
    faceDF$soilN[faceDF$Site=="ORNLFACE"] <- 0.112
    faceDF$soilP[faceDF$Site=="ORNLFACE"] <- NA
    faceDF$NPP[faceDF$Site=="ORNLFACE"] <- orDF$fc_npp.mean[orDF$co2=="AMB"]
    
    faceDF$MAT_sd[faceDF$Site=="ORNLFACE"] <- 0.9
    faceDF$MAP_sd[faceDF$Site=="ORNLFACE"] <- 218
    faceDF$soilN_sd[faceDF$Site=="ORNLFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="ORNLFACE"] <- NA
    faceDF$NPP_sd[faceDF$Site=="ORNLFACE"] <- orDF$fc_npp.sd[orDF$co2=="AMB"]
    
    ### assign values - AspenFACE
    faceDF$MAT[faceDF$Site=="AspenFACE"] <- 6.0
    faceDF$MAP[faceDF$Site=="AspenFACE"] <- 662
    faceDF$Age[faceDF$Site=="AspenFACE"] <- 1.0
    faceDF$soilN[faceDF$Site=="AspenFACE"] <- NA
    faceDF$soilP[faceDF$Site=="AspenFACE"] <- NA
    faceDF$NPP[faceDF$Site=="AspenFACE"] <- asDF$fc_npp.mean[asDF$co2=="AMB"]
    
    faceDF$MAT_sd[faceDF$Site=="AspenFACE"] <- 0.8
    faceDF$MAP_sd[faceDF$Site=="AspenFACE"] <- 122
    faceDF$soilN_sd[faceDF$Site=="AspenFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="AspenFACE"] <- NA
    faceDF$NPP_sd[faceDF$Site=="AspenFACE"] <- asDF$fc_npp.sd[asDF$co2=="AMB"]
    
    ### assign values - PopFACE
    faceDF$MAT[faceDF$Site=="PopFACE"] <- 14
    faceDF$MAP[faceDF$Site=="PopFACE"] <- 800
    faceDF$Age[faceDF$Site=="PopFACE"] <- 3
    faceDF$soilN[faceDF$Site=="PopFACE"] <- 0.11 
    faceDF$soilP[faceDF$Site=="PopFACE"] <- NA
    faceDF$NPP[faceDF$Site=="PopFACE"] <- pop.npp.m
    
    faceDF$MAT_sd[faceDF$Site=="PopFACE"] <- NA
    faceDF$MAP_sd[faceDF$Site=="PopFACE"] <- NA
    faceDF$soilN_sd[faceDF$Site=="PopFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="PopFACE"] <- 0.0001 * sqrt(3)
    faceDF$NPP_sd[faceDF$Site=="PopFACE"] <- pop.npp.sd
    
    
    ### assign values - WebFACE
    faceDF$MAT[faceDF$Site=="WebFACE"] <- 11
    faceDF$MAP[faceDF$Site=="WebFACE"] <- 900
    faceDF$Age[faceDF$Site=="WebFACE"] <- 110
    faceDF$soilN[faceDF$Site=="WebFACE"] <- 0.28
    faceDF$soilP[faceDF$Site=="WebFACE"] <- NA
    faceDF$NPP[faceDF$Site=="WebFACE"] <- NA
    
    faceDF$MAT_sd[faceDF$Site=="WebFACE"] <- NA
    faceDF$MAP_sd[faceDF$Site=="WebFACE"] <- NA
    faceDF$soilN_sd[faceDF$Site=="WebFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="WebFACE"] <- NA
    faceDF$NPP_sd[faceDF$Site=="WebFACE"] <- NA
    
    ### assign values - BiForFACE
    faceDF$MAT[faceDF$Site=="BiForFACE"] <- 9
    faceDF$MAP[faceDF$Site=="BiForFACE"] <- 690
    faceDF$Age[faceDF$Site=="BiForFACE"] <- 150
    faceDF$soilN[faceDF$Site=="BiForFACE"] <- NA
    faceDF$soilP[faceDF$Site=="BiForFACE"] <- NA
    faceDF$NPP[faceDF$Site=="BiForFACE"] <- NA
    
    faceDF$MAT_sd[faceDF$Site=="BiForFACE"] <- NA
    faceDF$MAP_sd[faceDF$Site=="BiForFACE"] <- NA
    faceDF$soilN_sd[faceDF$Site=="BiForFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="BiForFACE"] <- NA
    faceDF$NPP_sd[faceDF$Site=="BiForFACE"] <- NA
    
    
    ### assign values - AmazonFACE
    #faceDF$MAT[faceDF$Site=="AmazonFACE"] <- 26.7
    #faceDF$MAP[faceDF$Site=="AmazonFACE"] <- 2400
    #faceDF$Age[faceDF$Site=="AmazonFACE"] <- 200
    #faceDF$soilN[faceDF$Site=="AmazonFACE"] <- NA
    #faceDF$soilP[faceDF$Site=="AmazonFACE"] <- NA
    #faceDF$NPP[faceDF$Site=="AmazonFACE"] <- NA
    #
    #faceDF$MAT_sd[faceDF$Site=="AmazonFACE"] <- NA
    #faceDF$MAP_sd[faceDF$Site=="AmazonFACE"] <- NA
    #faceDF$soilN_sd[faceDF$Site=="AmazonFACE"] <- NA
    #faceDF$soilP_sd[faceDF$Site=="AmazonFACE"] <- NA
    #faceDF$NPP_sd[faceDF$Site=="AmazonFACE"] <- NA
    
    
    ### assign values - SwedFACE
    #faceDF$MAT[faceDF$Site=="SwedFACE"] <- 5.5
    #faceDF$MAP[faceDF$Site=="SwedFACE"] <- 688
    #faceDF$Age[faceDF$Site=="SwedFACE"] <- 34
    #faceDF$soilN[faceDF$Site=="SwedFACE"] <- NA
    #faceDF$soilP[faceDF$Site=="SwedFACE"] <- NA
    #faceDF$NPP[faceDF$Site=="SwedFACE"] <- NA
    #
    #faceDF$MAT_sd[faceDF$Site=="SwedFACE"] <- NA
    #faceDF$MAP_sd[faceDF$Site=="SwedFACE"] <- NA
    #faceDF$soilN_sd[faceDF$Site=="SwedFACE"] <- NA
    #faceDF$soilP_sd[faceDF$Site=="SwedFACE"] <- NA
    #faceDF$NPP_sd[faceDF$Site=="SwedFACE"] <- NA
    
    
    ### assign values - FlakalidenWTC
    faceDF$MAT[faceDF$Site=="FlakalidenWTC"] <-  2.3
    faceDF$MAP[faceDF$Site=="FlakalidenWTC"] <-  600
    faceDF$Age[faceDF$Site=="FlakalidenWTC"] <- 41
    faceDF$soilN[faceDF$Site=="FlakalidenWTC"] <- NA
    faceDF$soilP[faceDF$Site=="FlakalidenWTC"] <- NA
    faceDF$NPP[faceDF$Site=="FlakalidenWTC"] <- 291
    
    faceDF$MAT_sd[faceDF$Site=="FlakalidenWTC"] <-  NA
    faceDF$MAP_sd[faceDF$Site=="FlakalidenWTC"] <-  NA
    faceDF$soilN_sd[faceDF$Site=="FlakalidenWTC"] <- NA
    faceDF$soilP_sd[faceDF$Site=="FlakalidenWTC"] <- NA
    faceDF$NPP_sd[faceDF$Site=="FlakalidenWTC"] <- NA
    
    ### plot global soil N and P spatial maps, with face points projected onto it
    #source("R/global_comparison/read_global_soil_data.R")
    #faceDF <- read_global_soil_data(faceDF=faceDF)
    #write.csv(faceDF, "data/support_data/Dai/faceDF.csv", row.names=F)
    
    ### the Dai dataset is too big to publish,
    ### we will just manually insert the numbers here
    faceDF <- read.csv("data/support_data/Dai/faceDF.csv")
    
    ### calcualte soil N as g N m-2 for top 50 cm soil, based on bulk density
    faceDF$soilN <- faceDF$TN1 / 100 * faceDF$BD1 * 1000000 * 0.5
    
    ### get soil labile P (g m-2) from Yang et al. 2014
    source("R/global_comparison/read_Yang_soil_P_data.R")
    faceDF <- read_Yang_soil_P_data(faceDF)
    
    ### ignore AmazonFACE and SwedFACE
    #faceDF$soilN[faceDF$Site%in%c("AmazonFACE", "SwedFACE")] <- NA
    #faceDF$labP[faceDF$Site%in%c("AmazonFACE", "SwedFACE")] <- NA
    
    ### make NP lines
    ab_1 <- data.frame(faceDF$soilN, faceDF$soilN / 20)
    colnames(ab_1) <- c("ab_n", "ab_p")
    
    ab_2 <- data.frame(faceDF$soilN, faceDF$soilN / 100)
    colnames(ab_2) <- c("ab_n", "ab_p")
    

    ### prepare data frames to plot
    glob.climDF$MAT_pos <- NA
    glob.climDF$MAT_neg <- NA
    glob.climDF$MAP_pos <- NA
    glob.climDF$MAP_neg <- NA
    
    subDF1 <- faceDF[,c("Biome", "MAT", "MAP")]
    subDF1$MAT_pos <- faceDF$MAT + faceDF$MAT_sd
    subDF1$MAT_neg <- faceDF$MAT - faceDF$MAT_sd
    subDF1$MAP_pos <- faceDF$MAP + faceDF$MAP_sd
    subDF1$MAP_neg <- faceDF$MAP - faceDF$MAP_sd
    
    plotDF1 <- rbind(glob.climDF, subDF1)
    
    ### this is just the forest plot
    plotDF1 <- subset(plotDF1, Biome%in%c(1,2,3,4,5,6,12, 
                                         15:22))
    
    forests <- c("Tropical moist broadleaf",            
                 "Tropical dry broadleaf",            
                 "Tropical conifer",               
                 "Temperate broadleaf & mixed",      
                 "Temperate conifer",                
                 "Boreal forests",              
                 "Mediterranean forests",                
                 "EucFACE",
                 "DukeFACE",
                 "ORNLFACE",
                 "AspenFACE",
                 "PopFACE",
                 "WebFACE",
                 #"AmazonFACE",
                 "BiForFACE",
                 #"SwedFACE",
                 "FlakalidenWTC")    
    
    forests2 <- c("EucFACE",
                 "DukeFACE",
                 "ORNLFACE",
                 "AspenFACE",
                 "PopFACE",
                 "WebFACE",
                 #"AmazonFACE",
                 "BiForFACE",
                 #"SwedFACE",
                 "FlakalidenWTC")  
    
    
    ### create plotting settings
    col.list <- c(viridis(7), "red", brewer.pal(7,"Paired"))
    col.list2 <- c("red", brewer.pal(7,"Paired"))
    
    
    p1 <- ggplot() +
        geom_hex(plotDF1, mapping=aes(x=MAT, y=MAP), bins = 50) +
        scale_fill_continuous(type = "viridis") +
        geom_point(faceDF, mapping=aes(x=MAT, y=MAP, 
                                       color=factor(Biome),
                                       shape=factor(Biome), 
                                       size = factor(Biome)),
                   inherit.aes = FALSE)+
        xlab(expression("MAT (" * degree * "C)")) + 
        ylab("MAP (mm)") +
        scale_color_manual(name="", 
                           values=col.list2,
                           labels=forests2) +
        scale_shape_manual(values=c(17, rep(15, 7)),
                           labels=forests2) +
        scale_size_manual(values=c(rep(4, 8)),
                          labels=forests2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=6),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(size = "none",
               shape = "none",
               #fill = viridis,
               color = guide_legend(ncol=5, override.aes = 
                                        list(size = 4, shape=c(17, rep(15, 7)), 
                                             colour=col.list2)))
    
    
    ### prepare NPP vs. age
    col.list <- c("red", brewer.pal(7,"Paired"))
    
    p2 <- ggplot() +
        geom_point(data=forcDF3, aes(stand.age, mean), col="grey", size=1)+
        geom_point(data=faceDF, aes(Age, NPP, fill=as.factor(Biome),
                                       shape=as.factor(Biome)), size=2)+
        #geom_abline(intercept = coefficients(fit.npp3)[[1]], slope = coefficients(fit.npp3)[[2]],
        #            lty=2)+
        #annotate(geom="text", x=270, y=1900, 
        #         label=paste0("NPP = ", round(coefficients(fit.npp3)[[2]], 2), "Age + ", 
        #                      round(coefficients(fit.npp3)[[1]],2)),
        #          color="black", size=2.2)+
        #annotate(geom="text", x=410, y=1600, 
        #         label=paste0("p = ", pvalue3),
        #         color="black", size=2.2)+
        ylab(expression("NPP (g C " * m^-2 * " " * yr^-1 *" )")) +
        xlab("Age (yr)")+
        theme_linedraw() +
        xlim(0, 500)+
        scale_fill_manual(name="Sites", 
                           values=col.list) +
        scale_shape_manual(values=c(24, rep(22, 7)),
                           labels="") +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=8), 
              axis.text.x = element_text(size=8),
              axis.text.y=element_text(size=8),
              axis.title.y=element_text(size=8),
              legend.text=element_text(size=8),
              legend.title=element_text(size=8),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(0, 2000),
                           breaks = c(0,500,1000,1500, 2000))
    
    
    p4 <- ggplot() +
        geom_point(data=forcDF4, aes(stand.age, mean), col="grey", size=1)+
        geom_point(data=faceDF, aes(Age, NPP, fill=as.factor(Biome),
                                    shape=as.factor(Biome)), size=2)+
        #geom_abline(intercept = coefficients(fit.npp4)[[1]], slope = coefficients(fit.npp4)[[2]],
        #            lty=2)+
        #annotate(geom="text", x=270, y=1900, 
        #         label=paste0("NPP = ", round(coefficients(fit.npp4)[[2]], 2), "Age + ", 
        #                      round(coefficients(fit.npp4)[[1]],2)),
        #         color="black", size=2.2)+
        #annotate(geom="text", x=410, y=1600, 
        #         label=paste0("p = ", pvalue4),
        #         color="black", size=2.2)+
        ylab(expression("NPP (g C " * m^-2 * " " * yr^-1 *" )")) +
        xlab("Age (yr)")+
        theme_linedraw() +
        xlim(0, 500)+
        scale_fill_manual(name="Sites", 
                          values=col.list) +
        scale_shape_manual(values=c(24, rep(22, 7)),
                           labels="") +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=8), 
              axis.text.x = element_text(size=8),
              axis.text.y=element_text(size=8),
              axis.title.y=element_text(size=8),
              legend.text=element_text(size=8),
              legend.title=element_text(size=8),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(0, 2000),
                           breaks = c(0,500,1000,1500, 2000))
    
    #plot(p2)
    
    
    p3 <- ggplot() +
        geom_line(data=ab_1, aes(ab_n, ab_p), lty=2)+
        geom_line(data=ab_2, aes(ab_n, ab_p), lty=3)+
        geom_point(data=faceDF, aes(soilN, labP, fill=as.factor(Biome),
                                    shape=as.factor(Biome)), size=2)+
        scale_fill_manual(name="Sites", 
                          values=col.list) +
        scale_shape_manual(values=c(24, rep(22, 7)),
                           labels="") +
        annotate(geom="text", x=1200, y=80, 
                 label="20:1",
                 color="black", size=2.8)+
        annotate(geom="text", x=2000, y=35, 
                 label="100:1",
                 color="black", size=2.8)+
        theme_linedraw() +
        ylab(expression("Labile P (g " * m^-2 * ")")) +
        xlab(expression("Soil N (g " * m^-2 * ")")) +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=8), 
              axis.text.x = element_text(size=8),
              axis.text.y=element_text(size=8),
              axis.title.y=element_text(size=8),
              legend.text=element_text(size=8),
              legend.title=element_text(size=8),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        ylim(0, 100)+
        xlim(0, 2500)
    
    #plot(p3)
    

    plot.with.inset <-
        ggdraw() +
        draw_plot(p1) +
        draw_plot(p2, x = 0.12, y = .72, width = .3, height = .25)+
        draw_plot(p3, x = 0.12, y = .47, width = .3, height = .25)
    
    
    
    ggsave(filename = "output/ED_Figure_2.jpg", 
           plot = plot.with.inset,
           width = 183, 
           height = 125,
           units = "mm",
           dpi = 300)
    
    
    
    
    #plot.with.inset2 <-
    #    ggdraw() +
    #    draw_plot(p1) +
    #    draw_plot(p4, x = 0.12, y = .72, width = .3, height = .25)+
    #    draw_plot(p3, x = 0.12, y = .47, width = .3, height = .25)
    
    
    #ggsave(filename = "output/ED_Figure_2_no_understorey.pdf", 
    #       plot = plot.with.inset2,
    #       width = 17, 
    #       height = 12,
    #       units = "cm",
    #       dpi = 300)
 
    
 }
