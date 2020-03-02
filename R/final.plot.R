#rm(list=ls())
#set.seed(20150605)
#x <- list(X1=rnorm(100),X2=rt(50,8),X3=rchisq(80,2))
#out <- overlapping::overlap(x)
#OV <- out$OV
final.plot <- function(x, OV = NULL ) {
  
    AREA <- NULL
    for (i1 in 1:(length(x) - 1)) {
      for (i2 in (i1 + 1):(length(x))) {
        A <- data.frame(x = x[[i1]],
                        group = names(x)[i1],
                        k = paste(names(x)[i1],
                                  names(x)[i2], sep = "-", collapse = ""))
        B <- data.frame(x = x[[i2]],
                        group = names(x)[i2],
                        k = paste(names(x)[i1],
                                  names(x)[i2], sep = "-", collapse = ""))
        AREA <- rbind(AREA, rbind(A, B))
      }
    }
    
    if (!is.null(OV)){
      
      OV <- data.frame(OV=OV,k=names(OV))
      AREA <- merge(AREA,OV,by="k")
      AREA$k <- paste0(AREA$k," (ov. perc. ",round(AREA$OV*100),")")
      
#      for (j in 1:length(levels(AREA$k))) {
#        levels(AREA$k)[j] <- paste(levels(AREA$k)[j], " (ov. perc. ",
#                                   round(OV[grep(levels(AREA$k)[j],
#                                                 names(OV), fixed = TRUE)]*100), ")", sep = "")    
#      }
    }
    ggplot(AREA, aes(x = x)) +
      facet_wrap(~k) +
      geom_density(aes(fill = AREA$group), alpha = .35) +
      xlab("") + theme(legend.title = element_blank()) 
}

#final.plot(x,OV)
