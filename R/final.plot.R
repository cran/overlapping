#rm(list=ls())
#source("/home/el/lavori/Rdevel/overlapping_1.3/R/cutnumeric.R")
#source("/home/el/lavori/Rdevel/overlapping_1.3/R/overlap.R")
#load("~/lavori/Rdevel/testoverlapping/data/testfinalplot.rda")
final.plot <- function(DD,OV) {
  
  xx <- yy <- list()
  JJ <- unique(DD$k)
  index <- order(JJ)
  for (i in JJ) {
    xxx <- DD$x[(DD$k==i)&(DD$w==1)]
    yyy <- DD$y[(DD$k==i)&(DD$w==1)]
    xx <- c(xx,list(sort(xxx)))
    yy <- c(yy,list(yyy[order(xxx)]))
  }
  
  xyplot(y~x|factor(k),data=DD,groups=DD$j,panel=function(...){
    panel.xyplot(...,type="l")
    panel.polygon(xx[[index[packet.number()]]],yy[[index[packet.number()]]],col="#74d600",border="#74d600")
    panel.text(min(DD$x),max(DD$y),
               paste("overlap = ",round(OV[index[packet.number()]]*100,2),"%",sep=""),pos=4)
  },xlab="",ylab="")
}
