plotconfusion <- function(cm,predname="Predicted\nLevel",refname="Undiagnosed\nPrevalence Level",
                          addtitle=TRUE) {
  cm_d <- as.data.frame(cm$table) # extract the confusion matrix values as data.frame
  cm_d$Pct <- round(100*cm_d$Freq/sum(cm_d$Freq),1)
  ntot<-sum(cm_d$Freq)
  #  print(range(cm_d$Pct))
  #  print(cm_d)
  cm_d$label <- paste0(cm_d$Pct,"%")
  xynames=c("","")
  if (predname!="") {
    xynames[1]<-predname
  } else {
    xynames[1] <- names(cm_d)[1]
  }
  if (refname!="") {
    xynames[2]<-refname
  } else {
    xynames[2] <- names(cm_d)[2]
  }
  names(cm_d)[1:2]<-c("Prediction","Reference")
  #  cm_st <-data.frame(cm$overall) # confusion matrix statistics as data.frame
  #  cm_st$cm.overall <- round(cm_st$cm.overall,2) # round the values
  Freqsums<-aggregate(Freq~Prediction,sum,data=cm_d)
  rownames(Freqsums)<-Freqsums$Prediction
  cm_d$Pct_rel <- 100*cm_d$Freq/Freqsums[cm_d$Prediction,"Freq"]
  cm_d$diag <- cm_d$Prediction == cm_d$Reference # Get the Diagonal
  cm_d$ndiag <- cm_d$Prediction != cm_d$Reference # Off Diagonal     
  # cm_d$Prediction <-  factor(cm_d$Prediction,
  #                           levels=rev(levels(cm_d$Prediction))) # diagonal starts at top left
  #cm_d$ref_pct <- cm_d$Pct * ifelse(is.na(cm_d$diag),-1,1)
  plt1 <-  ggplot(data = cm_d, aes(x = Reference , y =  Prediction, fill = Pct_rel))+
    scale_x_discrete(position = "bottom",
                     label=function(x) abbreviate(x, minlength=1)) +
    scale_y_discrete(label=function(x) abbreviate(x, minlength=1))+
    geom_tile(na.rm=T) +
    scale_fill_viridis_c(guide = "none",direction = -1,
                         option = "magma",begin=0.5,
                         limits=c(0,100)) +
    geom_text(aes(label = label), color = 'black', size = 3)+
    xlab(xynames[2])+ylab(xynames[1])+
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "none",
          panel.border = element_blank(),
          plot.background = element_blank(),
          axis.line = element_blank(),
          plot.title.position = "plot"
    )
  if (addtitle) plt1 <- plt1+ggtitle(xynames[1])
  plt1  
}
