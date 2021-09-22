library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(caret)
functionfolder<-"Functions"
resultsfolder<-"Results"
source(file.path(functionfolder,"load_data.R"))
source(file.path(functionfolder,"calibrate_data.R"))
source(file.path(functionfolder,"plotconfusion.R"))

## Confusion matrices

CDC.confusion <-
  confusionMatrix(cleandata$CDCLevelCommTrans, 
                  cleandata$IU.Level.mc.Text, 
                  dnn = c("CDC Level (Overall)", "Actual IU Level"),
                  mode="everything")
pconf.cdc<-plotconfusion(CDC.confusion,predname = "CDC Level (Overall)")

CDC.confusion.Cases <-
  confusionMatrix(cleandata$CDCLevelCommTrans.Cases, 
                  cleandata$IU.Level.mc.Text, 
                  dnn = c("CDC Level (Cases)", "Actual IU Level"),
                  mode="everything")
pconf.cdc.cases<-plotconfusion(CDC.confusion.Cases,predname = "CDC Level (Cases)")

CDC.confusion.Pos <-
  confusionMatrix(cleandata$CDCLevelCommTrans.Pos, 
                  cleandata$IU.Level.mc.Text, 
                  dnn = c("CDC Level (Positivity)", "Actual IU Level"),
                  mode="everything")
pconf.cdc.pos<-plotconfusion(CDC.confusion.Pos,predname = "CDC Level (Positivity)")

GM.confusion <-
  confusionMatrix(cleandata$IU.Level.gm.Text, 
                  cleandata$IU.Level.mc.Text, 
                  dnn = c("GM Model Level", "Actual IU Level"),
                  mode="everything")

pconf.gm<-plotconfusion(GM.confusion,predname = "Geometric Mean-based Level")

pconf.all<-ggarrange(pconf.cdc,pconf.cdc.cases,pconf.cdc.pos,pconf.gm,nrow=2,ncol=2)
print(pconf.all)
ggsave(file.path(resultsfolder,"CDC_Category_errors.pdf"),pconf.all,height=6,width=5,scale=1.4)
ggsave(file.path(resultsfolder,"CDC_Category_errors.jpg"),pconf.all,height=6,width=5,scale=1.4)

## Performance measures

basislevels<-c("CDC Level (Overall)",
               "CDC Level (Cases only)",
               "CDC Level (Positivity only)",
               "GM criteria"
)
basislevels<-factor(basislevels,levels=basislevels)

perf.overall <- cbind(data.frame(Basis=basislevels),
                      as.data.frame(rbind(CDC.confusion$overall,
                                          CDC.confusion.Cases$overall,
                                          CDC.confusion.Pos$overall,
                                          GM.confusion$overall))
)
fwrite(perf.overall,
       file.path(resultsfolder,"CDC_Category_perf.overall.csv"))
perf.byclass <- cbind(data.frame(Basis=sort(rep(basislevels,4))),
                      rbind(
                        cbind(data.frame(Class=rownames(CDC.confusion$byClass)),
                              data.frame(CDC.confusion$byClass)),
                        cbind(data.frame(Class=rownames(CDC.confusion.Cases$byClass)),
                              data.frame(CDC.confusion.Cases$byClass)),
                        cbind(data.frame(Class=rownames(CDC.confusion.Pos$byClass)),
                              data.frame(CDC.confusion.Pos$byClass)),
                        cbind(data.frame(Class=rownames(GM.confusion$byClass)),
                              data.frame(GM.confusion$byClass))
                      )
)
rownames(perf.byclass) <- NULL
fwrite(perf.byclass,
       file.path(resultsfolder,"CDC_Category_perf.byclass.csv"))

## Plot densities 

plotdata.df <- rbind(
  data.frame(Basis=basislevels[1],
             IU.per100k.mc=cleandata$IU.per100k.mc,
             CDCLevelCategory=cleandata$CDCLevelCommTrans),
  data.frame(Basis=basislevels[2],
             IU.per100k.mc=cleandata$IU.per100k.mc,
             CDCLevelCategory=cleandata$CDCLevelCommTrans.Cases),
  data.frame(Basis=basislevels[3],
             IU.per100k.mc=cleandata$IU.per100k.mc,
             CDCLevelCategory=cleandata$CDCLevelCommTrans.Pos),
  data.frame(Basis=basislevels[4],
             IU.per100k.mc=cleandata$IU.per100k.mc,
             CDCLevelCategory=cleandata$IU.Level.gm.Text)
)
risklevels <- c("Low","Moderate","Substantial","High")
riskfactors <- factor(risklevels,levels=risklevels)

pdens.all <- ggplot(subset(plotdata.df,Basis != "GM criteria"))+
  geom_density(aes(x=IU.per100k.mc,y=..density..*..n..,
                   fill=CDCLevelCategory,group=CDCLevelCategory),
               color="grey",alpha=0.5,bw=0.01)+
  geom_density(aes(x=IU.per100k.mc,y=..density..*..n..), # "true"
               fill=NA,color="grey",linetype="dotted",bw=0.01)+
  geom_vline(xintercept=IU.breaks,linetype="dotted")+
  scale_x_log10(limits=c(1,10000))+
  scale_linetype("")+
  scale_color_manual("CDC\nRisk Level",
                     values=c("DodgerBlue","Yellow","Orange","Red"))+
  scale_fill_manual("CDC\nRisk Level",
                    values=c("DodgerBlue","Yellow","Orange","Red"))+
  theme_bw()+
  theme(legend.position = "top",axis.text.y = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(size = 12))+
  xlab("Undiagnosed Infection Prevalence (per 100,000)")+
  ylab("Frequency")+
  facet_wrap(~Basis,ncol=1)+
  annotation_logticks(side="b")
pdens.all

## Combine with inset confusion matrix

pconf.tab <- tibble(x=0,y=Inf,Basis=basislevels,
                    plt=list(pconf.cdc+theme(plot.title=element_blank()),
                             pconf.cdc.cases+theme(plot.title=element_blank()),
                             pconf.cdc.pos+theme(plot.title=element_blank()),
                             pconf.gm+theme(plot.title=element_blank())))
pdens.all.inset<-pdens.all+geom_plot(data = subset(pconf.tab,Basis != "GM criteria"), 
                                  aes(x, y, label = plt),vp.height=0.9,vp.width=0.35)
pdens.all.inset

ggsave(file.path(resultsfolder,"CDC_Category_errors_density.jpg"),
       pdens.all.inset,height=6,width=4,scale=1.6)
ggsave(file.path(resultsfolder,"CDC_Category_errors_density.pdf"),
       pdens.all.inset,height=6,width=4,scale=1.6)

save(pdens.all.inset,file=file.path(resultsfolder,"Fig1A.Rdata"))

