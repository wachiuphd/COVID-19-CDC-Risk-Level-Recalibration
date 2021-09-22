library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(caret)
functionfolder<-"Functions"
resultsfolder<-"Results"
source(file.path(functionfolder,"load_data.R"))
source(file.path(functionfolder,"calibrate_data.R"))
source(file.path(functionfolder,"recalibrate_data.R"))
source(file.path(functionfolder,"plotconfusion.R"))

# Confusion matrices

Cases.confusion.Rec <-
  confusionMatrix(cleandata$IU.Level.Rec.Cases.Text, 
                  cleandata$IU.Level.Rec.mc.Text, 
                  dnn = c("Recalibrated Cases Level", "Actual IU Level"),
                  mode="everything")
pconf.Cases.Rec<-plotconfusion(Cases.confusion.Rec,predname = "Recalibrated Level\n(Cases only)")

Pos.confusion.Rec <-
  confusionMatrix(cleandata$IU.Level.Rec.Pos.Text, 
                  cleandata$IU.Level.Rec.mc.Text, 
                  dnn = c("Recalibrated Positivity Level", "Actual IU Level"),
                  mode="everything")
pconf.Pos.Rec<-plotconfusion(Pos.confusion.Rec,predname = "Recalibrated Level\n(Positivity only)")


Cases.Pos.Indep.confusion.Rec <-
  confusionMatrix(riskfactors[1+pmax(cleandata$IU.Level.Rec.Cases,
                                    cleandata$IU.Level.Rec.Pos)], 
                  cleandata$IU.Level.Rec.mc.Text, 
                  dnn = c("Recalibrated Max(Cases,Positivity) Level", "Actual IU Level"),
                  mode="everything")
pconf.Cases.Pos.Indep.Rec<-plotconfusion(Cases.Pos.Indep.confusion.Rec,
                                   predname = "Recalibrated Level\n(Max Cases,Positivity)")

Cases.Pos.confusion.Rec <-
  confusionMatrix(cleandata$IU.Level.Rec.Cases.Pos.Text, 
                  cleandata$IU.Level.Rec.mc.Text, 
                  dnn = c("Recalibrated Cases and Positivity Level", "Actual IU Level"),
                  mode="everything")
pconf.Cases.Pos.Rec<-plotconfusion(Cases.Pos.confusion.Rec,
                                   predname = "Recalibrated Level\n(Overall)")

GM.confusion.Rec <-
  confusionMatrix(cleandata$IU.Level.Rec.gm.Text, 
                  cleandata$IU.Level.Rec.mc.Text, 
                  dnn = c("GM Model Level", "Actual IU Level"),
                  mode="everything")
pconf.gm.Rec<-plotconfusion(GM.confusion.Rec,predname = "Recalibrated Level\n(Geometric Mean-based)")

pconf.Rec.all<-ggarrange(pconf.Cases.Pos.Rec,
                         pconf.Cases.Rec,pconf.Pos.Rec,
                         pconf.gm.Rec,nrow=2,ncol=2)

print(pconf.Rec.all)
ggsave(file.path(resultsfolder,"Rec_Category.pdf"),pconf.Rec.all,height=6,width=5,scale=1.4)
ggsave(file.path(resultsfolder,"Rec_Category.jpg"),pconf.Rec.all,height=6,width=5,scale=1.4)

### Save

basislevels<-c("Recalibrated Level (Overall)",
               "Recalibrated Level (Cases only)",
               "Recalibrated Level (Positivity only)",
               "GM criteria"
)
basislevels<-factor(basislevels,levels=basislevels)

perf.overall <- cbind(data.frame(Basis=basislevels),
                      as.data.frame(rbind(Cases.Pos.confusion.Rec$overall,
                                          Cases.confusion.Rec$overall,
                                          Pos.confusion.Rec$overall,
                                          GM.confusion.Rec$overall))
)
fwrite(perf.overall,
       file.path(resultsfolder,"Rec_Category_perf.overall.csv"))
perf.byclass <- cbind(data.frame(Basis=sort(rep(basislevels,3))),
                      rbind(
                        cbind(data.frame(Class=rownames(Cases.Pos.confusion.Rec$byClass)),
                              data.frame(Cases.Pos.confusion.Rec$byClass)),
                        cbind(data.frame(Class=rownames(Cases.confusion.Rec$byClass)),
                              data.frame(Cases.confusion.Rec$byClass)),
                        cbind(data.frame(Class=rownames(Pos.confusion.Rec$byClass)),
                              data.frame(Pos.confusion.Rec$byClass)),
                        cbind(data.frame(Class=rownames(GM.confusion.Rec$byClass)),
                              data.frame(GM.confusion.Rec$byClass))
                      )
)
rownames(perf.byclass) <- NULL
fwrite(perf.byclass,
       file.path(resultsfolder,"Rec_Category_perf.byclass.csv"))



### results - Include Overall, Cases only, Not Borderline
basislevels.plot <- c("Recalibrated Risk Level (Overall)",
                      "Recalibrated Risk Level (Cases only)",
                      "Modified CDC Risk Level (Overall)")
basislevels.plot<-factor(basislevels.plot,levels=basislevels.plot)

plotdata.Rec.df <- rbind(
  data.frame(Basis=basislevels.plot[1],
             IU.per100k.mc=cleandata$IU.per100k.mc,
             CDCLevelCategory=cleandata$IU.Level.Rec.Cases.Pos.Text),
  data.frame(Basis=basislevels.plot[2],
             IU.per100k.mc=cleandata$IU.per100k.mc,
             CDCLevelCategory=cleandata$IU.Level.Rec.Cases.Text),
  data.frame(Basis=basislevels.plot[3],
             IU.per100k.mc=cleandata$IU.per100k.mc,
             CDCLevelCategory=cleandata$CDCLevelCommTrans.Mod)
)

pdens.Rec.all <- ggplot(plotdata.Rec.df)+
  geom_density(aes(x=IU.per100k.mc,y=..density..*..n..,
                   fill=CDCLevelCategory,group=CDCLevelCategory),
               color="grey",alpha=0.5,bw=0.01)+
  geom_density(aes(x=IU.per100k.mc,y=..density..*..n..), # "true"
               fill=NA,color="grey",linetype="dotted",bw=0.01)+
  geom_vline(xintercept=IU.breaks.Rec,linetype="dotted")+
  scale_x_log10(limits=c(1,10000))+
  scale_linetype("")+
  scale_color_manual("Recalibrated\nRisk Level",
                     values=c("DodgerBlue","Yellow","Red"))+
  scale_fill_manual("Recalibrated\nRisk Level",
                    values=c("DodgerBlue","Yellow","Red"))+
  theme_bw()+
  theme(legend.position = "top",axis.text.y = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(size = 12))+
  xlab("Undiagnosed Infection Prevalence (per 100,000)")+
  ylab("Frequency")+
  facet_wrap(~Basis,ncol=1)+
  annotation_logticks(side="b")
pdens.Rec.all

load(file.path(resultsfolder,"Fig1B-panel3.Rdata"))

pconf.Rec.tab <- tibble(x=0,y=Inf,Basis=basislevels.plot,
                        plt=list(pconf.Cases.Pos.Rec+theme(plot.title=element_blank()),
                                 pconf.Cases.Rec+theme(plot.title=element_blank()),
                                 pconf.cdc.Mod+theme(plot.title=element_blank())))
pdens.Rec.all.inset<-pdens.Rec.all+geom_plot(data = pconf.Rec.tab, 
                                             aes(x, y, label = plt),vp.height=0.9,vp.width=0.35)
pdens.Rec.all.inset

ggsave(file.path(resultsfolder,"Rec_Category_errors_density.jpg"),
       pdens.Rec.all.inset,height=6,width=4,scale=1.6)
ggsave(file.path(resultsfolder,"Rec_Category_errors_density.pdf"),
       pdens.Rec.all.inset,height=6,width=4,scale=1.6)

save(pdens.Rec.all.inset,file=file.path(resultsfolder,"Fig1B.Rdata"))

load(file.path(resultsfolder,"Fig1A.Rdata"))
fig1 <- ggarrange(pdens.all.inset,pdens.Rec.all.inset,nrow=1,ncol=2,labels=c("A","B"))
ggsave(file.path(resultsfolder,"Fig1.jpg"),
       fig1,height=6,width=8,scale=1.65)
ggsave(file.path(resultsfolder,"Fig1.pdf"),
       fig1,height=6,width=8,scale=1.65)
