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

CDC.confusion.Mod <-
  confusionMatrix(cleandata$CDCLevelCommTrans.Mod, 
                  cleandata$IU.Level.Rec.mc.Text, 
                  dnn = c("CDC Level (Overall)", "Actual IU Level"),
                  mode="everything")
pconf.cdc.Mod<-plotconfusion(CDC.confusion.Mod,predname = "CDC Modified Level\n(Overall)")

CDC.confusion.Mod.Cases <-
  confusionMatrix(cleandata$CDCLevelCommTrans.Mod.Cases, 
                  cleandata$IU.Level.Rec.mc.Text, 
                  dnn = c("CDC Level (Cases)", "Actual IU Level"),
                  mode="everything")
pconf.cdc.Mod.cases<-plotconfusion(CDC.confusion.Mod.Cases,predname = "CDC Modified Level\n(Cases)")

CDC.confusion.Mod.Pos <-
  confusionMatrix(cleandata$CDCLevelCommTrans.Mod.Pos, 
                  cleandata$IU.Level.Rec.mc.Text, 
                  dnn = c("CDC Level (Positivity)", "Actual IU Level"),
                  mode="everything")
pconf.cdc.Mod.pos<-plotconfusion(CDC.confusion.Mod.Pos,predname = "CDC Modified Level\n(Positivity)")

GM.confusion.Rec <-
  confusionMatrix(cleandata$IU.Level.Rec.gm.Text, 
                  cleandata$IU.Level.Rec.mc.Text, 
                  dnn = c("GM Model Level", "Actual IU Level"),
                  mode="everything")
pconf.gm.Rec<-plotconfusion(GM.confusion.Rec,predname = "Geometric Mean-based\nModified Level")


pconf.Mod.all<-ggarrange(pconf.cdc.Mod,pconf.cdc.Mod.cases,pconf.cdc.Mod.pos,pconf.gm.Rec,nrow=2,ncol=2)

print(pconf.Mod.all)
ggsave(file.path(resultsfolder,"CDC_Category_Mod_errors.pdf"),pconf.Mod.all,height=6,width=5,scale=1.4)
ggsave(file.path(resultsfolder,"CDC_Category_Mod_errors.jpg"),pconf.Mod.all,height=6,width=5,scale=1.4)

basislevels.Mod<-c("CDC Modified Level (Overall)",
               "CDC Modified Level (Cases only)",
               "CDC Modified Level (Positivity only)",
               "GM Modified Criteria"
)
basislevels.Mod<-factor(basislevels.Mod,levels=basislevels.Mod)



perf.Mod.overall <- cbind(data.frame(Basis=basislevels.Mod),
                      as.data.frame(rbind(CDC.confusion.Mod$overall,
                                          CDC.confusion.Mod.Cases$overall,
                                          CDC.confusion.Mod.Pos$overall,
                                          GM.confusion.Rec$overall))
)
fwrite(perf.Mod.overall,
       file.path(resultsfolder,"CDC_Category_Mod_perf.overall.csv"))
perf.Mod.byclass <- cbind(data.frame(Basis=sort(rep(basislevels.Mod,3))),
                      rbind(
                        cbind(data.frame(Class=rownames(CDC.confusion.Mod$byClass)),
                              data.frame(CDC.confusion.Mod$byClass)),
                        cbind(data.frame(Class=rownames(CDC.confusion.Mod.Cases$byClass)),
                              data.frame(CDC.confusion.Mod.Cases$byClass)),
                        cbind(data.frame(Class=rownames(CDC.confusion.Mod.Pos$byClass)),
                              data.frame(CDC.confusion.Mod.Pos$byClass)),
                        cbind(data.frame(Class=rownames(GM.confusion.Rec$byClass)),
                              data.frame(GM.confusion.Rec$byClass))
                      )
)
rownames(perf.Mod.byclass) <- NULL
fwrite(perf.Mod.byclass,
       file.path(resultsfolder,"CDC_Category_Mod_perf.byclass.csv"))



plotdata.Mod.df <- rbind(
  data.frame(Basis=basislevels.Mod[1],
             IU.per100k.mc=cleandata$IU.per100k.mc,
             CDCLevelCategory=cleandata$CDCLevelCommTrans.Mod),
  data.frame(Basis=basislevels.Mod[2],
             IU.per100k.mc=cleandata$IU.per100k.mc,
             CDCLevelCategory=cleandata$CDCLevelCommTrans.Mod.Cases),
  data.frame(Basis=basislevels.Mod[3],
             IU.per100k.mc=cleandata$IU.per100k.mc,
             CDCLevelCategory=cleandata$CDCLevelCommTrans.Mod.Pos),
  data.frame(Basis=basislevels.Mod[4],
             IU.per100k.mc=cleandata$IU.per100k.mc,
             CDCLevelCategory=cleandata$IU.Level.Rec.gm.Text)
)

pdens.Mod.all <- ggplot(subset(plotdata.Mod.df,Basis != "GM Modified Criteria"))+
  geom_density(aes(x=IU.per100k.mc,y=..density..*..n..,
                   fill=CDCLevelCategory,group=CDCLevelCategory),
               color="grey",alpha=0.5,bw=0.01)+
  geom_density(aes(x=IU.per100k.mc,y=..density..*..n..), # "true"
               fill=NA,color="grey",linetype="dotted",bw=0.01)+
  geom_vline(xintercept=IU.breaks.Rec,linetype="dotted")+
  scale_x_log10(limits=c(1,10000))+
  scale_linetype("")+
  scale_color_manual("CDC\nMod Risk Level",
                     values=c("DodgerBlue","Yellow","Red"))+
  scale_fill_manual("CDC\nMod Risk Level",
                    values=c("DodgerBlue","Yellow","Red"))+
  theme_bw()+
  theme(legend.position = "top",axis.text.y = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(size = 12))+
  xlab("Undiagnosed Infection Prevalence (per 100,000)")+
  ylab("Frequency")+
  facet_wrap(~Basis,ncol=1)+
  annotation_logticks(side="b")
pdens.Mod.all

pconf.Mod.tab <- tibble(x=0,y=Inf,Basis=basislevels.Mod,
                    plt=list(pconf.cdc.Mod+theme(plot.title=element_blank()),
                             pconf.cdc.Mod.cases+theme(plot.title=element_blank()),
                             pconf.cdc.Mod.pos+theme(plot.title=element_blank()),
                             pconf.gm.Rec+theme(plot.title=element_blank())))
pdens.Mod.all.inset<-pdens.Mod.all+geom_plot(data = subset(pconf.Mod.tab,Basis != "GM Modified Criteria"), 
                                     aes(x, y, label = plt),vp.height=0.9,vp.width=0.35)
pdens.Mod.all.inset

ggsave(file.path(resultsfolder,"CDC_Category_Mod_errors_density.jpg"),
       pdens.Mod.all.inset,height=6,width=4,scale=1.6)
ggsave(file.path(resultsfolder,"CDC_Category_Mod_errors_density.pdf"),
       pdens.Mod.all.inset,height=6,width=4,scale=1.6)

## Save confusion matrix plot of CDC modified categories (overall)
save(pconf.cdc.Mod,file=file.path(resultsfolder,"Fig1B-panel3.Rdata"))

