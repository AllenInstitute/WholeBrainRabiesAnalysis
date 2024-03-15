#load("../ipsi_contra_PV_Feb.RData") #PV with contra/ipsi
#August15_BG.R
library(gridExtra)
library(ggplot2)
load("../BG_list_March.RData")#846 experiments
order<-read_csv("../Analysis/data/order1.csv",col_names = FALSE)#cortical areas in 6 modules
cortex_module <- read_csv("C:/Users/shenqiny/OneDrive - Allen Institute/Documents/R-markdown/M2/Jan/cortex_module_merged_April_median.csv", col_types = cols(...1 = col_skip()))


#Generate pie chart for SNr and SNc experiments
BG_ipsi <- read_excel("data/BG_August15.xlsx", 
                          sheet = "BG_ipsi_August15")
BG_contra <- read_excel("data/BG_August15.xlsx", 
                          sheet = "BG_contra_August15")
bi<-BG_ipsi[,c("specimen", "image_series_id", "target")]
BG_contra$FRP=as.numeric(BG_contra$FRP)
bi[,4:317]<-BG_ipsi[,6:319]+BG_contra[,5:318]
#compare the inputs for groups
pie_generation<-function(input) {
  bi<-input
  pie<-bi[,c( "specimen", "image_series_id", "target" )]
  pie$isocortex<-rowSums(bi[,which(names(bi)=="FRP"):which(names(bi)=="AUDv")])
  pie$OLF<-rowSums(bi[,which(names(bi)=="MOB"):which(names(bi)=="TR")])
  pie$HPF<-rowSums(bi[,which(names(bi)=="CA1"):which(names(bi)=="APr")])
  pie$CTXsp<-rowSums(bi[,which(names(bi)=="CLA"):which(names(bi)=="PA")])
  #pie$CNU<-rowSums(bi[,which(names(bi)=="CP"):which(names(bi)=="BAC")])
  pie$STR<-rowSums(bi[,which(names(bi)=="CP"):which(names(bi)=="MEA")])
  pie$PAL<-rowSums(bi[,which(names(bi)=="GPe"):which(names(bi)=="BAC")])
  pie$TH<-rowSums(bi[,which(names(bi)=="VAL"):which(names(bi)=="LH")])
  pie$HY<-rowSums(bi[,which(names(bi)=="SO"):which(names(bi)=="ME")])
  pie$MB<-rowSums(bi[,which(names(bi)=="SCs"):which(names(bi)=="DR")])
  pie$PON<-rowSums(bi[,which(names(bi)=="NLL"):which(names(bi)=="SLD")])
  pie$MY<-rowSums(bi[,which(names(bi)=="AP"):which(names(bi)=="RO")])
  pie$CB<-rowSums(bi[,which(names(bi)=="LING"):which(names(bi)=="DN")])
  return(pie)
}
big<-pie_generation(bi)
anno<-big[56:61,]
anno$side<-c("L", "M", "L", "M", "M","M")
anno$specimen<-factor(anno$specimen, levels=unique(anno$specimen))

anno<-gather(anno, key=area, value=fraction, 4:15)

area_12<-c("STR", "MB", "isocortex", "HY", "PAL","PON", "MY", "TH", "HPF", "CTXsp", "OLF","CB")
p1=ggplot(anno)+geom_point( aes(x=area, y=fraction, color=specimen, fill=side,shape=side), size=3)+
  scale_x_discrete(limits=area_12)+theme_bw()+
  scale_color_manual(values=c("#d53e4f","#f46d43","#fdae61", "darkgreen","lightgreen", "#aadca3","#1e90ff", "skyblue"))+
  scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61", "darkgreen","lightgreen", "#aadca3","#1e90ff", "skyblue"))+
  theme( panel.grid.minor = element_blank(),legend.background = element_rect(fill = NA))

p2=ggplot(anno)+geom_jitter( aes(x=area, y=fraction, color=side, fill=side,shape=side), size=4, position = position_jitter(width = 0.1, height = 0))+
  scale_x_discrete(limits=area_12[1:2])+theme_bw()+
  scale_color_manual(values=c("#d53e4f", "darkgreen"))+scale_shape_manual(values=c(16,23))+scale_fill_manual(values=c("#d53e4f", "darkgreen"))+
  theme( panel.grid.minor = element_blank(),legend.position = "none")+scale_y_continuous(expand = c(0, 0), limits = c(0, 0.8))

p3=ggplot(anno)+geom_jitter( aes(x=area, y=fraction, color=side,fill=side, shape=side),size=4,position = position_jitter(width = 0.1, height = 0))+
  scale_x_discrete(limits=area_12[3:12])+theme_bw()+
  scale_color_manual(values=c("#d53e4f", "darkgreen"))+scale_shape_manual(values=c(16,23))+scale_fill_manual(values=c("#d53e4f", "darkgreen"))+
  theme( panel.grid.minor = element_blank(),legend.background = element_rect(fill = NA))+scale_y_continuous(expand = c(0, 0), limits = c(0, 0.2))

pdf("output/Fig6g.pdf", width = 10, height = 10) # Open a new pdf file

grid.arrange(p2,p3,
             ncol=2, nrow=1, widths=c(5,18), heights=c( 8)) #10x10

dev.off() # Close the file
