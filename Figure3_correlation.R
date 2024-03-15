library(readr)
library(dplyr)
library(tidyr)
library(readxl)
antero_ipsi <- read_excel("data/Figure3_cor/antero_ipsi.xlsx")
retro_ipsi <- read_excel("data/Figure3_cor/retro_ipsi.xlsx")
order<-read_csv("data/order1.csv",col_names = FALSE)#cortical areas in 6 modules
areas=order$X1[c(1:43, 100:143)]
retro_ipsi=retro_ipsi[retro_ipsi$target %in% areas, ]
retro_ipsi=retro_ipsi[, c("target", retro_ipsi$target)]

antero_ipsi=antero_ipsi[antero_ipsi$source_area %in% areas, ]
antero_ipsi=antero_ipsi[, c("source_area", antero_ipsi$source_area)]
#retro_TC: thalamic inputs to cortical targets
retro_TC=retro_ipsi[retro_ipsi$target %in% order$X1[1:43],c("target", order$X1[100:143])]
#antero_CT: cortical projection ending in thalamus
antero_CT=antero_ipsi[antero_ipsi$source_area %in% order$X1[1:43], c("source_area", order$X1[100:143])]

#retro_CT: cortical inputs to thalamic targets
retro_CT=retro_ipsi[retro_ipsi$target %in% order$X1[100:143],c("target", order$X1[1:43])]
#antero_TC: thalamic projection to cortex
antero_TC=antero_ipsi[antero_ipsi$source_area %in% order$X1[100:143], c("source_area", order$X1[1:43])]

retro_TC$connection="retro"
antero_CT$connection="antero"
names(retro_TC)[1]="injection"
names(antero_CT)[1]="injection"
retro_TC$id=paste0(retro_TC$connection, "_", retro_TC$injection)
antero_CT$id=paste0(antero_CT$connection, "_", antero_CT$injection)
retro_TC=retro_TC[, c("id", "connection", "injection", order$X1[100:143])]
antero_CT=antero_CT[, c("id", "connection", "injection", order$X1[100:143])]
mt=rbind(retro_TC, antero_CT)
mt=mt[,-c(2:3)]
rowof=mt$id
mt=mt[,-1]

mt[, sapply(mt, is.character)] <- apply(mt[, sapply(mt, is.character)], 2, as.numeric)
rownames(mt)=rowof
cor<-cor(t(mt), method="pearson")
c_p<-cor[grepl("retro",rownames(cor)), grepl("antero", colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column() %>% tidyr::pivot_longer(-rowname)
c_p=c_p[!is.na(c_p$value),]
c_p=c_p%>%spread(key=name, value=value)
c_matrix<-cor[grepl("retro",rownames(cor)), grepl("antero", colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column()
#remove NA row and columns and arrange order according to cortical module
#rTC_aCT sheet in summary.xlsx

#retro_CT: cortical inputs to thalamic targets
retro_CT=retro_ipsi[retro_ipsi$target %in% order$X1[100:143],c("target", order$X1[1:43])]
#antero_TC: thalamic projection to cortex
antero_TC=read_csv("data/Figure3_cor/th_antero.csv")
antero_TC=antero_TC[antero_TC$primary_source %in% order$X1[100:143], c("primary_source", order$X1[1:43])]
#antero_TC=antero_TC%>%group_by(injection)%>%summarise(across(where(is.numeric), median, na.rm = TRUE))
retro_CT$connection="retro"
antero_TC$connection="antero"
names(retro_CT)[1]="injection"
names(antero_TC)[1]="injection"
tarea=c("MD"  ,"PVT" ,  "PT"  ,  "RE" , "MH"  ,  "LH",  "VPM" ,"VAL"  , "VM"  , "SMT"  ,    "CM"  ,"PO" ,  "PCN",   "CL" , "AV", "AM"   , "AD"  ,  "IAD" ,  "LD"  ,  "LGd" , "LGv",  "LP" ,    "MG"   )
retro_CT=retro_CT[retro_CT$injection%in%tarea,]
antero_TC=antero_TC[antero_TC$injection%in%tarea,]
retro_CT=retro_CT[match(tarea, retro_CT$injection),]
antero_TC=antero_TC[match(tarea, antero_TC$injection),]

retro_CT$id=paste0(retro_CT$connection, "_", retro_CT$injection)
antero_TC$id=paste0(antero_TC$connection, "_", antero_TC$injection)
retro_CT=retro_CT[, c("id", "connection", "injection", order$X1[1:43])]
antero_TC=antero_TC[, c("id", "connection", "injection", order$X1[1:43])]
intersect(retro_CT$injection, antero_TC$injection)
mc=rbind(retro_CT, antero_TC)
mc=as.data.frame(mc)
mc=mc[mc$FRP!="NA",]
mc=mc[,-c(2:3)]
rowof=mc$id
mc[, sapply(mc, is.character)] <- apply(mc[, sapply(mc, is.character)], 2, as.numeric)

mc=mc[,-1]
rownames(mc)=rowof

cor<-cor((t(mc)), method="pearson")
c_p<-cor[grepl("retro",rownames(cor)), grepl("antero", colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column() %>% tidyr::pivot_longer(-rowname)
c_p=c_p[!is.na(c_p$value),]
c_p=c_p%>%spread(key=name, value=value)
c_matrix<-cor[grepl("retro",rownames(cor)), grepl("antero", colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column()
#write_csv(c_matrix, "rCT_aTC.csv")
#rCT_aTC sheet in summary

#make retroCC matrix
retro_CC=retro_ipsi[retro_ipsi$target %in% order$X1[1:43],c("target", order$X1[1:43])]
retro_CC$connection="retro"
names(retro_CC)[1]="injection"

retro_CC$id=paste0(retro_CC$connection, "_", retro_CC$injection)
retro_CC=retro_CC[, c("id", "connection", "injection", order$X1[1:43])]

mc=rbind(retro_CC, antero_TC)
mc=as.data.frame(mc)
mc=mc[mc$FRP!="NA",]
mc=mc[,-c(2:3)]
rowof=mc$id
mc[, sapply(mc, is.character)] <- apply(mc[, sapply(mc, is.character)], 2, as.numeric)

mc=mc[,-1]
rownames(mc)=rowof

cor<-cor((t(mc)), method="pearson")
c_p<-cor[grepl("retro",rownames(cor)), grepl("antero", colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column() %>% tidyr::pivot_longer(-rowname)
c_p=c_p[!is.na(c_p$value),]
c_p=c_p%>%spread(key=name, value=value)
c_matrix<-cor[grepl("retro",rownames(cor)), grepl("antero", colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column()
#write_csv(c_matrix, "rCC_aTC.csv")
#rCC_aTC sheet in summary.xlsx


#retro_CT: cortical inputs to thalamic targets
#anteroCC
antero_CC=antero_ipsi[antero_ipsi$source_area %in% order$X1[1:43],c("source_area", order$X1[1:43])]
antero_CC$connection="antero"

names(antero_CC)[1]="injection"
antero_CC$id=paste0(antero_CC$connection, "_", antero_CC$injection)
antero_CC=antero_CC[, c("id", "connection", "injection", order$X1[1:43])]
mt=rbind(retro_CT, antero_CC)
mt=mt[,-c(2:3)]
rowof=mt$id
mt=mt[,-1]

mt[, sapply(mt, is.character)] <- apply(mt[, sapply(mt, is.character)], 2, as.numeric)
rownames(mt)=rowof

cor<-cor(t(mt), method="pearson")
c_p<-cor[grepl("retro",rownames(cor)), grepl("antero", colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column() %>% tidyr::pivot_longer(-rowname)
c_p=c_p[!is.na(c_p$value),]
c_p=c_p%>%spread(key=name, value=value)
c_matrix<-cor[grepl("retro",rownames(cor)), grepl("antero", colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column()
#write_csv(c_matrix, "rCT_aCC.csv")
#rCT_aCC sheet in summary

##retroCC and retroCT
mt=rbind(retro_CC, retro_CT)
mt=mt[,-c(2:3)]
rowof=mt$id
mt=mt[,-1]

mt[, sapply(mt, is.character)] <- apply(mt[, sapply(mt, is.character)], 2, as.numeric)
rownames(mt)=rowof

cor<-cor(t(mt), method="pearson")
c_matrix<-cor[grepl(paste0(order$X1[1:43], collapse = "|"),rownames(cor)), grepl(paste0(order$X1[100:143], collapse = "|"), colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column()
#write_csv(c_matrix, "rCC_rCT.csv")
#rCC_rCT sheet



m=read_csv("/Users/shenqinyao/Documents/ar_corr.csv")
m=m[,-c(2:3)]
rowof=m$id
m=m[,-1]
##retroCC and anteroCC
mt=rbind(retro_CC, antero_CC)
mt=mt[,-c(2:3)]
rowof=mt$id
mt=mt[,-1]

mt[, sapply(mt, is.character)] <- apply(mt[, sapply(mt, is.character)], 2, as.numeric)
rownames(mt)=rowof

cor<-cor(t(mt), method="pearson")
c_p<-cor[grepl("retro",rownames(cor)), grepl("antero", colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column() %>% tidyr::pivot_longer(-rowname)
c_p=c_p[!is.na(c_p$value),]
c_p=c_p%>%spread(key=name, value=value)
c_matrix<-cor[grepl("retro",rownames(cor)), grepl("antero", colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column()
#write_csv(c_matrix, "rCC_rCT.csv")
#rCC_aCC sheet


#############
#Get correlation of modules by different connection types
##CC

arCC <- read_excel("data/Figure3_cor/summary.xlsx", sheet = "rCC_aCC")
c_module<-read_csv("data/Figure3_cor/cortex_module.csv")
arCC=gather(arCC, key=aCC, value=correlation, 2:32)
names(arCC)[1]="rCC"
arCC$retro_module=NA
arCC$retro_module[grepl(paste0(c_module$area[c_module$module==1], collapse = "|"), arCC$rCC)]="1"
arCC$retro_module[grepl(paste0(c_module$area[c_module$module==2], collapse = "|"), arCC$rCC)]="2"
arCC$retro_module[grepl(paste0(c_module$area[c_module$module==3], collapse = "|"), arCC$rCC)]="3"
arCC$retro_module[grepl(paste0(c_module$area[c_module$module==4], collapse = "|"), arCC$rCC)]="4"
arCC$retro_module[grepl(paste0(c_module$area[c_module$module==5], collapse = "|"), arCC$rCC)]="5"

arCC$antero_module=NA
arCC$antero_module[grepl(paste0(c_module$area[c_module$module==1], collapse = "|"), arCC$aCC)]="1"
arCC$antero_module[grepl(paste0(c_module$area[c_module$module==2], collapse = "|"), arCC$aCC)]="2"
arCC$antero_module[grepl(paste0(c_module$area[c_module$module==3], collapse = "|"), arCC$aCC)]="3"
arCC$antero_module[grepl(paste0(c_module$area[c_module$module==4], collapse = "|"), arCC$aCC)]="4"
arCC$antero_module[grepl(paste0(c_module$area[c_module$module==5], collapse = "|"), arCC$aCC)]="5"
arCC$type[arCC$retro_module==arCC$antero_module]="intra"
arCC$type[arCC$retro_module!=arCC$antero_module]="inter"
intra=arCC$correlation[arCC$type=="intra"]
inter=arCC$correlation[arCC$type=="inter"]
h1 = hist(intra,col = "red",breaks=30,border=FALSE, xlim = range(-0.5,1)) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h1$density = h1$counts/sum(h1$counts)*100


h2=hist(inter, col = "blue",border=FALSE,  breaks=30)
h2$density = h2$counts/sum(h2$counts)*100
png("output/Fig3d_arCC.png", width = 800, height = 600, units = "px")
plot(h1,col = "red",border=FALSE,xlim = range(-0.4,1),ylim=range(0,25),freq=FALSE)
plot(h2,col = "blue",border=FALSE, add=TRUE,freq=FALSE)#6x8
dev.off()

#########################################
#####CT connections
rTC_aCT <- read_excel("data/Figure3_cor/summary.xlsx", sheet = "rTC_aCT")
t_module<-read_csv("data/Figure3_cor/TH_module.csv")
rTC_aCT=gather(rTC_aCT, key=aCT, value=correlation, 2:32)
names(rTC_aCT)[1]="rTC"
rTC_aCT$retro_module=NA
rTC_aCT$retro_module[grepl(paste0(c_module$area[c_module$module==1], collapse = "|"), rTC_aCT$rTC)]="1"
rTC_aCT$retro_module[grepl(paste0(c_module$area[c_module$module==2], collapse = "|"), rTC_aCT$rTC)]="2"
rTC_aCT$retro_module[grepl(paste0(c_module$area[c_module$module==3], collapse = "|"), rTC_aCT$rTC)]="3"
rTC_aCT$retro_module[grepl(paste0(c_module$area[c_module$module==4], collapse = "|"), rTC_aCT$rTC)]="4"
rTC_aCT$retro_module[grepl(paste0(c_module$area[c_module$module==5], collapse = "|"), rTC_aCT$rTC)]="5"

rTC_aCT$antero_module=NA
rTC_aCT$antero_module[grepl(paste0(c_module$area[c_module$module==1], collapse = "|"), rTC_aCT$aCT)]="1"
rTC_aCT$antero_module[grepl(paste0(c_module$area[c_module$module==2], collapse = "|"), rTC_aCT$aCT)]="2"
rTC_aCT$antero_module[grepl(paste0(c_module$area[c_module$module==3], collapse = "|"), rTC_aCT$aCT)]="3"
rTC_aCT$antero_module[grepl(paste0(c_module$area[c_module$module==4], collapse = "|"), rTC_aCT$aCT)]="4"
rTC_aCT$antero_module[grepl(paste0(c_module$area[c_module$module==5], collapse = "|"), rTC_aCT$aCT)]="5"

rTC_aCT$type[rTC_aCT$retro_module==rTC_aCT$antero_module]="intra"
rTC_aCT$type[rTC_aCT$retro_module!=rTC_aCT$antero_module]="inter"
intra=rTC_aCT$correlation[rTC_aCT$type=="intra"]
inter=rTC_aCT$correlation[rTC_aCT$type=="inter"]
h1 = hist(intra,col = "red",breaks=30,border=FALSE, xlim = range(-0.5,1)) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h1$density = h1$counts/sum(h1$counts)*100


h2=hist(inter, col = "blue",border=FALSE,  breaks=30)
h2$density = h2$counts/sum(h2$counts)*100
plot(h1,col = "red",border=FALSE,xlim = range(-0.3,1),ylim=range(0,20),freq=FALSE)
plot(h2,col = "blue",border=FALSE, add=TRUE,freq=FALSE)#6x8

######another type of CT
rCT_aTC <- read_excel("data/Figure3_cor/summary.xlsx", sheet = "rCT_aTC")
t_module<-read_csv("data/Figure3_cor/TH_module.csv")
rCT_aTC=gather(rCT_aTC, key=aTC, value=correlation, 2:24)
names(rCT_aTC)[1]="rCT"
rCT_aTC$retro_module=NA
rCT_aTC$retro_module[grepl(paste0(t_module$area[t_module$module==1], collapse = "|"), rCT_aTC$rCT)]="1"
rCT_aTC$retro_module[grepl(paste0(t_module$area[t_module$module==2], collapse = "|"), rCT_aTC$rCT)]="2"
rCT_aTC$retro_module[grepl(paste0(t_module$area[t_module$module==3], collapse = "|"), rCT_aTC$rCT)]="3"
rCT_aTC$retro_module[grepl(paste0(t_module$area[t_module$module==4], collapse = "|"), rCT_aTC$rCT)]="4"
rCT_aTC$retro_module[grepl(paste0(t_module$area[t_module$module==5], collapse = "|"), rCT_aTC$rCT)]="5"


rCT_aTC$antero_module=NA
rCT_aTC$antero_module[grepl(paste0(t_module$area[t_module$module==1], collapse = "|"), rCT_aTC$aTC)]="1"
rCT_aTC$antero_module[grepl(paste0(t_module$area[t_module$module==2], collapse = "|"), rCT_aTC$aTC)]="2"
rCT_aTC$antero_module[grepl(paste0(t_module$area[t_module$module==3], collapse = "|"), rCT_aTC$aTC)]="3"
rCT_aTC$antero_module[grepl(paste0(t_module$area[t_module$module==4], collapse = "|"), rCT_aTC$aTC)]="4"
rCT_aTC$antero_module[grepl(paste0(t_module$area[t_module$module==5], collapse = "|"), rCT_aTC$aTC)]="5"

rCT_aTC$type[rCT_aTC$retro_module==rCT_aTC$antero_module]="intra"
rCT_aTC$type[rCT_aTC$retro_module!=rCT_aTC$antero_module]="inter"
intra1=rCT_aTC$correlation[rCT_aTC$type=="intra"]
inter1=rCT_aTC$correlation[rCT_aTC$type=="inter"]
h1 = hist(intra1,col = "red",breaks=30,border=FALSE, xlim = range(-0.5,1)) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h1$density = h1$counts/sum(h1$counts)*100


h2=hist(inter1, col = "blue",border=FALSE,  breaks=30)
h2$density = h2$counts/sum(h2$counts)*100
plot(h1,col = "red",border=FALSE,xlim = range(-0.4,1),ylim=range(0,25),freq=FALSE)
plot(h2,col = "blue",border=FALSE, add=TRUE,freq=FALSE)#6x8

#########combined the two CT
intra2=c(intra, intra1)
inter2=c(inter, inter2)
h1 = hist(intra2,col = "red",breaks=30,border=FALSE, xlim = range(-0.5,1)) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h1$density = h1$counts/sum(h1$counts)*100


h2=hist(inter2, col = "blue",border=FALSE,  breaks=30)
h2$density = h2$counts/sum(h2$counts)*100
png("output/Fig3d_CT.png", width = 800, height = 600, units = "px")

plot(h1,col = "red",border=FALSE,xlim = range(-0.4,1),ylim=range(0,25),freq=FALSE)
plot(h2,col = "blue",border=FALSE, add=TRUE,freq=FALSE)#6x8
dev.off()
#######################
##combo of CC-CT
rCC_aTC <- read_excel("data/Figure3_cor/summary.xlsx", sheet = "rCC_aTC")
rCC_aTC=gather(rCC_aTC, key=aTC, value=correlation, 2:29)
names(rCC_aTC)[1]="rCC"
rCC_aTC$retro_module=NA
rCC_aTC$retro_module[grepl(paste0(c_module$area[c_module$module==1], collapse = "|"), rCC_aTC$rCC)]="1"
rCC_aTC$retro_module[grepl(paste0(c_module$area[c_module$module==2], collapse = "|"), rCC_aTC$rCC)]="2"
rCC_aTC$retro_module[grepl(paste0(c_module$area[c_module$module==3], collapse = "|"), rCC_aTC$rCC)]="3"
rCC_aTC$retro_module[grepl(paste0(c_module$area[c_module$module==4], collapse = "|"), rCC_aTC$rCC)]="4"
rCC_aTC$retro_module[grepl(paste0(c_module$area[c_module$module==5], collapse = "|"), rCC_aTC$rCC)]="5"


rCC_aTC$antero_module=NA
rCC_aTC$antero_module[grepl(paste0(t_module$area[t_module$module==1], collapse = "|"), rCC_aTC$aTC)]="1"
rCC_aTC$antero_module[grepl(paste0(t_module$area[t_module$module==2], collapse = "|"), rCC_aTC$aTC)]="2"
rCC_aTC$antero_module[grepl(paste0(t_module$area[t_module$module==3], collapse = "|"), rCC_aTC$aTC)]="3"
rCC_aTC$antero_module[grepl(paste0(t_module$area[t_module$module==4], collapse = "|"), rCC_aTC$aTC)]="4"
rCC_aTC$antero_module[grepl(paste0(t_module$area[t_module$module==5], collapse = "|"), rCC_aTC$aTC)]="5"

rCC_aTC$type[rCC_aTC$retro_module==rCC_aTC$antero_module]="intra"
rCC_aTC$type[rCC_aTC$retro_module!=rCC_aTC$antero_module]="inter"
intra=rCC_aTC$correlation[rCC_aTC$type=="intra"]
inter=rCC_aTC$correlation[rCC_aTC$type=="inter"]
h1 = hist(intra,col = "red",breaks=30,border=FALSE, xlim = range(-0.5,1)) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h1$density = h1$counts/sum(h1$counts)*100

h2=hist(inter, col = "blue",border=FALSE,  breaks=30)
h2$density = h2$counts/sum(h2$counts)*100
plot(h1,col = "red",border=FALSE,xlim = range(-0.4,1),ylim=range(0,25),freq=FALSE)
plot(h2,col = "blue",border=FALSE, add=TRUE,freq=FALSE)#6x8

##CC-CT
rCT_aCC <- read_excel("data/Figure3_cor/summary.xlsx", sheet = "rCT_aCC")
rCT_aCC=gather(rCT_aCC, key=aCC, value=correlation, 2:35)
names(rCT_aCC)[1]="rCT"
rCT_aCC$retro_module=NA
rCT_aCC$retro_module[grepl(paste0(t_module$area[t_module$module==1], collapse = "|"), rCT_aCC$rCT)]="1"
rCT_aCC$retro_module[grepl(paste0(t_module$area[t_module$module==2], collapse = "|"), rCT_aCC$rCT)]="2"
rCT_aCC$retro_module[grepl(paste0(t_module$area[t_module$module==3], collapse = "|"), rCT_aCC$rCT)]="3"
rCT_aCC$retro_module[grepl(paste0(t_module$area[t_module$module==4], collapse = "|"), rCT_aCC$rCT)]="4"
rCT_aCC$retro_module[grepl(paste0(t_module$area[t_module$module==5], collapse = "|"), rCT_aCC$rCT)]="5"


rCT_aCC$antero_module=NA
rCT_aCC$antero_module[grepl(paste0(c_module$area[c_module$module==1], collapse = "|"), rCT_aCC$aCC)]="1"
rCT_aCC$antero_module[grepl(paste0(c_module$area[c_module$module==2], collapse = "|"), rCT_aCC$aCC)]="2"
rCT_aCC$antero_module[grepl(paste0(c_module$area[c_module$module==3], collapse = "|"), rCT_aCC$aCC)]="3"
rCT_aCC$antero_module[grepl(paste0(c_module$area[c_module$module==4], collapse = "|"), rCT_aCC$aCC)]="4"
rCT_aCC$antero_module[grepl(paste0(c_module$area[c_module$module==5], collapse = "|"), rCT_aCC$aCC)]="5"

rCT_aCC$type[rCT_aCC$retro_module==rCT_aCC$antero_module]="intra"
rCT_aCC$type[rCT_aCC$retro_module!=rCT_aCC$antero_module]="inter"
intra1=rCT_aCC$correlation[rCT_aCC$type=="intra"]
inter1=rCT_aCC$correlation[rCT_aCC$type=="inter"]
h1 = hist(intra1,col = "red",breaks=30,border=FALSE, xlim = range(-0.5,1)) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h1$density = h1$counts/sum(h1$counts)*100

h2=hist(inter1, col = "blue",border=FALSE,  breaks=30)
h2$density = h2$counts/sum(h2$counts)*100
plot(h1,col = "red",border=FALSE,xlim = range(-0.4,1),ylim=range(0,25),freq=FALSE)
plot(h2,col = "blue",border=FALSE, add=TRUE,freq=FALSE)#6x8

inter=c(inter, inter1)
intra=c(intra, intra1)

################
rCC_rCT <- read_excel("data/Figure3_cor/summary.xlsx", sheet = "rCC_rCT")
rCC_rCT=gather(rCC_rCT, key=rCT, value=correlation, 2:27)
names(rCC_rCT)[1]="rCC"
rCC_rCT$c_module=NA
rCC_rCT$c_module[grepl(paste0(c_module$area[c_module$module==1], collapse = "|"), rCC_rCT$rCC)]="1"
rCC_rCT$c_module[grepl(paste0(c_module$area[c_module$module==2], collapse = "|"), rCC_rCT$rCC)]="2"
rCC_rCT$c_module[grepl(paste0(c_module$area[c_module$module==3], collapse = "|"), rCC_rCT$rCC)]="3"
rCC_rCT$c_module[grepl(paste0(c_module$area[c_module$module==4], collapse = "|"), rCC_rCT$rCC)]="4"
rCC_rCT$c_module[grepl(paste0(c_module$area[c_module$module==5], collapse = "|"), rCC_rCT$rCC)]="5"


rCC_rCT$t_module=NA
rCC_rCT$t_module[grepl(paste0(t_module$area[t_module$module==1], collapse = "|"), rCC_rCT$rCT)]="1"
rCC_rCT$t_module[grepl(paste0(t_module$area[t_module$module==2], collapse = "|"), rCC_rCT$rCT)]="2"
rCC_rCT$t_module[grepl(paste0(t_module$area[t_module$module==3], collapse = "|"), rCC_rCT$rCT)]="3"
rCC_rCT$t_module[grepl(paste0(t_module$area[t_module$module==4], collapse = "|"), rCC_rCT$rCT)]="4"
rCC_rCT$t_module[grepl(paste0(t_module$area[t_module$module==5], collapse = "|"), rCC_rCT$rCT)]="5"

rCC_rCT$type[rCC_rCT$t_module==rCC_rCT$c_module]="intra"
rCC_rCT$type[rCC_rCT$t_module!=rCC_rCT$c_module]="inter"
intra1=rCC_rCT$correlation[rCC_rCT$type=="intra"]
inter1=rCC_rCT$correlation[rCC_rCT$type=="inter"]
h1 = hist(intra1,col = "red",breaks=30,border=FALSE, xlim = range(-0.5,1)) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h1$density = h1$counts/sum(h1$counts)*100

h2=hist(inter1, col = "blue",border=FALSE,  breaks=30)
h2$density = h2$counts/sum(h2$counts)*100
plot(h1,col = "red",border=FALSE,xlim = range(-0.4,1),ylim=range(0,20),freq=FALSE)
plot(h2,col = "blue",border=FALSE, add=TRUE,freq=FALSE)#6x8

intra=c(intra, intra1)
inter=c(inter, inter1)
h1 = hist(intra,col = "red",breaks=30,border=FALSE, xlim = range(-0.5,1)) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h1$density = h1$counts/sum(h1$counts)*100

h2=hist(inter, col = "blue",border=FALSE,  breaks=30)
h2$density = h2$counts/sum(h2$counts)*100

png("output/Fig3d_comb.png", width = 800, height = 600, units = "px")
plot(h1,col = "red",border=FALSE,xlim = range(-0.4,1),ylim=range(0,22),freq=FALSE)
plot(h2,col = "blue",border=FALSE, add=TRUE,freq=FALSE)#6x8
dev.off()
