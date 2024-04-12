library(readr)
library(dplyr)
library(tidyr)
library(readxl)
antero_ipsi <- read_excel("antero_ipsi.xlsx")
retro_ipsi <- read_excel("retro_ipsi.xlsx")
order<-read_csv("../data/order1.csv",col_names = FALSE)
areas=order$X1[c(1:43, 100:143)]
retro_ipsi=retro_ipsi[retro_ipsi$target %in% areas, ]
retro_ipsi=retro_ipsi[, c("target", retro_ipsi$target)]

antero_ipsi=antero_ipsi[antero_ipsi$source_area %in% areas, ]
antero_ipsi=antero_ipsi[, c("source_area", antero_ipsi$source_area)]
#retro_TC: thalamic inputs to cortical targets
retro_TC=retro_ipsi[retro_ipsi$target %in% order$X1[1:43],c("target", order$X1[100:143])]
#antero_CT: cortical projection ending in thalamus
antero_CT=antero_ipsi[antero_ipsi$source_area %in% order$X1[1:43], c("source_area", order$X1[100:143])]


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
rownames(mt)=rowof
mt[, sapply(mt, is.character)] <- apply(mt[, sapply(mt, is.character)], 2, as.numeric)
rownames(mt)=rowof
cor<-cor(t(mt), method="pearson")
c_p<-cor[grepl("retro",rownames(cor)), grepl("antero", colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column() %>% tidyr::pivot_longer(-rowname)
c_p=c_p[!is.na(c_p$value),]
c_p=c_p%>%spread(key=name, value=value)
c_matrix<-cor[grepl("retro",rownames(cor)), grepl("antero", colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column()
#write_csv(c_matrix, "eFigure9b.csv")
#remove NA row and columns and arrange order according to cortical module

#retro_CT: cortical inputs to thalamic targets
retro_CT=retro_ipsi[retro_ipsi$target %in% order$X1[100:143],c("target", order$X1[1:43])]
#antero_TC: thalamic projection to cortex
antero_TC=read_csv("th_antero.csv")
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
c_p<-cor[grepl("antero",rownames(cor)), grepl("antero", colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column() %>% tidyr::pivot_longer(-rowname)
c_p=c_p[!is.na(c_p$value),]
c_p=c_p%>%spread(key=name, value=value)
c_matrix<-cor[grepl("retro",rownames(cor)), grepl("antero", colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column()
#write_csv(c_matrix, "eFigure9c.csv")

#make retroCC matrix
retro_CC=retro_ipsi[retro_ipsi$target %in% order$X1[1:43],c("target", order$X1[1:43])]
#antero_TC: thalamic projection to cortex
retro_CC$connection="retro"
#antero_TC$connection="antero"
names(retro_CC)[1]="injection"
#names(antero_TC)[1]="injection"

retro_CC$id=paste0(retro_CC$connection, "_", retro_CC$injection)
#antero_TC$id=paste0(antero_TC$connection, "_", antero_TC$injection)
retro_CC=retro_CC[, c("id", "connection", "injection", order$X1[1:43])]
#antero_TC=antero_TC[, c("id", "connection", "injection", order$X1[1:43])]

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
#write_csv(c_matrix, "eFigure9e.csv")

#antero_CT=
#antero_TC
#antero_TT
#antero_CC=
#antero_TC=
#antero_CT=
#antero_TC=
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
#write_csv(c_matrix, "eFigure9d.csv")

##retroCC and retroCT
mt=rbind(retro_CC, retro_CT)
mt=mt[,-c(2:3)]
rowof=mt$id
mt=mt[,-1]

mt[, sapply(mt, is.character)] <- apply(mt[, sapply(mt, is.character)], 2, as.numeric)
rownames(mt)=rowof

cor<-cor(t(mt), method="pearson")
c_p<-cor[grepl("retro",rownames(cor)), grepl("retro", colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column() %>% tidyr::pivot_longer(-rowname)
c_p=c_p[!is.na(c_p$value),]
c_p=c_p%>%spread(key=name, value=value)
c_matrix<-cor[grepl(paste0(order$X1[1:43], collapse = "|"),rownames(cor)), grepl(paste0(order$X1[100:143], collapse = "|"), colnames(cor))]%>% as.data.frame %>% tibble::rownames_to_column()
#write_csv(c_matrix, "efigure9f.csv")


###############
mc=rbind(retro_CC, antero_CC)
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
#write_csv(c_matrix, "eFigure9a.csv")


