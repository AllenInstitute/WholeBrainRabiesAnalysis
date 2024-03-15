library(ggplot2)
library(stringr)
library(readr)
#August8_SC.R
load("data/ipsi_contra_PV_Feb.RData") #PV with contra/ipsi
load("data/BG_list_March.RData")#846 experiments
#check SC layers
all_areas_list<-read_csv("data/all_areas_list.csv")
sc<-all_areas_list[grepl("SC", all_areas_list$structure),2:3]
sc<-sc[10:19,]
sc_ipsi<-ipsi_PV[,c("specimen",sc$graph_order)]%>%subset(specimen %in% ipsi_new$specimen[ipsi_new$target %in% c('VPM', "LGd", "LP", "MG", "PoT", "PO")])

for (a in sc$graph_order) {
  names(sc_ipsi)[which(names(sc_ipsi)==a)]=sc$structure[which(sc$graph_order==a)]
}
 sc_ipsi<-left_join (ipsi_new[ipsi_new$target %in% c('VPM', "LGd", "LP", "MG", "PoT", "PO"), c("specimen","target")],sc_ipsi)
 test<-sc_ipsi%>%group_by(target)%>%summarise(across(where(is.numeric), list(sum=sum ), na.rm = TRUE))
 names(test)<-str_replace(names(test), "_sum", "")
test<-test[,-which(names(test)=="SCO")] 
test2<-gather(test,key=input, value=percentage, 2:10)
g_1<-ggplot(test2%>%subset(input%in% c("SCs", "SCm"))%>%subset( target=="LGd"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("LGd")
g_4<-ggplot(test2%>%subset(input%in% c("SCs", "SCm"))%>%subset( target=="LP"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("LP")

g_2<-ggplot(test2%>%subset(input%in% c("SCs", "SCm"))%>%subset( target=="VPM"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("VPM")
g_5<-ggplot(test2%>%subset(input%in% c("SCs", "SCm"))%>%subset( target=="PO"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("PO")

g_3<-ggplot(test2%>%subset(input%in% c("SCs", "SCm"))%>%subset( target=="MG"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("MG")
g_6<-ggplot(test2%>%subset(input%in% c("SCs", "SCm"))%>%subset( target=="PoT"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("PoT")

plot.list <- list(g_1, g_4, g_2, g_5,g_3, g_6)

hlay <- rbind(c(1,2),
              c(3,4),c(5,6))
# c(7,8,NA),
# c(9,10,NA))

gridExtra::grid.arrange(grobs=plot.list, layout_matrix=hlay)#12x4

test2$input<-factor(test2$input, levels=c("SCs",  "SCzo","SCsg" ,"SCop", "SCm", "SCig", "SCiw", "SCdg", "SCdw"))
g_1<-ggplot(test2%>%subset(input%in% c("SCig", "SCiw", "SCdg", "SCdw"))%>%subset( target=="LGd"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("LGd")
g_4<-ggplot(test2%>%subset(input%in% c("SCig", "SCiw", "SCdg", "SCdw"))%>%subset( target=="LP"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("LP")

g_2<-ggplot(test2%>%subset(input%in% c("SCig", "SCiw", "SCdg", "SCdw"))%>%subset( target=="VPM"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("VPM")
g_5<-ggplot(test2%>%subset(input%in% c("SCig", "SCiw", "SCdg", "SCdw"))%>%subset( target=="PO"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("PO")

g_3<-ggplot(test2%>%subset(input%in% c("SCig", "SCiw", "SCdg", "SCdw"))%>%subset( target=="MG"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("MG")
g_6<-ggplot(test2%>%subset(input%in% c("SCig", "SCiw", "SCdg", "SCdw"))%>%subset( target=="PoT"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("PoT")

plot.list <- list(g_1, g_4, g_2, g_5,g_3, g_6)

hlay <- rbind(c(1,2),
              c(3,4),c(5,6))
# c(7,8,NA),
# c(9,10,NA))

gridExtra::grid.arrange(grobs=plot.list, layout_matrix=hlay)#12x4

g_1<-ggplot(test2%>%subset(input%in% c("SCzo","SCsg" ,"SCop"))%>%subset( target=="LGd"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("LGd")
g_4<-ggplot(test2%>%subset(input%in% c("SCzo","SCsg" ,"SCop"))%>%subset( target=="LP"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("LP")

g_2<-ggplot(test2%>%subset(input%in% c("SCzo","SCsg" ,"SCop"))%>%subset( target=="VPM"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("VPM")
g_5<-ggplot(test2%>%subset(input%in% c("SCzo","SCsg" ,"SCop"))%>%subset( target=="PO"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("PO")

g_3<-ggplot(test2%>%subset(input%in% c("SCzo","SCsg" ,"SCop"))%>%subset( target=="MG"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("MG")
g_6<-ggplot(test2%>%subset(input%in% c("SCzo","SCsg" ,"SCop"))%>%subset( target=="PoT"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +
  coord_polar("y", start=0) +theme_void()+ggtitle("PoT")

plot.list <- list(g_1, g_4, g_2, g_5,g_3, g_6)

hlay <- rbind(c(1,2),
              c(3,4),c(5,6))
# c(7,8,NA),
# c(9,10,NA))

gridExtra::grid.arrange(grobs=plot.list, layout_matrix=hlay)#12x4

g_1=ggplot(test2%>%subset( target=="LGd")%>%subset(input!="SCs")%>%subset(input!="SCm"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +theme_void()+ggtitle("LGd")
g_2=ggplot(test2%>%subset( target=="VPM")%>%subset(input!="SCs")%>%subset(input!="SCm"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +theme_void()+ggtitle("VPM")
g_3=ggplot(test2%>%subset( target=="MG")%>%subset(input!="SCs")%>%subset(input!="SCm"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +theme_void()+ggtitle("MG")
g_4=ggplot(test2%>%subset( target=="LP")%>%subset(input!="SCs")%>%subset(input!="SCm"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +theme_void()+ggtitle("LP")
g_5=ggplot(test2%>%subset( target=="PO")%>%subset(input!="SCs")%>%subset(input!="SCm"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +theme_void()+ggtitle("PO")
g_6=ggplot(test2%>%subset( target=="PoT")%>%subset(input!="SCs")%>%subset(input!="SCm"), aes(x="", y=percentage, fill=input))+ geom_bar(stat="identity") +theme_void()+ggtitle("PoT")
plot.list <- list(g_1, g_4, g_2, g_5,g_3, g_6)

hlay <- rbind(c(1,2,3,4,5,6))
# c(7,8,NA),
# c(9,10,NA))

gridExtra::grid.arrange(grobs=plot.list, layout_matrix=hlay)#12x4

test<-ipsi_new[ipsi_new$target %in% c('VPM', "LGd", "LP", "MG", "PoT", "PO"),c("target", "specimen","SCs",  "SCm")]
test<-gather(test,key=input, value=percentage, 3:4)
test$input<-factor(test$input, levels=c("SCs", "SCm"))
test$x<-paste0(test$target, sep = "-", test$input)

ggplot(test,aes(x=x, y=percentage, group=input,color=input))+ geom_point ()+
  scale_x_discrete(limits=c("LGd-SCs","LGd-SCm","LP-SCs" ,"LP-SCm" ,"VPM-SCs","VPM-SCm","PO-SCs","PO-SCm","MG-SCs","MG-SCm","PoT-SCs","PoT-SCm"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
