library(knitr)
library(kableExtra)
library(dplyr)
library(readr)
library(htmltools)
#library(pagedown)
library(stringi)
library(stringr)
#could generate everything from the supplementary NPV table
#load("../ipsi_contra_PV_Feb.RData") #PV with contra/ipsi
load("data/BG_list_March.RData")#846 experiments
order<-read_csv("data/order1.csv",col_names = FALSE)#cortical areas in 6 modules
test=ipsi_new[,1:3]%>%subset(target %in% c(order$X1[1:43]))
test=left_join(test, cortex_exp[,-3])

tb=test%>%group_by(target)%>%summarise(n=n())
tb2=test[, c(3,4)]%>%group_by(target)%>%summarise(n=length(unique(Cre)))
names(tb2)[2]="Cre lines"
tb=left_join(tb, tb2)
tb=tb[,c(1,3,2)]
tb$target=factor(tb$target, levels=order$X1[1:43])
tb=tb[order(tb$target),]

table_c= tb%>%kbl(booktabs = T, caption = "Summary of cortical experiments")%>%
  footnote(c("*SSp-bfd-rll experiments excluded"))%>%
    kable_classic(full_width = F, html_font = "Arial")
kableExtra::save_kable(table_c, file = "output/Fig1d_cortex.html")

##summarise thalamic and BG experiments
test=ipsi_new[,1:3]%>%subset(!(target %in% c(order$X1[1:43], "SSp-bfd-rll")))
test$Cre=substr(test$specimen, 1,5)
tb=test%>%group_by(target)%>%summarise(n=n())
tb2=test[, c(3,4)]%>%group_by(target)%>%summarise(n=length(unique(Cre)))
names(tb2)[2]="Cre lines"
tb=left_join(tb, tb2)
tb=tb[,c(1,3,2)]
tb$target=factor(tb$target, levels=order$X1)
tb=tb[order(tb$target),]

table_sub= tb%>%kbl(booktabs = T, caption = "Summary of subcortical experiments")%>%
  footnote(c("*BG"))%>%
  kable_classic(full_width = F, html_font = "Arial")
kableExtra::save_kable(table_sub, file = "output/Fig1d_sub.html")


save(ipsi_new, contra_new, cortex_exp, file="data/BG_list_March.RData")


