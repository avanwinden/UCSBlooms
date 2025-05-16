#R-code and example data accompanying the manuscript "The multivariate analysis of variance as a powerful approach for circular data" by Lukas Landler, Graeme Ruxton and Pascal Malkemper
library(circular)
library(sjPlot)

data(pigeons) # load data from in package circular 

#define the variables needed 
ori.p <- rad(pigeons$bearing) #direction needs to be in radians -> convert to radians 
trt.p <- as.factor(pigeons$treatment) #extract the treatment from data set 

#perform MANOVA for treatment effect 
MANOVA_pigeon<-summary(manova(cbind(cos(ori.p),sin(ori.p)) ~ trt.p),intercept=T)

#Write the table -> this takes a few steps, because we also show "<0.01" for everything below 0.01
MANOVA_table<-as.data.frame(MANOVA_pigeon$stats)
MANOVA_table$`Pr(>F)`<- round(MANOVA_table$`Pr(>F)`,3)
MANOVA_table$`Pr(>F)`<-ifelse( (MANOVA_table$`Pr(>F)`<0.01), "<0.01", MANOVA_table$`Pr(>F)`) 
MANOVA_table_<-MANOVA_table[-c(3),-c(1,4,5)]
rownames(MANOVA_table_)<-c("Intercept","Treatment")
tab_df(MANOVA_table_,file="MANOVA_Table_pigeon.doc",col.header = c("Pillai","approx. F", "p"),show.rownames=TRUE) #doesn't show the row name in output for some reason, added it by hand


manova(cbind(cos(ori.p),sin(ori.p)) ~ trt.p, data=pigeons)
summary(manova(cbind(cos(ori.p),sin(ori.p)) ~ trt.p),intercept=T)
summary(manova(cbind(cos(ori.p),sin(ori.p)) ~ trt.p, data=pigeons))
