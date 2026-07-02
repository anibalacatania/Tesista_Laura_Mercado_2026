library(SensoMineR)
library(readxl)
library(missMDA)
library(lmerTest)
library(predictmeans)
library(tidyverse)
library(missMDA)

####################Datos Sensoriales######################




data <- read_excel("data.xlsx")
data<-as.data.frame(data)


data$producto<-as.factor(data$producto)
data$panelista<-as.factor(data$panelista)

table(data$panelista,data$producto)
str(data)




res.panellipse <- panellipse(data,col.p=1,col.j=2,firstvar=3,level.search.desc=1,graph.type = "ggplot")


str(data)


### Análisis multivariado para chequear que no se produzcan errores de tipo 1

da.a=as.matrix(data [,-c(1:3)])
da.man<-manova(da.a~(panelista+producto), data=data)
a<-summary(da.man, test="Wilks")
output <- capture.output(a)
as.data.frame(output)




res.panelperf <- panelperf(data,firstvar=4,formul="~producto+panelista",random=F)

res.panelperf$p.value
coltable(res.panelperf$p.value[order(res.panelperf$p.value[,1]),],col.lower="gray", level.lower = 0.05,cex=0.8)

