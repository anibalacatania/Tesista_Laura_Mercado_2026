library(readxl)
library(ggplot2)
library("FactoMineR")
library(tidyverse)
library(readxl)
datos <- read_excel("data.xlsx")
# Cargar las librerías necesarias
library(dplyr)
library(tidyr)

# Suponiendo que tu data frame se llama df
# Primero, vamos a pivotar las variables para tener todas en una sola columna
df_long <- datos %>%
  pivot_longer(cols = -c(Producto, Rep, Valuador), names_to = "Variable", values_to = "Valor")

# Luego, creamos una columna que combine Rep y Variable para distinguir entre Rep 1 y Rep 2
df_long <- df_long %>%
  mutate(Rep_Variable = paste0(Variable, "_Rep", Rep))

# Finalmente, pivotamos de vuelta para tener cada combinación de Rep y Variable como columnas separadas
df_wide <- df_long %>%
  select(-Rep, -Variable) %>%
  pivot_wider(names_from = Rep_Variable, values_from = Valor)%>%
  select(-Valuador)%>%
  group_by(Producto) %>%
  summarise(across(everything(), mean, na.rm = TRUE))%>%
  column_to_rownames(var = "Producto")

mfa<-MFA(df_wide, group=c(14,14),
    name.group=c("Rep1","Rep2"))
res.mfa<-plot( MFA(df_wide, group=c(14,14),
          name.group=c("Rep1","Rep2")), choix = "ind", partial="all")

summary(mfa)
rv<-as.data.frame(mfa$group$RV)
rv2<-c("rep1","rep2","MFA")
a<-cbind(rv2,rv)
a
mfa$group
res.mfa$inertia.ratio


########### quimicos IG ##########
res.panelperf <- panelperf(as.data.frame(datos),firstvar=4,formul="~Producto+Valuador+Rep+Producto:Valuador+Producto:Rep+Valuador:Rep",random=T)
res.panelperf$p.value
