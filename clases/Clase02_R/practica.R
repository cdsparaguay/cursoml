#######contratos dncp
data = read.csv(file = 'ContratosDNCP2018.csv', header = T, stringsAsFactors = F)

#write.csv(data, 'output.csv', row.names=F)

data$value.amount = as.numeric(data$value.amount)
data$tender.procuringEntity.name = as.factor(data$tender.procuringEntity.name)
data$tender.additionalProcurementCategories = as.factor(data$tender.additionalProcurementCategories)
data$tender.procurementMethodDetails = as.factor(data$tender.procurementMethodDetails)
data$status = as.factor(data$status)
data$tender.awardCriteriaDetails = as.factor(data$tender.awardCriteriaDetails)

str(data)

table(data$tender.procuringEntity.name)

##barplot
barplot(table(data$tender.additionalProcurementCategories),
        col = 'darkgreen', 
        main = 'Cantidad de contratos por categorías',
        las = 2)

##ggplot2
library(stringr)
library(ggplot2)


##gg barplot
ggplot(data, aes(x=str_wrap(tender.additionalProcurementCategories,35)))+
  geom_bar(stat="count", width=0.5, fill="darkgreen")+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+
  labs(title="Contratos por Categoría", x="Categoría", y="Cantidad")

##gg histogram variable categorica
ggplot(data, aes(x=str_wrap(tender.procurementMethodDetails,35)))+
  geom_histogram(stat="count", fill="darkgreen")+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+
  labs(title="Contratos por Modalidad", x="Modalidad", y="Cantidad")

#medidas de tendencia central
mean(data$value.amount, na.rm=TRUE)
median(data$value.amount, na.rm=TRUE)
summary(data$value.amount/1000000)
quantile(data$value.amount/1000000,probs = c(0.15,0.6,0.85,0.95))

#max min
min(data$value.amount, na.rm=TRUE)
max(data$value.amount, na.rm=TRUE)

##Interquartil Range: Q3-Q1
IQR(data$value.amount, na.rm=TRUE)


##gg histogram variable numerica
ggplot(data, aes(data$value.amount))+
  geom_histogram(breaks=seq(0, 1000000000, by=100000000), aes(fill=..count..))+
  scale_fill_gradient(low = "yellow", high = "orange")+
  labs(title="Contratos por Monto", x="Monto en Gs", y="Cantidad")

##gg histogram variable numerica clasificada por una categorica
ggplot(data, aes(x=(value.amount/1000000),fill=tender.awardCriteriaDetails))+
  geom_histogram(breaks=seq(0, 1000, by=100))+
  labs(title="Contratos por Monto", x="Monto en Mill. Gs", y="Cantidad", 
       fill="Sist. de Adj.")

table(data$tender.awardCriteriaDetails)

##gg scatter plot variable numerica y categorica
q99 = quantile(data$value.amount,probs=0.99)
dataQ99 = subset(data, data$value.amount <= q99)

ggplot(dataQ99, aes(x=str_wrap(tender.additionalProcurementCategories,35), 
                 y=(value.amount/1000000))) +
  geom_point(size=1)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+
  labs(title="Montos x Categorías", x="Categorías", y="Montos en Mill Gs")

ggplot(dataQ99, aes(x=str_wrap(tender.procurementMethodDetails,35), 
                    y=(value.amount/1000000))) +
  geom_point(size=1)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+
  labs(title="Montos x Modalidades", x="Modalidades", y="Montos en Mill Gs")


##gg boxplot
##Caja: Q3 a Q1, Raya dentro de la Caja: Mediana (Q2), Bigotes: +|- 1.5*IQR
dataQ95 = subset(data, data$value.amount <= 1500000000)

ggplot(dataQ95, aes(x=str_wrap(tender.additionalProcurementCategories,35), 
                 y=(value.amount/1000000))) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=1)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+
  labs(title="Montos x Categorias", x="Categorias", y="Montos en Mill Gs")

ggplot(dataQ95, aes(x=str_wrap(tender.procurementMethodDetails,35), 
                   y=(value.amount/1000000))) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=1)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+
  labs(title="Montos x Modalidades", x="Modalidades", y="Montos en Mill Gs")


##medidas de dispersión
var(data$value.amount/1000000)
sd(data$value.amount/1000000)
var(dataQ95$value.amount/1000000)
sd(dataQ95$value.amount/1000000)

library(dplyr)
#install.packages("dplyr")
data$dateSigned = as.Date(data$dateSigned,format = "%Y-%m-%d")
data = mutate(data, month=as.numeric(format(dateSigned, "%m")))
data = mutate(data, monthName=months(dateSigned))
data$monthName = factor(data$monthName,
                        levels = c('enero','febrero','marzo','abril',
                                   'mayo','junio','julio','agosto',
                                   'septiembre','octubre','noviembre','diciembre'),
                        ordered = TRUE)
dataSinNA = subset(data, !is.na(value.amount) & !is.na(month))
dataQ95SinNA = subset(data, data$value.amount <= 1500000000 
                      & !is.na(value.amount) & !is.na(month))

cor(dataSinNA$value.amount, dataSinNA$month)
cor(dataQ95SinNA$value.amount, dataQ95SinNA$month)

ggplot(dataQ95SinNA, aes(x=monthName, y=(value.amount/1000000))) +
  geom_point(size=1)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+
  labs(title="Montos x Mes", x="Mes", y="Montos en Mill Gs")

ggplot(dataQ95SinNA, aes(x=monthName,y=(value.amount/1000000))) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=1)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+
  labs(title="Montos x Mes", x="Mes", y="Montos en Mill Gs")

dataQ95a100SinNA = subset(data, data$value.amount > 1500000000 
                      & !is.na(value.amount) & !is.na(month))

ggplot(dataQ95a100SinNA, aes(x=monthName, y=(value.amount/1000000000))) +
  geom_point(size=1)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+
  labs(title="Montos x Mes", x="Mes", y="Montos en Miles de Mill Gs")

