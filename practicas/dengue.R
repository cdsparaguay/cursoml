datos = read.csv(file = 'data/dengue_clima.csv', header = T, stringsAsFactors = F)

cm = cor(datos)

#write.csv(cm,'cm_dengue_clima.csv',row.names = T)

cm03 = subset(cm, cm[,"cantidad"] >= 0.3 | cm[,"cantidad"] <= -0.3)

datosFiltrados = datos[,c("temperatura_media_media..7.","temperatura_media_media..8.",
         "temperatura_media_media..9.","temperatura_media_media..10.",
         "temperatura_media_media..11.","nivel..5.","nivel..6.",
         "nivel..7.","nivel..8.","nivel..9.","nivel..10.",
         "nivel..11.","cantidad")]

plot(x = datosFiltrados$temperatura_media_media..11., y = datosFiltrados$cantidad)
plot(x = datosFiltrados$nivel..11., y = datosFiltrados$cantidad)

m1 = lm(cantidad ~ temperatura_media_media..11., data = datosFiltrados)
m2 = lm(cantidad ~ nivel..11., data = datosFiltrados)
m3 = lm(cantidad ~ temperatura_media_media..11. + nivel..11., data = datosFiltrados)
summary(m1)
summary(m2)
summary(m3)

##### TRAINING & VALIDATION
## lm train, 230 primeros registros (70%)
training = head(datosFiltrados, 230)
m4 = lm(cantidad ~ temperatura_media_media..11. + nivel..11., data = training)
summary(m4)
m4NonNeg = m4$fitted.values
m4NonNeg[m4NonNeg < 0] = 0

## plot real vs trained
matplot(cbind(training$cantidad,m4NonNeg),type="l",col=c("red","green"),lty=c(1,1))

## lm predict, 98 últimos registros (30%)
validation = tail(datosFiltrados, 98)
p4 = predict(m4, validation, se.fit = TRUE)
p4NonNeg = p4$fit
p4NonNeg[p4NonNeg < 0] = 0

## plot real vs predicted
matplot(cbind(validation$cantidad,p4NonNeg),type="l",col=c("red","green"),lty=c(1,1))

######################################
## PCA
pcaDatos = prcomp(datosFiltrados[1:12], scale = TRUE)
screeplot(pcaDatos)
summary(pcaDatos)

## 90% cummulative proportion of variance con PC1-4
trainUnificado = head(cbind(pcaDatos$x[,1:4], datosFiltrados[13]), 230)
m = lm(cantidad ~ PC1 + PC2 + PC3 + PC4, data = trainUnificado)
summary(m)
mNonNeg = m$fitted.values
mNonNeg[mNonNeg < 0] = 0
matplot(cbind(trainUnificado$cantidad,mNonNeg),type="l",col=c("red","green"),lty=c(1,1))

## validation
validationUnificado = tail(cbind(pcaDatos$x[,1:4], datosFiltrados[13]) , 98)
p = predict(m, validationUnificado, se.fit = TRUE)
pNonNeg = p$fit
pNonNeg[pNonNeg < 0] = 0
matplot(cbind(validationUnificado$cantidad,pNonNeg),type="l",col=c("red","green"),lty=c(1,1))

######################################
## PCA con datos completos originales
datos2 = cbind(datos[12:99], datos[101:111], datos[100])
pcaDatos2 = prcomp(datos2[1:99], scale = TRUE)
screeplot(pcaDatos2)
summary(pcaDatos2)

## 90% cummulative proportion of variance con PC1-26
trainUnificado = head(cbind(pcaDatos2$x[,1:26], datos2[100]), 230)
m = lm(cantidad ~ PC1 + PC2 + PC3 + PC4 +
                  PC5 + PC6 + PC7 + PC8 +
                  PC9 + PC10 + PC11 + PC12 +
                  PC13 + PC14 + PC15 + PC16 +
                  PC17 + PC18 + PC19 + PC20 +
                  PC21 + PC22 + PC23 + PC24 +
                  PC25 + PC26, data = trainUnificado)
summary(m)
mNonNeg = m$fitted.values
mNonNeg[mNonNeg < 0] = 0
matplot(cbind(trainUnificado$cantidad,mNonNeg),type="l",col=c("red","green"),lty=c(1,1))

## validation
validationUnificado = tail(cbind(pcaDatos2$x[,1:26], datos2[100]) , 98)
p = predict(m, validationUnificado, se.fit = TRUE)
pNonNeg = p$fit
pNonNeg[pNonNeg < 0] = 0
matplot(cbind(validationUnificado$cantidad,pNonNeg),type="l",col=c("red","green"),lty=c(1,1))

