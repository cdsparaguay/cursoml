## creacion de vectores e impresion
v=c()                 ## vector vacio
nu=c(0.5,0.6)         ## vector de 2 numeros 
l1=c(TRUE,FALSE,TRUE) ## vector de 3 logicos
l2=c(T,F)             ## vector de 2 logicos
ch=c('a')             ## vector de 1 caracter
it=9:29               ## vector de 21 numeros secuenciales enteros
co=c(1+0i, 2+4i)      ## vector de 2 numeros complejos
v=vector('numeric',length = 100)
v[1]=5
v                     ## autoimpresion 
print(v)

## coercion implicita segun precedencia
y=c(1.7,'a')
y
y=c(TRUE,FALSE,2)
y
y=c('a',TRUE,FALSE)
y
y=c('a',TRUE,FALSE,1.7)
y

## coercion explicita (casting)
x=0:6
class(x)
x=as.numeric(x)
x
class(x)
x=as.logical(x)
x
x=as.character(x)
x
x=c('a','b','c')
x=as.numeric(x)
x
x=as.logical(x)
x

## vectores n-dimensionales (matrices)
m=matrix(1:6,nrow = 2,ncol = 3)
m
dim(m)
m[2,1]
m[1,2]
m[1,3]
x=1:3
y=10:12
cbind(x,y)
z=rbind(x,y)
z
rbind(z,m)
rbind(y,m)

## listas
x=list(1,'a',TRUE,1+4i)
x
x[1]
x[[1]]

## factores, datos categoricos
#x=factor(c('yes','yes','no','yes','no','z','a'))
x=factor(c('yes','yes','no','yes','no'), levels=c('yes','no'))
x
levels(x)
table(x)

## valores desconocidos
x=c(1,2,NA,10,3)
is.na(x)

## data frames
x=data.frame(c1=1:5,c2=c(T,T,F,F,T),c3=c('a','b','c','d','e'))
x
x[3]        ## por default el acceso del primer indice es columna
x[1,3]      ## acceso por matriz ya es en el orden fila, columna
nrow(x)
ncol(x)
dim(x)
names(x)

## estructuras de control: if
x=10
y=0
if(x>3){
  y=10
}else{
  y=50
}
y
x=10
if(x==10){
  print('El valor de x es el esperado')
}

## estructuras de control: for
x=data.frame(c1=c('Jorge','Manuel','Jazmin','Julio'),c2=c(15,18,21,23),c3=c('','','',''))
for(i in seq_len(nrow(x))){
  if(x[i,'c2']<20){
    x[i,'c3']='menor'
  }else{
    x[i,'c3']='mayor'
  }
}
x
x=data.frame(c1=4:6,c2=18:20)
x
for(i in seq_len(nrow(x))){
  print(x[i, 'c1'])
  print(x[i, 2])
}

## apply
x = data.frame(c1 = 1:3, c2=10:12)
x
apply(x, 2, median) ##margin: 1-por fila, 2-por columna

x = data.frame(c1 = 1:2, c2=10:11)
x
mult = function(x,c){
  return (x*c)
}
apply(x,2,mult,5)

x = data.frame(c1 = 1:2, c2=10:11)
x
area_circulo=function(r){
  return (pi*r^2)
}
sapply(x[,'c1'],area_circulo)    ##la , permite elegir en que dimensión retorna

x = data.frame(c1 = 1:5, c2=10:14)
x
rowSums(x)
rowMeans(x)
colSums(x)
colMeans(x)

##lectura/escritura de datos
data = read.csv(file = 'clases/Clase02_R/ContratosDNCP2018.csv', header = T,stringsAsFactors = F)

	##,colClasses = c('numeric','character','character','character','character','character','character','character','character')

## row.names es el ordinal que usa R para numerar los registros
write.csv(data,'clases/Clase02_R/output.csv',row.names = F)

str(data)   ##describir el diccionario de datos

## Histograma
library(datasets)
autos=mtcars
hist(autos$mpg,col = 'green', main = 'Distribución de millas por galón',
     xlab = 'Millas por galón',ylab = 'Frecuencia')
hist(autos$hp,col = 'lightgreen', main = 'Distribución de horse power',
     xlab = 'Horse Power',ylab = 'Frecuencia')

## Boxplot
boxplot(autos$hp,col = 'lightgreen', main = 'Distribución de horse power',
            ylab = 'Horse power')

## Barplot
barplot(table(autos$am),col = 'lightgreen', main = 'Nro. de vehículos por tipo de transmisión',
        xlab = 'Tipo de Transmisión')

## Plot: series y scatter
plot(presidents, ylab = 'Porcentaje de aprobación (%)', xlab='Año',
     main='Aprobación (primer cuatrimestre) Presidentes de EEUU')

## Scatterplot
plot(autos$mpg, autos$wt, col = 'blue', main = 'Relación entre peso y millas por galón',
     xlab = 'Millas por galón', ylab = 'Peso (libras)')


