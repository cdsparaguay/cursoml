## cap 1 practical statistics for data science
pop=c(4779736,710231,6392017,2915918,37253956,5029196,3574097,897934)
mrate=c(5.7,5.6,4.7,5.6,4.4,2.8,2.4,5.8)
sd(pop)
IQR(pop)
mad(pop)
mean(pop,trim = 0.15)

sort(pop)

boxplot(cbind(scale(pop),scale(mrate)))

hist(pop, breaks = 10)

## rnorm: random normal distribution
## las: labels position, names: labels names
data<-data.frame(Stat11=rnorm(100,mean=3,sd=2),
                 Stat21=rnorm(100,mean=4,sd=1),
                 Stat31=rnorm(100,mean=6,sd=0.5),
                 Stat41=rnorm(100,mean=10,sd=0.5),
                 Stat12=rnorm(100,mean=4,sd=2),
                 Stat22=rnorm(100,mean=4.5,sd=2),
                 Stat32=rnorm(100,mean=7,sd=0.5),
                 Stat42=rnorm(100,mean=8,sd=3),
                 Stat13=rnorm(100,mean=6,sd=0.5),
                 Stat23=rnorm(100,mean=5,sd=3),
                 Stat33=rnorm(100,mean=8,sd=0.2),
                 Stat43=rnorm(100,mean=4,sd=4))
boxplot(data, las = 2,at =c(1,2,3,4,6,7,8,9,11,12,13,14),
         names = c('Station 1','Station 2','Station 3',
                  'Station 4','Station 1','Station 2',
                  'Station 3','Station 4','Station 1',
                  'Station 2','Station 3','Station 4'))






