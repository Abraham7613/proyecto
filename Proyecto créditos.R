
setwd("C:/Users/DELL/Desktop")
credit<- read.csv(file= "credito.csv", header=T,)
install.packages("DescTools")
install.packages("MASS")
#Promedio del ingreso
library(dplyr)
income <-select(credit, income) %>% unlist
mean(income)

#varianza del ingreso
var(income)

#desviación estándar
sd(income)
#histograma
hist(income)
hist(loan)
#filtrar por impago
impago<- filter(credit, Def=="0")
#edad promedio de impago
edimpago<-select(impago, age) %>% unlist
edimpagosna<-na.omit(edimpago)
mean(edimpagosna)
median(edimpagosna)

#regresión lineal

credit<- read.csv(file= "credito.csv", header=T,)
head(credit, n=10)
tail(credit, n=10)
attach(credit)

#nombres
names(credit)
cor(credit)


##### plot####
plot(x = credit$income, y = credit$loan,)
str(credit)
cor(credit$income,credit$loan)

## lineal model ###
regresions <- lm(income ~ loan, data = credit)
summary(regresions)

#gráfico con abline
plot(credit$income, credit$loan, xlab='income', ylab='loan')
abline(regresions)
mean(income)
median(income)

#pedicción
nuevos.loans <- data.frame(income = seq(4000, 4532))
predict(regresions, nuevos.loans)

#intervalos de confianza
confint(regresions)

#conf int 90
confint(regresions, level = 0.90)

#homocedasticidad 
residuos <- rstandard(regresions)
valores.ajustados <- fitted(regresions)
plot(valores.ajustados, residuos)

#normalidad
qqnorm(residuos)
qqline(residuos)
