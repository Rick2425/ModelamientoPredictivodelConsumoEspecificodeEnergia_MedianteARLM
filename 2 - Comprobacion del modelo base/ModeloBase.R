#librerias
library(writexl)
library(lmtest)
library(sasLM)
library(car)
library(olsrr)
#Asignando niveles de referencia
datos$Turno <- factor(datos$Turno, levels = c("Manana", "Tarde", "Noche"))
datos$Estado <- factor(datos$Estado, levels = c("Nuevo", "Medio", "Desgastado", "Critico"))
datos$Tipo_Mineral <- factor(datos$Tipo_Mineral, levels = c("Escanio", "Porfidico", "Cuarcita"))
str(datos)
#Creacion del modelo base y primeras observaciones
modelo1 = lm(SEC~.,data = datos)
modelo1$coefficients

#solo sirve dereferente, al analizar los supuestos
#se concluye que al no haber normalidad 
#es imposible usar estas herramientas
aov1(SEC~.,Data = datos)
summary(modelo1)

#Análisis de valores atípicos e influenciales

#Diagnostico de la matriz sombrero
#Para el punto de balanceo(leverange)
hii = hatvalues(modelo1)
p = sum(hii)
n = nrow(datos)
2*p/n>1 #Asegura que se puede usar este diagnóstico
d1 = which(abs(hii) > 2*p/n)
d1
#Residuos Estandarizados
re<-rstandard(modelo1)
d2<-which(abs(re)>2)
d2
#Residuos Estudentizados
res<-rstudent(modelo1)
d3<-which(abs(res)>2)
d3
#Extrapolación oculta
hmax<-which.max(hii)
X1 <- model.matrix(SEC ~ ., data = datos)
x1 <- as.matrix(X1[1, ])
eo1<-t(x1)%*%solve(t(X1)%*%X1)%*%x1
eo1>hmax

n1<-nrow(X1)
resul<-c()
for(i in 1:n1){
  resul[i]<-t(X1[i,])%*%solve(t(X1)%*%X1)%*%X1[i,]
}

d4<-which(resul>hmax)
d4
#Distancia de Cook
di<-cooks.distance(modelo1)
ref1<-qf(0.5,p,n-p)
d5<-which(di>ref1)
d5
#DFFITS
dfit<-dffits(modelo1)
ref2<-2*sqrt(p/n)
d6<-which(abs(dfit)>ref2)
d6
#DFBETAS
dfbe<-dfbetas(modelo1)
ref3<-2/sqrt(n)
res2<-abs(dfbe[,-1])>ref3
d7<-which(apply(res2,1,sum)>2)
d7
#COVRATIO
covr<-covratio(modelo1)
d8<-which(covr>1+3*p/n | covr<1-3*p/n)
d8
#Del análisis de todos los diagnósticos se concuerda 
#que 34 observaciones afectn de forma severa nuestra data
#por lo que analizaremos que tanto mejora nuestro modelo
#luego de eliminar estos datos

#Supuestos antes de la eliminacion de las observaciones
shapiro.test(modelo1$residuals)#no se cumple
bptest(modelo1)#no se cumple
dwtest(modelo1, alternative = "t")# se cumple

filas_a_eliminar <- c(119, 121, 178, 188, 227, 230, 311, 388, 390, 453, 
                      467, 492, 596, 635, 641, 677, 743, 754, 756, 857, 
                      858, 860, 877, 881, 909, 981, 1010, 1012, 1019, 1043, 
                      1095, 1098, 1118, 1170)
datos2 <- datos[-filas_a_eliminar, ]
datos3 <- subset(datos2, SEC > 0) #Analizamos un caso adicional
#es fisicamente imposible que esto ocurra
modelo2 = lm(SEC~.,data = datos2)
modelo3 = lm(SEC~.,data = datos3) 
#Supuestos despues de la eliminacion de las observaciones
shapiro.test(modelo2$residuals)#se cumple
bptest(modelo2)# se cumple
dwtest(modelo2, alternative = "t")#se cumple
#Supuestos despues de la eliminacion de observaciones negativas
shapiro.test(modelo3$residuals)#se cumple
bptest(modelo3)# se cumple
dwtest(modelo3, alternative = "t")#se cumple

#Efectivamente la anulación de las 34 observaciones
#tuvo un impacto netamente positivo en el modelo

#Ademas la eliminacion de observaciones nulas
#que tambien tuvo un efecto favorable en la normalidad
#y la independencia, aumentando su significancia
#aunque la homocedasticidad perdio algunas unidades

#El modelo final de este análisis es modelo3
modelo3$coefficients
aov1(SEC~.,Data = datos3)
summary(modelo3)

#Guardando el conjunto de datos depurado
write_xlsx(datos3, path = "Proyecto_Depurado.xlsx")
