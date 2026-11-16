library(olsrr)
library(psych)
library(lmtest)
#Cargando el modelo
modelo = lm(SEC~.,data = datos)
summary(modelo)
n_obs <- nrow(datos)
shapiro.test(modelo$residuals)
bptest(modelo)
dwtest(modelo, alternative = "t")
#Método anidado
mod_sin_bond       <- update(modelo, . ~ . - Bond)
mod_sin_alim       <- update(modelo, . ~ . - Alimentación)
mod_sin_solidos    <- update(modelo, . ~ . - Sólidos)
mod_sin_presion    <- update(modelo, . ~ . - Presión)
mod_sin_tamano     <- update(modelo, . ~ . - Tamaño)
mod_sin_velocidad  <- update(modelo, . ~ . - Velocidad)
mod_sin_lubricante <- update(modelo, . ~ . - Lubricante)
mod_sin_mineral    <- update(modelo, . ~ . - Tipo_Mineral)
mod_sin_turno      <- update(modelo, . ~ . - Turno)
mod_sin_estado     <- update(modelo, . ~ . - Estado)

anova(mod_sin_bond, modelo)
anova(mod_sin_alim, modelo)
anova(mod_sin_solidos, modelo)
anova(mod_sin_presion, modelo)
anova(mod_sin_tamano, modelo)
anova(mod_sin_velocidad, modelo)
anova(mod_sin_lubricante, modelo)
anova(mod_sin_mineral, modelo)
anova(mod_sin_turno, modelo)
anova(mod_sin_estado, modelo)


# Definir el modelo final parsimonioso
modelo_final <- lm(SEC ~ Bond + Alimentación + Sólidos + Tamaño + 
                     Velocidad + Tipo_Mineral + Estado, 
                   data = datos)

# Ver el resumen con las métricas y coeficientes limpios
summary(modelo_final)
#verificacion de supuestos
shapiro.test(modelo_final$residuals)
bptest(modelo_final)
dwtest(modelo_final,alternative = "t")

