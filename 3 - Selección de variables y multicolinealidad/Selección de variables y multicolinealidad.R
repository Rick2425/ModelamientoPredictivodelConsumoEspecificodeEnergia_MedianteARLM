install.packages("psych")
#librerias
library(olsrr)
library(psych)
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
#Modelo de este análisis
modelo2 = lm(SEC~Bond+ Alimentación+Sólidos+ Tamaño+ Velocidad+ Tipo_Mineral+ Estado,data = datos)
summary(modelo2)
#Modelo generado por mejores subconjuntos
res <- ols_step_all_possible(modelo)
a<- res$result

# 1. MÉTRICAS A MAXIMIZAR (Varianza explicada y capacidad predictiva)
mejor_adjr    <- which.max(a$adjr)     # R-cuadrado ajustado
a[1023,]
mejor_predrsq <- which.max(a$predrsq)  # R-cuadrado predictivo
a[848,]
# 2. CRITERIOS DE PENALIZACIÓN A MINIMIZAR (Parsimonia del modelo)
mejor_aic     <- which.min(a$aic)      # Criterio de Akaike
a[968,]
mejor_bic     <- which.min(a$sbic)     # BIC (Schwarz Bayesian Criterion)
a[968,]
mejor_cp      <- which.min(a$cp)       # Cp de Mallows
a[848,]
# 3. MÉTRICAS DE ERROR A MINIMIZAR (Precisión de estimación)
mejor_rmse    <- which.min(a$rmse)     # Raíz del Error Cuadrático Medio
a[1023,]
mejor_fpe     <- which.min(a$fpe)      # Error de Predicción Final de Akaike
a[968]
#Los modelos mas relevantes en base a las métricas
#contenidas en el metodo de los mejores subconjuntos
#son a[1023,], a[968,], a[848,]
a[1023,]; a[968,]; a[848,]

#El modelo final, luego de diagnosticar los resultados es
modelo3 = lm(SEC~Bond+ Alimentación+Sólidos+ Tamaño+ Velocidad+ Tipo_Mineral+ Estado,data = datos)
summary(modelo3)
shapiro.test(modelo3$residuals)
bptest(modelo3)
dwtest(modelo3, alternative = "t")
#Modelo generado por métodos computacionales
modelo_step_aic <- step(modelo, direction = "both")
modelo_step_bic <- step(modelo, direction = "both", k = log(n_obs))

#Analizando multicolinealidad

# ANÁLISIS MATRICIAL CON VARIABLES CONTINUAS Y CATEGÓRICAS (DUMMIES)
# ==========================================

# 1. Extraemos la Matriz de Diseño del modelo completo
X_total <- model.matrix(modelo3)[ , -1]

# 2. Matriz de Correlación Total (Punto-Biserial y Phi incluidas)
rs_total <- cor(X_total)
rs_total

# Determinante y KMO
cat("\n--- 1. DETERMINANTE Y KMO GLOBAL ---\n")
cat("Determinante (|R|):", det(rs_total), "\n")
cat("KMO Global:", KMO(r = rs_total)$MSA, "\n")

# 3. ANÁLISIS DE EIGENSISTEMA Y NÚMERO DE CONDICIÓN (Test de Belsley)
# Estandarizamos la matriz (matemáticamente necesario para los autovalores, 
# aunque las dummies pierdan su forma de 0 y 1)
Xse_total <- scale(X_total)
nc_total <- t(Xse_total) %*% Xse_total
# Extracción de autovalores
auto_total <- eigen(nc_total)$val
auto_total
# Número de Condición Global (Kappa)
numero_condicion_total <- sqrt(max(auto_total) / min(auto_total))
numero_condicion_total
# Índices de Condición por componente
print(sqrt(max(auto_total) / auto_total))

