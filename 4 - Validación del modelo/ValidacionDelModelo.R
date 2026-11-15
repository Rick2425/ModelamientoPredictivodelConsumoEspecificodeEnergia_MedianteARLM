#install.packages("caret", type = "binary")
library(caret)
library(car)
# 1. Fijar una semilla para que el remuestreo sea reproducible
set.seed(2026) 

# 2. Configurar el "Cerebro" del entrenamiento: 10-Fold CV
control_cv <- trainControl(method = "cv", number = 20)

# 3. Entrenar el Modelo 848 sometiéndolo a las 10 pruebas ciegas
modelo_cv <- train(SEC ~ Bond + Alimentación + Sólidos + Tamaño + 
                     Velocidad + Tipo_Mineral + Estado, 
                   data = datos, 
                   method = "lm", 
                   trControl = control_cv)

# 4. Imprimir el resumen del rendimiento Global
cat("\n=== RENDIMIENTO PROMEDIO FUERA DE LA MUESTRA ===\n")
print(modelo_cv)

# 5. Ver las notas de los 10 exámenes por separado (estabilidad)
cat("\n=== RENDIMIENTO EN CADA UNO DE LOS 10 FOLDS ===\n")
print(modelo_cv$resample)

