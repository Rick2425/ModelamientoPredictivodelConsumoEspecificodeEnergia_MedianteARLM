library(ggplot2)
library(dplyr)

hist(datos3$SEC)
min(datos3$SEC)
max(datos3$SEC)
# ==============================================================================
# SECCIÓN 1: Bond (Índice de trabajo de Bond - kWh/t)
# ==============================================================================
# Evaluar asimetría y posibles subpoblaciones (bimodalidad por tipo de roca).
ggplot(datos3, aes(x = Bond)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#2b5c8f", color = "white", alpha = 0.85) +
  geom_density(color = "#d95f02", linewidth = 1) +
  geom_vline(aes(xintercept = mean(Bond, na.rm = TRUE)), color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_vline(aes(xintercept = median(Bond, na.rm = TRUE)), color = "darkgreen", linetype = "dotted", linewidth = 0.8) +
  labs(title = "Distribución de Bond (kWh/t)", x = "Bond (kWh/t)", y = "Densidad") +
  theme_minimal()
ggsave("Bond.png", width = 8, height = 5, dpi = 300, bg = "white")

frecuencias = table(datos3$Bond)
as.numeric(names(frecuencias[frecuencias == max(frecuencias)]))
max(datos3$Bond)
min(datos3$Bond)
mean(datos3$Bond)
median(datos3$Bond)


ggplot(datos3, aes(x = Bond, fill = Tipo_Mineral, color = Tipo_Mineral)) +
  geom_density(alpha = 0.35, linewidth = 1) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Distribución de Densidad de Bond según Tipo de Mineral",
    x = "Bond (kWh/t)",
    y = "Densidad",
    fill = "Tipo de Mineral",
    color = "Tipo de Mineral"
  ) +
  theme_minimal()
# ==============================================================================
# SECCIÓN 2: Alimentación (Flujo de alimentación al molino - ton/h)
# ==============================================================================
# Detectar colas izquierdas (caídas de flujo o paradas no programadas) y puntos extremos.
ggplot(datos3, aes(x = Alimentación)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#2b5c8f", color = "white", alpha = 0.85) +
  geom_density(color = "#d95f02", linewidth = 1) +
  geom_vline(aes(xintercept = mean(Alimentación, na.rm = TRUE)), color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_vline(aes(xintercept = median(Alimentación, na.rm = TRUE)), color = "darkgreen", linetype = "dotted", linewidth = 0.8) +
  labs(title = "Distribución de Alimentación (ton/h)", x = "Alimentación (ton/h)", y = "Densidad") +
  theme_minimal()



frecuencias = table(datos3$Alimentación)
as.numeric(names(frecuencias[frecuencias == max(frecuencias)]))
max(datos3$Alimentación)
min(datos3$Alimentación)
mean(datos3$Alimentación)
median(datos3$Alimentación)

# Comparar ajuste lineal (rojo) vs no lineal LOESS (naranja) para detectar 
# curvatura hiperbólica/logarítmica, heterocedasticidad y outliers bivariados.

ggplot(datos3, aes(x = Alimentación, y = SEC)) +
  geom_point(alpha = 0.5, color = "#2b5c8f", size = 2) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE, linewidth = 1) +
  geom_smooth(method = "loess", color = "#d95f02", se = TRUE, linewidth = 1) +
  labs(
    title = "Relación entre Alimentación y Consumo Específico de Energía (SEC)",
    subtitle = "Línea Roja: Ajuste Lineal (OLS) | Línea Naranja: Tendencia Suavizada (LOESS)",
    x = "Alimentación (ton/h)",
    y = "SEC (kWh/t)"
  ) +
  theme_minimal()
# ==============================================================================
# SECCIÓN 3: Sólidos (Porcentaje de sólidos en la pulpa - %)
# ==============================================================================
# Verificar rango operativo estricto y detectar valores fuera de límites físicos.
ggplot(datos3, aes(x = Sólidos)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#2b5c8f", color = "white", alpha = 0.85) +
  geom_density(color = "#d95f02", linewidth = 1) +
  geom_vline(aes(xintercept = mean(Sólidos, na.rm = TRUE)), color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_vline(aes(xintercept = median(Sólidos, na.rm = TRUE)), color = "darkgreen", linetype = "dotted", linewidth = 0.8) +
  labs(title = "Distribución de Sólidos (%)", x = "Sólidos (%)", y = "Densidad") +
  theme_minimal()


frecuencias = table(datos3$Sólidos)
as.numeric(names(frecuencias[frecuencias == max(frecuencias)]))
max(datos3$Sólidos)
min(datos3$Sólidos)
mean(datos3$Sólidos)
median(datos3$Sólidos)

# ==============================================================================
# SECCIÓN 4: Presión (Presión hidráulica de los descansos - PSI)
# ==============================================================================
# Identificar lecturas anómalas por sobrecarga o ruido instrumental en descansos.
ggplot(datos3, aes(x = Presión)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#2b5c8f", color = "white", alpha = 0.85) +
  geom_density(color = "#d95f02", linewidth = 1) +
  geom_vline(aes(xintercept = mean(Presión, na.rm = TRUE)), color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_vline(aes(xintercept = median(Presión, na.rm = TRUE)), color = "darkgreen", linetype = "dotted", linewidth = 0.8) +
  labs(title = "Distribución de Presión Hidráulica (PSI)", x = "Presión (PSI)", y = "Densidad") +
  theme_minimal()

frecuencias = table(datos3$Presión)
as.numeric(names(frecuencias[frecuencias == max(frecuencias)]))
max(datos3$Presión)
min(datos3$Presión)
mean(datos$Presión)
median(datos$Presión)


# ==============================================================================
# SECCIÓN 5: Tamaño (Tamaño del 80% de mineral de entrada F80 - mm)
# ==============================================================================
# Evaluar sesgo en granulometría; fuerte candidata a transformación (log o raíz) por apalancamiento.
ggplot(datos3, aes(x = Tamaño)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#2b5c8f", color = "white", alpha = 0.85) +
  geom_density(color = "#d95f02", linewidth = 1) +
  geom_vline(aes(xintercept = mean(Tamaño, na.rm = TRUE)), color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_vline(aes(xintercept = median(Tamaño, na.rm = TRUE)), color = "darkgreen", linetype = "dotted", linewidth = 0.8) +
  labs(title = "Distribución de Tamaño de Entrada (mm)", x = "Tamaño (mm)", y = "Densidad") +
  theme_minimal()


frecuencias = table(datos3$Tamaño)
as.numeric(names(frecuencias[frecuencias == max(frecuencias)]))
max(datos3$Tamaño)
min(datos3$Tamaño)
mean(datos3$Tamaño)
median(datos$Tamaño)

ggplot(datos3, aes(x = Tamaño, y = SEC)) +
  geom_point(alpha = 0.5, color = "#2b5c8f", size = 2) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE, linewidth = 1) +
  geom_smooth(method = "loess", color = "#d95f02", se = TRUE, linewidth = 1) +
  labs(
    title = "Relación entre Tamaño de Entrada (F80) y SEC",
    subtitle = "Línea Roja: Ajuste Lineal (OLS) | Línea Naranja: Tendencia Suavizada (LOESS)",
    x = "Tamaño (mm)",
    y = "SEC (kWh/t)"
  ) +
  theme_minimal()
# ==============================================================================
# SECCIÓN 6: Velocidad (Velocidad operacional - % de velocidad crítica)
# ==============================================================================
# Detectar regímenes de operación fijos o caídas bruscas de rotación del molino.
ggplot(datos3, aes(x = Velocidad)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#2b5c8f", color = "white", alpha = 0.85) +
  geom_density(color = "#d95f02", linewidth = 1) +
  geom_vline(aes(xintercept = mean(Velocidad, na.rm = TRUE)), color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_vline(aes(xintercept = median(Velocidad, na.rm = TRUE)), color = "darkgreen", linetype = "dotted", linewidth = 0.8) +
  labs(title = "Distribución de Velocidad Operacional (%)", x = "Velocidad (% vel. crítica)", y = "Densidad") +
  theme_minimal()


frecuencias = table(datos3$Velocidad)
as.numeric(names(frecuencias[frecuencias == max(frecuencias)]))
max(datos3$Velocidad)
min(datos3$Velocidad)
mean(datos3$Velocidad)
median(datos3$Velocidad)
# ==============================================================================
# SECCIÓN 7: Lubricante (Temperatura de aceite - °C)
# ==============================================================================
# Observar colas derechas asociadas a sobrecalentamiento por fricción o desgaste crítico.
ggplot(datos3, aes(x = Lubricante)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#2b5c8f", color = "white", alpha = 0.85) +
  geom_density(color = "#d95f02", linewidth = 1) +
  geom_vline(aes(xintercept = mean(Lubricante, na.rm = TRUE)), color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_vline(aes(xintercept = median(Lubricante, na.rm = TRUE)), color = "darkgreen", linetype = "dotted", linewidth = 0.8) +
  labs(title = "Distribución de Temperatura del Lubricante (°C)", x = "Lubricante (°C)", y = "Densidad") +
  theme_minimal()

frecuencias = table(datos3$Lubricante)
as.numeric(names(frecuencias[frecuencias == max(frecuencias)]))
max(datos3$Lubricante)
min(datos3$Lubricante)
mean(datos3$Lubricante)
median(datos3$Lubricante)

# ==============================================================================
# SECCIÓN 8: Tipo_Mineral (Categórica Nominal: Escanio, Porfidico, Cuarcita)
# ==============================================================================
# Verificar el balance de clases para asegurar estabilidad en la estimación de variables dummy.
ggplot(datos3, aes(x = Tipo_Mineral, fill = Tipo_Mineral)) +
  geom_bar(alpha = 0.85, width = 0.6) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Frecuencia por Tipo de Mineral", x = "Tipo de Mineral", y = "Frecuencia (n)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("Tipo_Mineral.png", width = 8, height = 5, dpi = 300, bg = "white")


# ==============================================================================
# SECCIÓN 9: Turno (Categórica Nominal: Manana, Tarde, Noche)
# ==============================================================================
# Comprobar representatividad equitativa de los horarios operativos en la muestra.
datos3 %>%
  mutate(Turno = factor(Turno, levels = c("Manana", "Tarde", "Noche"))) %>%
  ggplot(aes(x = Turno, fill = Turno)) +
  geom_bar(alpha = 0.85, width = 0.6) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, fontface = "bold") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Frecuencia por Turno Laboral", x = "Turno", y = "Frecuencia (n)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("Turno.png", width = 8, height = 5, dpi = 300, bg = "white")


# ==============================================================================
# SECCIÓN 10: Estado (Categórica Ordinal: Nuevo, Medio, Desgastado, Critico)
# ==============================================================================
# Ordenar jerárquicamente los niveles de desgaste y evaluar si la clase 'Critico' está subrepresentada.
datos3 %>%
  mutate(Estado = factor(Estado, levels = c("Nuevo", "Medio", "Desgastado", "Critico"))) %>%
  ggplot(aes(x = Estado, fill = Estado)) +
  geom_bar(alpha = 0.85, width = 0.6) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, fontface = "bold") +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Frecuencia por Estado de la Máquina", x = "Estado", y = "Frecuencia (n)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("Estado.png", width = 8, height = 5, dpi = 300, bg = "white")



# ==============================================================================
# 1. AUDITORÍA NUMÉRICA DE SEC Y CONTEO EXACTO DE ANOMALÍAS
# ==============================================================================
summary(datos$SEC)

cat("Filas con SEC <= 0 (Imposibles físicos):", sum(datos$SEC <= 0, na.rm = TRUE), "\n")
cat("Filas con SEC > 22 (Outliers superiores):", sum(datos$SEC > 22, na.rm = TRUE), "\n")
cat("Filas con Bond > 25 (Cluster alto apalancamiento):", sum(datos$Bond > 25, na.rm = TRUE), "\n")

# Ver si las filas de Bond > 25 son las mismas que tienen SEC anómalo
datos %>% 
  filter(Bond > 25 | SEC <= 0 | SEC > 22) %>% 
  select(Bond, Alimentación, Tamaño, Velocidad, Tipo_Mineral, Estado, SEC) %>% 
  head(20)


# ==============================================================================
# 2. BOXPLOTS BIVARIADOS: SEC vs VARIABLES CATEGÓRICAS
# ==============================================================================
# 2.1 SEC por Tipo de Mineral
ggplot(datos, aes(x = Tipo_Mineral, y = SEC, fill = Tipo_Mineral)) +
  geom_boxplot(alpha = 0.8, outlier.color = "red", outlier.shape = 16) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribución de SEC según Tipo de Mineral", x = "Tipo de Mineral", y = "SEC (kWh/t)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("Boxplot_SEC_Tipo_Mineral.png", width = 8, height = 5, dpi = 300, bg = "white")

# 2.2 SEC por Turno
datos %>%
  mutate(Turno = factor(Turno, levels = c("Manana", "Tarde", "Noche"))) %>%
  ggplot(aes(x = Turno, y = SEC, fill = Turno)) +
  geom_boxplot(alpha = 0.8, outlier.color = "red", outlier.shape = 16) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Distribución de SEC según Turno Laboral", x = "Turno", y = "SEC (kWh/t)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("Boxplot_SEC_Turno.png", width = 8, height = 5, dpi = 300, bg = "white")

# 2.3 SEC por Estado de la Máquina
datos %>%
  mutate(Estado = factor(Estado, levels = c("Nuevo", "Medio", "Desgastado", "Critico"))) %>%
  ggplot(aes(x = Estado, y = SEC, fill = Estado)) +
  geom_boxplot(alpha = 0.8, outlier.color = "red", outlier.shape = 16) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Distribución de SEC según Estado de la Máquina", x = "Estado", y = "SEC (kWh/t)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("Boxplot_SEC_Estado.png", width = 8, height = 5, dpi = 300, bg = "white")


library(PerformanceAnalytics)
chart.Correlation(datos[,1:8], histogram=TRUE)
