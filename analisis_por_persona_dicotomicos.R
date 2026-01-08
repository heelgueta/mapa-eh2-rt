# =============================================================================
# Análisis de Tiempos de Respuesta por Persona - Variables Dicotómicas
# =============================================================================

# Cargar librerías necesarias
if (!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("viridis")) install.packages("viridis", dependencies = TRUE)
if (!require("RColorBrewer")) install.packages("RColorBrewer", dependencies = TRUE)
if (!require("gridExtra")) install.packages("gridExtra", dependencies = TRUE)
if (!require("scales")) install.packages("scales", dependencies = TRUE)

library(tidyverse)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(gridExtra)
library(scales)

# Cargar datos
datos <- read.csv("mapa-eh2-rt-anon.csv", stringsAsFactors = FALSE)

# Variables dicotómicas
vars_dicotomicas <- c("dt02_gend", "dt05_inte", "dt39_pact")
vars_existentes <- vars_dicotomicas[vars_dicotomicas %in% names(datos)]

cat("=== ANÁLISIS POR PERSONA - VARIABLES DICOTÓMICAS ===\n\n")
cat("Variables analizadas:", paste(vars_existentes, collapse = ", "), "\n")
cat("Número de personas:", nrow(datos), "\n\n")

# Crear ID de persona
datos$persona_id <- paste0("P", sprintf("%02d", 1:nrow(datos)))

# Preparar datos en formato largo para análisis por persona
datos_largos <- datos %>%
  select(persona_id, all_of(vars_existentes)) %>%
  pivot_longer(
    cols = all_of(vars_existentes),
    names_to = "variable",
    values_to = "tiempo"
  ) %>%
  mutate(
    tiempo = as.numeric(tiempo),
    variable = factor(variable, levels = vars_existentes)
  ) %>%
  filter(!is.na(tiempo) & tiempo > 0)

# Estadísticas por persona
estadisticas_persona <- datos_largos %>%
  group_by(persona_id) %>%
  summarise(
    n_items = n(),
    media = mean(tiempo),
    mediana = median(tiempo),
    sd = sd(tiempo),
    min = min(tiempo),
    max = max(tiempo),
    iqr = IQR(tiempo),
    .groups = "drop"
  ) %>%
  arrange(media)

cat("=== ESTADÍSTICAS POR PERSONA ===\n")
print(estadisticas_persona)
cat("\n")

# Estadísticas por persona y variable
estadisticas_persona_variable <- datos_largos %>%
  group_by(persona_id, variable) %>%
  summarise(
    tiempo = first(tiempo),  # Solo hay un valor por persona-variable
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = tiempo
  )

cat("=== TIEMPOS POR PERSONA Y VARIABLE ===\n")
print(estadisticas_persona_variable)
cat("\n")

# =============================================================================
# VISUALIZACIONES
# =============================================================================

# 1. HEATMAP: Tiempos por persona y variable
heatmap_plot <- datos_largos %>%
  ggplot(aes(x = variable, y = persona_id, fill = tiempo)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis_c(
    name = "Tiempo\n(ms)",
    option = "plasma",
    trans = "log10",
    labels = comma
  ) +
  labs(
    title = "Heatmap: Tiempos de Respuesta por Persona y Variable",
    subtitle = "Variables Dicotómicas - Escala logarítmica",
    x = "Variable",
    y = "Persona",
    fill = "Tiempo (ms)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12),
    legend.position = "right",
    panel.grid = element_blank()
  )

# Guardar heatmap
png(
  filename = "heatmap_personas_dicotomicas.png",
  width = 10,
  height = 8,
  units = "in",
  res = 300
)
print(heatmap_plot)
dev.off()
cat("Gráfico guardado: heatmap_personas_dicotomicas.png\n\n")

# 2. GRÁFICO DE LÍNEAS: Patrón de cada persona
lineas_plot <- datos_largos %>%
  ggplot(aes(x = variable, y = tiempo, group = persona_id, color = persona_id)) +
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_y_continuous(
    trans = "log10",
    labels = comma,
    name = "Tiempo de respuesta (ms)"
  ) +
  scale_color_viridis_d(option = "turbo", guide = "none") +
  labs(
    title = "Patrones Individuales de Tiempos de Respuesta",
    subtitle = "Cada línea representa una persona",
    x = "Variable",
    y = "Tiempo de respuesta (ms, escala log)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

# Guardar gráfico de líneas
png(
  filename = "lineas_personas_dicotomicas.png",
  width = 10,
  height = 7,
  units = "in",
  res = 300
)
print(lineas_plot)
dev.off()
cat("Gráfico guardado: lineas_personas_dicotomicas.png\n\n")

# 3. BOXPLOT POR VARIABLE con puntos individuales por persona
boxplot_personas <- datos_largos %>%
  ggplot(aes(x = variable, y = tiempo)) +
  geom_boxplot(
    fill = "#3498DB",
    color = "#2C3E50",
    alpha = 0.6,
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(color = persona_id),
    width = 0.2,
    height = 0,
    alpha = 0.7,
    size = 2.5
  ) +
  scale_y_continuous(
    trans = "log10",
    labels = comma,
    name = "Tiempo de respuesta (ms)"
  ) +
  scale_color_viridis_d(option = "turbo", guide = "none") +
  labs(
    title = "Distribución de Tiempos por Variable",
    subtitle = "Cada punto representa una persona",
    x = "Variable",
    y = "Tiempo de respuesta (ms, escala log)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12)
  )

# Guardar boxplot
png(
  filename = "boxplot_personas_dicotomicas.png",
  width = 10,
  height = 7,
  units = "in",
  res = 300
)
print(boxplot_personas)
dev.off()
cat("Gráfico guardado: boxplot_personas_dicotomicas.png\n\n")

# 4. GRÁFICO DE BARRAS: Media por persona (ordenado)
barras_media <- estadisticas_persona %>%
  ggplot(aes(x = reorder(persona_id, media), y = media)) +
  geom_bar(stat = "identity", fill = "#E74C3C", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = media - sd, ymax = media + sd),
    width = 0.3,
    color = "#2C3E50",
    linewidth = 0.8
  ) +
  scale_y_continuous(
    labels = comma,
    name = "Tiempo promedio (ms)"
  ) +
  labs(
    title = "Tiempo Promedio de Respuesta por Persona",
    subtitle = "Barras de error = ±1 SD",
    x = "Persona",
    y = "Tiempo promedio (ms)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12)
  )

# Guardar gráfico de barras
png(
  filename = "barras_media_personas.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300
)
print(barras_media)
dev.off()
cat("Gráfico guardado: barras_media_personas.png\n\n")

# 5. GRÁFICO DE PERFILES: Todas las personas en un panel
perfiles_plot <- datos_largos %>%
  ggplot(aes(x = variable, y = tiempo)) +
  geom_line(
    aes(group = persona_id, color = persona_id),
    linewidth = 1,
    alpha = 0.6
  ) +
  geom_point(
    aes(color = persona_id),
    size = 2,
    alpha = 0.8
  ) +
  facet_wrap(~ persona_id, ncol = 4, scales = "free_y") +
  scale_y_continuous(
    labels = comma,
    name = "Tiempo (ms)"
  ) +
  scale_color_viridis_d(option = "turbo", guide = "none") +
  labs(
    title = "Perfiles Individuales de Tiempos de Respuesta",
    subtitle = "Cada panel muestra una persona",
    x = "Variable",
    y = "Tiempo de respuesta (ms)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 11),
    strip.text = element_text(size = 9, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Guardar perfiles
png(
  filename = "perfiles_individuales.png",
  width = 14,
  height = 10,
  units = "in",
  res = 300
)
print(perfiles_plot)
dev.off()
cat("Gráfico guardado: perfiles_individuales.png\n\n")

# 6. MATRIZ DE CORRELACIÓN: Relación entre variables por persona
# Crear matriz de datos ancha
matriz_datos <- datos_largos %>%
  pivot_wider(
    names_from = variable,
    values_from = tiempo
  ) %>%
  select(-persona_id) %>%
  select(all_of(vars_existentes))

# Calcular correlaciones
correlaciones <- cor(matriz_datos, use = "complete.obs")

# Convertir a formato largo para gráfico
correlaciones_long <- correlaciones %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(
    cols = -var1,
    names_to = "var2",
    values_to = "correlacion"
  ) %>%
  mutate(
    var1 = factor(var1, levels = vars_existentes),
    var2 = factor(var2, levels = vars_existentes)
  )

# Heatmap de correlaciones
correlacion_plot <- correlaciones_long %>%
  ggplot(aes(x = var1, y = var2, fill = correlacion)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = round(correlacion, 2)),
    color = "white",
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#E74C3C",
    mid = "white",
    high = "#3498DB",
    midpoint = 0,
    name = "Correlación"
  ) +
  labs(
    title = "Correlación entre Variables Dicotómicas",
    subtitle = "Basado en tiempos de respuesta por persona",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    panel.grid = element_blank()
  )

# Guardar correlaciones
png(
  filename = "correlaciones_variables.png",
  width = 8,
  height = 7,
  units = "in",
  res = 300
)
print(correlacion_plot)
dev.off()
cat("Gráfico guardado: correlaciones_variables.png\n\n")

# 7. TABLA RESUMEN VISUAL: Estadísticas clave por persona
# Crear tabla visual con estadísticas
tabla_resumen <- estadisticas_persona %>%
  mutate(
    media_formateada = paste0(round(media, 0), " ms"),
    mediana_formateada = paste0(round(mediana, 0), " ms"),
    rango_formateado = paste0(round(min, 0), " - ", round(max, 0), " ms")
  ) %>%
  select(persona_id, media_formateada, mediana_formateada, rango_formateado)

cat("=== RESUMEN POR PERSONA ===\n")
print(tabla_resumen)
cat("\n")

# Guardar tabla como CSV
write.csv(
  estadisticas_persona,
  file = "estadisticas_por_persona.csv",
  row.names = FALSE
)
cat("Tabla guardada: estadisticas_por_persona.csv\n\n")

cat("=== ANÁLISIS POR PERSONA COMPLETADO ===\n")
cat("Revisa los gráficos generados para explorar patrones individuales.\n")
