# =============================================================================
# Análisis del Tiempo Total (tott) de la Encuesta
# =============================================================================

# Cargar librerías necesarias
if (!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("gridExtra")) install.packages("gridExtra", dependencies = TRUE)
if (!require("scales")) install.packages("scales", dependencies = TRUE)
if (!require("viridis")) install.packages("viridis", dependencies = TRUE)

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(scales)
library(viridis)

# Cargar datos
datos <- read.csv("mapa-eh2-rt-anon.csv", stringsAsFactors = FALSE)

cat("=== ANÁLISIS DEL TIEMPO TOTAL (tott) ===\n\n")

# Verificar que existe la variable tott
if (!"tott" %in% names(datos)) {
  stop("La variable 'tott' no se encuentra en el dataset")
}

# Preparar datos
tott <- as.numeric(datos$tott)
tott_limpio <- tott[!is.na(tott) & tott > 0]

cat("Número de casos:", length(tott_limpio), "\n")
cat("Casos con NA o valores <= 0:", sum(is.na(tott) | tott <= 0), "\n\n")

# =============================================================================
# ESTADÍSTICAS DESCRIPTIVAS
# =============================================================================

# Calcular estadísticas básicas
estadisticas <- data.frame(
  n = length(tott_limpio),
  media = mean(tott_limpio),
  mediana = median(tott_limpio),
  sd = sd(tott_limpio),
  q25 = quantile(tott_limpio, 0.25),
  q75 = quantile(tott_limpio, 0.75),
  iqr = IQR(tott_limpio),
  min = min(tott_limpio),
  max = max(tott_limpio)
)

# Intentar calcular skewness
skewness_val <- NA
tryCatch({
  if (!require("moments", quietly = TRUE)) {
    install.packages("moments", dependencies = TRUE)
  }
  library(moments)
  skewness_val <- skewness(tott_limpio)
}, error = function(e) {
  skewness_val <<- NA
})
estadisticas$skewness <- skewness_val

# Convertir a minutos y segundos para interpretación
estadisticas_min <- data.frame(
  estadisticas,
  media_min = round(estadisticas$media / 60000, 2),
  mediana_min = round(estadisticas$mediana / 60000, 2),
  min_min = round(estadisticas$min / 60000, 2),
  max_min = round(estadisticas$max / 60000, 2)
)

cat("=== ESTADÍSTICAS DESCRIPTIVAS ===\n")
cat("En milisegundos:\n")
print(round(estadisticas[, c("n", "media", "mediana", "sd", "q25", "q75", "iqr", "min", "max")], 2))
cat("\nEn minutos:\n")
cat("  Media:", estadisticas_min$media_min, "min\n")
cat("  Mediana:", estadisticas_min$mediana_min, "min\n")
cat("  Mínimo:", estadisticas_min$min_min, "min\n")
cat("  Máximo:", estadisticas_min$max_min, "min\n")
if (!is.na(estadisticas$skewness)) {
  cat("  Asimetría:", round(estadisticas$skewness, 2), "\n")
}
cat("\n")

# Percentiles adicionales
percentiles <- quantile(tott_limpio, probs = c(0.05, 0.10, 0.90, 0.95))
cat("=== PERCENTILES ===\n")
cat("P5:", round(percentiles[1]/60000, 2), "min\n")
cat("P10:", round(percentiles[2]/60000, 2), "min\n")
cat("P90:", round(percentiles[3]/60000, 2), "min\n")
cat("P95:", round(percentiles[4]/60000, 2), "min\n")
cat("\n")

# =============================================================================
# FUNCIÓN PARA CREAR GRÁFICO COMBINADO
# =============================================================================

crear_grafico_combinado <- function(data, titulo) {
  # Preparar datos
  df <- data.frame(tiempo = data)
  
  # Calcular estadísticas
  stats <- data.frame(
    media = mean(data),
    mediana = median(data),
    q25 = quantile(data, 0.25),
    q75 = quantile(data, 0.75)
  )
  
  # 1. Histograma
  hist_plot <- ggplot(df, aes(x = tiempo)) +
    geom_histogram(
      bins = 30,
      fill = "#4A90E2",
      color = "white",
      alpha = 0.7
    ) +
    geom_vline(
      xintercept = stats$media,
      color = "#E74C3C",
      linetype = "dashed",
      linewidth = 1.2
    ) +
    geom_vline(
      xintercept = stats$mediana,
      color = "#2ECC71",
      linetype = "dashed",
      linewidth = 1.2
    ) +
    annotate(
      "text",
      x = stats$media,
      y = Inf,
      label = "Media",
      color = "#E74C3C",
      vjust = 1.5,
      hjust = 0.5,
      size = 3.5,
      fontface = "bold"
    ) +
    annotate(
      "text",
      x = stats$mediana,
      y = Inf,
      label = "Mediana",
      color = "#2ECC71",
      vjust = 1.5,
      hjust = 0.5,
      size = 3.5,
      fontface = "bold"
    ) +
    labs(
      title = "Histograma del Tiempo Total",
      x = "Tiempo total (milisegundos)",
      y = "Frecuencia"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10)
    ) +
    scale_x_continuous(labels = comma)
  
  # 2. Boxplot con puntos individuales
  box_plot <- ggplot(df, aes(x = "", y = tiempo)) +
    geom_boxplot(
      fill = "#3498DB",
      color = "#2C3E50",
      alpha = 0.7,
      outlier.shape = NA
    ) +
    geom_jitter(
      width = 0.2,
      height = 0,
      alpha = 0.6,
      color = "#34495E",
      size = 2
    ) +
    geom_point(
      data = data.frame(x = "", y = stats$mediana),
      aes(x = x, y = y),
      color = "#2ECC71",
      size = 4,
      shape = 18
    ) +
    labs(
      title = "Boxplot del Tiempo Total",
      x = "",
      y = "Tiempo total (milisegundos)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 10),
      axis.ticks.x = element_blank()
    ) +
    scale_y_continuous(labels = comma)
  
  # 3. Tabla de estadísticas
  stats_text <- paste0(
    "n = ", length(data), "\n",
    "Media = ", round(stats$media, 0), " ms\n",
    "  (", round(stats$media/60000, 2), " min)\n",
    "SD = ", round(sd(data), 0), " ms\n",
    "Mediana = ", round(stats$mediana, 0), " ms\n",
    "  (", round(stats$mediana/60000, 2), " min)\n",
    "IQR = ", round(IQR(data), 0), " ms\n",
    "Min = ", round(min(data), 0), " ms\n",
    "  (", round(min(data)/60000, 2), " min)\n",
    "Max = ", round(max(data), 0), " ms\n",
    "  (", round(max(data)/60000, 2), " min)"
  )
  
  stats_table <- ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = stats_text,
      size = 3.5,
      hjust = 0.5,
      vjust = 0.5,
      family = "mono"
    ) +
    theme_void() +
    labs(title = "Estadísticas Descriptivas") +
    theme(
      plot.title = element_text(
        size = 12,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 10)
      )
    )
  
  # Combinar gráficos
  grid.arrange(
    hist_plot,
    box_plot,
    stats_table,
    ncol = 3,
    widths = c(2, 2, 1.3),
    top = titulo
  )
}

# =============================================================================
# VISUALIZACIONES
# =============================================================================

# 1. Gráfico combinado principal
png(
  filename = "grafico_tiempo_total_combinado.png",
  width = 14,
  height = 5,
  units = "in",
  res = 300
)
crear_grafico_combinado(tott_limpio, "Análisis del Tiempo Total de la Encuesta")
dev.off()
cat("Gráfico guardado: grafico_tiempo_total_combinado.png\n\n")

# 2. Histograma con escala en minutos
hist_min <- ggplot(data.frame(tiempo_min = tott_limpio / 60000), aes(x = tiempo_min)) +
  geom_histogram(
    bins = 20,
    fill = "#9B59B6",
    color = "white",
    alpha = 0.7
  ) +
  geom_vline(
    xintercept = mean(tott_limpio) / 60000,
    color = "#E74C3C",
    linetype = "dashed",
    linewidth = 1.2
  ) +
  geom_vline(
    xintercept = median(tott_limpio) / 60000,
    color = "#2ECC71",
    linetype = "dashed",
    linewidth = 1.2
  ) +
  labs(
    title = "Distribución del Tiempo Total",
    subtitle = "Tiempo total de la encuesta por participante",
    x = "Tiempo total (minutos)",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  ) +
  scale_x_continuous(labels = comma)

png(
  filename = "histograma_tiempo_total_minutos.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300
)
print(hist_min)
dev.off()
cat("Gráfico guardado: histograma_tiempo_total_minutos.png\n\n")

# 3. Gráfico de barras por persona (ordenado)
datos$persona_id <- paste0("P", sprintf("%02d", 1:nrow(datos)))
datos$tott_num <- as.numeric(datos$tott)
datos_ordenados <- datos %>%
  filter(!is.na(tott_num) & tott_num > 0) %>%
  arrange(tott_num) %>%
  mutate(persona_id = factor(persona_id, levels = persona_id))

barras_personas <- ggplot(datos_ordenados, aes(x = persona_id, y = tott_num / 60000)) +
  geom_bar(stat = "identity", fill = "#E67E22", alpha = 0.8) +
  geom_hline(
    yintercept = mean(tott_limpio) / 60000,
    color = "#E74C3C",
    linetype = "dashed",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = length(datos_ordenados$persona_id) * 0.5,
    y = mean(tott_limpio) / 60000,
    label = paste0("Media = ", round(mean(tott_limpio) / 60000, 2), " min"),
    color = "#E74C3C",
    vjust = -0.5,
    size = 4,
    fontface = "bold"
  ) +
  labs(
    title = "Tiempo Total por Persona",
    subtitle = "Ordenado de menor a mayor tiempo",
    x = "Persona",
    y = "Tiempo total (minutos)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12)
  ) +
  scale_y_continuous(labels = comma)

png(
  filename = "barras_tiempo_total_personas.png",
  width = 12,
  height = 6,
  units = "in",
  res = 300
)
print(barras_personas)
dev.off()
cat("Gráfico guardado: barras_tiempo_total_personas.png\n\n")

# 4. Gráfico de densidad (violin plot + boxplot)
densidad_plot <- ggplot(data.frame(tiempo_min = tott_limpio / 60000), aes(x = "", y = tiempo_min)) +
  geom_violin(fill = "#3498DB", alpha = 0.5, color = NA) +
  geom_boxplot(width = 0.2, fill = "#E74C3C", alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "#34495E", size = 2) +
  labs(
    title = "Distribución del Tiempo Total",
    subtitle = "Violin plot + Boxplot + Puntos individuales",
    x = "",
    y = "Tiempo total (minutos)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.ticks.x = element_blank()
  ) +
  scale_y_continuous(labels = comma)

png(
  filename = "violin_tiempo_total.png",
  width = 8,
  height = 7,
  units = "in",
  res = 300
)
print(densidad_plot)
dev.off()
cat("Gráfico guardado: violin_tiempo_total.png\n\n")

# 5. Q-Q plot para verificar normalidad
qq_plot <- ggplot(data.frame(tiempo = tott_limpio), aes(sample = tiempo)) +
  stat_qq(alpha = 0.6, size = 2) +
  stat_qq_line(color = "#E74C3C", linewidth = 1) +
  labs(
    title = "Q-Q Plot del Tiempo Total",
    subtitle = "Para evaluar normalidad de la distribución",
    x = "Cuantiles teóricos",
    y = "Cuantiles observados"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

png(
  filename = "qqplot_tiempo_total.png",
  width = 8,
  height = 7,
  units = "in",
  res = 300
)
print(qq_plot)
dev.off()
cat("Gráfico guardado: qqplot_tiempo_total.png\n\n")

# 6. Análisis de outliers usando IQR
Q1 <- quantile(tott_limpio, 0.25)
Q3 <- quantile(tott_limpio, 0.75)
IQR_val <- IQR(tott_limpio)
limite_inferior <- Q1 - 1.5 * IQR_val
limite_superior <- Q3 + 1.5 * IQR_val

outliers <- tott_limpio[tott_limpio < limite_inferior | tott_limpio > limite_superior]

cat("=== ANÁLISIS DE OUTLIERS (método IQR) ===\n")
cat("Límite inferior:", round(limite_inferior/60000, 2), "min\n")
cat("Límite superior:", round(limite_superior/60000, 2), "min\n")
cat("Número de outliers:", length(outliers), "\n")
if (length(outliers) > 0) {
  cat("Outliers (en minutos):", paste(round(sort(outliers)/60000, 2), collapse = ", "), "\n")
}
cat("\n")

# Guardar estadísticas
write.csv(
  estadisticas_min,
  file = "estadisticas_tiempo_total.csv",
  row.names = FALSE
)
cat("Tabla guardada: estadisticas_tiempo_total.csv\n\n")

cat("=== ANÁLISIS DEL TIEMPO TOTAL COMPLETADO ===\n")
