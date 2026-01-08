# =============================================================================
# Análisis de Tiempos de Respuesta - Variables Open (Respuestas Abiertas)
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

# Identificar variables Open de tiempo de respuesta
# Variables que empiezan con "ot" son Open (respuestas abiertas)
vars_open <- c(
  "ot01_agey",
  "ot03_occu",
  "ot04_tliv",
  "ot40_wact",
  "ot41_word",
  "ot42_kno1",
  "ot43_kno2",
  "ot44_kno3",
  "ot45_kno4",
  "ot46_kno5"
)

# Verificar que las variables existen
vars_existentes <- vars_open[vars_open %in% names(datos)]
cat("Variables Open encontradas:", length(vars_existentes), "de", length(vars_open), "\n")
cat("Variables:", paste(vars_existentes, collapse = ", "), "\n\n")

# Verificación de unidades: mostrar algunos ejemplos para confirmar si son ms o seg
cat("=== VERIFICACIÓN DE UNIDADES ===\n")
cat("Mostrando algunos valores de ejemplo para verificar unidades:\n")
# Muestra solo las primeras 3 variables para no saturar
for (var in head(vars_existentes, 3)) {
  valores <- as.numeric(datos[[var]])
  valores <- valores[!is.na(valores) & valores > 0]
  if (length(valores) > 0) {
    ejemplos <- head(sort(valores), 5)
    cat("\n", var, ":\n")
    cat("  Ejemplos (5 menores):", paste(round(ejemplos, 2), collapse = ", "), "\n")
    cat("  Si son milisegundos:", paste(round(ejemplos/1000, 2), "seg", collapse = ", "), "\n")
    cat("  Si son segundos:", paste(round(ejemplos/60, 2), "min", collapse = ", "), "\n")
    cat("  Mediana:", round(median(valores), 2), "\n")
    if (median(valores) < 10000) {
      cat("  -> Parece que son MILISEGUNDOS (mediana =", round(median(valores)/1000, 2), "seg)\n")
    } else {
      cat("  -> Podrían ser SEGUNDOS (mediana =", round(median(valores)/60, 2), "min)\n")
    }
  }
}
cat("\n")

# Función para calcular estadísticas descriptivas
calcular_estadisticas <- function(x, nombre_var) {
  x_limpio <- as.numeric(x)
  x_limpio <- x_limpio[!is.na(x_limpio) & x_limpio > 0]  # Eliminar NAs y valores no válidos
  
  if (length(x_limpio) == 0) {
    return(data.frame(
      variable = nombre_var,
      n = 0,
      media = NA,
      sd = NA,
      mediana = NA,
      q25 = NA,
      q75 = NA,
      iqr = NA,
      min = NA,
      max = NA
    ))
  }
  
  data.frame(
    variable = nombre_var,
    n = length(x_limpio),
    media = mean(x_limpio),
    sd = sd(x_limpio),
    mediana = median(x_limpio),
    q25 = quantile(x_limpio, 0.25),
    q75 = quantile(x_limpio, 0.75),
    iqr = IQR(x_limpio),
    min = min(x_limpio),
    max = max(x_limpio)
  )
}

# Función para identificar outliers usando IQR
identificar_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- IQR(x, na.rm = TRUE)
  limite_inferior <- Q1 - 1.5 * IQR_val
  limite_superior <- Q3 + 1.5 * IQR_val
  return(x >= limite_inferior & x <= limite_superior)
}

# Calcular estadísticas para cada variable Open
cat("Calculando estadísticas por variable...\n")
estadisticas_individuales <- map_dfr(vars_existentes, function(var) {
  calcular_estadisticas(datos[[var]], var)
})

# Imprimir estadísticas individuales (resumen)
cat("=== ESTADÍSTICAS POR VARIABLE (Resumen) ===\n")
print(estadisticas_individuales %>% 
  select(variable, n, media, mediana, sd, min, max) %>%
  arrange(media))
cat("\n")

# Guardar estadísticas completas
write.csv(
  estadisticas_individuales,
  file = "estadisticas_open_individuales.csv",
  row.names = FALSE
)
cat("Estadísticas guardadas: estadisticas_open_individuales.csv\n\n")

# Combinar todos los tiempos de respuesta de variables Open
todos_tiempos <- datos %>%
  select(all_of(vars_existentes)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "tiempo") %>%
  mutate(tiempo = as.numeric(tiempo)) %>%
  filter(!is.na(tiempo) & tiempo > 0)

# Estadísticas combinadas
estadisticas_combinadas <- calcular_estadisticas(todos_tiempos$tiempo, "Todas las Open")
cat("=== ESTADÍSTICAS COMBINADAS (Todas las variables Open) ===\n")
print(estadisticas_combinadas)
cat("\n")

# Función para crear gráfico combinado (histograma + boxplot con puntos)
crear_grafico_combinado <- function(data, var_name, titulo) {
  # Preparar datos
  data_limpio <- data %>%
    filter(!is.na(tiempo) & tiempo > 0)
  
  if (nrow(data_limpio) == 0) {
    return(NULL)
  }
  
  # Calcular estadísticas para el gráfico
  stats <- calcular_estadisticas(data_limpio$tiempo, var_name)
  
  # Crear histograma
  hist_plot <- ggplot(data_limpio, aes(x = tiempo)) +
    geom_histogram(bins = 30, fill = "#E67E22", color = "white", alpha = 0.7) +
    geom_vline(xintercept = stats$media, color = "#E74C3C", linetype = "dashed", linewidth = 1.2) +
    geom_vline(xintercept = stats$mediana, color = "#2ECC71", linetype = "dashed", linewidth = 1.2) +
    annotate("text", x = stats$media, y = Inf, label = "Media", 
             color = "#E74C3C", vjust = 1.5, hjust = 0.5, size = 3, fontface = "bold") +
    annotate("text", x = stats$mediana, y = Inf, label = "Mediana", 
             color = "#2ECC71", vjust = 1.5, hjust = 0.5, size = 3, fontface = "bold") +
    labs(
      title = paste0("Histograma: ", titulo),
      x = "Tiempo de respuesta (milisegundos)",
      y = "Frecuencia"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10)
    ) +
    scale_x_continuous(labels = comma)
  
  # Crear boxplot con puntos individuales
  box_plot <- ggplot(data_limpio, aes(x = "", y = tiempo)) +
    geom_boxplot(
      fill = "#E67E22",
      color = "#2C3E50",
      alpha = 0.7,
      outlier.shape = NA  # Ocultar outliers del boxplot para mostrarlos como puntos
    ) +
    geom_jitter(
      width = 0.2,
      height = 0,
      alpha = 0.4,
      color = "#34495E",
      size = 0.8
    ) +
    geom_point(
      data = data.frame(x = "", y = stats$mediana),
      aes(x = x, y = y),
      color = "#2ECC71",
      size = 3,
      shape = 18
    ) +
    labs(
      title = paste0("Boxplot con puntos: ", titulo),
      x = "",
      y = "Tiempo de respuesta (ms)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 10),
      axis.ticks.x = element_blank()
    ) +
    scale_y_continuous(labels = comma)
  
  # Crear tabla de estadísticas
  stats_text <- paste0(
    "n = ", stats$n, "\n",
    "Media = ", round(stats$media, 2), " ms\n",
    "SD = ", round(stats$sd, 2), " ms\n",
    "Mediana = ", round(stats$mediana, 2), " ms\n",
    "IQR = ", round(stats$iqr, 2), " ms\n",
    "Min = ", round(stats$min, 2), " ms\n",
    "Max = ", round(stats$max, 2), " ms"
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
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5, margin = margin(b = 10))
    )
  
  # Combinar gráficos
  grid.arrange(
    hist_plot,
    box_plot,
    stats_table,
    ncol = 3,
    widths = c(2, 2, 1.2),
    top = paste0("Análisis de Tiempos de Respuesta: ", titulo)
  )
}

# Crear gráfico combinado para todas las variables Open juntas
cat("Generando gráfico combinado (todas las variables Open)...\n\n")

png(
  filename = "grafico_todas_open.png",
  width = 14,
  height = 5,
  units = "in",
  res = 300
)

crear_grafico_combinado(todos_tiempos, "Todas", "Todas las Variables Open")

dev.off()

cat("Gráfico guardado: grafico_todas_open.png\n\n")

# Versión sin outliers del gráfico combinado
cat("Generando gráfico combinado SIN OUTLIERS...\n\n")

# Identificar outliers en todos los tiempos combinados
todos_tiempos_sin_outliers_combinado <- todos_tiempos %>%
  filter(identificar_outliers(tiempo))

png(
  filename = "grafico_todas_open_sin_outliers.png",
  width = 14,
  height = 5,
  units = "in",
  res = 300
)

crear_grafico_combinado(todos_tiempos_sin_outliers_combinado, "Todas", "Todas las Variables Open (SIN OUTLIERS)")

dev.off()

cat("Gráfico guardado: grafico_todas_open_sin_outliers.png\n\n")

# Crear gráfico comparativo: todas las variables en un solo panel
cat("Generando gráfico comparativo...\n\n")

# Boxplot comparativo (con rotación de etiquetas para que se vean)
boxplot_comparativo <- ggplot(todos_tiempos, aes(x = variable, y = tiempo, fill = variable)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 0.6, color = "#34495E") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Comparación de Tiempos de Respuesta entre Variables Open",
    subtitle = paste("Total de", length(vars_existentes), "variables"),
    x = "Variable",
    y = "Tiempo de respuesta (ms)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = comma)

# Guardar gráfico comparativo
png(
  filename = "grafico_comparativo_open.png",
  width = 12,
  height = 7,
  units = "in",
  res = 300
)

print(boxplot_comparativo)
dev.off()

cat("Gráfico guardado: grafico_comparativo_open.png\n\n")

# Versión sin outliers del gráfico comparativo
cat("Generando gráfico comparativo SIN OUTLIERS...\n\n")

# Identificar outliers por variable
todos_tiempos_sin_outliers <- todos_tiempos %>%
  group_by(variable) %>%
  mutate(es_outlier = !identificar_outliers(tiempo)) %>%
  ungroup() %>%
  filter(!es_outlier)

boxplot_comparativo_sin_outliers <- ggplot(todos_tiempos_sin_outliers, aes(x = variable, y = tiempo, fill = variable)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 0.6, color = "#34495E") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Comparación de Tiempos de Respuesta entre Variables Open",
    subtitle = paste("SIN OUTLIERS - Total de", length(vars_existentes), "variables"),
    x = "Variable",
    y = "Tiempo de respuesta (ms)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = comma)

png(
  filename = "grafico_comparativo_open_sin_outliers.png",
  width = 12,
  height = 7,
  units = "in",
  res = 300
)

print(boxplot_comparativo_sin_outliers)
dev.off()

cat("Gráfico guardado: grafico_comparativo_open_sin_outliers.png\n\n")

# Versión ordenada por mediana del gráfico comparativo sin outliers
cat("Generando gráfico comparativo SIN OUTLIERS ordenado por mediana...\n\n")

# Calcular medianas por variable para ordenar
medianas_por_variable <- todos_tiempos_sin_outliers %>%
  group_by(variable) %>%
  summarise(mediana = median(tiempo), .groups = "drop") %>%
  arrange(mediana)

# Crear factor ordenado por mediana
todos_tiempos_sin_outliers_ordenado <- todos_tiempos_sin_outliers %>%
  mutate(
    variable_ordenada = factor(variable, levels = medianas_por_variable$variable)
  )

boxplot_comparativo_sin_outliers_ordenado <- ggplot(
  todos_tiempos_sin_outliers_ordenado, 
  aes(x = variable_ordenada, y = tiempo, fill = variable_ordenada)
) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 0.6, color = "#34495E") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Comparación de Tiempos de Respuesta entre Variables Open",
    subtitle = paste("SIN OUTLIERS - Ordenado por mediana - Total de", length(vars_existentes), "variables"),
    x = "Variable (ordenadas por mediana)",
    y = "Tiempo de respuesta (ms)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = comma)

png(
  filename = "grafico_comparativo_open_sin_outliers_ordenado_mediana.png",
  width = 12,
  height = 7,
  units = "in",
  res = 300
)

print(boxplot_comparativo_sin_outliers_ordenado)
dev.off()

cat("Gráfico guardado: grafico_comparativo_open_sin_outliers_ordenado_mediana.png\n\n")

# Crear gráfico de distribución de medias por variable
cat("Generando gráfico de distribución de medias...\n\n")

medias_plot <- estadisticas_individuales %>%
  ggplot(aes(x = reorder(variable, media), y = media)) +
  geom_bar(stat = "identity", fill = "#E67E22", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = media - sd, ymax = media + sd),
    width = 0.3,
    color = "#2C3E50",
    linewidth = 0.5
  ) +
  labs(
    title = "Media de Tiempos de Respuesta por Variable Open",
    subtitle = "Ordenadas de menor a mayor tiempo promedio",
    x = "Variable",
    y = "Tiempo promedio (ms)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12)
  ) +
  scale_y_continuous(labels = comma)

png(
  filename = "barras_medias_open.png",
  width = 12,
  height = 7,
  units = "in",
  res = 300
)

print(medias_plot)
dev.off()

cat("Gráfico guardado: barras_medias_open.png\n\n")

cat("=== ANÁLISIS COMPLETADO ===\n")
cat("Revisa los gráficos generados para identificar posibles outliers.\n")
cat("Total de variables Open analizadas:", length(vars_existentes), "\n")
